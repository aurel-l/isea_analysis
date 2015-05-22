#!/usr/bin/python3

import argparse
import csv
import re
import time
import os
import warnings
import pymysql
import yaml
import subprocess


# load config file
try:
    config_file = open(
        os.path.join(os.path.dirname(__file__), 'config.yaml'),
        'r'
    )
except FileNotFoundError:
    print('This script expects a config.yaml file')
    print('It must contain at least a user and a passwd field')
    raise
config = yaml.load(config_file)

# variables
var = {
    'host':   config.get('host', 'localhost'),
    'port':   config.get('port', 3306),
    'user':   config.get('user'),
    'passwd': config.get('passwd'),
    'db':     config.get('db', 'repast'),
    'param': [
        'run', 'date', 'migrationProb', 'startingDistributionFile',
        'modelVersion', 'randomSeed', 'poissonMean', 'initialDemeAgentNumber',
        'graphFile', 'growthRate', 'marriageThres'
    ],
    'admix': [
        'run', 'date', 'Island', 'DnaAdmixture', 'AutosomeAdmixture',
        'XChrAdmixture', 'YChrAdmixture', 'MitoAdmixture'
    ]
}


def paramGenerator(param, date, version):
    unique_keys = set()
    for p in param:
        key = (p['run'], date)
        if (key not in unique_keys):
            unique_keys.add(key)
            yield (
                p['run'],
                date,
                p['migrationProb'],
                p['startingDistributionFile'].rsplit('/', 1)[-1],
                version,
                p['randomSeed'],
                p['poissonMean'],
                p['initialDemeAgentNumber'],
                p['graphFile'].rsplit('/', 1)[-1],
                p['growthRate'],
                p['marriageThres']
            )


def admixGenerator(admix, date):
    for a in admix:
        yield (
            a['run'],
            date,
            a['Island'],
            a['DnaAdmixture'],
            a['AutosomeAdmixture'],
            a['XChrAdmixture'],
            a['YChrAdmixture'],
            a['MitoAdmixture']
        )

# parse arguments
parser = argparse.ArgumentParser(description='fills the DB')
parser.add_argument(
    'files', type=str, nargs='+',
    help='repast output files that needs to be stored in the DB'
)
parser.add_argument(
    '--version', '-v', type=int,
    help='version of the model used'
)
args = parser.parse_args()


# DB access
conn = pymysql.connect(
    host=var['host'], port=var['port'], user=var['user'], passwd=var['passwd'],
    db=var['db'], autocommit=False
)

for f in args.files:
    # change path if path is not absolute
    if (not f.startswith('/')):
        f = '../R/{}'.format(f)
    # execute merge.R
    sp = subprocess.Popen(
        '../R/merge.R {}'.format(f),
        stdout=subprocess.PIPE,
        shell=True
    )

    # read pipe
    csv_text = sp.stdout.read().decode("utf-8")

    # extract date
    date = time.strptime(
        re.search('(\d{4}\..{3}\.\d{2}\.(\d{2}_){2}\d{2})', f).group(1),
        "%Y.%b.%d.%H_%M_%S"
    )

    cur = conn.cursor()

    cur.executemany(
        'INSERT INTO batch_param ({}) values ({})'.format(
            ', '.join(var['param']),
            ', '.join('%s' for _ in var['param'])
        ),
        paramGenerator(
            csv.DictReader(csv_text.splitlines()),
            date,
            args.version
        )
    )

    warnings.filterwarnings('ignore', 'Data truncated*')
    cur.executemany(
        'INSERT INTO admixtureByNode ({}) values ({})'.format(
            ', '.join(var['admix']),
            ', '.join('%s' for _ in var['admix'])
        ),
        admixGenerator(csv.DictReader(csv_text.splitlines()), date)
    )

    cur.close()
    conn.commit()

conn.close()
