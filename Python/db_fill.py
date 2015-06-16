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
    ],
    'sep': re.compile('"?,"?')
}
var['sql'] = {
    'param': 'INSERT INTO batch_param ({}) values ({})'.format(
        ', '.join(var['param']),
        ', '.join('%s' for _ in var['param'])
    ),
    'admix': 'INSERT INTO admixtureByNode ({}) values ({})'.format(
        ', '.join(var['admix']),
        ', '.join('%s' for _ in var['admix'])
    )
}


def param_generator(indices, date, version):
    row = yield
    while True:
        row = yield (
            row[indices['run']],
            date,
            row[indices['migrationProb']],
            row[indices['startingDistributionFile']].rsplit('/', 1)[-1],
            version,
            row[indices['randomSeed']],
            row[indices['poissonMean']],
            row[indices['initialDemeAgentNumber']],
            row[indices['graphFile']].rsplit('/', 1)[-1],
            row[indices['growthRate']],
            row[indices['marriageThres']]
        )


def admix_generator(indices, date):
    row = yield
    while True:
        row = yield (
            row[indices['run']],
            date,
            row[indices['Island']],
            row[indices['DnaAdmixture']],
            row[indices['AutosomeAdmixture']],
            row[indices['XChrAdmixture']],
            row[indices['YChrAdmixture']],
            row[indices['MitoAdmixture']]
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
parser.add_argument(
    '--yes', '-y', action='store_true',
    help='version of the model used'
)
args = parser.parse_args()

for f in args.files:
    print('processing {}...'.format(f))

    # DB access
    conn = pymysql.connect(
        host=var['host'], port=var['port'], user=var['user'],
        passwd=var['passwd'], db=var['db'], autocommit=False
    )
    warnings.filterwarnings('ignore', 'Data truncated*')

    # extract date
    date = time.strptime(
        re.search('(\d{4}\..{3}\.\d{2}\.(\d{2}_){2}\d{2})', f).group(1),
        "%Y.%b.%d.%H_%M_%S"
    )

    # change path if path is not absolute
    #if (not f.startswith('/')):
    #    f = '../R/{}'.format(f)
    # execute merge.R
    script_path = os.path.dirname(os.path.realpath(__file__))
    sp = subprocess.Popen(
        '{}/R_modules/merge.R -p -f {}'.format(script_path, f),
        stdout=subprocess.PIPE,
        shell=True
    )

    indices = {
        col: i for (i, col) in
        enumerate(re.findall('\w+', next(sp.stdout).decode('utf-8')))
    }

    param_g = param_generator(indices, date, args.version)
    next(param_g)
    admix_g = admix_generator(indices, date)
    next(admix_g)

    cur = conn.cursor()

    loop = 0
    line = next(sp.stdout).decode('utf-8')
    while line:
        row = re.split(var['sep'], line.split('\n')[0])

        if (loop % 21 == 0):
            # once for every simulation, store param values
            cur.execute(var['sql']['param'], param_g.send(row))

        cur.execute(var['sql']['admix'], admix_g.send(row))

        try:
            line = next(sp.stdout).decode('utf-8')
        except StopIteration:
            line = ''
        loop += 1

    cur.close()
    user_input = args.yes or input(
        'Are you sure you want to store these simulations in the DB [Y/n] '
    )
    if (user_input.lower() in [True, '', 'y', 'yes', 'sure', 'yep', 'yeah']):
        conn.commit()
    conn.close()
