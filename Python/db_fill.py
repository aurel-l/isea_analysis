#!/usr/bin/python3

import argparse
import csv
import re
import time
import pymysql
import pandas

# variables
var = {
    'host':   'localhost',
    'port':   3306,
    'user':   'aluciani',
    'passwd': 'db',
    'db':     'repast',
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
    for p in param:
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


def admixGenerator(admix, date, exclude_runs = set()):
    for (run, island), a in admix.iterrows():
        if not run in exclude_runs:
            yield (
                run,
                date,
                island,
                str(a['DnaAdmixture']),
                str(a['AutosomeAdmixture']),
                str(a['XChrAdmixture']),
                str(a['YChrAdmixture']),
                str(a['MitoAdmixture'])
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
    cur = conn.cursor()

    # read files
    param = csv.DictReader(open(f.replace('.txt', '.batch_param_map.txt')))
    admix = pandas.read_csv(f)

    # extract Island information
    admix['Island'] = pandas.Series(
        [x[0:4] for x in admix['Label']],
        index=admix.index
    )

    exclude_runs = set()
    for obs in [
        'DnaAdmixture', 'AutosomeAdmixture', 'XChrAdmixture',
        'YChrAdmixture', 'MitoAdmixture'
    ]:
        exclude_runs = exclude_runs.union(
            set(
                admix[pandas.isnull(admix[obs])]['run']
            )
        )

    # extract date
    date = time.strptime(
        re.search('(\d{4}\..{3}\.\d{2}\.(\d{2}_){2}\d{2})', f).group(1),
        "%Y.%b.%d.%H_%M_%S"
    )

    cur.executemany(
        'INSERT INTO batch_param ({}) values ({})'.format(
            ', '.join(var['param']),
            ', '.join('%s' for _ in var['param'])
        ),
        paramGenerator(param, date, args.version)
    )

    cur.executemany(
        'INSERT INTO admixtureByNode ({}) values ({})'.format(
            ', '.join(var['admix']),
            ', '.join('%s' for _ in var['admix'])
        ),
        admixGenerator(
            admix.groupby(['run', 'Island']).mean(),
            date,
            exclude_runs
        )
    )

    cur.close()
    conn.commit()

conn.close()
