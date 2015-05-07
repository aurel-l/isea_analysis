#!/usr/bin/python3

import argparse
import sys
import os
import yaml
import pymysql

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
    'defaults': [
        'batch_param.run', 'batch_param.date', 'migrationProb',
        'startingDistributionFile', 'modelVersion', 'randomSeed',
        'poissonMean', 'initialDemeAgentNumber', 'graphFile', 'growthRate',
        'marriageThres', 'Island', 'DnaAdmixture', 'AutosomeAdmixture',
        'XChrAdmixture', 'YChrAdmixture', 'MitoAdmixture'
    ]
}

# parse arguments
parser = argparse.ArgumentParser(description='queries the DB')
parser.add_argument(
    '--conditions', '-c', type=str,
    help='condition of query'
)
parser.add_argument(
    '--query', '-q', type=str,
    help='DB query'
)
parser.add_argument(
    '--outfile', '-o', type=str,
    help='output file'
)
args = parser.parse_args()

if args.query:
    # security check
    not_secure = False
    not_secure |= not args.query.strip().upper().startswith('SELECT')
    not_secure |= any(
        not_allowed in args.query.upper()
        for not_allowed in ['DELETE', 'DROP', 'UPDATE']
    )
    if not_secure:
        raise(Warning('Not a secure query'))
else:
    args.query = 'SELECT '
    args.query += ', '.join(var['defaults'])
    args.query += ' FROM batch_param INNER JOIN admixtureByNode'
    args.query += ' ON batch_param.date = admixtureByNode.date'
    args.query += ' AND batch_param.run = admixtureByNode.run'

if args.conditions:
    args.query += ' WHERE ' + args.conditions

# DB access
conn = pymysql.connect(
    host=var['host'], port=var['port'], user=var['user'], passwd=var['passwd'],
    db=var['db'], autocommit=False
)
cur = conn.cursor()
cur.execute(args.query)

# connect to output
if args.outfile:
    output = open(args.outfile, 'w', newline='')
else:
    output = sys.stdout

# write to csv
output.write(','.join(h.rsplit('.', 1)[-1] for h in var['defaults']))
output.write('\n')
for row in cur:
    output.write(','.join(str(cell) for cell in row))
    output.write('\n')

cur.close()
conn.commit()
conn.close()
