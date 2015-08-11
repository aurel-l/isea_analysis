#!/usr/bin/python3

import argparse
import sys
import os
import yaml
import pymysql

# opens config file
try:
    config_file = open(
        os.path.join(os.path.dirname(__file__), 'config.yaml'),
        'r'
    )
# no config file found, intercepts error
except FileNotFoundError:
    # displays useful message
    print('This script expects a config.yaml file')
    print('It must contain at least a \'user\' and a \'passwd\' field')
    # re-raise same error
    raise
# loads config file
config = yaml.load(config_file)

# variables
var = {
    # from config file
    'host':   config.get('host', 'localhost'),
    'port':   config.get('port', 3306),
    'user':   config.get('user'),
    'passwd': config.get('passwd'),
    'db':     config.get('db', 'repast'),
    # other defaults
    'defaults': [
        'batch_param.run', 'batch_param.date', 'migrationProb',
        'startingDistributionFile', 'modelVersion', 'randomSeed',
        'poissonMean', 'initialDemeAgentNumber', 'graphFile', 'growthRate',
        'marriageThres', 'Island', 'DnaAdmixture', 'AutosomeAdmixture',
        'XChrAdmixture', 'YChrAdmixture', 'MitoAdmixture'
    ]
}

# parses CLI arguments
parser = argparse.ArgumentParser(description='queries the DB')
parser.add_argument(
    '--conditions', '-c', type=str,
    help='conditions of query'
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

# if a query was provided, checks it to see if it is somewhat secure
if args.query:
    not_secure = False
    # has to start with SELECT
    not_secure |= not args.query.strip().upper().startswith('SELECT')
    # disallow certain modifying keywords
    not_secure |= any(
        not_allowed in args.query.upper()
        for not_allowed in ['DELETE', 'DROP', 'UPDATE']
    )
    if not_secure:
        # raises a warning and stops here
        raise(Warning('Not a secure query'))
# if no query provided, builds it
else:
    args.query = 'SELECT '
    # fields
    args.query += ', '.join(var['defaults'])
    # joining tables
    args.query += ' FROM batch_param INNER JOIN admixtureByNode'
    # joining fields
    args.query += ' ON batch_param.date = admixtureByNode.date'
    args.query += ' AND batch_param.run = admixtureByNode.run'

# if conditions provided
if args.conditions:
    # adds the conditions to the query
    args.query += ' WHERE ' + args.conditions

# adds to the query the ordering of rows by date and run,
# so that rows from the same simulation are together (important!)
args.query += ' ORDER BY batch_param.date, batch_param.run'

# DB access
# opens a connection to the database
conn = pymysql.connect(
    host=var['host'], port=var['port'], db=var['db'],
    user=var['user'], passwd=var['passwd'],
    # disables autocommit, just in case something happens in the middle
    autocommit=False,
    # important for streaming of the rows
    # otherwise, it would just load all rows before being able to read them
    cursorclass=pymysql.cursors.SSCursor
)
# gets a cursor from the database connection
cur = conn.cursor()
# asks the cursor to execute the query
cur.execute(args.query)

# opens the output
if args.outfile:
    # either the one provided by the user
    output = open(args.outfile, 'w', newline='')
else:
    # or stdout
    output = sys.stdout

try:
    # writes in csv format
    # starts with the header
    output.write(','.join(h.rsplit('.', 1)[-1] for h in var['defaults']))
    output.write('\n')
    # for every result row in the cursor
    for row in cur:
        # writes it to the output
        output.write(','.join(str(cell) for cell in row))
        output.write('\n')
except BrokenPipeError:
    # catch broken pipe (e.g. when using 'head' command)
    output.close()
finally:
    # cleans up
    cur.close()
    conn.commit()
    conn.close()
