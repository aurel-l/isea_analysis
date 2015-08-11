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
    print('It must contain at least a user and a passwd field')
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
    'param': [
        'run', 'date', 'migrationProb', 'startingDistributionFile',
        'modelVersion', 'randomSeed', 'poissonMean', 'initialDemeAgentNumber',
        'graphFile', 'growthRate', 'marriageThres'
    ],
    'admix': [
        'run', 'date', 'Island', 'DnaAdmixture', 'AutosomeAdmixture',
        'XChrAdmixture', 'YChrAdmixture', 'MitoAdmixture'
    ],
    'sep': re.compile('"?,"?'),
    'n_islands': 21,
    'merging_script': '{}/R_modules/merge.R'.format(
        # gets the path of the current script
        os.path.dirname(os.path.realpath(__file__))
    )
}

# raises an error if the merging script path is incorrect
if (not os.path.isfile(var['merging_script'])):
    raise(FileNotFoundError('missing merge.R script'))

# SQL string templates
var['sql'] = {
    # inserting parameter values to the batch_param table
    'param': 'INSERT INTO batch_param ({}) values ({})'.format(
        ', '.join(var['param']),
        ', '.join('%s' for _ in var['param'])
    ),
    # inserting admixture values to the admixtureByNode table
    'admix': 'INSERT INTO admixtureByNode ({}) values ({})'.format(
        ', '.join(var['admix']),
        ', '.join('%s' for _ in var['admix'])
    )
}


# creates a parameter value generator from non-changing data
# the generator outputs a tuple of values from row data
# the tuple can be used as is to create an INSERT query from the template
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


# creates a admixture value generator from non-changing data
# the generator outputs a tuple of values from row data
# the tuple can be used as is to create an INSERT query from the template
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

# parses CLI arguments
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
    help=(
        'commit the simulations to the database'
        ' without asking for user confirmation'
    )
)
args = parser.parse_args()

# treats every admixture file provided by the user, one at a time
for f in args.files:
    print('processing {}...'.format(f))

    # DB access
    # opens a connection to the database
    conn = pymysql.connect(
        host=var['host'], port=var['port'], db=var['db'],
        user=var['user'], passwd=var['passwd'],
        # disables autocommit, just in case something happens in the middle
        autocommit=False
    )
    # data truncation is expected, so filters out those warnings
    warnings.filterwarnings('ignore', 'Data truncated*')

    # extracts date from file name
    date = time.strptime(
        re.search('(\d{4}\..{3}\.\d{2}\.(\d{2}_){2}\d{2})', f).group(1),
        "%Y.%b.%d.%H_%M_%S"
    )

    # executes merging script in an independant subprocess
    sp = subprocess.Popen(
        '{} -pf {}'.format(var['merging_script'], f),
        # sets a reference to the subprocess' stdout pipe
        stdout=subprocess.PIPE,
        shell=True
    )

    # column name as key, index of the column as value
    indices = {
        col: i for (i, col) in
        # gets the information from the first line of the subprocess (header)
        enumerate(re.findall('\w+', next(sp.stdout).decode('utf-8')))
    }

    # creates a generator for parameters, providing values that will no change
    param_g = param_generator(indices, date, args.version)
    # kicks off the generator
    next(param_g)
    # creates a generator for admixtures, providing values that will no change
    admix_g = admix_generator(indices, date)
    # kicks off the generator
    next(admix_g)

    # gets a cursor from the database connection
    cur = conn.cursor()

    loop = 0
    # reads a new line from the output pipe from the merge.R subprocess
    line = next(sp.stdout).decode('utf-8')
    # loops until 'line' is empty
    while line:
        # splits the row to extract every value
        row = re.split(var['sep'], line.split('\n')[0])

        if (loop % var['n_islands'] == 0):
            # once for every simulation, store parameter values
            cur.execute(var['sql']['param'], param_g.send(row))

        # store admixture values
        cur.execute(var['sql']['admix'], admix_g.send(row))

        try:
            # reads a new line from the subprocess
            line = next(sp.stdout).decode('utf-8')
        except StopIteration:
            # if the pipe is empty, sets 'line' to '' to finish the loop
            line = ''
        loop += 1

    # closes the cursor
    cur.close()
    # gets the user input about commiting the changes
    # either from the arguments or asks for it
    user_input = args.yes or input(
        'Are you sure you want to store these simulations in the DB [Y/n] '
    )
    # if the user is OK with the changes
    if (user_input.lower() in [True, '', 'y', 'yes', 'sure', 'yep', 'yeah']):
        # commit the changes
        conn.commit()
    # and closes the connection
    conn.close()
