#!/usr/bin/env python

import os
from subprocess import call

os.chdir('./regression')
call(['./clear_regression_results.py'])
