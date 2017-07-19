#!/usr/bin/env python

import os
from subprocess import call

os.chdir('./system')
call(['./check_system_results.py'])
