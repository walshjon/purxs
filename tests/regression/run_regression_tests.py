#!/usr/bin/env python

import os
from subprocess import call

os.chdir('./system')
call(['./run_system_tests.py'])
