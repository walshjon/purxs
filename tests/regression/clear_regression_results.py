#!/usr/bin/env python

import os
from subprocess import call

os.chdir('./system')
call(['./clear_system_results.py'])
