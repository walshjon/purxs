#!/usr/bin/env python

import os
from glob import glob

num_objects = len(os.listdir('.'))
num_dirs = 0
for i_obj in range(num_objects):
  if os.path.isdir(os.listdir('.')[i_obj]):
    num_dirs += 1

for i_dir in range(1,num_dirs+1):
  os.chdir('./case-'+str(i_dir))
  files = (glob('statepoint*')
           + glob('error.log')
           + glob('*txt')
           + glob('*dat')
           + glob('*endf')
           + glob('*core')
           + glob('summary*'))
  for f in files:
    os.remove('./'+f)
  os.chdir('..')

os.remove('./test_system_results.dat')
