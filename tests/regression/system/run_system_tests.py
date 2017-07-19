#!/usr/bin/env python

import os
from subprocess import call

purxs_only = [8, 9, 10, 11, 12, 13]
num_objects = len(os.listdir('.'))
num_dirs = 0
for i_obj in range(num_objects):
  if os.path.isdir(os.listdir('.')[i_obj]):
    num_dirs += 1

for i_dir in range(1,num_dirs+1):
  os.chdir('./case-'+str(i_dir))
  call(['echo', 'Running system test case-'+str(i_dir)+'...'])
  if i_dir in purxs_only:
    call(['../../../../../build/bin/openmc', '.', '--purxs'])
  else:
    call(['../../../../../build/bin/openmc', '.'])
  os.chdir('..')
