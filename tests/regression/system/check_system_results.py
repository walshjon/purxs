#!/usr/bin/env python

import os
from glob import glob
import sys
import subprocess
from subprocess import call
import h5py as h5

num_objects = len(os.listdir('.'))
num_dirs = 0
for i_obj in range(num_objects):
  if os.path.isdir(os.listdir('.')[i_obj]):
    num_dirs += 1

test_results = []
for i_dir in range(1,num_dirs+1):
  os.chdir('./case-'+str(i_dir))
  state_files = glob('statepoint*')
  dat_files = glob('*dat')
  err_files = glob('error.log')
  if len(state_files + dat_files + err_files) == 0:
    sys.exit('No output files for case-'+str(i_dir))
  elif len(err_files) != 0:
    sys.exit('Error log files present for case-'+str(i_dir))
  elif len(state_files) > 1:
    sys.exit('Multiple statepoint output files for case-'+str(i_dir))

  if len(state_files) == 1:
    hf = h5.File(state_files[0], 'r')
    test_results.append(str(hf['k_combined'][0])+' '+str(hf['k_combined'][1]))
  elif len(dat_files) == 1:
    test_results.append(subprocess.check_output('tail -n3 '+dat_files[0]+' | head -n1', shell=True).rstrip())
  elif len(dat_files) == 2:
    test_results.append(subprocess.check_output('tail -n1 '+dat_files[0], shell=True).rstrip()+' '+subprocess.check_output('tail -n1 '+dat_files[1], shell=True).rstrip())
  os.chdir('..')

test_file = open('test_system_results.dat', 'w')
for result in test_results:
  test_file.write(result+'\n')
test_file.close()

ref_results = []
ref_file = open('reference_system_results.dat', 'r')
for line in ref_file:
  ref_results.append(line)
ref_file.close()
test_results = []
test_file = open('test_system_results.dat', 'r')
for line in test_file:
  test_results.append(line)
test_file.close()

for i_result in range(len(ref_results)):
  if ref_results[i_result] == test_results[i_result]:
    call(['echo', 'case-'+str(i_result+1)+' passed'])
  else:
    call(['echo', 'case-'+str(i_result+1)+' failed'])
