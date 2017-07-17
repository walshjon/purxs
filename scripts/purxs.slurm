#!/bin/bash -l

export EXE=openmc

#SBATCH -n 1
#SBATCH -p $PARTITION
#SBATCH -t 12:00:00

srun -n1 $EXE $PWD --purxs # PURXS-only mode, no OpenMC transport
#srun -n1 $EXE $PWD         # regular OpenMC transport simulation using PURXS for processing, as specified in urr.xml
