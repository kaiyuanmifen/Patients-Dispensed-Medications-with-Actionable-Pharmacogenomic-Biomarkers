#!/bin/bash

#SBATCH -c 4
#SBATCH -N 1
#SBATCH -t 0-04:35                         # Runtime in D-HH:MM format
#SBATCH -p short                           # Partition to run in
#SBATCH --mem=40000

Rscript Co_dispenses.R $1
