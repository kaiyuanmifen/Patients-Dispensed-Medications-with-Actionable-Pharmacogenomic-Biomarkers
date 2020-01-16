#!/bin/bash


#SBATCH -t 0-08:05                         # Runtime in D-HH:MM format
#SBATCH -p short                           # Partition to run in
#SBATCH --mem=95000

Rscript DownloadData.R
