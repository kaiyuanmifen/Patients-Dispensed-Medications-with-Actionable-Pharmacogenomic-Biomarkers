#!/bin/bash

#after running the scripts on SQL server

#download data
sbatch DownloadData.sh

#processing data 
sbatch MAPBCount.sh

#running co-dispenses 

./Co_dispenses.sh

#run analysis of co_dispenses 

Rscript Co_dispenses_analysis_aggregation.R

#visualize all results 

Rscript Visualization.R