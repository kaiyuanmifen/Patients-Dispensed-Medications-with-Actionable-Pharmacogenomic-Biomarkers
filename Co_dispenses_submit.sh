#!/bin/bash

for i in {1..50}
	do
	  sbatch Co_dispenses.sh "$i" 

	done

