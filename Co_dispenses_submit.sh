#!/bin/bash

for i in {1..10}
	do
	  sbatch Co_dispenses.sh "$i" 

	done

