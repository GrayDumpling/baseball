#!/bin/bash
#SBATCH --job-name=batting_avg
#SBATCH -p hns,normal
#SBATCH --time=24:00:00
#SBATCH --mail-user=ffcai@stanford.edu
#SBATCH --mail-type=ALL
#SBATCH --array=0-20

Rscript batting_average.R
