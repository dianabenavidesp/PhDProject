#!/bin/bash
#SBATCH -A uoa00440
#SBATCH --ntasks=12
#SBATCH --cpus-per-task=1
#SBATCH --time=10:00:00
#SBATCH --mem-per-cpu=2G
module load R

# Give this script doMPI_example.R or snow_example.R as an argument (ie: $1)
# Our R has a patched copy of the snow library so that there is no need to use RMPISNOW.
srun Rscript samples5.R