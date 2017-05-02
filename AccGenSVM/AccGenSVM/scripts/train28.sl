#!/bin/bash
#SBATCH -J OpenMP_JOB
#SBATCH -A uoa00440         # Project Account
#SBATCH --time=02:00:00     # Walltime
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=1   # 8 OpenMP Threads

module load Java/1.8.0_40

