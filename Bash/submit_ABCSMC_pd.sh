#!/bin/bash
#SBATCH --time=5-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=8
#SBATCH --job-name=edd_ABCSMC_pd
#SBATCH --output=logs/edd_ABCSMC_pd-%j.log
#SBATCH --mem=16GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

name=${1}

ml R

Rscript ~/repos/eveABC/Script/run_ABCSMC_pd.R ${name}
