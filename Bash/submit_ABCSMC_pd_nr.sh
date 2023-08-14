#!/bin/bash
#SBATCH --time=7-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --job-name=edd_ABCSMC_pd
#SBATCH --output=logs/edd_ABCSMC_pd-%j.log
#SBATCH --mem=32GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

name=${1}

ml R

Rscript ~/repos/eveABC/Script/run_ABCSMC_pd_nr.R ${name}
