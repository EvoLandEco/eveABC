#!/bin/bash
#SBATCH --time=7-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=10
#SBATCH --job-name=edd_ABCSMC_ed
#SBATCH --output=logs/edd_ABCSMC_ed-%j.log
#SBATCH --mem=64GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

name=${1}

ml R

Rscript ~/repos/eveABC/Script/run_ABCSMC_ed.R ${name}
