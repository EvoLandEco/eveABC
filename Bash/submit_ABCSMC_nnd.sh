#!/bin/bash
#SBATCH --time=7-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=64
#SBATCH --job-name=edd_ABCSMC_nnd
#SBATCH --output=logs/edd_ABCSMC_nnd-%j.log
#SBATCH --mem=128GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

name=${1}

ml R

Rscript ~/repos/eveABC/Script/run_ABCSMC_nnd.R ${name}
