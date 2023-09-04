#!/bin/bash
#SBATCH --time=3-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --job-name=edd_ABCSMC_accu_nnd
#SBATCH --output=logs/edd_ABCSMC_accu_nnd-%j.log
#SBATCH --mem=32GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

name=${1}

ml R

Rscript ~/repos/eveABC/Script/run_ABCSMC_accu_nnd.R ${name}
