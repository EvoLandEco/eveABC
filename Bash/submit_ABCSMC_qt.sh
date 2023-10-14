#!/bin/bash
#SBATCH --time=5-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --job-name=edd_ABCSMC_qt
#SBATCH --output=logs/edd_ABCSMC_qt-%j.log
#SBATCH --mem=16GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

# Get the command line arguments
name=$1
nsim=$2
metric=$3
set=$4

ml R
Rscript -e "devtools::install_github('EvoLandEco/eve')"
Rscript -e "devtools::install_github('EvoLandEco/eveABC')"

Rscript ../Script/run_ABCSMC_accu_qt.R ${name} ${nsim} ${metric} ${set}