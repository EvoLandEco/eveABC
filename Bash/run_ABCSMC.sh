#!/bin/bash
#SBATCH --time=04:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --job-name=edd_ABCSMC
#SBATCH --output=logs/edd_ABCSMC-%j.log
#SBATCH --mem=32GB
#SBATCH --partition=regular

name=${1}

ml R

Rscript -e "devtools::install_github('EvoLandEco/eve')"
Rscript -e "devtools::install_github('EvoLandEco/eveABC')"
Rscript -e "install.packages('EasyABC')"

Rscript ~/repos/eveABC/Script/run_ABCSMC.R ${name}