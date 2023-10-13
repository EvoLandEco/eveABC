#!/bin/bash
#SBATCH --time=2-2:00:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --job-name=edd_ABCSMC_create_target
#SBATCH --output=logs/edd_ABCSMC_create_target-%j.log
#SBATCH --mem=32GB
#SBATCH --partition=regular
#SBATCH --mail-type=ALL

nrep=${1}

ml R

Rscript -e "devtools::install_github('EvoLandEco/eve')"
Rscript -e "devtools::install_github('EvoLandEco/eveABC')"
Rscript -e "install.packages('EasyABC')"

Rscript ~/repos/eveABC/Script/run_ABCSMC_create_target.R ${nrep}
