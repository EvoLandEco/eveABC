#!/bin/bash
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --job-name=edd_ABCSMC_accu_nnd_init
#SBATCH --output=logs/edd_ABCSMC_accu_nnd_init-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=short

name=${1}

ml R

Rscript -e "devtools::install_github('EvoLandEco/eve')"
Rscript -e "devtools::install_github('EvoLandEco/eveABC')"
Rscript -e "install.packages('EasyABC')"

sbatch ~/repos/eveABC/Bash/submit_ABCSMC_accu_nnd.sh ${name}