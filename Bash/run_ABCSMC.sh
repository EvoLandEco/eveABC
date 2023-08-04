#!/bin/bash
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --job-name=edd_ABCSMC_init
#SBATCH --output=logs/edd_ABCSMC_init-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=short

name=${1}

ml R

Rscript -e "devtools::install_github('EvoLandEco/eve')"
Rscript -e "devtools::install_github('EvoLandEco/eveABC')"
Rscript -e "install.packages('EasyABC')"

sbatch ~/repos/eveABC/Bash/submit_ABCSMC_pd.sh ${name}
sbatch ~/repos/eveABC/Bash/submit_ABCSMC_ed.sh ${name}
sbatch ~/repos/eveABC/Bash/submit_ABCSMC_nnd.sh ${name}