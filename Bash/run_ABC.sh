#!/bin/bash
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=edd_ABC_init
#SBATCH --output=logs/edd_ABC_init-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=short

ml R

Rscript -e "devtools::install_github('EvoLandEco/eve')"
Rscript -e "devtools::install_github('EvoLandEco/eveABC')"

name=${1}
nrep=${2}

for (( param_set = 1; param_set <= 150; param_set++ ))
do
sbatch ~/repos/eveABC/Bash/submit_ABC.sh ${name} \
                                    ${param_set} \
                                    ${nrep}
done