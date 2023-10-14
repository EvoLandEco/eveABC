#!/bin/bash
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --job-name=edd_ABCSMC_qt_init
#SBATCH --output=logs/edd_ABCSMC_qt_init-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=short

# Get the command line arguments
name=$1
nsim=$2

# Check if the necessary arguments are provided
if [[ -z $name ]] || [[ -z $nsim ]]; then
    echo "Usage: $0 <name> <nsim>"
    exit 1
fi

# Check if name is a string (note: in bash, everything is a string, so this check is somewhat redundant)
if [[ ! $name =~ ^[a-zA-Z0-9_]+$ ]]; then
    echo "Error: name must be a string."
    exit 1
fi

# Check if nsim is an integer
if [[ ! $nsim =~ ^[0-9]+$ ]]; then
    echo "Error: nsim must be an integer."
    exit 1
fi

# Loop over the specified strings
for metric in pd ed nnd
do
    # Loop over the range 1 to 4
    for set in {1..4}
    do
        # Run the Rscript command
        sbatch submit_ABCSMC_qt.sh $name $nsim $metric $set
    done
done