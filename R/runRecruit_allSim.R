#####################################
# Run all recruitment simulations
# Will Vieira
#####################################

# simulation info
simInfo <- yaml::read_yaml('_simulation_info.yml')

spIds <- simInfo$spIds
simName <- simInfo$simName
simulations <- simInfo$simulations
nChains <- simInfo$nC



for(Sim in 1:length(simulations))
{
    bash <- paste0("#!/bin/bash
#SBATCH --account=def-dgravel
#SBATCH -t 4-20:00:00
#SBATCH --mem-per-cpu=2000M
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=", nChains, "
#SBATCH --job-name=", simulations[Sim], "
#SBATCH --mail-user=willian.vieira@usherbrooke.ca
#SBATCH --mail-type=FAIL
#SBATCH --array=1-", length(spIds), "

R -f R/runRecruit_sim", Sim, ".R")

    # create sub file
    writeLines(bash, 'sub.sh')

    # submit job
    if(system('hash sbatch') == 0)
    {
        system('sbatch sub.sh')
    }else{
        print('Sbatch not found, skipping job submission.')
    }
}
