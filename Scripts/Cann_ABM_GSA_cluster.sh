#!/bin/bash

# setting the home directory
#SBATCH -D /home/mjculsha

# making sure I use serial nodes only
#SBATCH --partition=serial

# set the number of nodes to 1
#SBATCH --nodes=1

# set number of processes per node
#SBATCH --ntasks-per-node=16

# set max wall time to 2 hours
#SBATCH --time=02:00:00

# set the name of the job
#SBATCH --job-name=cannibal

# mail alerts at beginning and end
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# send mail here
#SBATCH --mail-user=mjculshawmaurer@ucdavis.edu

# create an array of tasks, keeping it to 10 for testing
#SBATCH --array=1-10

# start job from the directory it was submitted
cd $SLURM_SUBMIT_DIR

# load NetLogo
module load java netlogo

bash netlogo-headless.sh \
  --model /home/mjculsha/Cann_ABM_remote/Cannibalism_Infection_Model_8_14_17.nlogo \
  --setup-file /home/mjculsha/Cann_ABM_remote/Scripts/GSAExperiments.xml \
  --experiment Test$SLURM_ARRAY_TASK_ID \
  --table /home/mjculsha/Cann_ABM_remote/GSA_Outputs/GSA_Output_Test$SLURM_ARRAY_TASK_ID.csv