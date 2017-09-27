#!/bin/bash
rsync -avze 'ssh -p 2022' mjculsha@farm.cse.ucdavis.edu:/home/mjculsha/Cann_ABM_remote/GSA_Outputs \
/Users/MJ/GitHub/Cannibalism_ABM_Project
