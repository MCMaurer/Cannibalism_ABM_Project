#!/bin/bash
task(){
	java -Xmx1024m -Dfile.encoding=UTF-8 -classpath /Applications/NetLogo_6.0/Java/netlogo-6.0.0.jar \
  org.nlogo.headless.Main \
  --model /Users/MJ/GitHub/Cannibalism_ABM_Project/Cannibalism_Infection_Model_8_14_17.nlogo \
  --setup-file /Users/MJ/GitHub/Cannibalism_ABM_Project/GSAExperiments.xml \
  --experiment Test$1 \
  --table /Users/MJ/GitHub/Cannibalism_ABM_Project/GSA_Outputs/GSA_Output_Test$1.csv
}

N=4
(
for x in {1..10}; do 
   ((i=i%N)); ((i++==0)) && wait
   task "$x" & 
done
)