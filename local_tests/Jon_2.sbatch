#!/bin/bash
#SBATCH -N 1
#SBATCH -n 2
#SBATCH --mem-per-cpu=15625
#SBATCH --time=00:05:00
#SBATCH --output=%j.log

module load R/3.2.1

./Jon_test.R \
--inputDir="/nearline/sherman-ngs/KM_Temp_Imaging/Microfluidics/BSL3/CellAsic, RvC, limiting PI, 9-1-15" \
--dataDir="Experimental images" \
--outputDir="/nethome/jcallahan/Projects/Sherman/Results/Sep01" \
--chambers="xy01,xy02,xy03" \
--channels="c1,c3" \
--channelNames="phase,green" \
--startFrame=8 \
--backgroundIndex=2 \
--verbose


