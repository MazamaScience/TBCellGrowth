#!/bin/bash
#SBATCH -N 1
#SBATCH -n 2
#SBATCH --mem-per-cpu=15625
#SBATCH --time=00:05:00
#SBATCH --output=%j.log

module load R/3.2.1

./Jon_test.R \
--inputDir="/nearline/sherman-ngs/KM_Temp_Imaging/Microfluidics/BSL3/CellAsic, RvC, RPL22, & pEXCF-0023, 6-29-15" \
--outputDir="/nethome/jcallahan/Projects/Sherman/Results/Jun29" \
--chambers="xy01" \
--channels="c1" --channelNames="phase" \
--nFrames="8" \
--verbose

###R CMD BATCH ~/R/test-png.R

