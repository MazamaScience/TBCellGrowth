#!/bin/bash
#SBATCH -N 1
#SBATCH -n 3
#SBATCH --mem-per-cpu=15625
#SBATCH --time=00:05:00
#SBATCH --output=%j.log

module load R/3.2.1

./flow_exec.R \
--inputDir="/nearline/sherman-ngs/KM_Temp_Imaging/Microfluidics/BSL3/CellAsic, RvC, limiting PI, 9-1-15" \
--dataDir="Experimental images" \
--outputDir="/nearline/sherman-ngs/KM_Temp_Imaging/Microfluidics/BSL3/CellAsic, RvC, limiting PI, 9-1-15/TBCellGrowth_analyzed" \
--chambers="xy06" \
--channels="c1,c3" \
--channelNames="phase,green" \
--startFrame=8 \
--backgroundIndex=2 \
--verbose


