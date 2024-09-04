#!/bin/bash
#PBS -P kr68
#PBS -l storage=gdata/kr68+gdata/if89
#PBS -q normal
#PBS -l ncpus=8
#PBS -l mem=64gb
#PBS -l jobfs=32GB
#PBS -l wd
#PBS -o /g/data/kr68/andre/shinyApp/Modules/logfiles
#PBS -e /g/data/kr68/andre/shinyApp/Modules/logfiles
#PBS -l walltime=1:00:00

module load R/4.3.1

Rscript /g/data/kr68/andre/shinyApp/Modules/Process_All_data.R ${SAMPLE}
