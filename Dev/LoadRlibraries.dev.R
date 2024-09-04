#rm(list = ls())

setwd("/g/data/kr68/andre/shinyApp")

#.libPaths(c("/g/data/kr68/andre/R_libs",.libPaths()))
.libPaths(c("/g/data/kr68/andre/R_libs"))

options(scipen = 999)

outdir <- Sys.getenv("OUTDIR")

Sys.setenv(TRACKS_DIR = sprintf("%s/tracks",outdir))

tracks_dir <- Sys.getenv("TRACKS_DIR")

# Check if the directory exists
if (dir.exists(tracks_dir)) {
  # Directory exists, delete all files in it
  files <- list.files(tracks_dir, full.names = TRUE)
  file.remove(files)
  cat("Existing directory found. Deleted all files in:", tracks_dir, "\n")
} else {
  # Directory does not exist, create it
  dir.create(tracks_dir, recursive = TRUE)
  cat("Directory did not exist. Created directory:", tracks_dir, "\n")
}

print("Loading libraries...")
suppressMessages({
  library(shinyWidgets) # ok
  library(data.table) # ok
  library(stringr) # ok
  library(shinyBS) # ok
  library(DT) # ok
  library(shinyjs) # ok
  library(igvShiny) # ok
  library(VariantAnnotation) # ok
  library(GenomicAlignments) # ok
  library(liftOver) # ok
  library(plotly) # ok
  library(ggplot2) # ok
  library(tidyr) # ok
  library(dplyr) # ok
  library(openxlsx) # ok
  library(shinyalert) # ok
  library(shinybusy)
  library(sortable)
})

print("Libraries succesfully loaded!")

file.edit("Dev/runShinyApp.dev.R")