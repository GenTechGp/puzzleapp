#!/usr/bin/env Rscript
# load an RData file and run the app
# usage: ./run.R data.RData

library(shiny)

args <- commandArgs(trailingOnly=TRUE)
load(args[1])
runApp()
