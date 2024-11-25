#!/usr/bin/env Rscript
# Load an RData file and run the app. Optionally specify a localhost port.
# Run this script from the same directory as app.R.
# usage: ./run.R <RData> [port]

library(shiny)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
	port = as.integer(args[2])
	if (is.na(port))
		stop(sprintf("invalid port: '%s'", args[2]))
	options(shiny.port = port)
} else if (length(args) != 1) {
	stop("usage: ./run.R <RData> [port]")
}

load(args[1])
runApp()
