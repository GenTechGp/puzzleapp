#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(puzzleapp))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: <preprocess|pipeline|webapp> [args...]")
}
subcommand <- args[1]
rest <- args[-1]

switch(subcommand,
  preprocess = {
    if (length(rest) < 1) stop("Usage: preprocess <config.yaml>")
    run_preprocess(config_yaml = rest[1])
  },
  pipeline = {
    if (length(rest) < 2) stop("Usage: pipeline <config.yaml> <filter_table.tsv> [output_dir]")
    output_dir <- if (length(rest) >= 3) rest[3] else "pipeline_output"
    run_pipeline(config_yaml = rest[1], filter_table = rest[2], output_dir = output_dir)
  },
  webapp = {
    port <- if (length(rest) >= 1) as.integer(rest[1]) else 8888
    run_app(port = port, host = "0.0.0.0")
  },
  stop("Unknown subcommand: ", subcommand, ". Expected one of: preprocess, pipeline, webapp")
)
