#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(puzzleapp))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: <preprocess|pipeline|webapp> [args...]   (webapp [port] [host])")
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
    # webapp [port] [host]. 0.0.0.0 is needed under Docker (-p forwards to the
    # container's interface). Under Singularity the app shares the host network,
    # so on a shared machine pass 127.0.0.1 and reach it through an SSH tunnel.
    # Access needs the ?token=... URL printed at startup; set PUZZLEAPP_TOKEN
    # to choose the token.
    port <- if (length(rest) >= 1) as.integer(rest[1]) else 8888
    host <- if (length(rest) >= 2) rest[2] else "0.0.0.0"
    run_app(port = port, host = host)
  },
  stop("Unknown subcommand: ", subcommand, ". Expected one of: preprocess, pipeline, webapp")
)
