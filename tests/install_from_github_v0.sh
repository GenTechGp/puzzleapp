#!/usr/bin/env bash
set -euo pipefail

# This script tests installation from GitHub in a clean, isolated R library.
# It clones the repo into a temporary working directory, installs, and runs the app.

# Load modules (HPC environments)
module load intel-compiler-llvm/2025.2.0
module load intel-mkl/2025.2.0
module load R/4.5.0

# Create a temporary working root (will hold both the clone and the R library)
tmp_root="$(mktemp -d)"
echo "Using temporary working root: $tmp_root"

# Paths inside the temp root
work_dir="$tmp_root/puzzleapp_github"
local_lib="$tmp_root/Rlib"

# Always clean up the temp root (even on Ctrl+C)
cleanup() {
  echo "Cleaning up: $tmp_root"
  rm -rf "$tmp_root" || true
}
trap cleanup EXIT INT TERM

# Shallow clone the 'pack' branch into the temp work_dir
# git clone --branch pack --depth 1 https://github.com/KCCGGenomeTechLab/puzzleapp.git "$work_dir"
git clone --branch pack --depth 1 git@github.com:KCCGGenomeTechLab/puzzleapp.git "$work_dir"
cd "$work_dir"

# Write an R runner with the library path hard-coded
cat > run_puzzleapp.R <<EOF
local_lib <- "$local_lib"

if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)

# Prefer the temp lib for installs and loads
.libPaths(c(local_lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Install the package and its dependencies into the temp lib
remotes::install_local(".", lib = local_lib, dependencies = TRUE, upgrade = "never")

# Run the app from the temp lib
library(puzzleapp, lib.loc = local_lib)
puzzleapp::run_app()
EOF

# Run the app (Ctrl+C to stop)
Rscript --vanilla run_puzzleapp.R