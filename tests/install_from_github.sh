#!/usr/bin/env bash
set -euo pipefail

# ============================================================
# PuzzleApp GitHub Installer & Runner
#
# This script installs PuzzleApp from GitHub, either temporarily
# (for quick testing) or persistently (to a specified R library path).
#
# Usage examples:
#
# 1. Default quick test (temporary install, auto-cleanup)
#    ./install_puzzleapp.sh
#
# 2. Persistent install to a specific R library path
#    ./install_puzzleapp.sh --dest /data/Rlibs/puzzleapp
#
# 3. Install from a specific branch (latest commit)
#    ./install_puzzleapp.sh --branch dev
#
# 4. Install from a specific commit
#    ./install_puzzleapp.sh --commit 3afc93a
#
# 5. Install from a specific version tag
#    ./install_puzzleapp.sh --version v0.9.6
#
# ============================================================

# --- HPC module setup (customize as needed) ---
module load intel-compiler-llvm/2025.2.0
module load intel-mkl/2025.2.0
module load R/4.5.0

# --- Default settings ---
REPO="git@github.com:KCCGGenomeTechLab/puzzleapp.git"
BRANCH="pack"
COMMIT=""
VERSION=""
DEST=""

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
  case $1 in
    --branch) BRANCH="$2"; shift 2 ;;
    --commit) COMMIT="$2"; shift 2 ;;
    --version) VERSION="$2"; shift 2 ;;
    --dest) DEST="$2"; shift 2 ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--branch BRANCH | --commit HASH | --version TAG] [--dest PATH]"
      exit 1
      ;;
  esac
done

# --- Validate options ---
if [[ -n "$COMMIT" && -n "$VERSION" ]]; then
  echo "Error: --commit and --version cannot be used together."
  exit 1
fi

# --- Working directories ---
if [[ -n "$DEST" ]]; then
  tmp_root="$DEST"
  echo "Installing persistently into: $DEST"
else
  tmp_root="$(mktemp -d)"
  echo "Using temporary working root: $tmp_root"
  cleanup() {
    echo "Cleaning up: $tmp_root"
    rm -rf "$tmp_root" || true
  }
  trap cleanup EXIT INT TERM
fi

work_dir="$tmp_root/puzzleapp_repo"
local_lib="$tmp_root/Rlib"

# --- Clone repo ---
echo "Cloning repository from: $REPO"

if [[ -n "$VERSION" ]]; then
  echo "Checking out version tag: $VERSION"
  git clone --branch "$VERSION" --depth 1 "$REPO" "$work_dir"
elif [[ -n "$COMMIT" ]]; then
  echo "Checking out specific commit: $COMMIT"
  git clone "$REPO" "$work_dir"
  cd "$work_dir"
  git checkout "$COMMIT"
else
  echo "Checking out branch: $BRANCH (latest commit)"
  git clone --branch "$BRANCH" --depth 1 "$REPO" "$work_dir"
  cd "$work_dir"
fi

# --- Print resolved commit hash ---
resolved_commit=$(git rev-parse HEAD)
echo "Resolved commit hash: $resolved_commit"

# --- Write R runner script ---
cat > run_puzzleapp.R <<EOF
local_lib <- "$local_lib"
if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(local_lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"))
options(Ncpus = max(1L, parallel::detectCores(logical = TRUE) - 1L))

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_local(".", lib = local_lib, dependencies = TRUE, upgrade = "never")

library(puzzleapp, lib.loc = local_lib)
puzzleapp::run_app()
EOF

# --- Run the app ---
echo "Starting PuzzleApp (commit: $resolved_commit)"
Rscript --vanilla run_puzzleapp.R
