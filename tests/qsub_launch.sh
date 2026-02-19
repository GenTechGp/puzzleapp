#!/usr/bin/env bash
#PBS -P project
#PBS -N puzzleapp_test
#PBS -q normal
#PBS -l walltime=01:00:00
#PBS -l ncpus=1
#PBS -l mem=16GB
#PBS -l storage=gdata/kr68+gdata/if89
#PBS -l wd
#PBS -W umask=0022

set -euo pipefail

# change these to if89 places later
APP_DIR="/g/data/if89/testdir/puzzleapp/app/27112025/puzzleapp_repo/"
RLIBS="/g/data/if89/testdir/puzzleapp/app/27112025/Rlib"

# Pick a port (override at submit time: qsub -v PORT=8895 ...)
PORT="${PORT:-8895}"
NODE="$(hostname -s)"
INFO_DIR="/g/data/$PROJECT/$(whoami)/puzzleapp_sessions"
# INFO_DIR="/g/data/kr68/puzzleapp/Scripts/shiny_sessions"
mkdir -p "${INFO_DIR}"

INFO_FILE="${INFO_DIR}/in_${PBS_JOBID}.txt"

cat > "${INFO_FILE}" <<EOF
============================================================
Shiny job started on node: ${NODE}
Remote app port (on compute node): ${PORT}

To open the app in your browser, run this on your laptop:

  ssh -L <LOCAL_PORT>:localhost:${PORT} gadi "ssh -N -L ${PORT}:localhost:${PORT} ${NODE}"

Example:
  ssh -L 8898:localhost:${PORT} gadi "ssh -N -L ${PORT}:localhost:${PORT} ${NODE}"

Then open:
  http://localhost:<LOCAL_PORT>

If the port is already in use:
  - Change only the LOCAL port (left side of -L)
  - e.g. 8898 -> 8899, 8900, etc.

You do NOT need to change the remote port (${PORT}).

Note: the ssh command will stay running and appear to "hang" — this is normal.
Leave it open and open the URL in your browser.

To stop the tunnel:
  - Press Ctrl+C in the terminal running ssh
  - If you closed that terminal, find the process and kill it:
      ps aux | grep 'localhost:${PORT}'
      kill <PID>
============================================================
EOF

echo "Connection instructions written to:"
echo "  ${INFO_FILE}"

module load intel-compiler-llvm/2025.2.0
module load intel-mkl/2025.2.0
module load R/4.5.0

# --- Write R runner script ---
cat > run_puzzleapp.R <<EOF
app_dir <- "$APP_DIR"
rlibs <- "$RLIBS"
setwd(app_dir)
.libPaths(c(rlibs, .libPaths()))
devtools::load_all(); run_app(port=${PORT})
EOF

# --- Run the app ---
echo "Starting PuzzleApp at remoteport ${PORT}"
Rscript --vanilla run_puzzleapp.R


