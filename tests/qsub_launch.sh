#!/usr/bin/env bash
#
# EDIT the -P and -l storage lines below before submitting.
#
# storage must list gdata/if89 (where the puzzleapp module lives) AND every
# filesystem you need, including the one you submit from and the one holding
# your data. A filesystem missing from this list is mounted read-only in the
# job, which is easy to mistake for a permissions problem.
#
#PBS -P <your-project>
#PBS -N puzzleapp_test
#PBS -q normal
#PBS -l walltime=02:00:00
#PBS -l ncpus=4
#PBS -l mem=64GB
#PBS -l storage=gdata/if89+gdata/<your-project>+scratch/<your-project>
#PBS -l wd
#PBS -W umask=0022

set -euo pipefail

# Pick a port (override at submit time: qsub -v PORT=8895 ...)
PORT="${PORT:-8895}"
NODE="$(hostname -s)"

INFO=$(cat <<EOF
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
)

# Print to stdout first, so the instructions always reach the PBS .o file.
# Writing the file is best-effort: if the submission directory is on a
# filesystem missing from -l storage it is mounted read-only, and under
# `set -e` a failed redirect would otherwise kill the job before it starts.
echo "${INFO}"

INFO_FILE="${PBS_O_WORKDIR:-$HOME}/ssh_connect_${PBS_JOBID}.txt"
# Subshell, with stderr redirected outside it: a failed redirection is reported
# by the shell itself before the command's own 2>/dev/null would apply, so
# without this the .e file gets a bare error that looks like a job failure.
if ( printf '%s\n' "${INFO}" > "${INFO_FILE}" ) 2>/dev/null; then
  echo "Connection instructions also written to: ${INFO_FILE}"
else
  echo "NOTE: could not write ${INFO_FILE} - is that filesystem in -l storage?"
  echo "      The instructions above still apply."
fi

# The if89 modulefiles are always needed, because the puzzleapp module
# soft-prereqs Rlib/4.4.2 from there. MODULE_DIR additionally points at a
# testing install, e.g.
#   qsub -v PORT=8895,MODULE_DIR=$HOME/ables-software-testing/modules qsub_launch.sh
MODULE_DIR="${MODULE_DIR:-/g/data/if89/apps/modulefiles}"
PUZZLEAPP_MODULE="${PUZZLEAPP_MODULE:-puzzleapp/0.2.1}"

module use /g/data/if89/apps/modulefiles
module use "${MODULE_DIR}"
module load "${PUZZLEAPP_MODULE}"

# --- Run the app ---
echo "Starting PuzzleApp at remote port ${PORT}"
puzzleapp --version
puzzleapp --port "${PORT}"
