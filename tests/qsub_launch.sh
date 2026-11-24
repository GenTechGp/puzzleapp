#!/usr/bin/env bash
#
# Start the puzzleapp web app on a Gadi compute node.
#
# EDIT the -P and -l storage lines below before submitting.
#
# storage must list gdata/if89 (where the puzzleapp module lives) AND every
# filesystem you need, including the one you submit from and the one holding
# your data. A filesystem missing from this list is invisible to the job,
# which is easy to mistake for a permissions problem.
#
#   qsub tests/qsub_launch.sh
#   qsub -v PORT=8895 tests/qsub_launch.sh                       # fixed port
#   qsub -v PUZZLEAPP_SIF=/path/to/puzzleapp.sif tests/qsub_launch.sh   # test image
#
#PBS -P <your-project>
#PBS -N puzzleapp
#PBS -q normal
#PBS -l walltime=02:00:00
#PBS -l ncpus=4
#PBS -l mem=16GB
#PBS -l jobfs=10GB
#PBS -l storage=gdata/if89+gdata/<your-project>+scratch/<your-project>
#PBS -l wd
# Owner-only files: the job output and ssh_connect file contain the access token.
#PBS -W umask=0077

set -euo pipefail
umask 0077

# Compute nodes are shared, so pick a random port unless one is given.
PORT="${PORT:-$(shuf -i 20000-40000 -n 1)}"
NODE="$(hostname -s)"
PUZZLEAPP_MODULE="${PUZZLEAPP_MODULE:-ghcr.io/gentechgp/puzzleapp/0.2.4}"

# Access token. Passed in the environment, which other users cannot read
# (unlike command-line arguments, which any user on the node can list).
PUZZLEAPP_TOKEN="$(head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n')"
export PUZZLEAPP_TOKEN

INFO=$(cat <<EOF
============================================================
puzzleapp job started on node: ${NODE}
Remote app port (on compute node): ${PORT}

1. On your laptop, open the tunnel (leave it running):

  ssh -N -L 8898:localhost:${PORT} -J ${USER}@gadi.nci.org.au ${USER}@${NODE}

2. Open this link (keep the token - it is your access key; do not share it):

  http://localhost:8898/?token=${PUZZLEAPP_TOKEN}

The app takes about 20 seconds to start listening.

If local port 8898 is already in use, change it in both the ssh command
and the link (8898 -> 8899, 8900, ...).

The ssh command appears to "hang" - this is normal. Press Ctrl+C to close
the tunnel. Stop the app with
  qdel ${PBS_JOBID}
============================================================
EOF
)

# Print to stdout first, so the instructions always reach the PBS .o file.
# Writing the file is best-effort: if the submission directory is on a
# filesystem missing from -l storage, a failed redirect under `set -e` would
# otherwise kill the job before it starts.
echo "${INFO}"

INFO_FILE="${PBS_O_WORKDIR:-$HOME}/ssh_connect_${PBS_JOBID}.txt"
# Subshell, with stderr redirected outside it: a failed redirection is reported
# by the shell itself before the command's own 2>/dev/null would apply.
if ( printf '%s\n' "${INFO}" > "${INFO_FILE}" ) 2>/dev/null; then
  echo "Connection instructions also written to: ${INFO_FILE}"
else
  echo "NOTE: could not write ${INFO_FILE} - is that filesystem in -l storage?"
  echo "      The instructions above still apply."
fi

# The if89 SHPC modules do not load singularity themselves.
module load singularity

# Listen on the node's loopback only; the SSH tunnel delivers traffic there.
if [[ -n "${PUZZLEAPP_SIF:-}" ]]; then
  echo "Starting puzzleapp from ${PUZZLEAPP_SIF} on port ${PORT}"
  singularity exec "${PUZZLEAPP_SIF}" \
    Rscript /usr/local/bin/puzzleapp-entrypoint.R webapp "${PORT}" 127.0.0.1
else
  module use -a /g/data/if89/shpcroot/modules
  module load "${PUZZLEAPP_MODULE}"
  echo "Starting puzzleapp (${PUZZLEAPP_MODULE}) on port ${PORT}"
  puzzleapp webapp "${PORT}" 127.0.0.1
fi
