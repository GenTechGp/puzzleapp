# Setup preferences

outdir <- Sys.getenv("OUTDIR")
if (outdir == "") {
    outdir <- "."
}
pref <- reactiveValues(outdir = outdir, variants = character(0), panelapp = character(0), phenotype = character(0))

# Define preferences directory path
pref_dir <- file.path(isolate(pref$outdir), "preferences")

# Check if the preferences directory exists, if not, create it
if (!dir.exists(pref_dir)) {
  dir.create(pref_dir, recursive = TRUE)
}
