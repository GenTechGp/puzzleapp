# Setup preferences

outdir <- Sys.getenv("OUTDIR")
if (outdir == "") {
    outdir <- "."
}
pref <- reactiveValues(outdir = outdir)
