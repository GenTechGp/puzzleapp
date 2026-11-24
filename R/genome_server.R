# Serve local genome files (FASTA, index, optional GFF3) through Shiny's own
# HTTP port using session$registerDataObj(). Supports HTTP Range requests
# (206 Partial Content) required by igv.js for byte-range fetching of large
# reference files.

.igv_allowed_exts <- c("fa", "fasta", "fna", "fai", "gff", "gff3",
                        "bam", "bai", "vcf", "gz", "tbi", "cram", "crai", "bb")

#' Register genome files as HTTP endpoints on the Shiny server.
#'
#' Attaches a range-capable HTTP handler for each file onto Shiny's own httpuv
#' server via session$registerDataObj(). Files are served through the existing
#' Shiny port -- no separate server, no firewall or CORS issues.
#'
#' @param session  The Shiny session object.
#' @param files    Named character vector from make_file_map().
#' @param base_url The Shiny app base URL (e.g. "http://localhost:8888").
#'
#' @return Named character vector: basename -> absolute URL.
register_genome_files <- function(session, files, base_url) {
  files <- .validate_genome_files(files)
  urls  <- character(length(files))
  names(urls) <- names(files)

  for (nm in names(files)) {
    fpath   <- files[[nm]]
    safe_nm <- gsub("[^a-zA-Z0-9._-]", "_", nm)
    rel_url <- session$registerDataObj(
      name       = paste0("igv_", safe_nm),
      data       = list(path = fpath),
      filterFunc = .genome_range_handler
    )
    full_url   <- paste0(base_url, "/", sub("^/", "", rel_url))
    urls[[nm]] <- full_url
    log_info(sprintf("[genome] registered '%s' -> %s", nm, full_url))
  }

  log_info(sprintf("[genome] %d genome file(s) registered via Shiny session", length(files)))
  urls
}

# HTTP handler: supports HEAD, full GET, and Range GET (206 Partial Content).
.genome_range_handler <- function(data, req) {
  fpath        <- data$path
  file_size    <- file.info(fpath)$size
  method       <- req$REQUEST_METHOD %||% "GET"
  range_header <- req$HTTP_RANGE
  log_debug(sprintf("[genome-handler] %s %s range='%s'",
                    method, basename(fpath), range_header %||% "none"))

  base_headers <- list(
    "Content-Type"  = .igv_mime_type(fpath),
    "Accept-Ranges" = "bytes"
  )

  if (!is.null(range_header) && nchar(range_header) > 0) {
    range_str <- sub("bytes=", "", range_header, fixed = TRUE)
    parts     <- strsplit(range_str, "-", fixed = TRUE)[[1]]
    start     <- as.numeric(parts[1])
    end       <- if (length(parts) > 1 && nchar(parts[2]) > 0)
                   as.numeric(parts[2]) else file_size - 1
    end       <- min(end, file_size - 1)
    nbytes    <- end - start + 1

    con  <- file(fpath, "rb")
    on.exit(close(con))
    seek(con, where = start, origin = "start")
    body <- readBin(con, raw(), n = nbytes)

    return(list(
      status  = 206L,
      headers = c(base_headers, list(
        "Content-Range"  = sprintf("bytes %.0f-%.0f/%.0f", start, end, file_size),
        "Content-Length" = as.character(nbytes)
      )),
      body = body
    ))
  }

  if (method == "HEAD") {
    return(list(
      status  = 200L,
      headers = c(base_headers, list("Content-Length" = as.character(file_size))),
      body    = ""
    ))
  }

  list(
    status  = 200L,
    headers = c(base_headers, list("Content-Length" = as.character(file_size))),
    body    = readBin(fpath, raw(), n = file_size)
  )
}

#' Build a genome spec for a locally-served custom genome.
#'
#' @param name   Genome display name.
#' @param urls   Named URL vector returned by register_genome_files().
#' @param fasta  Absolute path to the FASTA file.
#' @param fai    Absolute path to the FASTA index file.
#' @param locus  Initial locus string (e.g. "chr1:1-10000" or "all").
#' @param gff    Optional absolute path to a GFF3 annotation file (reserved for future use).
build_local_genome_spec <- function(name, urls, fasta, fai, locus = "all",
                                    gff = NULL) {
  list(
    genomeName   = name,
    stockGenome  = FALSE,
    dataMode     = "http",
    validated    = TRUE,
    fasta        = urls[[basename(fasta)]],
    fastaIndex   = urls[[basename(fai)]],
    initialLocus = locus,
    annotation   = if (!is.null(gff)) urls[[basename(gff)]] else NA
  )
}

#' Build a named file map for register_genome_files().
#'
#' @param ...  Absolute file paths. NULL/NA values are silently dropped.
make_file_map <- function(...) {
  paths <- Filter(function(x) !is.null(x) && !is.na(x) && nchar(trimws(x)) > 0, c(...))
  if (length(paths) == 0L) stop("No valid file paths provided")
  nms <- basename(paths)
  if (anyDuplicated(nms))
    stop("Duplicate basenames \u2014 rename files to make them unique")
  setNames(as.character(paths), nms)
}

.validate_genome_files <- function(files) {
  stopifnot(is.character(files), !is.null(names(files)), all(nchar(names(files)) > 0))
  files <- sapply(files, normalizePath, mustWork = TRUE, USE.NAMES = TRUE)
  for (fpath in files) {
    ext <- tolower(tools::file_ext(fpath))
    if (!ext %in% .igv_allowed_exts)
      stop(sprintf("Extension '.%s' not permitted: %s", ext, fpath))
    link <- Sys.readlink(fpath)
    if (nchar(link) > 0 && !startsWith(normalizePath(link, mustWork = FALSE), dirname(fpath)))
      stop(sprintf("Symlink points outside its directory: %s", fpath))
  }
  files
}

.igv_mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    fa = , fasta = , fna = , fai = , gff = , gff3 = , vcf = "text/plain",
    bam = , bai = , cram = , crai = , tbi = , bb = "application/octet-stream",
    gz  = "application/gzip",
    "application/octet-stream"
  )
}

#' Base URL of the app as the browser sees it.
#'
#' Built from the browser-reported host and port, so it is right behind an SSH
#' tunnel. Reverse proxies (e.g. ARE/RStudio) mount Shiny under a path prefix,
#' which `url_pathname` carries (e.g. "/rstudio/proxy/5637/"). The query string
#' (where the access token lives) is deliberately left out.
#'
#' @param session The Shiny session.
#' @return A URL without a trailing slash, e.g. "http://localhost:8898".
shiny_base_url <- function(session) {
  cd <- session$clientData
  shiny::isolate({
    protocol <- cd$url_protocol
    hostname <- cd$url_hostname
    port     <- cd$url_port %||% ""
    pathname <- cd$url_pathname %||% ""
  })
  use_port <- nchar(port) > 0 &&
              !(port == "80"  && protocol == "http:")  &&
              !(port == "443" && protocol == "https:")
  host <- if (use_port)
            sprintf("%s//%s:%s", protocol, hostname, port)
          else
            sprintf("%s//%s",    protocol, hostname)
  paste0(host, sub("/+$", "", pathname))
}

# Path for a track file in a per-process scratch directory under tempdir().
.igv_track_file <- function(ext) {
  dir <- file.path(tempdir(), "igv_tracks")
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  tempfile(tmpdir = dir, fileext = ext)
}

#' Load alignments into the IGV browser as a BAM track.
#'
#' Replaces igvShiny::loadBamTrackFromLocalData(), whose JavaScript builds the
#' file URL as `window.location.href + path`. That breaks whenever the page
#' URL has a query string (the access token, or ?newSession=), and it serves
#' the file from the app-wide "tracks" path, which needs no session. Here the
#' BAM and its index are served through the session (register_genome_files)
#' with absolute URLs.
#'
#' @param session      Shiny session object.
#' @param id           Namespaced output ID of the igvShiny widget.
#' @param track_name   Display name for the track.
#' @param data         A GAlignments object.
#' @param display_mode "EXPANDED", "SQUISHED", or "COLLAPSED".
load_bam_track <- function(session, id, track_name, data, display_mode = "EXPANDED") {
  igvShiny::removeTracksByName(session, id, track_name)
  bam <- .igv_track_file(".bam")
  rtracklayer::export(data, bam, format = "BAM")   # also writes <bam>.bai
  bai <- paste0(bam, ".bai")
  has_index <- file.exists(bai)
  files <- if (has_index) make_file_map(bam, bai) else make_file_map(bam)
  urls <- register_genome_files(session, files, shiny_base_url(session))
  session$sendCustomMessage("puzzleapp_loadBamTrack", list(
    elementID   = id,
    trackName   = track_name,
    url         = urls[[basename(bam)]],
    indexURL    = if (has_index) urls[[basename(bai)]] else NULL,
    displayMode = display_mode
  ))
}

#' Load variants into the IGV browser as a VCF track.
#'
#' Replaces igvShiny::loadVcfTrack() for the same reason as load_bam_track().
#'
#' @param session    Shiny session object.
#' @param id         Namespaced output ID of the igvShiny widget.
#' @param track_name Display name for the track.
#' @param data       A VCF object (VariantAnnotation).
load_vcf_track <- function(session, id, track_name, data) {
  igvShiny::removeTracksByName(session, id, track_name)
  vcf <- .igv_track_file(".vcf")
  VariantAnnotation::writeVcf(data, vcf)
  urls <- register_genome_files(session, make_file_map(vcf), shiny_base_url(session))
  session$sendCustomMessage("puzzleapp_loadVcfTrack", list(
    elementID = id,
    trackName = track_name,
    url       = urls[[basename(vcf)]]
  ))
}

#' Return the tags$script() required by load_annotation_track(),
#' load_bam_track() and load_vcf_track().
#'
#' Include this once in your Shiny UI, e.g. inside tags$head():
#'   tags$head(genome_server_js())
genome_server_js <- function() {
  shiny::tags$script(shiny::HTML('
Shiny.addCustomMessageHandler("puzzleapp_loadBamTrack", function(msg) {
  var browser = document.getElementById(msg.elementID).igvBrowser;
  if (!browser) { console.warn("puzzleapp_loadBamTrack: browser not ready"); return; }
  var config = {
    type:        "alignment",
    format:      "bam",
    name:        msg.trackName,
    url:         msg.url,
    displayMode: msg.displayMode || "EXPANDED",
    order:       Number.MAX_VALUE
  };
  if (msg.indexURL) { config.indexURL = msg.indexURL; } else { config.indexed = false; }
  browser.loadTrack(config);
});
Shiny.addCustomMessageHandler("puzzleapp_loadVcfTrack", function(msg) {
  var browser = document.getElementById(msg.elementID).igvBrowser;
  if (!browser) { console.warn("puzzleapp_loadVcfTrack: browser not ready"); return; }
  browser.loadTrack({
    type:             "variant",
    format:           "vcf",
    name:             msg.trackName,
    url:              msg.url,
    indexed:          false,
    displayMode:      "EXPANDED",
    height:           100,
    visibilityWindow: 1000000,
    order:            Number.MAX_VALUE
  });
});
Shiny.addCustomMessageHandler("igvshiny_loadAnnotationTrack", function(msg) {
  var browser = document.getElementById(msg.elementID).igvBrowser;
  if (!browser) { console.warn("igvshiny_loadAnnotationTrack: browser not ready"); return; }
  var ext = msg.url.split(".").pop().toLowerCase().split("?")[0];
  var fmt = msg.format || (ext === "bb" ? "bigbed" : ext);
  browser.loadTrack({
    type:        "annotation",
    format:      fmt,
    url:         msg.url,
    name:        msg.trackName,
    displayMode: msg.displayMode || "EXPANDED",
    height:      msg.trackHeight || 100,
    order:       Number.MAX_VALUE
  });
});
'))
}

#' Load an annotation track (BigBed, GFF3, etc.) into the IGV browser.
#'
#' Uses a custom JS message handler registered via genome_server_js().
#' Format is auto-detected from the URL extension if not specified.
#'
#' @param session      Shiny session object.
#' @param id           Output ID of the igvShiny widget (namespaced, e.g. ns("igvShiny_0")).
#' @param track_name   Display name for the track.
#' @param url          URL to the annotation file.
#' @param format       igv.js format string ("bigbed", "gff3", etc.). Auto-detected if NULL.
#' @param display_mode "EXPANDED", "SQUISHED", or "COLLAPSED".
#' @param track_height Track height in pixels.
load_annotation_track <- function(session, id, track_name, url, format = NULL,
                                  display_mode = "EXPANDED", track_height = 100) {
  msg <- list(
    elementID   = id,
    trackName   = track_name,
    url         = url,
    displayMode = display_mode,
    trackHeight = track_height
  )
  if (!is.null(format)) msg$format <- format
  session$sendCustomMessage("igvshiny_loadAnnotationTrack", msg)
}
