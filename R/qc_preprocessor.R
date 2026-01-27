# ============================================================
# QC Preprocessor: Standalone HTML (Coverage + VAF)
# No Pandoc. No deps dir. Single self-contained HTML output.
# Three independent plots (each with its own toolbox)
# ============================================================

# suppressPackageStartupMessages({
#   library(yaml)
#   library(data.table)
#   library(dplyr)
#   library(plotly)
#   library(htmlwidgets)
#   library(htmltools)
#   library(RColorBrewer)
#   library(base64enc)
# })

# ------------------------------------------------------------
# Config & Pedigree
# ------------------------------------------------------------

read_qc_config <- function(config_path) {
  cfg <- yaml::read_yaml(config_path)
  if (is.null(cfg$samples) || length(cfg$samples) == 0)
    stop("Config missing 'samples'")
  if (is.null(cfg$paths))
    stop("Config missing 'paths'")
  cfg
}

build_pedigree_from_config <- function(cfg) {
  ped <- rbindlist(lapply(cfg$samples, as.data.table), fill = TRUE)
  if (!"sample_id" %in% names(ped))
    stop("Each sample must define 'sample_id'")
  if (!"code" %in% names(ped))
    ped[, code := seq_len(.N)]
  ped[, .(code = as.integer(code), sample_id = as.character(sample_id))]
}

# ------------------------------------------------------------
# Coverage loading
# ------------------------------------------------------------

sanitize_chrom <- function(x) sub("_region$", "", as.character(x))

.pick_col <- function(nms, candidates) {
  nms <- tolower(nms)
  for (c in candidates) if (c %in% nms) return(c)
  NULL
}

# Use *_region mean values when present (genome_statistics-like files).
# Fallback to length-weighted or simple per-chrom means otherwise.
read_and_summarize_coverage <- function(path, sample_id) {
  dt <- tryCatch(fread(path), error = function(e) NULL)
  if (is.null(dt)) return(NULL)

  setnames(dt, names(dt), tolower(names(dt)))

  chrom_col <- .pick_col(names(dt), c("chrom", "chromosome", "contig"))
  cov_col   <- .pick_col(names(dt), c("mean", "average_coverage", "avg_coverage", "coverage", "depth", "mean_coverage"))
  start_col <- .pick_col(names(dt), c("start", "begin"))
  end_col   <- .pick_col(names(dt), c("end", "stop"))
  len_col   <- .pick_col(names(dt), c("length", "len"))

  if (is.null(chrom_col) || is.null(cov_col)) return(NULL)

  dt[, (chrom_col) := as.character(get(chrom_col))]
  dt[, (cov_col)   := as.numeric(get(cov_col))]

  has_region <- any(grepl("_region$", dt[[chrom_col]]))
  if (has_region) {
    region_dt <- dt[grepl("_region$", get(chrom_col))]
    out <- region_dt[, .(
      CHROM = sub("_region$", "", get(chrom_col)),
      AVERAGE_COVERAGE = get(cov_col)
    )]
    out[CHROM == "total_region", CHROM := "total"]
    out[, SAMPLE := sample_id]
    return(out)
  }

  if (!is.null(start_col) && !is.null(end_col)) {
    dt[, (start_col) := as.numeric(get(start_col))]
    dt[, (end_col)   := as.numeric(get(end_col))]
    dt[, width := pmax(1, get(end_col) - get(start_col))]
    out <- dt[, .(
      AVERAGE_COVERAGE = sum(get(cov_col) * width, na.rm = TRUE) / sum(width, na.rm = TRUE)
    ), by = chrom_col]
    setnames(out, chrom_col, "CHROM")
    out[, SAMPLE := sample_id]
    return(out)
  }

  out <- dt[, .(CHROM = get(chrom_col), AVERAGE_COVERAGE = get(cov_col))]
  out[, SAMPLE := sample_id]
  out
}

keep_canonical_coverage <- function(dt) {
  canonical <- c(paste0("chr", 1:22), "chrX", "chrY", "total")
  dt[, CHROM := sanitize_chrom(CHROM)]
  dt <- dt[CHROM %chin% canonical]
  dt <- dt[, .(AVERAGE_COVERAGE = mean(as.numeric(AVERAGE_COVERAGE), na.rm = TRUE)),
           by = .(SAMPLE, CHROM)]
  levels <- c(paste0("chr", 1:22), "chrX", "chrY", "total")
  dt[, CHROM := factor(CHROM, levels = levels)]
  dt
}

load_coverage_from_config <- function(cfg) {
  pieces <- lapply(cfg$samples, function(s) {
    read_and_summarize_coverage(s$coverage, s$sample_id)
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) return(NULL)
  keep_canonical_coverage(rbindlist(pieces, fill = TRUE))
}

# ------------------------------------------------------------
# VAF processing
# ------------------------------------------------------------

process_vaf <- function(snvs_dt, pedigree, max_n = 200000) {
  cat(colnames(snvs_dt), sep = ", ")
  if (is.null(snvs_dt)){
    cat("No snvs tsv is provided\n")
    return(NULL)
  }
  if (is.null(snvs_dt) || !"CATEGORY" %in% names(snvs_dt)) {
    cat("No CATEGORY column is found\n")
    return(NULL)
  }
  dt <- as.data.table(snvs_dt)[CATEGORY == "SNV & Indel"]
  if (!nrow(dt)) return(NULL)

  if (nrow(dt) > max_n) {
    set.seed(1)
    dt <- dt[sample(.N, max_n)]
  }

  vaf_cols <- grep("^VAF_", names(dt), value = TRUE)
  if (!length(vaf_cols)) return(NULL)
  if (!"ID" %in% names(dt)) dt[, ID := .I]

  long <- melt(
    dt[, c("ID", vaf_cols), with = FALSE],
    id.vars = "ID",
    variable.name = "variable",
    value.name = "AF"
  )

  long[, code := as.integer(sub("^VAF_", "", variable))]
  long[, AF := as.numeric(AF)]

  merge(long, pedigree, by = "code")[is.finite(AF)]
}

# ------------------------------------------------------------
# HTML asset inliner (Pandoc-free)
# ------------------------------------------------------------

inline_html_assets <- function(html_path) {
  html_dir <- dirname(html_path)
  html <- paste(readLines(html_path, warn = FALSE), collapse = "\n")

  is_remote <- function(x) grepl("^(https?:)?//", x)

  # Inline CSS
  css_pattern <- '<link[^>]+rel=[\'"]?stylesheet[\'"]?[^>]+href=[\'"]([^\'"]+)[\'"][^>]*>'
  matches <- regmatches(html, gregexpr(css_pattern, html, perl = TRUE))[[1]]
  for (ms in matches) {
    href <- sub('.*href=[\'"]([^\'"]+)[\'"].*', '\\1', ms)
    if (!is_remote(href)) {
      f <- file.path(html_dir, href)
      if (file.exists(f)) {
        css <- paste(readLines(f, warn = FALSE), collapse = "\n")
        html <- sub(ms, paste0("<style>\n", css, "\n</style>"), html, fixed = TRUE)
      }
    }
  }

  # Inline JS
  js_pattern <- '<script[^>]+src=[\'"]([^\'"]+)[\'"][^>]*></script>'
  matches <- regmatches(html, gregexpr(js_pattern, html, perl = TRUE))[[1]]
  for (ms in matches) {
    src <- sub('.*src=[\'"]([^\'"]+)[\'"].*', '\\1', ms)
    if (!is_remote(src)) {
      f <- file.path(html_dir, src)
      if (file.exists(f)) {
        js <- paste(readLines(f, warn = FALSE), collapse = "\n")
        html <- sub(ms, paste0("<script>\n", js, "\n</script>"), html, fixed = TRUE)
      }
    }
  }

  writeLines(html, html_path)
}

# For a whole page (tagList/browsable), use htmltools::save_html then inline
save_page_no_pandoc <- function(page, out_path) {
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  libdir <- file.path(dirname(out_path),
                      paste0(tools::file_path_sans_ext(basename(out_path)), "_files"))
  htmltools::save_html(page, file = out_path, background = "white", libdir = libdir)
  inline_html_assets(out_path)
  if (dir.exists(libdir)) unlink(libdir, recursive = TRUE, force = TRUE)
  message("Wrote standalone HTML: ", out_path)
}

# ------------------------------------------------------------
# Plot generation (three independent widgets)
# ------------------------------------------------------------

generate_coverage_vaf_html <- function(coverage_data, snvs_dt, pedigree, out_path) {
  if (is.null(coverage_data) || !nrow(coverage_data))
    stop("No coverage data")

  samples <- sort(unique(coverage_data$SAMPLE))
  palette <- setNames(
    RColorBrewer::brewer.pal(max(3, length(samples)), "Set2")[seq_along(samples)],
    samples
  )

  # Top spacer + legend (only top)
  top_spacer <- tags$div(style = "height: 24px;")
  legend_html <- tags$div(
    style = "font-family:sans-serif;font-size:12px;margin-bottom:16px;",
    tags$strong("Samples: "),
    lapply(names(palette), function(s) {
      tags$span(
        style = "display:inline-flex;align-items:center;margin-right:12px;",
        tags$span(style = sprintf(
          "width:10px;height:10px;background:%s;border-radius:50%%;display:inline-block;margin-right:6px;",
          palette[[s]]
        )),
        s
      )
    })
  )

  # Average coverage — independent widget
  p_avg <- plot_ly(
    coverage_data,
    x = ~CHROM, y = ~AVERAGE_COVERAGE,
    color = ~SAMPLE, colors = palette,
    type = "scatter", mode = "markers",
    marker = list(size = 9, opacity = 0.6)
  ) %>%
    layout(
      title = "Average Coverage",
      xaxis = list(type = "category", title = ""),
      yaxis = list(
        title = "Average Coverage",
        tickmode = "array",
        tickvals = c(0, 20, 40, 60, 80, 100),
        ticktext = c("0", "20", "40", "60", "80", "100")
      ),
      showlegend = FALSE,
      dragmode = "pan"
    )

  # Normalized coverage — independent widget
  auto <- paste0("chr", 1:22)
  auto_means <- coverage_data %>%
    dplyr::filter(CHROM %in% auto) %>%
    dplyr::group_by(SAMPLE) %>%
    dplyr::summarize(avg_auto = mean(AVERAGE_COVERAGE, na.rm = TRUE), .groups = "drop")

  norm <- coverage_data %>%
    dplyr::left_join(auto_means, by = "SAMPLE") %>%
    dplyr::mutate(norm_cov = AVERAGE_COVERAGE / avg_auto)

  p_norm <- plot_ly(
    norm,
    x = ~CHROM, y = ~norm_cov,
    color = ~SAMPLE, colors = palette,
    type = "scatter", mode = "markers",
    marker = list(size = 9, opacity = 0.6)
  ) %>%
    layout(
      title = "Normalized Coverage",
      xaxis = list(type = "category", title = ""),
      yaxis = list(title = "Normalized Coverage"),
      showlegend = FALSE,
      dragmode = "pan"
    )

  # VAF density — independent widget
  p_vaf <- NULL
  vaf_long <- process_vaf(snvs_dt, pedigree)
  if (!is.null(vaf_long)) {
    dens <- vaf_long[, {
      if (.N < 2) return(NULL)
      d <- density(AF, from = 0, to = 1)
      data.table(x = d$x, y = d$y)
    }, by = sample_id]
    dens[, sample_id := factor(sample_id, levels = names(palette))]

    p_vaf <- plot_ly(
      dens,
      x = ~x, y = ~y,
      color = ~sample_id,
      colors = palette,
      type = "scatter", mode = "lines"
    ) %>%
      layout(
        title = "VAF Density",
        xaxis = list(title = "Allele Fraction", range = c(0, 1)),
        yaxis = list(title = "Density"),
        showlegend = FALSE,
        dragmode = "pan"
      )
  } else {
    message("No valid VAF data found; skipping VAF plot")
    # add a placeholder paragraph
    p_vaf <- tags$p("No valid VAF data found; VAF plot not generated.")
  }

  # Assemble page with spacing between independent widgets
  spacer <- tags$div(style = "height: 28px;")
  page <- tagList(
    top_spacer,
    legend_html,
    p_avg,
    spacer,
    p_norm,
    if (!is.null(p_vaf)) spacer,
    if (!is.null(p_vaf)) p_vaf
  )

  save_page_no_pandoc(page, out_path)
}

# ------------------------------------------------------------
# Gateway
# ------------------------------------------------------------

generate_qc_htmls_from_config <- function(config_path) {
  cfg <- read_qc_config(config_path)
  ped <- build_pedigree_from_config(cfg)
  cov <- load_coverage_from_config(cfg)
  snvs <- if (!is.null(cfg$paths$snvs_tsv) && nzchar(cfg$paths$snvs_tsv))
    fread(cfg$paths$snvs_tsv) else NULL

  out <- cfg$paths$coverage_vaf_html
  if (is.null(out) || !nzchar(out)) stop("paths.coverage_vaf_html not set")

  generate_coverage_vaf_html(cov, snvs, ped, out)
  invisible(out)
}