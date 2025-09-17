# modules/home.R
homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("preferred_cols"),
          label = "Preferred columns (applied once at initial render)",
          choices = character(0),
          multiple = TRUE
        )
      ),
      column(
        6,
        textInput(
          ns("tsv_path"),
          label = "TSV file path (optional)",
          placeholder = "/path/to/file.tsv"
        ),
        actionButton(ns("use_tsv"), "Use TSV", icon = icon("file-import"))
      )
    ),
    fluidRow(
      column(3, actionButton(ns("load"), "Load datasets", icon = icon("database"))),
      column(3, numericInput(ns("n"), "Sample size (n)", value = 30, min = 1, step = 1)),
      column(3, actionButton(ns("resample"), "Resample datasetA/B", icon = icon("shuffle"))),
      column(3, actionButton(ns("delete_datasets"), "Delete all datasets", icon = icon("trash"), class = "btn-danger"))
    ),
    fluidRow(
      column(3, actionButton(ns("refresh_a"), "Refresh datasetA (noise)", icon = icon("rotate"))),
      column(3, actionButton(ns("refresh_b"), "Refresh datasetB (noise)", icon = icon("rotate")))
    ),
    tags$hr(),
    helpText("Home: Create and refresh datasets under shared_store$data_for_data. Data tab: pick an active dataset with the dropdown and edit inline.")
  )
}

homeServer <- function(id, shared_store, shared_rx) {
  moduleServer(id, function(input, output, session) {

    `%||%` <- function(x, y) if (is.null(x)) y else x

    # ---- Default dataset (single line; swap here if needed) ----
    dataset_df <- iris
    default_base_dt <- data.table::as.data.table(dataset_df)

    # Initialize preferred_cols choices from default dataset (UI remains dataset-agnostic)
    try({
      updateSelectizeInput(session, "preferred_cols", choices = names(default_base_dt), server = TRUE)
    }, silent = TRUE)

    # Ensure .row_id exists and is the last column (data.table)
    add_row_id_dt <- function(dt) {
      stopifnot(inherits(dt, "data.table"))
      dt[, .row_id := .I]
      cn <- colnames(dt)
      dt <- dt[, c(setdiff(cn, ".row_id"), ".row_id"), with = FALSE]
      dt
    }

    # Sample base from given data.table
    sample_base_dt <- function(n, base_dt) {
      stopifnot(inherits(base_dt, "data.table"))
      n_max <- nrow(base_dt)
      n <- max(1, min(n %||% 1, n_max))
      if (n_max <= 0) return(base_dt[0])
      idx <- sample.int(n_max, n, replace = FALSE)
      base_dt[idx, , drop = FALSE]
    }

    # Make a noisy variant (keep .row_id stable)
    make_variant_from_dt <- function(dt, sd_noise = 0.2, seed_offset = 0L) {
      stopifnot(inherits(dt, "data.table"))
      set.seed(123 + as.integer(seed_offset))
      num_cols <- names(which(sapply(dt, is.numeric)))
      num_cols <- setdiff(num_cols, ".row_id")
      if (length(num_cols)) {
        for (col in num_cols) {
          dt[, (col) := round(get(col) + rnorm(.N, sd = sd_noise), 3)]
        }
      }
      dt
    }

    # ---- Use TSV: read header to populate preferred column choices; record chosen path ----
    observeEvent(input$use_tsv, {
      path <- trimws(input$tsv_path %||% "")
      if (identical(path, "") || !file.exists(path)) {
        showNotification("TSV path is empty or does not exist.", type = "error", duration = 3)
        return()
      }
      # Try reading only the header (fast)
      dt_hdr <- NULL
      ok <- TRUE
      tryCatch({
        dt_hdr <- data.table::fread(path, sep = "\t", nrows = 0L, showProgress = FALSE)
      }, error = function(e) {
        ok <<- FALSE
        showNotification(sprintf("Failed to read TSV header: %s", e$message), type = "error", duration = 4)
      })
      if (!ok || is.null(dt_hdr)) return()

      cols <- names(dt_hdr) %||% character(0)
      updateSelectizeInput(session, "preferred_cols", choices = cols, server = TRUE)

      # Record explicitly chosen path
      shared_store$selected_tsv_path <- path
      showNotification("TSV columns loaded. Remember to click 'Load datasets' to apply.", type = "message", duration = 3)
      cat(sprintf("[Home] Use TSV: path set to '%s' with %d columns\n", path, length(cols)))
    })

    # ---- Load datasets (default + datasetA + datasetB) ----
    observeEvent(input$load, {
      # Determine the base dataset: use confirmed TSV if present, else default
      base_full_dt <- NULL
      path <- shared_store$selected_tsv_path %||% NULL
      if (!is.null(path) && file.exists(path)) {
        ok <- TRUE
        tryCatch({
          base_full_dt <- data.table::fread(path, sep = "\t", showProgress = FALSE)
          base_full_dt <- add_extra_columns(base_full_dt)
        }, error = function(e) {
          ok <<- FALSE
          showNotification(sprintf("Failed to read TSV. Falling back to default dataset. Error: %s", e$message), type = "warning", duration = 5)
          cat(sprintf("[Home][WARN] TSV read failed: %s\n", e$message))
        })
        if (!ok) base_full_dt <- default_base_dt
      } else {
        base_full_dt <- default_base_dt
      }
      base_full_dt <- data.table::as.data.table(base_full_dt)

      # Persist the base used for subsequent operations
      shared_store$base_full_dt <- base_full_dt

      # Capture preferred columns once at load
      shared_store$preferred_cols <- input$preferred_cols %||% character(0)
      n <- input$n %||% 30

      # Build datasets
      default_dt <- make_boundary_table(base_full_dt, round_base = 10, slice_pct = 10)

      base_dt <- add_row_id_dt(sample_base_dt(n, base_full_dt))
      datasetA_dt <- data.table::copy(base_dt)  # base real
      datasetB_dt <- make_variant_from_dt(data.table::copy(base_dt), sd_noise = 0.15, seed_offset = 1L)  # variant real

      # Install under shared_store$data_for_data
      shared_store$data_for_data <- list()  # reset
      shared_store$filter_for_data <- list()
      shared_store$data_for_data[["[Synthetic] Boundary"]] <- default_dt
      shared_store$data_for_data[["datasetA"]] <- datasetA_dt
      shared_store$data_for_data[["datasetB"]] <- datasetB_dt

      bump_version(shared_rx = shared_rx)
      src_lab <- if (!is.null(path) && file.exists(path)) sprintf("TSV (%s)", path) else "default dataset"
      cat(sprintf("[Home] Loaded datasets from %s: %s (n=%d default, %d A, %d B)\n",
                  src_lab,
                  paste(names(shared_store$data_for_data), collapse = ", "),
                  nrow(default_dt), nrow(datasetA_dt), nrow(datasetB_dt)))
      showNotification(sprintf("Datasets loaded from %s.", src_lab), type = "message", duration = 3)
    })

    # ---- Resample (recreate datasetA and datasetB only) ----
    observeEvent(input$resample, {
      base_full_dt <- shared_store$base_full_dt
      if (is.null(base_full_dt)) {
        showNotification("Please 'Load datasets' first.", type = "warning", duration = 3)
        return()
      }
      n <- input$n %||% 30
      base_dt <- add_row_id_dt(sample_base_dt(n, base_full_dt))
      shared_store$data_for_data[["datasetA"]] <- data.table::copy(base_dt)
      shared_store$data_for_data[["datasetB"]] <- make_variant_from_dt(data.table::copy(base_dt), sd_noise = 0.15, seed_offset = 1L)
      bump_version(shared_rx = shared_rx)
      cat(sprintf("[Home] Resampled datasetA and datasetB (n=%d)\n", n))
    })

    # ---- Refresh noise on datasetA ----
    observeEvent(input$refresh_a, {
      dt <- shared_store$data_for_data[["datasetA"]]
      if (!is.null(dt) && nrow(dt)) {
        dt2 <- make_variant_from_dt(data.table::copy(dt), sd_noise = 0.05, seed_offset = as.integer(Sys.time()))
        # keep .row_id from original
        dt2[, .row_id := dt$.row_id]
        shared_store$data_for_data[["datasetA"]] <- dt2
        bump_version(shared_rx = shared_rx)
        cat(sprintf("[Home] Refreshed datasetA (n=%d)\n", nrow(dt2)))
      }
    })

    # ---- Refresh noise on datasetB ----
    observeEvent(input$refresh_b, {
      dt <- shared_store$data_for_data[["datasetB"]]
      if (!is.null(dt) && nrow(dt)) {
        dt2 <- make_variant_from_dt(data.table::copy(dt), sd_noise = 0.05, seed_offset = as.integer(Sys.time()) + 1L)
        dt2[, .row_id := dt$.row_id]
        shared_store$data_for_data[["datasetB"]] <- dt2
        bump_version(shared_rx = shared_rx)
        cat(sprintf("[Home] Refreshed datasetB (n=%d)\n", nrow(dt2)))
      }
    })

    # ---- Delete all datasets (reuse existing Synthetic Boundary; don't recompute) ----
    observeEvent(input$delete_datasets, {
      default_dt <- shared_store$data_for_data[["[Synthetic] Boundary"]]
      shared_store$data_for_data <- list()
      shared_store$data_for_data[["[Synthetic] Boundary"]] <- default_dt
      shared_store$filter_for_data <- list()
      bump_version(shared_rx = shared_rx)
      cat("[Home] Deleted all datasets\n")
      showNotification("Deleted all datasets.", type = "message", duration = 2)
    })
  })
}