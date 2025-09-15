source("modules/helper.R")

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
          choices = names(iris),
          multiple = TRUE
        )
      )
    ),
    fluidRow(
      column(3, actionButton(ns("load"), "Load datasets", icon = icon("database"))),
      column(3, numericInput(ns("n"), "Sample size (n)", value = 30, min = 1, max = nrow(iris), step = 1)),
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

    bump_version <- function() shared_rx$version(shared_rx$version() + 1L)

    # Ensure .row_id exists and is the last column (data.table)
    add_row_id_dt <- function(dt) {
      stopifnot(inherits(dt, "data.table"))
      dt[, .row_id := .I]
      # Move .row_id to last column
      cn <- colnames(dt)
      dt <- dt[, c(setdiff(cn, ".row_id"), ".row_id"), with = FALSE]
      dt
    }

    # Sample base from iris as data.table
    sample_base_dt <- function(n) {
      n <- max(1, min(n, nrow(iris)))
      idx <- sample.int(nrow(iris), n, replace = FALSE)
      dt <- as.data.table(iris[idx, , drop = FALSE])
      dt
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

    # ---- Load datasets (default + datasetA + datasetB) ----
    observeEvent(input$load, {
      # Capture preferred columns once at load
      shared_store$preferred_cols <- input$preferred_cols %||% character(0)
      n <- input$n %||% 30

      # Build datasets
      default_dt <- make_boundary_table(data.table::as.data.table(iris), round_base = 10, slice_pct = 10)

      base_dt <- add_row_id_dt(sample_base_dt(n))
      datasetA_dt <- copy(base_dt)                                  # base real
      datasetB_dt <- make_variant_from_dt(copy(base_dt), sd_noise = 0.15, seed_offset = 1L)  # variant real

      # Install under shared_store$data_for_data
      shared_store$data_for_data <- list()  # reset
      shared_store$data_for_data[["[Synthetic] Boundary"]]  <- default_dt
      shared_store$data_for_data[["datasetA"]] <- datasetA_dt
      shared_store$data_for_data[["datasetB"]] <- datasetB_dt

      bump_version()
      cat(sprintf("[Home] Loaded datasets: %s (n=%d default, %d A, %d B)\n",
                  paste(names(shared_store$data_for_data), collapse = ", "),
                  nrow(default_dt), nrow(datasetA_dt), nrow(datasetB_dt)))
    })

    # ---- Resample (recreate datasetA and datasetB only) ----
    observeEvent(input$resample, {
      n <- input$n %||% 30
      base_dt <- add_row_id_dt(sample_base_dt(n))
      shared_store$data_for_data[["datasetA"]] <- copy(base_dt)
      shared_store$data_for_data[["datasetB"]] <- make_variant_from_dt(copy(base_dt), sd_noise = 0.15, seed_offset = 1L)
      bump_version()
      cat(sprintf("[Home] Resampled datasetA and datasetB (n=%d)\n", n))
    })

    # ---- Refresh noise on datasetA ----
    observeEvent(input$refresh_a, {
      dt <- shared_store$data_for_data[["datasetA"]]
      if (!is.null(dt) && nrow(dt)) {
        dt2 <- make_variant_from_dt(copy(dt), sd_noise = 0.05, seed_offset = as.integer(Sys.time()))
        # keep .row_id from original
        dt2[, .row_id := dt$.row_id]
        shared_store$data_for_data[["datasetA"]] <- dt2
        bump_version()
        cat(sprintf("[Home] Refreshed datasetA (n=%d)\n", nrow(dt2)))
      }
    })

    # ---- Refresh noise on datasetB ----
    observeEvent(input$refresh_b, {
      dt <- shared_store$data_for_data[["datasetB"]]
      if (!is.null(dt) && nrow(dt)) {
        dt2 <- make_variant_from_dt(copy(dt), sd_noise = 0.05, seed_offset = as.integer(Sys.time()) + 1L)
        dt2[, .row_id := dt$.row_id]
        shared_store$data_for_data[["datasetB"]] <- dt2
        bump_version()
        cat(sprintf("[Home] Refreshed datasetB (n=%d)\n", nrow(dt2)))
      }
    })

    # ---- Delete all datasets ----
    observeEvent(input$delete_datasets, {
      shared_store$data_for_data <- list()
      default_dt <- make_boundary_table(data.table::as.data.table(iris), round_base = 10, slice_pct = 10)
      shared_store$data_for_data[["[Synthetic] Boundary"]]  <- default_dt
      bump_version()
      cat("[Home] Deleted all datasets\n")
      showNotification("Deleted all datasets.", type = "message", duration = 2)
    })
  })
}