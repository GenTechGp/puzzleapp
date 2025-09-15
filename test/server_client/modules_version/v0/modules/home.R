homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Preferred columns selector (static iris names). Applied once at Load data.
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("preferred_cols"),
          label = "Preferred columns (order applied once; non-preferred hidden initially)",
          choices = names(iris),
          multiple = TRUE
        )
      )
    ),
    fluidRow(
      column(3, actionButton(ns("load"), "Load data", icon = icon("database"))),
      column(3, numericInput(ns("n"), "Sample size (n)", value = 30, min = 1, max = nrow(iris), step = 1)),
      column(3, actionButton(ns("resample"), "Resample n", icon = icon("shuffle"))),
      column(3, actionButton(ns("delete_datasets"), "Delete datasets", icon = icon("trash"), class = "btn-danger"))
    ),
    fluidRow(
      column(3, actionButton(ns("refresh_a"), "Refresh A (noise)", icon = icon("rotate"))),
      column(3, actionButton(ns("refresh_b"), "Refresh B (noise)", icon = icon("rotate")))
    ),
    tags$hr(),
    helpText("Home: Load/create and refresh datasets A and B. Data tab: switch, slice, view, and edit.")
  )
}

homeServer <- function(id, shared_store, shared_rx) {
  moduleServer(id, function(input, output, session) {

    # ---- Helpers (private to Home) ----
    `%||%` <- function(x, y) if (is.null(x)) y else x

    add_row_id <- function(df) {
      df$.row_id <- seq_len(nrow(df))
      # Ensure .row_id is last column to match Data's expectations
      df <- df[, c(setdiff(names(df), ".row_id"), ".row_id"), drop = FALSE]
      df
    }

    sample_base <- function(n) {
      n <- max(1, min(n, nrow(iris)))
      iris[sample.int(nrow(iris), n, replace = FALSE), , drop = FALSE]
    }

    make_variant_from <- function(df, seed_offset = 0) {
      set.seed(123 + as.integer(seed_offset))
      d <- df
      nums <- sapply(d, is.numeric)
      nums[".row_id"] <- FALSE
      d[nums] <- lapply(d[nums], function(col) round(col + rnorm(length(col), sd = 0.2), 3))
      d
    }

    bump_version <- function() shared_rx$version(shared_rx$version() + 1L)

    # Hardcoded iris "boundary" default table used only during Load (step 1 of 2)
    default_boundary_df <- function() {
      # Hardcoded global min/max for iris numeric columns
      min_row <- data.frame(
        Sepal.Length = 4.3,
        Sepal.Width  = 2.0,
        Petal.Length = 1.0,
        Petal.Width  = 0.1,
        Species      = factor("setosa", levels = levels(iris$Species)),
        stringsAsFactors = FALSE
      )
      max_row <- data.frame(
        Sepal.Length = 100.9,
        Sepal.Width  = 4.4,
        Petal.Length = 6.9,
        Petal.Width  = 2.5,
        Species      = factor("virginica", levels = levels(iris$Species)),
        stringsAsFactors = FALSE
      )
      mid_row <- data.frame(
        Sepal.Length = 5.8,
        Sepal.Width  = 3.0,
        Petal.Length = 3.7,
        Petal.Width  = 1.2,
        Species      = factor("versicolor", levels = levels(iris$Species)),
        stringsAsFactors = FALSE
      )

      # Build 30 rows so Data's initial 0–10% slice includes first 3 rows (min, max, mid)
      base3 <- rbind(min_row, max_row, mid_row)
      df <- base3[rep(1:3, length.out = 30), , drop = FALSE]

      add_row_id(df)
    }

    # ---- Load data (two-step: defaults -> real) ----
    observeEvent(input$load, {
      # 1) Capture preferred columns ONCE at load time (selection order preserved)
      shared_store$preferred_cols <- input$preferred_cols %||% character(0)
      cat(sprintf("[Home] Preferred cols (%d): %s\n",
                  length(shared_store$preferred_cols),
                  paste(shared_store$preferred_cols, collapse = ", ")))

      # 2) Step 1: Write default boundary table to A and B and bump version
      def <- default_boundary_df()
      shared_store$A <- def
      # shared_store$B <- def
      n0 <- input$n %||% 30
      base0 <- add_row_id(sample_base(n0))
      # shared_store$A <- base0
      shared_store$B <- make_variant_from(base0, seed_offset = 1)
      bump_version()
      cat(sprintf("[Home] Load step 2/2: real datasets A and B set (n=%d)\n", nrow(base0)))
    })

    # ---- Resample (recreate A and B) ----
    observeEvent(input$resample, {
      n <- input$n %||% 30
      base <- add_row_id(sample_base(n))
      shared_store$A <- base
      shared_store$B <- make_variant_from(base, seed_offset = 1)
      bump_version()
      cat(sprintf("[Home] Resampled datasets A and B (n=%d)\n", n))
    })

    # ---- Refresh A (noise on numeric cols, keep .row_id stable) ----
    observeEvent(input$refresh_a, {
      if (is.null(shared_store$A)) return()
      df <- shared_store$A
      if (!is.null(df) && nrow(df)) {
        nums <- sapply(df, is.numeric); nums[".row_id"] <- FALSE
        set.seed(as.integer(Sys.time()))
        if (any(nums)) {
          df[nums] <- Map(function(col) round(col + rnorm(length(col), sd = 0.05), 3), df[nums])
        }
        shared_store$A <- df
        bump_version()
        cat(sprintf("[Home] Refreshed A (n=%d)\n", nrow(df)))
      }
    })

    # ---- Refresh B (noise on numeric cols, keep .row_id stable) ----
    observeEvent(input$refresh_b, {
      if (is.null(shared_store$B)) return()
      df <- shared_store$B
      if (!is.null(df) && nrow(df)) {
        nums <- sapply(df, is.numeric); nums[".row_id"] <- FALSE
        set.seed(as.integer(Sys.time()) + 1L)
        if (any(nums)) {
          df[nums] <- Map(function(col) round(col + rnorm(length(col), sd = 0.05), 3), df[nums])
        }
        shared_store$B <- df
        bump_version()
        cat(sprintf("[Home] Refreshed B (n=%d)\n", nrow(df)))
      }
    })

    # ---- Delete datasets A and B (no confirmation) ----
    observeEvent(input$delete_datasets, {
      shared_store$A <- NULL
      shared_store$B <- NULL
      bump_version()
      cat("[Home] Deleted datasets A and B\n")
      showNotification("Deleted datasets A and B.", type = "message", duration = 2)
    })
  })
}