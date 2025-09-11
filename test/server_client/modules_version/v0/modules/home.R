homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, actionButton(ns("load"), "Load data", icon = icon("database"))),
      column(3, numericInput(ns("n"), "Sample size (n)", value = 30, min = 1, max = nrow(iris), step = 1)),
      column(3, actionButton(ns("resample"), "Resample n", icon = icon("shuffle")))
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

    # ---- Load data (explicit) ----
    observeEvent(input$load, {
      n0 <- input$n %||% 30
      base0 <- add_row_id(sample_base(n0))
      shared_store$A <- base0
      shared_store$B <- make_variant_from(base0, seed_offset = 1)
      bump_version()
      cat(sprintf("[Home] Loaded datasets A and B (n=%d)\n", nrow(base0)))
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
          df[nums] <- Map(function(col) col + rnorm(length(col), sd = 0.05), df[nums])
        }
        shared_store$B <- df
        bump_version()
        cat(sprintf("[Home] Refreshed B (n=%d)\n", nrow(df)))
      }
    })
  })
}