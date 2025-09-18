# filter_module.R
#
# QUAL Filter Module
# ------------------
# Provides a UI + server logic to apply a QUAL (or configurable column) range
# filter across ALL datasets stored in shared_data$original_data
#
# Results:
#   shared_data$filter_for_data  <- named list of logical vectors
#     Each logical vector has length nrow(dataset) and indicates whether the
#     corresponding row passes the filter (TRUE) or not (FALSE).
#
# Specifications (per user confirmation):
#   - Target column name exposed via FILTER_COL_NAME variable.
#   - Slider range: [0, 100], step = 1, initial full span (no filtering effect).
#   - Apply is ONLY executed when user clicks "Apply filter" button (no auto-update).
#   - If dataset is missing the target column -> ALL FALSE mask (dataset fully filtered out).
#   - NA values in the target column -> treated as FAIL (FALSE).
#   - No warnings or messages for missing columns (silent behavior).
#
# Future customization:
#   - Change FILTER_COL_NAME manually to adapt to a different column name.
#   - Extend by adding additional sliders / inputs and expanding logic inside the
#     observeEvent handler.
#
# Integration expectations:
#   - Downstream modules (e.g., Data table) should combine this mask with any
#     other selection logic:
#       mask <- shared_data$filter_for_data[[active_dataset_name]]
#       idx  <- which(mask)   # row indices passing the filter
#   - Ensure shared_data is a mutable list-like environment (e.g., environment).
#
# ------------------------------------------------------------------------------

# Exposed variable: change manually if needed
FILTER_COL_NAME  <- "QUAL"
FILTER_RANGE_MIN <- 0L
FILTER_RANGE_MAX <- 100L

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
filterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Filter"),
    fluidRow(
      column(
        width = 8,
        sliderInput(
          inputId = ns("qual_range"),
          label   = sprintf("%s range", FILTER_COL_NAME),
          min     = FILTER_RANGE_MIN,
          max     = FILTER_RANGE_MAX,
          value   = c(FILTER_RANGE_MIN, FILTER_RANGE_MAX),
          step    = 1,
          dragRange = TRUE
        )
      ),
      column(
        width = 4,
        br(),
        actionButton(ns("apply_filter"), "Apply filter", icon = icon("filter"))
      )
    ),
    # (Optional placeholder for future summary text)
    textOutput(ns("filter_summary"))
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
filterServer <- function(id, shared_data, shared_rx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Ensure containers exist
    if (is.null(shared_data$filter_for_data)) {
      shared_data$filter_for_data <- list()
    }

    # Reactive to display simple summary (can be expanded later)
    output$filter_summary <- renderText({
      rng <- input$qual_range
      if (is.null(rng) || length(rng) != 2) return("")
      sprintf("Current %s filter: [%d, %d]", FILTER_COL_NAME, as.integer(rng[1]), as.integer(rng[2]))
    })

    observeEvent(input$apply_filter, {
      rng <- input$qual_range
      if (is.null(rng) || length(rng) != 2) return()

      minv <- as.integer(rng[1])
      maxv <- as.integer(rng[2])

      data_list <- shared_data$original_data
      if (is.null(data_list) || !length(data_list)) {
        shared_data$filter_for_data <- list()
        return()
      }

      # Recompute masks for each dataset
      masks <- vector("list", length(data_list))
      names(masks) <- names(data_list)

      for (nm in names(data_list)) {
        df <- data_list[[nm]]

        # Defensive: if dataset not a data.frame-like object
        if (is.null(df) || !is.data.frame(df)) {
          masks[[nm]] <- logical()
          next
        }

        n <- nrow(df)
        if (!n) {
            masks[[nm]] <- logical(0)
            next
        }

        if (!(FILTER_COL_NAME %in% names(df))) {
          # Missing target column -> all FALSE (per spec)
          masks[[nm]] <- rep_len(FALSE, n)
          next
        }

        colv <- df[[FILTER_COL_NAME]]

        # Coerce to numeric if not already (defensive)
        if (!is.numeric(colv)) {
          suppressWarnings(colv <- as.numeric(colv))
        }

        # NA fails; inclusive range
        pass <- !is.na(colv) & colv >= minv & colv <= maxv
        masks[[nm]] <- pass
      }

      # Store all masks
      shared_data$filter_for_data <- masks

      # print masks to console for debugging
      cat("[filter masks] ", paste(sapply(masks, function(x) sum(x, na.rm = TRUE)), collapse = ", "), "\n", sep = "")
      bump_version(shared_rx = shared_rx)


      # (Optional: store metadata / last applied range)
      # shared_data$filter_meta$last_range <- c(minv, maxv)
      # shared_data$filter_meta$col <- FILTER_COL_NAME
    }, ignoreNULL = TRUE)
  })
}