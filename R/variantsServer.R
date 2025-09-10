variants_server <- function(id, shared_data, initial_order) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      cat("[DEBUG] All input names:", names(reactiveValuesToList(input)), "\n")
    })

    # Safety: require a reactiveVal-style initial_order (read/write).
    if (!is.function(initial_order) || length(formals(initial_order)) == 0) {
      stop("initial_order must be a reactiveVal (i.e. a function that accepts an argument to set and returns value when called without args).")
    }

    output$table <- renderDT({
      df <- shared_data()
      req(df)
      all_cols <- colnames(df)

      # determine visible columns using initial_order (reactiveVal expected)
      current_order <- isolate(initial_order())
      if (is.null(current_order) || length(current_order) == 0) {
        visible_cols <- all_cols
      } else {
        visible_cols <- intersect(current_order, all_cols)
      }
      hidden_cols <- setdiff(all_cols, visible_cols)

      # robust 0-based index computation for DataTables
      hidden_idx <- which(all_cols %in% hidden_cols) - 1  # 0-based for DT

      datatable(
        df,
        extensions = c("ColReorder", "Buttons"),
        options = list(
          dom = "Bfrtip",
          scrollX = TRUE,
          autoWidth = TRUE,
          stateSave = TRUE,
          stateDuration = -1,
          # ensure DT state includes column names
          columns = lapply(all_cols, function(n) list(name = n)),
          columnDefs = c(
            list(list(width = "150px", targets = "_all")),
            if (length(hidden_idx) > 0) list(list(visible = FALSE, targets = hidden_idx))
          ),
          colReorder = TRUE,
          buttons = list(list(extend = "colvis", text = "Select Columns"))
        ),
        selection = "none",
        rownames = FALSE
      )
    }, server = TRUE)  # always use server-side processing as requested

    # Observe DT state changes and update initial_order (avoid loops)
    observe({
      state <- input$table_state
      req(state)
      if (!is.null(state$columns) && length(state$columns) > 0) {
        # extract best available name for each column from state
        new_order <- vapply(state$columns, FUN.VALUE = character(1), FUN = function(col) {
          if (!is.null(col$name) && nzchar(col$name)) return(col$name)
          if (!is.null(col$data) && nzchar(col$data)) return(col$data)
          if (!is.null(col$title) && nzchar(col$title)) return(col$title)
          NA_character_
        }, USE.NAMES = FALSE)

        # drop any NA and keep only columns that still exist in the df
        new_order <- new_order[!is.na(new_order)]
        sd <- isolate(shared_data())
        df_cols <- if (!is.null(sd)) colnames(sd) else character(0)
        new_order <- intersect(new_order, df_cols)

        # update only when different to avoid reactive loops
        old_order <- isolate(initial_order())
        if (!identical(old_order, new_order)) {
          initial_order(new_order)
        }
      }
    })
  })
}