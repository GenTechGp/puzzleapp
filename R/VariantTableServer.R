# Define a server module for the "Tab Label"
tabServer <- function(id, filtered_data, vars, preselected_vars) {
  moduleServer(id, function(input, output, session) {

    #ns <- session$n
    ns <- shiny::NS(id)

    observe({
      # Example condition to set show_file_saving
      show_file_saving <- FALSE  # Or set this based on some reactive expression
      updateCheckboxInput(session, "show_file_saving", value = show_file_saving)
    })

    # Reactive value to store the order of columns
    ordered_columns <- reactiveVal(setdiff(preselected_vars,"Color"))

    # Generate sortable list of selected columns
    output$sortable_columns <- renderUI({
      selected_columns <- ordered_columns()
      if (!is.null(selected_columns)) {
        rank_list(
          text = NULL,
          labels = selected_columns,
          input_id = ns("column_order")
        )
      }
    })

    # Update the reactive value with the new order when it changes
    observeEvent(input$column_order, {
      ordered_columns(input$column_order)
    })

    # initialize help table
    transition <- reactiveValues()
    transition$table <- data.frame(
      "colnames" = vars,
      "filter" = rep("", length(vars)),
      "active" = vars %in% preselected_vars
    )
    
    # Update table if sidebar input is changed
    fileData <- reactive({
      req(filtered_data())  # Ensure filtered_data() is not NULL
      
      dataset <- as.data.frame(filtered_data())
      selected_vars <- input$selected_vars
      
      if (!("PRIORITY" %in% names(dataset))) dataset$PRIORITY <- 0
      if (!("NOTES" %in% names(dataset))) dataset$NOTES <- ""
      if (!("Color" %in% names(dataset))) dataset$Color <- "#FFFFFF"

      # Return dataset with the selected variables
      dataset[, selected_vars, drop = FALSE]
    })
    
    # before table is updated save all filter settings in transition$table
    observeEvent(c(input$selected_vars, input$table_search_columns), {
      active_cols <- transition$table$colnames %in% input$selected_vars
      if (length(input$table_search_columns) != 0) {
        transition$table$filter[active_cols] <- input$table_search_columns
      }
      transition$table$active <- active_cols

      current_order <- ordered_columns()
      new_selection <- setdiff(setdiff(input$selected_vars,"Color"), current_order)
      deselected <- setdiff(current_order, setdiff(input$selected_vars,"Color"))
      new_order <- c(setdiff(current_order, deselected), new_selection)
      ordered_columns(new_order)
    })

    observeEvent(fileData(), {
      show_spinner()
      #print("change")
      # update global search and column search strings
      default_search <- input$table_search

      # set column settings
      default_search_columns <- c(
        "",
        transition$table[transition$table[,"active"] == TRUE, "filter"]
      )

      # update the search terms on the proxy table (see below)
      proxy %>% updateSearch(keywords = list(global = default_search, columns = default_search_columns))
      proxy %>% selectRows(selected = input$table_rows_selected)
      hide_spinner()
    })


    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit

      # Retrieve the edited ID
      edited_id <- fileData()[info$row, "ID"]
      #print(info)
      #print(edited_id)

      # Ensure the edit was in the PRIORITY column


      # Get the full dataset, not just the currently selected columns
      full_dataset <- isolate(filtered_data())

      # Find the index of the row with the matching ID in the full dataset
      row_index <- which(full_dataset$ID == edited_id)

      if (length(row_index) == 1) {  # Ensure we have exactly one match

        # Handle edits to the PRIORITY column
        if (names(fileData())[info$col] == "PRIORITY") {

          # Update the PRIORITY value in the full dataset
          full_dataset[row_index, "PRIORITY"] <- as.numeric(info$value)

          # Update the Color column based on PRIORITY value
          full_dataset[row_index, "Color"] <- ifelse(full_dataset[row_index, "PRIORITY"] > 0, "#90EE90", "#FFCCCC")

          # Handle edits to the NOTES column
        } else if (names(fileData())[info$col] == "NOTES") {

          # Update the NOTES value in the full dataset
          full_dataset[row_index, "NOTES"] <- info$value

        }
        # Re-assign the updated data to the reactive object, preserving all columns
        filtered_data(full_dataset)
      } else {
        warning("ID not found or multiple matches.")
      }
    })
    

    observe({
      show_spinner()

      output$table <- DT::renderDT({

        # Check if fileData is valid and has columns
        if (!is.null(fileData()) && ncol(fileData()) > 0) {

          # Verify that fileData has column names
          if (length(names(fileData())) > 0) {

            # Retrieve selected columns
            selected_columns <- ordered_columns()

            # Prepare fileData by ensuring only existing columns are used
            fileData <- fileData()[, names(fileData())]

            # Check if all selected columns are present in fileData
            if (all(selected_columns %in% names(fileData()))) {

              # Case 1: If "Color" column is present, set up datatable with "Color" styling
              if ("Color" %in% names(fileData)) {
                DT::datatable(
                  fileData[, c(selected_columns, "Color"), drop = FALSE],
                  filter = list(position = "top", clear = TRUE),
                  selection = "none",
                  escape = FALSE,
                  options = list(
                    stateSave = FALSE,
                    lengthMenu = list(c(25, 50, -1), c("25", "50", "All")),
                    columnDefs = list(
                      # Hide "Color" column in display
                      list(visible = FALSE, targets = which(c(selected_columns, "Color") %in% c("Color"))),
                      list(targets = '_all', className = 'dt-body-nowrap')
                    )
                  ),
                  editable = list(target = "cell", columns = 1)
                ) %>%
                  # Apply row background color based on "Color" column values
                  formatStyle(columns = c(selected_columns, "Color"), valueColumns = "Color", backgroundColor = JS("value"), target = 'row')

                # Case 2: If "Level4" column is present, display datatable without "Color" styling
              } else if ("Level4" %in% names(fileData)) {
                DT::datatable(
                  fileData[, c(selected_columns), drop = FALSE],
                  filter = list(position = "top", clear = TRUE),
                  selection = "none",
                  escape = FALSE,
                  options = list(
                    stateSave = FALSE,
                    lengthMenu = list(c(25, 50, -1), c("25", "50", "All"))
                  )
                )
              }
            }
          }
        } else {
          # Display message if fileData is null or empty
          DT::datatable(
            data.frame(Message = "No data available"),
            selection = "none",
            escape = FALSE,
            options = list(
              dom = 't',
              paging = FALSE,
              searching = FALSE
            )
          )
        }

      })

      hide_spinner()
    })

    # Save table to output file
    observeEvent(input$save_file, {
      show_spinner()
      output_dir <- input$output_dir
      filename <- input$out_filename
      filetype <- input$filetype
      scope <- input$out_scope

      # Check if output directory is specified
      if (output_dir == "") {
        shinyalert("Error", "Output directory is empty. Please provide a valid directory.", type = "error")
        hide_spinner()
        return(NULL)
      }

      # Check if output directory exists
      if (!dir.exists(output_dir)) {
        shinyalert("Error", "Output directory does not exist. Please provide a valid directory.", type = "error")
        hide_spinner()
        return(NULL)
      }

      # Add the filetype extension to the filename
      file_extension <- switch(filetype,
                               "excel" = "xlsx",
                               "tsv" = "tsv",
                               "csv" = "csv")
      full_filename <- paste0(filename, ".", file_extension)
      output_path <- file.path(output_dir, full_filename)

      if (scope == "all") {
        dataset <- data.table(filtered_data())
        dataset[, c("ClinVar", "GNOMADv4", "Color") := NULL]
      } else {
        dataset <- data.table(fileData()[,names(fileData())])
        dataset[, c("ClinVar", "GNOMADv4", "Color") := NULL]
      }

      tryCatch({
        if (filetype == "excel") {
          write.xlsx(dataset, output_path)
        } else if (filetype == "tsv") {
          fwrite(dataset, output_path, sep = "\t")
        } else if (filetype == "csv") {
          fwrite(dataset, output_path, sep = ",")
        }
        shinyalert("Success", paste("File saved successfully:", full_filename), type = "success")
      }, error = function(e) {
        shinyalert("Error", paste("Error saving file:", e$message), type = "error")
      })
      hide_spinner()
    })

    proxy <- dataTableProxy("table")

  })
}
