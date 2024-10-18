# Define a server module for the "Tab Label"
tabServer <- function(id, filtered_data, vars, preselected_vars) {
  moduleServer(id, function(input, output, session) {
    
    #ns <- session$n
    ns <- shiny::NS(id)
    
    # Store initial order
    column_order <- reactiveVal(seq_along(vars))
    
    observe({
      # Example condition to set show_file_saving
      show_file_saving <- FALSE  # Or set this based on some reactive expression
      updateCheckboxInput(session, "show_file_saving", value = show_file_saving)
    })
    
    output$selected_vars_box <- renderUI({
      # clinvar_checkboxes_list <- list(prettyCheckboxGroup(ns("clinvar_checkboxes"), "",
      #                                                     choiceNames = clinvar_options_display,
      #                                                     choiceValues = clinvar_options,
      #                                                     selected = NULL,inline = TRUE))
      
      checkboxGroupInput(ns("selected_vars"), NULL,
                         choices = vars,
                         selected = preselected_vars)
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
    
    # Update table if sidebar input is changed (lacy)
    fileData <- reactive({
      if (!is.null(filtered_data())) {
        dataset <- filtered_data()
        #print(names(dataset))
        selected_vars <- input$selected_vars
        #print(selected_vars)
        # Add PRIORITY & NOTES columns at the beginning and Color column at the end
        # Add PRIORITY and Color columns if they don't exist
        if (!("PRIORITY" %in% names(dataset))) {
          dataset <- data.frame(PRIORITY = 0, NOTES="", dataset)
        }
        if (!("Color" %in% names(dataset))) {
          dataset$Color <-"#FFFFFF"
        }
        new_dataset <- dataset[, selected_vars, drop = FALSE]
      } else {
        new_dataset <- data.frame()  # Return an empty data frame if filtered_data() is NULL
      }
      
    })
    
    # before table is updated save all filter settings in transition$table
    observeEvent(c(input$selected_vars, input$table_search_columns), {
      # Set type
      #print("transition")
      transition$table[,"filter"] <- as.character(transition$table[,"filter"])
      
      # save filter settings in currently displayed columns
      if (length(input$table_search_columns) != 0) {
        transition$table[transition$table[,"active"] == TRUE, "filter"] <- input$table_search_columns
      }
      
      # save new column state after changing
      transition$table[,"active"] <- transition$table[,"colnames"] %in% input$selected_vars
      
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
    
    
    # observeEvent(input$table_cell_edit, {
    #   info <- input$table_cell_edit
    #   
    #   # Ensure the edit was in the PRIORITY column
    #   if (names(fileData())[info$col] == "PRIORITY") {
    #     
    #     # Update the PRIORITY value in the reactive dataset
    #     fileData <- isolate(fileData())
    #     fileData[info$row, "PRIORITY"] <- as.numeric(info$value)
    #     
    #     # Update the Color column based on PRIORITY value
    #     fileData[info$row, "Color"] <- ifelse(fileData[info$row, "PRIORITY"] > 0, "#90EE90", "#FFCCCC")  # example colors: light red for >0, light blue for <=0
    #     
    #     # Re-assign the updated data to the reactive object
    #     filtered_data(fileData)
    #   }
    # })
    
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
        
        # # Update the PRIORITY value in the full dataset
        # full_dataset[row_index, "PRIORITY"] <- as.numeric(info$value)
        # 
        # # Update the Color column based on PRIORITY value
        # full_dataset[row_index, "Color"] <- ifelse(full_dataset[row_index, "PRIORITY"] > 0, "#90EE90", "#FFCCCC")
        # 
        # # Re-assign the updated data to the reactive object, preserving all columns
        # #print(names(filtered_data()))
        # filtered_data(full_dataset)
        # #print(names(filtered_data()))
      } else {
        warning("ID not found or multiple matches.")
      }
    })
    
    
    observe({
      show_spinner()
      output$table <- DT::renderDT({
        
        if (!is.null(fileData()) && ncol(fileData()) > 0) {
          #print("not null")
          if (length(names(fileData())) > 0) {
            #print(length(names(fileData())))
            selected_columns <- ordered_columns()
            #print(selected_columns)
            #print(names(fileData()))
            
            fileData <- fileData()[,names(fileData())]
            #print(class(fileData))
            if (all(selected_columns %in% names(fileData()))) {
              if ("Color" %in% names(fileData)) {
                #print(names(fileData))
                #print(c(selected_columns,"Color"))
                DT::datatable(fileData[,c(selected_columns,"Color"),drop=FALSE], filter = list(position="top",clear=TRUE), selection = "none", escape = FALSE, options = list(stateSave = FALSE,lengthMenu = list(c(25, 50, -1), c("25", "50", "All")),
                                                                                                                                                                              columnDefs = list(
                                                                                                                                                                                list(visible = FALSE, targets = which(c(selected_columns,"Color") %in% c("Color"))),
                                                                                                                                                                                list(targets = '_all', className = 'dt-body-nowrap') 
                                                                                                                                                                              )),editable = list(target = "cell", columns = 1)) %>%
                  formatStyle(columns = c(selected_columns,"Color"), valueColumns = "Color", backgroundColor = JS("value"), target = 'row')
              } else if ("Level4" %in% names(fileData)) {
                #print("else if")
                DT::datatable(fileData[,c(selected_columns),drop=FALSE], filter = list(position="top",clear=TRUE), selection = "none", escape = FALSE,options = list(stateSave = FALSE,lengthMenu = list(c(25, 50, -1), c("25", "50", "All")))) 
              }
            }
          }
        } else {
          #print("else")
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
