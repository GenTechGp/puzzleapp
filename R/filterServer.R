#' Filter Tab Server
#' @param id Module ID
#' @param shared_store A reactiveValues object to share data across modules
#' @param shared_rx A list of reactive values for cross-module communication
#' @export
#' @import shiny

selectFiltersServer <- function(id, shared_store, shared_rx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    phenos <- reactiveVal()
    green_genes <- reactiveVal()
    red_genes <- reactiveVal()
    amber_genes <- reactiveVal()
    unclassified_genes <- reactiveVal()
    allele_table_ready <- reactiveVal(FALSE)
    updating_filters <- reactiveVal(FALSE)

    # Sessions
    sessions_dir <- reactive({
      req(shared_rx$data_version())
      if(is.null(shared_store$work_dir)) {
        return(NULL)
      }
      if(is.null(shared_store$pedigree)) {
        return(NULL)
      }
      pedigree <- shared_store$pedigree
      sample <- pedigree[kinship == "proband", sample_id]
      sprintf("%s/puzzleapp/saved_sessions/%s", shared_store$work_dir, sample)
    })
    sessions <- reactiveVal(character())
    observe({
      sessions(list_files(sessions_dir()))
    })

    # Update UI dropdown with available sessions
    observe({
      updateSelectizeInput(session, "available_sessions", choices = c("", sessions()), selected = "")
      cat("[filtServer] Updated UI dropdowns for available sessions\n")
      updateSelectizeInput(session, "delete_sessions", choices = c("", sessions()), selected = "")
    })

    observeEvent(input$btn_save_session, {
      snvs_data <- shared_store$original_data[["SNV"]]
      svs_data <- shared_store$original_data[["SV"]]
      if (is.null(snvs_data) || is.null(svs_data)) {
        showNotification("No data available to save. Please load datasets in the Home tab.", type = "error")
        return()
      }
      if (save_session_data(input, input$session_name, sessions_dir(), snvs_data, svs_data, phenos(), shared_store$samples, shared_store$sticky_work_dir)) {
        sessions(list_files(sessions_dir()))
        message("Session saved. It will appear in the list automatically.")
        showNotification(sprintf("Session '%s' saved.", input$session_name), type = "message")
      }
    })

    observeEvent(input$btn_load_session, {
      snvs_data <- shared_store$original_data[["SNV"]]
      svs_data <- shared_store$original_data[["SV"]]
      if (is.null(snvs_data) || is.null(svs_data)) {
        showNotification("No data available to load. Please load datasets in the Home tab.", type = "error")
        return()
      }
      if (is.null(input$available_sessions) || input$available_sessions == "") {
        showNotification("Please select a session to load.", type = "error")
        return()
      }
      loaded <- load_session_data(input, input$available_sessions, sessions_dir(), snvs_data, svs_data)
      if (!is.null(loaded)) {
        reset_params <- filters_state$all[['Reset_all']]
        update_filters_params(reset_params, session)
        update_filters_params(loaded$filters, session)
        phenos(c(loaded$filters[["HPO_Terms"]]))
        
        # pre-saved filter
        updating_filters(TRUE)  # start "isolated" mode
        choice <- loaded$filters[["PreSaved_Filter"]]
        choices <- c("", names(filters_state$all))
        # check if choice is in choices (choice can be empty)
        if (!is.null(choice) && choice != "" && choice %in% choices) {
          updateSelectizeInput(session, "pre_saved_filters", selected = choice)
        } else {
          updateSelectizeInput(session, "pre_saved_filters", selected = "")
        }
        shinyjs::delay(100, updating_filters(FALSE))  # release after updates

        # Wait 500 ms (enough time for UI to render) if loaded$filters$Inheritance is "Custom" then update custom alleles
        if (loaded$filters$Inheritance == "Custom") {
          log_info("[filtServer] Loaded session has Custom inheritance, will update custom alleles after delay")
          shinyjs::delay(500, {
            if (allele_table_ready()){
              update_custom_alleles(loaded$filters, session)
            } else {
              log_error("Allele table not ready after 500ms, cannot update custom alleles")
            }
          })
        }
        showNotification(sprintf("Session '%s' loaded.", input$available_sessions), type = "message")
        isolate({
          shinyjs::delay(500, shinyjs::click("btn_apply_filters"))
        })
      }
    })

    observeEvent(input$btn_delete_session, {
      if (is.null(input$delete_sessions) || input$delete_sessions == "") {
        showNotification("Please select a session to delete.", type = "error")
        return()
      }
      if (delete_session_data(input$delete_sessions, sessions_dir())) {
        sessions(list_files(sessions_dir()))
        message("Session deleted. It will be removed from the list automatically.")
        showNotification(sprintf("Session '%s' deleted.", input$delete_sessions), type = "message")
      }
    })

    alleleCustomTable <- function(ns, pedigree) {
      tagList(
        tags$table(
          tags$thead(tags$tr(tags$th(style = "padding: 0.5em 1em; min-width: 120px;", "Sample ID"),tags$th(style = "padding: 0.5em 1em; min-width: 120px;", "Allele Count"))),
          tags$tbody(
            lapply(pedigree, function(sample) {
              sid <- sample$sample_id
              radio_id <- ns(paste0("allele_", sid))
              tags$tr(
                tags$td(style = "vertical-align: baseline; padding-right: 1em;", sid),
                tags$td(radioButtons(inputId = radio_id,label = NULL,choices = c("None" = "", "0", "0-1", "1", "1-2", "2"),inline = TRUE,selected = "")))}))))
    }

    output$allele_ui <- renderUI({
      req(input$inher)
      if(is.null(shared_store$samples)) return(NULL)
      # ped <- convert_samples_to_pedigree(shared_store$samples)
      ped <- shared_store$samples
      if (input$inher == "") return(NULL)
      if (input$inher == "Custom") {
        # Render input radio buttons for custom allele counts
        ui <- alleleCustomTable(ns, ped)
        allele_table_ready(TRUE)
        log_info("[filtServer] Custom inheritance selected, setting allele_table_ready to TRUE")
        return(ui)
      } else {
        # Compute allele counts (named list) and convert to data.frame for display
        counts <- puzzlecore_compute_allele_table(ped, input$inher)
        if (length(counts) == 0) return(NULL)
        tbl <- data.frame(
          Sample_ID = names(counts),
          Allele_Count = unlist(counts, use.names = FALSE),
          stringsAsFactors = FALSE
        )
        allele_table_ready(TRUE)
        return(DT::datatable(tbl, rownames = FALSE, options = list(dom = 't', paging = FALSE)))
      }
    })

    observeEvent(input$inher, {
      allele_table_ready(FALSE)
      log_info("[filtServer] Inheritance mode changed, resetting allele_table_ready to FALSE")
    })
    outputOptions(output, "allele_ui", suspendWhenHidden = FALSE)

    observeEvent(input$btn_apply_filters, {
      cat("[filterServer] Apply filters clicked\n")
      snvs_data <- shared_store$original_data[["SNV"]]
      svs_data <- shared_store$original_data[["SV"]]
      panel_app_genes <- shared_store$panel_app_genes
      vep_consequences <- shared_store$vep_consequences
      phenotype_data <- shared_store$phenotype_data
      pedigree <- shared_store$pedigree
      if (is.null(snvs_data) || is.null(svs_data) || is.null(pedigree) || is.null(panel_app_genes) || is.null(vep_consequences) || is.null(phenotype_data)) {
        showNotification("No data available to filter. Please load datasets in the Home tab.", type = "error")
        return()
      }
      # ped <- convert_samples_to_pedigree(shared_store$samples)
      ped <- shared_store$samples
      allele_counts <- getAlleleCounts(ped, input)
      cat("class of allele_counts:", class(allele_counts), "\n")
      cat("Allele counts:\n")
      print(allele_counts)

      filter_table <- capture_filters(input, phenos(), samples, flag_save_samples=TRUE, flag_save_hpo_panelapp=TRUE, flag_save_presaved_filter=FALSE)
      filters       <- puzzlecore_parse_filter_table(filter_table)
      snv_filters   <- filters$snv_filters
      sv_filters    <- filters$sv_filters

      allele_counts_dt <- data.table(
        sample_id = names(allele_counts),
        allele_count = unlist(allele_counts, use.names = FALSE)
      )
      svlog_db <- shared_store$svlog_db
      filtered_data <- puzzlecore_variant_filter(snv_data=snvs_data, sv_data=svs_data, snv_filters=snv_filters, sv_filters=sv_filters, pedigree=pedigree, allele_tab=allele_counts_dt, panel_app_genes=panel_app_genes, vep_consequences=vep_consequences, phenotype_data=phenotype_data, svlog_db=svlog_db)
      shared_store$data_for_data[["SNV"]] <- filtered_data$snv
      shared_store$data_for_data[["SV"]] <- filtered_data$sv

      # add a check if columns order is same as before error out
      col_order <- colnames(shared_store$original_data[["SNV"]])
      col_order_filtered <- colnames(shared_store$data_for_data[["SNV"]])
      if (!identical(col_order, col_order_filtered)) {
        cat("original:", paste(col_order, collapse = ", "), "\n")
        cat("filtered:", paste(col_order_filtered, collapse = ", "), "\n")
        stop("Column order changed after filtering!")
      }
      showNotification(sprintf("snvs_data_filtered to %s rows", nrow(shared_store$data_for_data[["SNV"]])), type = "message")
      showNotification(sprintf("svs_data_filtered to %s rows", nrow(shared_store$data_for_data[["SV"]])), type = "message")

      # add spliceai_filter to value_for_data
      shared_store$value_for_data[["SNV"]] <- list(splice_numeric_threshold = snv_filters$spliceai_filter)
      shared_store$value_for_data[["SV"]] <- list(splice_numeric_threshold = sv_filters$spliceai_filter)

      bump_version(version_type = "data", shared_rx = shared_rx)
    })

    updateAnnotationSelection <- function(selected_option, checkbox_id, session) {
      cat(sprintf("Annotation selection changed: %s\n", selected_option))
      annotation_map <- list(
        "High impact" = c("Stop gained", "Start lost", "Stop lost", "Splice variant", "Frameshift variant"),
        "Moderate to high impact" = c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                                      "Frameshift variant", "Missense variant", "In-frame variant")
      )
      select <- annotation_map[[selected_option]]
      if (is.null(select)) select <- character(0)
      updateCheckboxGroupInput(session, checkbox_id, selected = select)
    }

    observeEvent(input$annotation, {
      updateAnnotationSelection(input$annotation, "conseq_checkboxes", session)
    })

    observeEvent(input$sv_annotation, {
      updateAnnotationSelection(input$sv_annotation, "sv_conseq_checkboxes", session)
    })

    observeEvent(input$sv_annotation, {
      updateAnnotationSelection(input$sv_annotation, "sv_conseq_checkboxes", session)
    })

    updatePathogenicitySelection <- function(selected_option, checkbox_id, session) {
      cat(sprintf("Pathogenicity selection changed: %s\n", selected_option))
      pathogenicity_map <- list(
        "Pathogenic/Likely pathogenic" = c("Pathogenic", "Likely pathogenic"),
        "Not benign" = c("Pathogenic", "Likely pathogenic", "VUS", "Conflicting")
      )
      select <- pathogenicity_map[[selected_option]]
      if (is.null(select)) select <- character(0)
      updateCheckboxGroupInput(session, checkbox_id, selected = select)
    }

    observeEvent(input$pathogenicity, {
      updatePathogenicitySelection(input$pathogenicity, "clinvar_checkboxes", session)
    })

    observeEvent(input$sv_pathogenicity, {
      updatePathogenicitySelection(input$sv_pathogenicity, "sv_clinvar_checkboxes", session)
    })

    ##################### PanelApp
    observeEvent(shared_rx$panelapp_version(), {
      panel_app_genes <- shared_store$panel_app_genes
      if(is.null(panel_app_genes)) return(NULL)
      cat("[filtServer] Observing panel_app_genes for updates\n")
      locs <- c("", unique(panel_app_genes$Level4))
      updateSelectizeInput(session, "panelapp", choices = locs, selected = NULL)
      updateSelectizeInput(session, "substract_panelapp_gene_lists", choices = locs, selected = NULL)
      cat("[filtServer] Updated PanelApp selection options\n")
    })

    # Helper: case-insensitive setdiff for gene symbols (preserves original casing of x)
    setdiff_ci <- function(x, y) {
      if (length(y) == 0) return(x)
      x[!(toupper(x) %in% toupper(y))]
    }

    # Recompute gene boxes when either positive list, subtract list, or subtract free-text genes change
    observeEvent(
      list(input$panelapp, input$substract_panelapp_gene_lists, input$substract_panelapp_genes),
      {
        panel_app_genes <- shared_store$panel_app_genes
        if (is.null(panel_app_genes)) return(NULL)
        cat("[filtServer] PanelApp filter or subtract options updated\n")

        # Positive selection: genes to include by Level4
        pos_tmp <- panel_app_genes[Level4 %in% input$panelapp]

        # Prepare initial category lists
        greens <- sort(unique(pos_tmp[Sources == "Green", Entity_Name]))
        reds <- sort(unique(pos_tmp[Sources == "Red", Entity_Name]))
        ambers <- sort(unique(pos_tmp[Sources == "Amber", Entity_Name]))
        unclassified <- sort(unique(pos_tmp[!(Sources %in% c("Green", "Red", "Amber")), Entity_Name]))

        # Subtract selections (by panel lists)
        sub_tmp <- panel_app_genes[Level4 %in% input$substract_panelapp_gene_lists]
        subtract_from_panelapp <- sort(unique(toupper(sub_tmp$Entity_Name)))

        # Parse free-text subtract genes using helper (NOT "custom" anymore)
        subtract_from_text <- parse_gene_list(input$substract_panelapp_genes)  # already uppercased

        # Union of all genes to remove (uppercased)
        remove_set <- unique(c(subtract_from_panelapp, subtract_from_text))

        # Apply subtraction across all categories (case-insensitive)
        if (length(remove_set) > 0) {
          greens <- setdiff_ci(greens, remove_set)
          reds <- setdiff_ci(reds, remove_set)
          ambers <- setdiff_ci(ambers, remove_set)
          unclassified <- setdiff_ci(unclassified, remove_set)
        }

        # Update reactive values used by UI boxes
        green_genes(greens)
        red_genes(reds)
        amber_genes(ambers)
        unclassified_genes(unclassified)
      },
      ignoreInit = FALSE
    )
    
    # Render UI outputs for each gene category
    output$green_genes <- renderText({
      val <- green_genes()
      # cat("[debug] renderText green_genes():", paste(val, collapse=", "), "\n")
      paste(val, collapse="; ")
    })
    output$red_genes <- renderText({ paste(red_genes(), collapse = "; ") })
    output$amber_genes <- renderText({ paste(amber_genes(), collapse = "; ") })
    output$unclassified_genes <- renderText({ paste(unclassified_genes(), collapse = "; ") })

    ##################### Phenotype
    # Add multiple IDs to the phenotype list
    observeEvent(input$phenotype_add, {
      cat("[filtServer] Adding phenotype terms\n")
      phenotype_data <- shared_store$phenotype_data
      if(is.null(phenotype_data)) return(NULL)
      # cat("[filtServer] Available HPO IDs:", paste(head(phenotype_data$hpo_id, 10), collapse = ", "), "\n")
      new_ids <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))
      new_ids <- trimws(new_ids)
      #print(new_ids)
      if (length(new_ids) > 0) {
        current_ids <- phenos()
        valid_new_ids <- new_ids[new_ids %in% phenotype_data$hpo_id & !(new_ids %in% current_ids)]
        if (length(valid_new_ids) > 0) {
          phenos(c(current_ids, valid_new_ids))
          updateTextInput(session, "phenotype_var", value = "")
        }
      }
    })

    observeEvent(input$phenotype_remove, {
      cat("[filtServer] Removing phenotype terms\n")
      current_ids <- phenos()
      ids_to_remove <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))  # Split by spaces, commas, or semicolons
      ids_to_remove <- ids_to_remove[ids_to_remove %in% current_ids]
      if (length(ids_to_remove) > 0) {
        ids_to_remove <- trimws(ids_to_remove)  # Remove leading/trailing spaces
        ids_to_remove <- ids_to_remove[ids_to_remove != ""]  # Remove empty strings
        # Remove the terms that are in the current list
        updated_ids <- setdiff(current_ids, ids_to_remove)
        # Update the current phenotype terms list
        phenos(updated_ids)
        # Clear the input field
        updateTextInput(session, "phenotype_var", value = "")
      }
    })
    # Display the list of HPO terms
    output$phenotype <- renderText({
      paste(phenos(), collapse="; ")
    })

    ##################### Pre-saved searches
    saved_filters <- "puzzleapp/saved_filters"
    filters_state <- reactiveValues(
      all = NULL,
      work_dir = NULL
    )

    update_filters_dropdowns <- function(session, shared_store, saved_filters) {
      filters_state$all <- read_search_files(file.path(shared_store$work_dir, saved_filters), flag_all = TRUE)
      filters_state$work_dir <- read_search_files(file.path(shared_store$work_dir, saved_filters), flag_all = FALSE)
      choices <- c("", names(filters_state$all))
      cat("[filtServer] Updating pre_saved_filters choices:", paste(choices, collapse = ", "), "\n")
      updateSelectizeInput(session, "pre_saved_filters", choices = choices, selected = isolate(input$pre_saved_filters))
      choices_delete <- c("", names(filters_state$work_dir))
      cat("[filtServer] Updating delete_pre_saved_filters choices:", paste(choices_delete, collapse = ", "), "\n")
      updateSelectizeInput(session, "delete_pre_saved_filters", choices = choices_delete, selected = "")
      # updateSelectizeInput(session, "delete_pre_saved_filters", choices = choices_delete, selected = isolate(input$delete_pre_saved_filters))
    }

    observe({
      req(shared_rx$data_version())
      update_filters_dropdowns(session, shared_store, saved_filters)
    })

    observeEvent(input$btn_save_filters, {
      req(input$filters_save_name)
      file_path <- shared_store$work_dir
      if (is.null(file_path) || file_path == "") {
        showNotification("Work directory not set. Cannot save filters.", type = "error")
        return()
      }
      file_path <- file.path(file_path, saved_filters)
      save_filters(input, phenos(), file_path, shared_store$samples, shared_store$sticky_work_dir)
      update_filters_dropdowns(session, shared_store, saved_filters)
      showNotification(sprintf("Filters saved as '%s'.", input$filters_save_name), type = "message")
    })

    observeEvent(input$btn_delete_pre_saved_filters, {
      req(input$delete_pre_saved_filters)
      if (!is.null(input$pre_saved_filters) && input$pre_saved_filters == input$delete_pre_saved_filters) {
        showNotification("Can't delete a filter that is currently selected. Error.", type = "error")
        return()
      }
      file_path <- shared_store$work_dir
      if (is.null(file_path) || file_path == "") {
        showNotification("Work directory not set. Cannot delete filters.", type = "error")
        return()
      }
      file_path <- file.path(file_path, saved_filters)
      delete_filters(input$delete_pre_saved_filters, file_path)
      update_filters_dropdowns(session, shared_store, saved_filters)
      showNotification(sprintf("Filters '%s' deleted.", input$delete_pre_saved_filters), type = "message")
    })

    observeEvent(input$pre_saved_filters, {
      if (updating_filters()) return()  # skip during isolated update
      reset_params <- filters_state$all[['Reset_SNV_SV']]
      update_filters_params(reset_params, session)
      selected_filter <- input$pre_saved_filters
      filter_params <- filters_state$all[[selected_filter]]
      update_filters_params(filter_params, session)
      if (length(filter_params[["HPO_Terms"]]) > 0) {
        phenos(c(filter_params[["HPO_Terms"]]))
      }

      # This code part is not tested properly yet
      if (!is.null(filter_params$Inheritance)) {
        # Wait 500 ms (enough time for UI to render) if loaded$filters$Inheritance is "Custom" then update custom alleles
        if (filter_params$Inheritance == "Custom") {
          log_info("[filtServer] Loaded session has Custom inheritance, will update custom alleles after delay")
          shinyjs::delay(500, {
            if (allele_table_ready()){
              update_custom_alleles(filter_params, session)
            } else {
              log_error("Allele table not ready after 500ms, cannot update custom alleles")
            }
          })
        }
      }

    }, ignoreInit = TRUE)

    observeEvent(input$btn_reset, {
      clear_input_fields(session)
      reset_params <- filters_state$all[['Reset_all']]
      update_filters_params(reset_params, session)
      phenos(character(0))
    })


    # Helper to normalize HPO IDs
    clean_hpo_ids <- function(x) {
      if (is.null(x) || !length(x)) return(character(0))
      x <- gsub("\u00A0", " ", as.character(x), fixed = TRUE)      # normalize NBSP
      hits <- regmatches(x, gregexpr("\\bHP:\\d{7}\\b", x, perl = TRUE))
      ids <- unlist(hits, use.names = FALSE)
      if (!length(ids)) return(character(0))
      ids <- ids[!duplicated(ids)]                                 # preserve first order
      ids
    }

    # Sanitize phenos() in place whenever it changes (and once on startup)
    observe({
      ids <- phenos()
      cleaned <- clean_hpo_ids(ids)
      if (!identical(ids, cleaned)) {
        isolate(phenos(cleaned))
      }
    })

  })


}
