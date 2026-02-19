#' Raw Filter Tab Server
#' @param id Module ID
#' @param shared_store A reactiveValues object to share data across modules
#' @param shared_rx A list of reactive values for cross-module communication
#' @export
#' @import shiny
rawFilterServer <- function(id, shared_store, shared_rx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    filters_df <- reactiveVal(NULL)
    dt_edits   <- reactiveVal(list())

    observeEvent(input$btn_load_raw_filters, {
      req(input$load_from_file); path <- input$load_from_file
      validate(need(file.exists(path), "File does not exist"))

      df <- tryCatch(
        read.table(path, header = FALSE, sep = "\t", quote = "", stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      validate(
        need(!is.null(df), "Failed to read file"),
        need(ncol(df) == 2, "File must have exactly 2 columns")
      )

      colnames(df) <- c("Name", "Value")
      filters_df(df)
      dt_edits(list())
    })

    output$filter_table <- DT::renderDT({
      req(filters_df())
      DT::datatable(
        filters_df(),
        rownames = FALSE,
        selection = "none",
        editable = list(target = "cell", disable = list(columns = 0)), # disable first column
        options = list(paging = FALSE, searching = FALSE, info = FALSE),
        escape = TRUE
      )
    })

    observeEvent(input$filter_table_cell_edit, {
      info <- input$filter_table_cell_edit
      # DT sends 0-based column index; convert to 1-based for R data.frame
      row <- info$row
      col <- info$col + 1
      value <- info$value
      cat(sprintf("Cell edited: row=%d col=%d (1-based) value=%s\n", row, col, value))

      edits <- dt_edits()
      edits[[length(edits) + 1]] <- list(row = row, col = col, value = value)
      dt_edits(edits)
    })

    observeEvent(input$btn_apply_raw_filters, {
      req(filters_df())
      df <- filters_df()

      # Apply queued edits
      for (e in dt_edits()) {
        # coerce to the correct type of the target cell
        # df[e$row, e$col] <- DT::coerceValue(e$value, df[e$row, e$col])
        original_value <- df[[e$col]][e$row]
        coerced_value  <- DT::coerceValue(e$value, original_value)
        df[e$row, e$col] <- coerced_value
      }

      # cat("=== Applied filters (from DT input) ===\n"); print(df)

      # Update the reactive data so the table reflects changes
      filters_df(df)

      # Clear the queued edits after applying
      dt_edits(list())
      # get a data.table and pass to raw_filter_apply
      dt <- data.table::data.table(df)
      raw_filter_apply(filter_table = dt)
    })

    raw_filter_apply <- function(filter_table){
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

      filters       <- puzzlecore_parse_filter_table(filter_table)
      snv_filters   <- filters$snv_filters
      sv_filters    <- filters$sv_filters
      allele_counts_dt <- puzzlecore_allele_counts_table(shared_store$samples, snv_filters$inheritance_filter, snv_filters$custom_allele_counts)
      
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
    }

  })
}