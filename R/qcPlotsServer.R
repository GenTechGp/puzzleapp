qcPlotsServer <- function(id, coverage_data, snvs_processed_data, pedigree_data, somalier) {
  moduleServer(id, function(input, output, session) {

    cat(sprintf("[qcServer] Module initialized\n"))
    
    ns <- session$ns

    # Function to process VAF data
    processVAFData <- function(data, pedigree) {
      cat("[qcServer] Processing VAF data\n")
      showNotification("Processing allele fraction data...", type = "message", duration = NULL, id = ns("notify_vaf"))
      if (is.null(data) || nrow(data[CATEGORY == "SNV & Indel"]) == 0) { 
        cat("[qcServer] Warning: VAF processing failed due to missing CATEGORY column\n")
        removeNotification(ns("notify_vaf"))
        return(NULL)
      }
      
      # Ensure pedigree has a 'code' column
      if (!"code" %in% names(pedigree)) {
        pedigree[, code := seq_len(.N)]
      }

      # Limit to 200,000 rows if necessary
      if (nrow(data) > 200000) {
        cat("[qcServer] Sampling VAF data: Reducing to 200,000 rows\n")
        showNotification("Sampling VAF data to 200,000 rows...", type = "message")
        sampled_data <- sample_n(data, 200000) 
      } else { 
        sampled_data <- data
      }

      # Extract and reshape VAF columns
      selected_columns <- c("ID", grep("^VAF_", names(sampled_data), value = TRUE))
      sampled_data <- sampled_data[CATEGORY == "SNV & Indel", ..selected_columns]

      sampled_data <- melt(sampled_data, id.vars = "ID")
      sampled_data[, c("variable", "code") := tstrsplit(variable, "_", fixed = TRUE)]
      sampled_data[, value := as.numeric(value)]
      sampled_data[, code := as.numeric(code)]

      # Merge with pedigree data
      sampled_data <- merge(sampled_data, pedigree, by = "code")
      sampled_data <- dcast(sampled_data[, .(ID, sample_id, variable, value)], ID + sample_id ~ variable, value.var = "value")
      setnames(sampled_data, old = names(sampled_data)[3], new = "AF")

      cat("[qcServer] VAF data processing complete\n")
      removeNotification(ns("notify_vaf"))
      return(sampled_data)
    }

    # Process VAF Data
    vaf_data <- processVAFData(snvs_processed_data, pedigree_data)
    
    cat("[qcServer] Rendering UI elements for coverage, VAF, and Somalier\n")
    showNotification("Rendering QC plot options...", type = "message")

    # Generate UI elements dynamically
    output$coverage_ui <- renderUI({
      if (!is.null(coverage_data)) {
        collapseUI("collapse_coverage", "Coverage analysis", "primary",
                   selectInput(ns("plot_type1"), "Coverage plot:", 
                               c("Average coverage", "Normalised coverage"))
        )
      }
    })

    output$vaf_ui <- renderUI({
      if (!is.null(snvs_processed_data) && nrow(snvs_processed_data[CATEGORY == "SNV & Indel"]) > 0) {
        collapseUI("collapse_vaf", "VAF distribution", "primary",
                   selectInput(ns("plot_type2"), "Allele fraction:", c("Allele fraction"))
        )
      }
    })

    output$somalier_ui <- renderUI({
      if (!is.null(somalier)) {
        collapseUI("collapse_somalier", "Somalier analysis", "primary",
                   selectInput(ns("x_var"), "X-axis:", 
                               c("relatedness", "ibs0", "ibs2", "hom_concordance", "shared_hets", 
                                 "shared_hom_alts", "hets_ab"), "ibs0"),
                   selectInput(ns("y_var"), "Y-axis:", 
                               c("relatedness", "ibs0", "ibs2", "hom_concordance", "shared_hets", 
                                 "shared_hom_alts", "hets_ab"), "ibs2")
        )
      }
    })

    # Dynamically render plots
    output$plot_output1 <- renderUI({
      req(input$plot_type1)
      if (!is.null(coverage_data)) {
        if (input$plot_type1 == "Average coverage") {
          plotlyOutput(ns("plot1"))
        } else {
          plotlyOutput(ns("plot2"))
        }
      }
    })

    output$plot_output2 <- renderUI({
      req(input$plot_type2)
      if (!is.null(snvs_processed_data) && nrow(snvs_processed_data[CATEGORY == "SNV & Indel"]) > 0) {
        plotlyOutput(ns("plot3"))
      }
    })

    output$somalier_output <- renderUI({
      if (!is.null(somalier)) {
        tagList(
          hr(),
          plotlyOutput(ns("somalier_plot")),
          DT::dataTableOutput(ns("definitions_table"))
        )
      }
    })

    # Plot coverage data
    if (!is.null(coverage_data)) {
      cat("[qcServer] Coverage data available - generating plots\n")
      output$plot1 <- renderPlotly({
        cat("[qcServer] Rendering coverage plot (Average Coverage)\n")
        showNotification("Rendering average coverage plot...", type = "message")
        plot_ly(coverage_data[CHROM != "chrM"], x = ~CHROM, y = ~AVERAGE_COVERAGE, color = ~SAMPLE,
                colors = RColorBrewer::brewer.pal(max(3, length(unique(coverage_data$SAMPLE))), "Set2"), 
                type = 'scatter', mode = 'markers', marker = list(size = 12, opacity = 0.6)) %>%
          layout(xaxis = list(title = ""), yaxis = list(title = "Average Coverage"), 
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      })

      output$plot2 <- renderPlotly({
        cat("[qcServer] Rendering coverage plot (Normalized Coverage)\n")
        showNotification("Rendering normalised coverage plot...", type = "message")
        autosomal_coverage <- coverage_data %>%
          filter(CHROM %in% paste0("chr", 1:22)) %>%
          group_by(SAMPLE) %>%
          summarize(avg_coverage = mean(AVERAGE_COVERAGE))

        normalized_data <- coverage_data %>%
          left_join(autosomal_coverage, by = "SAMPLE") %>%
          mutate(normalized_coverage = AVERAGE_COVERAGE / avg_coverage)

        plot_ly(normalized_data[CHROM != "chrM"], x = ~CHROM, y = ~normalized_coverage, color = ~SAMPLE,
                colors = RColorBrewer::brewer.pal(max(3, length(unique(normalized_data$SAMPLE))), "Set2"), 
                type = 'scatter', mode = 'markers', marker = list(size = 12, opacity = 0.6)) %>%
          layout(xaxis = list(title = ""), yaxis = list(title = "Average Coverage (Normalized)"), 
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      })
    }

    # Render VAF Plot
    if (!is.null(vaf_data)) {
      cat("[qcServer] VAF data available - generating plots\n")
      output$plot3 <- renderPlotly({
        cat("[qcServer] Rendering VAF distribution plot\n")
        showNotification("Rendering allele fraction plot...", type = "message")
        dens_list <- lapply(unique(vaf_data$sample_id), function(sample) {
          dens <- density(vaf_data$AF[vaf_data$sample_id == sample], na.rm = TRUE)
          data.frame(x = dens$x, y = dens$y, sample_id = sample)
        })

        dens_data <- do.call(rbind, dens_list)

        plot_ly(dens_data, x = ~x, y = ~y, color = ~sample_id, type = 'scatter', mode = 'lines',colors = RColorBrewer::brewer.pal(max(3, length(unique(dens_data$sample_id))), "Set2")) %>%
          layout(
            xaxis = list(title = "Allele Fraction", range = c(0, 1)),
            yaxis = list(title = "Density"),
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
            margin = list(b = 50)
          )
      })
    }

    # Plot Somalier
    if (!is.null(somalier)) {
      cat("[qcServer] Somalier data available - generating plots\n")
      output$somalier_plot <- renderPlotly({
        cat("[qcServer] Rendering Somalier relatedness plot\n")
        showNotification("Rendering Somalier plot...", type = "message")
        req(input$x_var, input$y_var)

        plot_ly(somalier, x = ~get(input$x_var), y = ~get(input$y_var), text = ~paste(sample_a, sample_b), 
                type = 'scatter', mode = 'markers', marker = list(size = 14, opacity = 0.6), color = ~pair) %>%
          layout(xaxis = list(title = input$x_var), yaxis = list(title = input$y_var),
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
                 margin = list(b = 50))
      })

      output$definitions_table <- DT::renderDataTable({
        datatable(somalier_dict, options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
      })
    }

  })
}
