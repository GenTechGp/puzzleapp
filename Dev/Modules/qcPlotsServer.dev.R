qcPlotsServer <- function(id,coverage_data,snvs_processed_data,pedigree_data,somalier) {
  moduleServer(id, function(input, output, session) {
    
    # Sample 1000 rows and select columns
    sampled_data <- snvs_processed_data  %>% sample_n(200000)
#    sampled_data <- sampled_data %>% 
#      select(ID, starts_with("AD_"), starts_with("DP_"))
    #selected_columns <- c("ID", grep("^AD_", names(sampled_data), value = TRUE), grep("^DP_", names(sampled_data), value = TRUE))
    selected_columns <- c("ID", grep("^VAF_", names(sampled_data), value = TRUE))
    sampled_data <- sampled_data[CATEGORY=="SNV & Indel", c(selected_columns),with=FALSE]
    sampled_data <- melt(sampled_data,id.vars=c("ID"))
    sampled_data <- sampled_data[,c("variable","code"):=tstrsplit(variable,"_",fixed=TRUE)]
    sampled_data$value <- as.numeric(sampled_data$value)
    sampled_data$code <- as.numeric(sampled_data$code)
    sampled_data <- merge(sampled_data,pedigree_data,by="code")
    sampled_data <- data.table(dcast(sampled_data[,.(ID,sample_id,variable,value)],ID+sample_id~variable,value.var="value"))
    names(sampled_data)[3] <- 'AF' 

    
    observe({
      show_spinner()
      output$plot_output1 <- renderUI({
        if (input$plot_type1 == "Average coverage") {
          plotlyOutput(session$ns("plot1"))
        } else {
          plotlyOutput(session$ns("plot2"))
        }
      })
      
      output$plot_output2 <- renderUI({
        if (input$plot_type2 == "Allele fraction") {
          plotlyOutput(session$ns("plot3"))
        }
      })
      
      ordered_levels <- c(paste0("chr", 1:22), "chrX", "chrY")
      
      coverage_data$CHROM <- factor(coverage_data$CHROM, levels = ordered_levels)
      
      output$plot1 <- renderPlotly({
        # Replace with actual plot generation code for Plot Type 1
        plot_ly(coverage_data[CHROM!="chrM"], x = ~CHROM, y = ~AVERAGE_COVERAGE, color = ~SAMPLE, type = 'scatter', mode = 'markers',
                marker = list(size = 12,opacity = 0.6)) %>%
          layout(title = NULL, 
                 xaxis = list(title = ""), 
                 yaxis = list(title = "Average Coverage"),
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      })
      
      output$plot2 <- renderPlotly({
        # Calculate average coverage across autosomal chromosomes for normalization
        autosomal_coverage <- coverage_data %>% 
          filter(CHROM %in% paste0("chr", 1:22)) %>% 
          group_by(SAMPLE) %>% 
          summarize(avg_coverage = mean(AVERAGE_COVERAGE))
        
        normalized_data <- coverage_data %>% 
          left_join(autosomal_coverage, by = "SAMPLE") %>% 
          mutate(normalized_coverage = AVERAGE_COVERAGE / avg_coverage)
        
        plot_ly(normalized_data[CHROM!="chrM"], x = ~CHROM, y = ~normalized_coverage, color = ~SAMPLE, type = 'scatter', mode = 'markers',
                marker = list(size = 12,opacity = 0.6)) %>%
          layout(title = NULL,
                 xaxis = list(title =""), 
                 yaxis = list(title = "Average Coverage (Normalized)"),
                 legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      })
      
      # output$plot3 <- renderPlot({
      #   ggplot(sampled_data, aes(x = AF, color = sample_id)) +
      #     geom_density() +
      #     labs(title = NULL, x = "Allele Fraction", y = "Density") +
      #     scale_x_continuous(limits=c(0,1))+
      #     theme(legend.position = "bottom", legend.title = element_blank())+
      #     theme_minimal()
      # })
      
      output$plot3 <- renderPlotly({
        dens_list <- lapply(unique(sampled_data$sample_id), function(sample) {
          dens <- density(sampled_data$AF[sampled_data$sample_id == sample], na.rm = TRUE)
          data.frame(x = dens$x, y = dens$y, sample_id = sample)
        })
        
        dens_data <- do.call(rbind, dens_list)
        
        plot_ly(dens_data, x = ~x, y = ~y, color = ~sample_id, type = 'scatter', mode = 'lines') %>%
          layout(
            title = NULL,
            xaxis = list(title = "Allele Fraction", range = c(0, 1)),
            yaxis = list(title = "Density"),
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
            margin = list(b = 50)  # Adjust bottom margin to avoid legend overlap
          )
      })
      
      # Prepare the data
      somalier <- merge(somalier, pedigree_data[, .(sample_id, kinship)], by.x = "sample_a", by.y = "sample_id", all.x = TRUE)
      setnames(somalier, "kinship", "kinship_a")
      somalier <- merge(somalier, pedigree_data[, .(sample_id, kinship)], by.x = "sample_b", by.y = "sample_id", all.x = TRUE)
      setnames(somalier, "kinship", "kinship_b")
      
      # Create a new column for pair relationship
      somalier[, pair := paste0(kinship_a, ":", kinship_b)]
      somalier$pair <- factor(somalier$pair,levels = sort(somalier$pair,decreasing = TRUE))
      
      # Create the somalier_dict data.table
      somalier_dict <- data.table(
        term = c("IBS0", "IBS2", "shared-hets", "shared-hom-alts", "relatedness","hom_concordance","hets_ab"),
        definition = c("The number of sites where one sample is homozygous reference and the other is homozygous alternate.",
                       "The number of sites where the samples have the same genotype.",
                       "The number of sites where both samples are heterozygotes.",
                       "The number of sites where both samples are homozygous alternate.",
                       "2 * (shared_hets - 2 * ibs0) / max(1, het_ab)",
                       "The proportion of sites where both samples are homozygous and have the same genotype.",
                       "The number of combined heterozygous counts for the two samples.")
      )
      
      output$somalier_plot <- renderPlotly({
        req(input$x_var, input$y_var)
        
        plot_ly(somalier, x = ~get(input$x_var), y = ~get(input$y_var), text = ~paste(sample_a, sample_b), type = 'scatter', mode = 'markers',
                marker = list(size = 14,opacity = 0.6), color = ~pair) %>%  # Adjust the size value as needed
          layout(
            xaxis = list(title = input$x_var),
            yaxis = list(title = input$y_var),
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
            margin = list(b = 50)  # Adjust bottom margin to avoid legend overlap
          )
      })
      
      # Render the definitions table
      output$definitions_table <- DT::renderDataTable({
        datatable(somalier_dict, options = list(pageLength = 10, autoWidth = TRUE), rownames = FALSE)
      })
      
      hide_spinner()
    })

  })
}