selectFiltersServer <- function(id, dataset, pedigree, panel_app_genes, vep_consequences, phenotype_data = phenotype_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    number_of_individuals <- reactiveVal(dim(pedigree)[1])
    
    allele_counts <- rep("",dim(pedigree)[1])
    names(allele_counts) <- pedigree$sample_id
    allele_counts <- reactiveValues(values=allele_counts)
    
    condition_status <- pedigree$status
    names(condition_status) <- pedigree$sample_id
    condition_status <- reactiveValues(values=condition_status)
    
    green_genes <- reactiveVal()
    red_genes <- reactiveVal()
    amber_genes <- reactiveVal()
    unclassified_genes <- reactiveVal()
    
    current_shortlisted_ids <- reactiveVal()
    current_blacklisted_ids <- reactiveVal()
    current_phenotype_terms <- reactiveVal()
    
    filtered_table_output <- reactiveVal(dataset)
    
    igv_coord_box <- reactiveVal(NULL)
    
    initialisation <- reactiveVal(TRUE)
    
    clinvar_options <- c("Pathogenic", "Likely pathogenic", "VUS","Conflicting","Benign","Likely benign","Not available")
    clinvar_options_display <- lapply(
      X = clinvar_options,
      FUN = function(x) {
        tags$div(
          style = "width: 140px;", x
        )
      }
    )
    
    consequence_options <- c("Stop gained","Start lost","Stop lost","Splice variant","Frameshift variant","Missense variant","In-frame variant","Synonymous variant","5'UTR variant","3'UTR variant","Intron variant","Other")
    consequence_options_display <- lapply(
      X = consequence_options,
      FUN = function(x) {
        tags$div(
          style = "width: 140px;", x
        )
      }
    )
    
    
    observe({
      shinyjs::enable(ns("inheritance"))
      shinyjs::enable(ns("pathogenicity"))
      
      shinyjs::enable(ns("apply_filter"))
    })
    
    observe({
      #show_spinner()
      for (i in 1:number_of_individuals()) {
        label <- paste0("status", i)
        sample_id <- pedigree$sample_id[i]
        new_status <- input[[label]]
        if (!is.null(new_status)) {
          if (new_status!=condition_status[["values"]][[sample_id]]) {
            condition_status[["values"]][[sample_id]] <- new_status
          }
        }
      }
      #hide_spinner()
    })
    
    output$additional_rows <- renderUI({
      
      additional_rows_list <- lapply(1:number_of_individuals(), function(i) {
        
        sample_id <- pedigree$sample_id[i]
        kinship <- pedigree$kinship[i]
        status <- pedigree$status[i]
        sex <- pedigree$sex[i]
        
        sample_id_labels <- rep("",number_of_individuals())
        kinship_labels <- rep("",number_of_individuals())
        status_labels <- rep("",number_of_individuals())
        sex_labels <- rep("",number_of_individuals())
        allele_count_labels <- rep("",number_of_individuals())
        
        sample_id_labels[1] <- "Sample ID:"
        kinship_labels[1] <- "Kinship:"
        status_labels[1] <- "Status:"
        sex_labels[1] <- "Genotypic sex:"
        allele_count_labels[1] <- "Allele count:"
        
        fluidRow(
          column(2,
                 selectInput(ns(paste0("kinship", i)), kinship_labels[i],
                             choices = kinship, 
                             selected = kinship)
          ),
          column(2,
                 selectInput(ns(paste0("name", i)), sample_id_labels[i],
                             choices = sample_id,
                             selected = sample_id)
          ),
          column(2,
                 selectInput(ns(paste0("sex", i)), sex_labels[i],
                             choices = sex,
                             selected = sex)
          ),
          column(2,
                 selectInput(ns(paste0("status", i)), status_labels[i],
                             choices = unique(pedigree$status),
                             selected = condition_status$values[[sample_id]])
          ),
          column(2,
                 selectInput(ns(paste0("allele", i)), allele_count_labels[i],
                             choices = c("", c("0","0-1","1","1-2","2")),
                             selected = allele_counts$values[[sample_id]])
          )
        )
      })
      do.call(tagList, additional_rows_list)
    })
    
    
    observe({
      
      #show_spinner()
      sample_ids <- pedigree$sample_id
      i <- 1
      for (current_id in sample_ids) {
        status <- condition_status[["values"]][[current_id]]
        kinship <- pedigree[pedigree$sample_id==current_id,kinship]
        sex <- pedigree[pedigree$sample_id==current_id,sex]
        if (input$inheritance == "Homozygous Recessive") {
          allele_counts$values[[current_id]] <- ifelse(status=="affected","2","0-1")
        } else if (input$inheritance == "Dominant/De Novo") {
          allele_counts$values[[current_id]] <- ifelse(status=="affected","1-2","0")
        } else if (input$inheritance == "Compound Heterozygous") {
          allele_counts$values[[current_id]] <- ifelse(status=="affected","1","0-1")
        } else if (input$inheritance == "X-Linked Recessive") {
          if (sex == "male") {
            allele_counts$values[[current_id]] <- ifelse(status=="affected","1","0")
          } else {
            allele_counts$values[[current_id]] <- ifelse(status=="affected","2","0-1")
          }
        } else if (input$inheritance == "Custom") {
          allele_counts$values[[current_id]] <- input[[paste0("allele", i)]]
        }
        i <- i + 1
      }
      #hide_spinner()
    })
    
    output$clinvar <- renderUI({
      #print("clinvar_checkboxes")
      clinvar_checkboxes_list <- list(prettyCheckboxGroup(ns("clinvar_checkboxes"), NULL,
                                                          choiceNames = clinvar_options_display,
                                                          choiceValues = clinvar_options,
                                                          selected = NULL,inline = TRUE))
      do.call(tagList,clinvar_checkboxes_list)
    })
    
    observeEvent(input$pathogenicity,{
      #show_spinner()
      #print("here")
      if (input$pathogenicity == "Pathogenic/Likely pathogenic") {
        #print("pathogenicity/checked")
        updatePrettyCheckboxGroup(session, inputId = "clinvar_checkboxes",
                                  choiceNames = clinvar_options_display,
                                  choiceValues = clinvar_options,
                                  selected = c("Pathogenic", "Likely pathogenic"),inline = TRUE)
      } else if (input$pathogenicity == "Not benign") {
        updatePrettyCheckboxGroup(session, inputId = "clinvar_checkboxes",
                                  choiceNames = clinvar_options_display,
                                  choiceValues = clinvar_options,
                                  selected = c("Pathogenic", "Likely pathogenic", "VUS","Conflicting"),inline = TRUE)
      } else if (input$pathogenicity == "") {
        updatePrettyCheckboxGroup(session, inputId = "clinvar_checkboxes",
                                  choiceNames = clinvar_options_display,
                                  choiceValues = clinvar_options,
                                  selected = NULL,inline = TRUE)
      }
      #hide_spinner() 
    })
    
    
    output$consequences <- renderUI({
      consequence_checkboxes_list <- list(prettyCheckboxGroup(ns("consequence_checkboxes"), NULL,
                                                              choiceNames = consequence_options_display,
                                                              choiceValues = consequence_options,
                                                              selected = NULL,inline = TRUE))
      do.call(tagList,consequence_checkboxes_list)
    })
    
    observeEvent(input$annotation, {
      #show_spinner()
      if (input$annotation == "High impact") {
        updatePrettyCheckboxGroup(session, inputId = "consequence_checkboxes",
                                  choiceNames = consequence_options_display,
                                  choiceValues = consequence_options,
                                  selected = c("Stop gained","Start lost","Stop lost","Splice variant","Frameshift variant"),inline = TRUE)
      } else if (input$annotation == "Moderate to high impact") {
        updatePrettyCheckboxGroup(session, inputId = "consequence_checkboxes",
                                  choiceNames = consequence_options_display,
                                  choiceValues = consequence_options,
                                  selected = c("Stop gained","Start lost","Stop lost","Splice variant","Frameshift variant","Missense variant","In-frame variant"),inline = TRUE)
      }
      #hide_spinner()
    })
    
    observe({
      #show_spinner()
      output$sv_features <- renderUI({
        sv_features_checkboxes_list <- list(prettyCheckboxGroup(ns("sv_features_checkboxes"), "SV type:",
                                                                choiceNames = c("Insertion", "Deletion", "Duplication", "Inversion","Translocation"),
                                                                choiceValues = c("Insertion", "Deletion", "Duplication", "Inversion","Translocation"),
                                                                selected = NULL,inline = TRUE))
        do.call(tagList,sv_features_checkboxes_list)
      })
      
      output$sv_relative_pos <- renderUI({
        sv_relative_pos_checkboxes_list <- list(prettyCheckboxGroup(ns("sv_relative_pos_checkboxes"), "Relative position:",
                                                                    choiceNames = c("Exonic", "Intronic", "UTR", "Promoter", "Intergenic"),
                                                                    choiceValues = c("Exonic", "Intronic", "UTR", "Promoter", "Intergenic"),
                                                                    selected = NULL,inline = TRUE))
        do.call(tagList,sv_relative_pos_checkboxes_list)
      })
      
      output$sv_consequence <- renderUI({
        sv_consequence_checkboxes_list <- list(prettyCheckboxGroup(ns("sv_consequence_checkboxes"), "Predicted consequences:",
                                                                   choiceNames = c("Loss of function (LoF)", "Copy Number Variation (CNV)", "Whole gene inversion", "Regulatory and Non-coding variants"),
                                                                   choiceValues = c("Loss of function (LoF)", "Copy Number Variation (CNV)", "Whole gene inversion", "Regulatory and Non-coding variants"),
                                                                   selected = "Loss of function (LoF)",inline = TRUE))
        do.call(tagList,sv_consequence_checkboxes_list)
      })
      
      output$green_genes <- renderUI({
        genes <- ''
        if (!is.null(input$panelapp)) {
          genes <- sort(unique(panel_app_genes[Level4 %in% input$panelapp & Sources =="Green",Entity_Name]))
        }
        green_genes(genes)
        genes <- paste(genes,collapse="; ")
        wellPanel(
          div(style = "font-weight: bold; margin-bottom: 10px;", "Green genes:"),
          div(style = "overflow-y: auto; max-height: calc(100% - 30px);",
              p(genes, style = "color: green;")),  # Change text color to red
          style = "width: 100%; height: 30vh;"  # Adjust the width as needed
        )
      })
      
      output$red_genes <- renderUI({
        genes <- ''
        if (!is.null(input$panelapp)) {
          genes <- sort(unique(panel_app_genes[Level4 %in% input$panelapp & Sources =="Red",Entity_Name]))
        }
        red_genes(genes)
        genes <- paste(genes,collapse="; ")
        wellPanel(
          div(style = "font-weight: bold; margin-bottom: 10px;", "Red genes:"),
          div(style = "overflow-y: auto; max-height: calc(100% - 30px);",
              p(genes, style = "color: red;")),  # Change text color to red
          style = "width: 100%; height: 30vh;"  # Adjust the width as needed
        )
      })
      
      output$amber_genes <- renderUI({
        genes <- ''
        if (!is.null(input$panelapp)) {
          genes <- sort(unique(panel_app_genes[Level4 %in% input$panelapp & Sources =="Amber",Entity_Name]))
        }
        amber_genes(genes)
        genes <- paste(genes,collapse="; ")
        wellPanel(
          div(style = "font-weight: bold; margin-bottom: 10px;", "Amber genes:"),
          div(style = "overflow-y: auto; max-height: calc(100% - 30px);",
              p(genes, style = "color: #FFBF00;")),  # Change text color to red
          style = "width: 100%; height: 30vh;"  # Adjust the width as needed
        )
      })
      
      output$unclassified_genes <- renderUI({
        genes <- ''
        if (!is.null(input$panelapp)) {
          genes <- sort(unique(panel_app_genes[Level4 %in% input$panelapp & !(Sources %in% c("Green","Red","Amber")),Entity_Name]))
        }
        unclassified_genes(genes)
        genes <- paste(genes,collapse="; ")
        wellPanel(
          div(style = "font-weight: bold; margin-bottom: 10px;", "Unclassified genes:"),
          div(style = "overflow-y: auto; max-height: calc(100% - 30px);",
              p(genes, style = "color: gray;")),  # Change text color to red
          style = "width: 80%; height: 20vh"  # Adjust the width as needed
        )
      })
      hide_spinner()
    })
    
################# IGV
    
    
    observeEvent(input$coords_button, {
      if (input$igv_var_id != "" && input$igv_var_id %in% dataset$ID) {
        
        chrom <- dataset[dataset$ID == input$igv_var_id, "CHROM"]
        pos <- dataset[dataset$ID == input$igv_var_id, "POS"]
        len <- dataset[dataset$ID == input$igv_var_id, "VAR_LENGTH"]
        flanking <- input$igv_flanking
        max_window <- input$igv_max_window
        
        start <- pos - flanking
        end <- pos + len + flanking
        
        if ((end - start) > max_window) {
          split_start <- paste0(chrom, ":", start, "-", (start + max_window))
          split_end <- paste0(chrom, ":", (end - max_window), "-", end)
          coords <- paste(split_start, split_end)
        } else {
          coords <- paste0(chrom, ":", start, "-", end)
        }
        print(coords)
        
        igv_coord_box(coords)
      }
    })
    
    output$igv_coord_box <- renderUI({
      variant_coord <- igv_coord_box()
      wellPanel(
        div(style = "overflow-y: auto; max-height: 100px;",
            p(variant_coord, style = "color: #000000;")),
        style = "width: 100%;"  # Adjust the width as needed
      )
    })

    
################# Short List
    
    # Add ID to the short list
    observeEvent(input$shortlisted_add, {
      #print("add+button")
      new_id <- input$shortlisted_var
      if (new_id != "" & new_id %in% dataset$ID) {
        current_ids <- current_shortlisted_ids()
        if (!(new_id %in% current_ids)) {
          current_shortlisted_ids(c(current_ids, new_id))
          updateTextInput(session, "shortlisted_var", value = "")
        } else {
          updateTextInput(session, "shortlisted_var", value = "")
        }
      }
      #print(current_shortlisted_ids()) # Debugging print statement
    })
    
    # Remove ID from the short list
    observeEvent(input$shortlisted_remove, {
      #print("remove-button")
      id_to_remove <- input$shortlisted_var
      if (id_to_remove != "") {
        current_ids <- current_shortlisted_ids()
        if (id_to_remove %in% current_ids) {
          current_shortlisted_ids(current_ids[current_ids != id_to_remove])
          updateTextInput(session, "shortlisted_var", value = "")
        } else {
          updateTextInput(session, "shortlisted_var", value = "")
        }
      }
      #print(current_shortlisted_ids()) # Debugging print statement
    })

    # Display the list of shortlisted IDs
    output$shortlist <- renderUI({
      if (length(current_shortlisted_ids()) > 0) {
        variant_ids <- paste(current_shortlisted_ids(),collapse="; ")
      } else {
        variant_ids <- ""
      }
      wellPanel(
        div(style = "font-weight: bold; margin-bottom: 10px;", "Shortlist:"),
        div(style = "overflow-y: auto; max-height: 300px;",
            p(variant_ids, style = "color: black;")),  # Change text color to red
        style = "width: 100%;"  # Adjust the width as needed
      )
    })
    
##################### Black List
    
    # Add ID to the black list
    observeEvent(input$blacklisted_add, {
      #print("add+button")
      new_id <- input$blacklisted_var
      if (new_id != "" & new_id %in% dataset$ID) {
        current_ids <- current_blacklisted_ids()
        if (!(new_id %in% current_ids)) {
          current_blacklisted_ids(c(current_ids, new_id))
          updateTextInput(session, "blacklisted_var", value = "")
        } else {
          updateTextInput(session, "blacklisted_var", value = "")
        }
      }
      #print(current_blacklisted_ids()) # Debugging print statement
    })
    
    # Remove ID from the black list
    observeEvent(input$blacklisted_remove, {
      #print("remove-button")
      id_to_remove <- input$blacklisted_var
      if (id_to_remove != "") {
        current_ids <- current_blacklisted_ids()
        if (id_to_remove %in% current_ids) {
          current_blacklisted_ids(current_ids[current_ids != id_to_remove])
          updateTextInput(session, "blacklisted_var", value = "")
        } else {
          updateTextInput(session, "blacklisted_var", value = "")
        }
      }
      #print(current_blacklisted_ids()) # Debugging print statement
    })
    
    # Display the list of blacklisted IDs
    output$blacklist <- renderUI({
      if (length(current_blacklisted_ids()) > 0) {
        variant_ids <- paste(current_blacklisted_ids(),collapse="; ")
      } else {
        variant_ids <- ""
      }
      wellPanel(
        div(style = "font-weight: bold; margin-bottom: 10px;", "Blacklist:"),
        div(style = "overflow-y: auto; max-height: 300px;",
            p(variant_ids, style = "color: black;")),  # Change text color to red
        style = "width: 100%;"  # Adjust the width as needed
      )
    })
    
##################### Phenotype    
    
    # Add ID to the phenotype list
    observeEvent(input$phenotype_add, {
      #print("add+button")
      new_id <- input$phenotype_var
      if (new_id != "") {
        current_ids <- current_phenotype_terms()
        if (!(new_id %in% current_ids) & new_id %in% phenotype_data$hpo_id) {
          current_phenotype_terms(c(current_ids, new_id))
          updateTextInput(session, "phenotype_var", value = "")
        } else {
          updateTextInput(session, "phenotype_var", value = "")
        }
      }
      #print(current_phenotype_terms()) # Debugging print statement
    })
    
    # Remove ID from the phenotype list
    observeEvent(input$phenotype_remove, {
      #print("remove-button")
      id_to_remove <- input$phenotype_var
      if (id_to_remove != "") {
        current_ids <- current_phenotype_terms()
        if (id_to_remove %in% current_ids) {
          current_phenotype_terms(current_ids[current_ids != id_to_remove])
          updateTextInput(session, "phenotype_var", value = "")
        } else {
          updateTextInput(session, "phenotype_var", value = "")
        }
      }
      #print(current_phenotype_terms()) # Debugging print statement
    })
    
    # Display the list of HPO terms
    output$phenotype <- renderUI({
      if (length(current_phenotype_terms()) > 0) {
        hpo_ids <- paste(current_phenotype_terms(),collapse="; ")
      } else {
        hpo_ids <- ""
      }
      wellPanel(
        div(style = "font-weight: bold; margin-bottom: 10px;", "HPO list:"),
        div(style = "overflow-y: auto; max-height: 300px;",
            p(hpo_ids, style = "color: black;")),  # Change text color to red
        style = "width: 100%;"  # Adjust the width as needed
      )
    })
    
##################### Filtering
    
    observeEvent(input$apply_filter, {
      show_spinner()
      # Read selected filters
      print("filtering...")
      inheritance_filter <- input$inheritance
      #print(inheritance_filter)
      pathogenicity_filter <- input$pathogenicity
      clinvar_filter <- input$clinvar_checkboxes
      #print(pathogenicity_filter)
      #print(clinvar_filter)
      #annotation_filter <- input$annotation
      annotation_filter <- input$consequence_checkboxes
      revel_value <- input$revel
      sift_filter <- input$sift
      polyphen_filter <- input$polyphen
      af_value <- input$af
      pass_variants_filter <- input$pass_variants
      genotype_quality_value <- input$genotype_quality
      allele_balance_value <- input$allele_balance
      panelapp_filter <- input$panelapp
      spliceai_filter <- input$spliceai_score
      # donor_loss_cutoff <- input$donor_loss
      # donor_gain_cutoff <- input$donor_gain
      # acceptor_loss_cutoff <- input$acceptor_loss
      # acceptor_gain_cutoff <- input$acceptor_gain
      sv_types <- input$sv_features_checkboxes
      sv_min_svlen <- input$min_svlen
      sv_max_svlen <- input$max_svlen
      sv_genotype_quality_value <- input$sv_genotype_quality
      sv_pass_variants_filter <- input$sv_pass_variants
      sv_relative_pos <- input$sv_relative_pos_checkboxes
      sv_consequences <- input$sv_consequence_checkboxes

      
      snv_filtered_data <- data.table(dataset)[CATEGORY=="SNV & Indel"]
      sv_filtered_data <- data.table(dataset)[CATEGORY=="SV"]
      
      # Add PRIORITY column to snv_filtered_data
      snv_filtered_data <- data.table(PRIORITY = 0, snv_filtered_data)
      # Add PRIORITY column to sv_filtered_data
      sv_filtered_data <- data.table(PRIORITY = 0, sv_filtered_data)
      
      # Add Color to snv_filtered_data
      snv_filtered_data <- data.table(snv_filtered_data,Color="#FFFFFF")
      # Add Color to sv_filtered_data
      sv_filtered_data <- data.table(sv_filtered_data,Color="#FFFFFF")
      
      # Shortlisted variants
      shortlisted_ids <- current_shortlisted_ids()
      if (length(shortlisted_ids) > 0) {
        split_shortlisted_ids <- unlist(strsplit(shortlisted_ids,"; "))
        snv_filtered_data_shortlisted <- snv_filtered_data[ID %in% split_shortlisted_ids]
        snv_filtered_data_shortlisted[,Color:="#ACF3AE"]
        snv_filtered_data_shortlisted[,PRIORITY:=1]
        snv_filtered_data <- snv_filtered_data[!(ID %in% split_shortlisted_ids)]
        sv_filtered_data_shortlisted <- sv_filtered_data[ID %in% split_shortlisted_ids]
        sv_filtered_data_shortlisted[,Color:="#ACF3AE"]
        sv_filtered_data_shortlisted[,PRIORITY:=1]
        sv_filtered_data <- sv_filtered_data[!(ID %in% split_shortlisted_ids)]
      }
      
      # 1) Filter by Mode of inheritance
      
      if (inheritance_filter != "") {
        compare_allele_count <- function(col,values) {
          values <- as.numeric(unlist(strsplit(values,"-")))
          if (length(values) == 1) {
            filtered_rows <- (col == values)
          } else if (length(values) == 2) {
            filtered_rows <- (col >= values[1] & col <= values[2])
          }
          return(filtered_rows[,1])
        }
        
        count_values <- allele_counts$values
        #print(count_values)
        
        allele_count <- rbindlist(lapply(names(count_values), function(name) data.table(sample_id = name, allele_count = count_values[[name]])))
        allele_count <- merge(pedigree,allele_count,by="sample_id")
        allele_count[,col_name:=paste0(c("alt_allele_count",code),collapse="_"),by=sample_id]
        
        #print(allele_count)
        
        for (i in seq_len(nrow(allele_count))) {
          col_name <- allele_count[i, col_name]
          val <- allele_count[i, allele_count]
          snv_filtered_data <- snv_filtered_data[compare_allele_count(snv_filtered_data[,col_name,with=FALSE],val)]
          sv_filtered_data <- sv_filtered_data[compare_allele_count(sv_filtered_data[,col_name,with=FALSE],val)]
        }
        if (inheritance_filter=="X-Linked Recessive") {
          snv_filtered_data <- snv_filtered_data[CHROM=="chrX"]
          sv_filtered_data <- sv_filtered_data[CHROM=="chrX"]
        }
    
      }
  
      # 2) Genes
      
      if (length(panelapp_filter) > 0) {
        genes <- panel_app_genes[Level4 %in% panelapp_filter,.(PANEL_APP=Level4,GENE_SYMBOL=Entity_Name)]
        genes <- genes[,.(PANEL_APP=paste(PANEL_APP,collapse=";")),by=GENE_SYMBOL]
        snv_filtered_data <- merge(snv_filtered_data,genes,by="GENE_SYMBOL")

        genes <- panel_app_genes[Level4 %in% panelapp_filter,.(PANEL_APP=Level4,GENE_SPLIT=Entity_Name)]
        id_vars <- names(sv_filtered_data)
        sv_split <- sv_filtered_data[, .(GENE_SPLIT = unlist(strsplit(GENE_SYMBOL, ","))), by = id_vars]
        merged_data <- merge(unique(sv_split), unique(genes), by = "GENE_SPLIT")
        merged_data[,GENE_SPLIT:=NULL]
        sv_filtered_data <- merged_data[, .(PANEL_APP = paste(unique(PANEL_APP), collapse = ";")), by = id_vars]
        setcolorder(sv_filtered_data, names(snv_filtered_data))
        #print(unique(sv_filtered_data$PANEL_APP))

      } else {
        snv_filtered_data[,PANEL_APP:=NA]
        sv_filtered_data[,PANEL_APP:=NA]
      }
      
      snv_filtered_data[is.na(CLINVAR),CLINVAR:="not available"]
      snv_filtered_data[,CLINVAR:=str_replace_all(CLINVAR,"_"," ")]
      snv_filtered_data[,CLINVAR:=as.factor(CLINVAR)]
      
      # 3) Pathogenicity
      
      if (length(clinvar_filter) > 0) {
        clinvar_filter <- gsub("VUS", "uncertain", clinvar_filter)
        # Filter rows based on keywords in b, ignoring case
        snv_filtered_data <- snv_filtered_data[grep(paste(clinvar_filter, collapse = '|'), CLINVAR, ignore.case = TRUE)]
        #print("clinvar")
        #print(pathogenicity_filter)
        #print(clinvar_filter)
      }
      
      # 4) Annotation
      
      if (length(annotation_filter) > 0) {
        vep_search_terms <- vep_consequences[consequence %in% annotation_filter,term]
        snv_filtered_data <- snv_filtered_data[grep(paste(vep_search_terms, collapse = '|'), CONSEQUENCE, ignore.case = TRUE)]
      }
      
      # 5) REVEL score
      
      if (revel_value > 0) {
        snv_filtered_data <- snv_filtered_data[REVEL>=revel_value]
      }
      
      # 6) SIFT
      if (length(sift_filter) > 0) {
        snv_filtered_data <- snv_filtered_data[grep(paste(sift_filter, collapse = '|'), SIFT, ignore.case = TRUE)]
      }
      
      # 7) Polyphen
      if (length(polyphen_filter) > 0) {
        #print(polyphen_filter)
        polyphen_search <- str_replace_all(polyphen_filter," ","_")
        #print(polyphen_search)
        #print(unique(snv_filtered_data$PolyPhen))
        snv_filtered_data <- snv_filtered_data[grep(paste(polyphen_search, collapse = '|'), PolyPhen, ignore.case = TRUE)]
      }
      
      # 8) Splice AI scores
      
      # if (donor_loss_cutoff > 0) {
      #   snv_filtered_data <- snv_filtered_data[Donor_Loss > donor_loss_cutoff]
      # }
      # 
      # if (donor_gain_cutoff > 0) {
      #   snv_filtered_data <- snv_filtered_data[Donor_Gain > donor_gain_cutoff]
      # }
      # 
      # if (acceptor_loss_cutoff > 0) {
      #   snv_filtered_data <- snv_filtered_data[Acceptor_Loss > acceptor_loss_cutoff]
      # }
      # 
      # if (acceptor_gain_cutoff > 0) {
      #   snv_filtered_data <- snv_filtered_data[Acceptor_Gain > acceptor_gain_cutoff]
      # }
      
      # 9) Allele Frequency
      if (af_value < 1) {
        if (af_value > 0) {
          snv_filtered_data <- snv_filtered_data[AF<=af_value]
        } else if (af_value == 0) {
          snv_filtered_data <- snv_filtered_data[is.na(AF)]
        }

      }
      
      # 10) Call filter
      if (pass_variants_filter != "") {
        if (pass_variants_filter == "PASS only variants") {
          snv_filtered_data <- snv_filtered_data[FILTER==pass_variants_filter]
        } 
      }
      
      # 11) Call Quality
      if (genotype_quality_value > 0) {
        snv_filtered_data <- snv_filtered_data[QUAL>=genotype_quality_value]
      }
      
      # 12) Allele balance (STILL NEEDS TO BE FIXED)
      
      #print(allele_balance_value)
      
      # 13) SV type
      sv_type_options <- c("Insertion"="INS", "Deletion"="DEL", "Duplication"="DUP", "Inversion"="INV","Translocation"="TRA")
      if (length(sv_types) > 0) {
        sv_type_selected <- sv_type_options[sv_types]
        names(sv_type_selected) <- NULL
        sv_filtered_data <- sv_filtered_data[VAR_TYPE %in% sv_type_selected]
        #print(sv_type_selected)
      }
      
      # 14) SV length
      if (sv_min_svlen > 0) {
        sv_filtered_data <- sv_filtered_data[VAR_LENGTH > sv_min_svlen]
      }
      if (sv_max_svlen > 0) {
        sv_filtered_data <- sv_filtered_data[VAR_LENGTH < sv_max_svlen]
      }
      
      # 15) SV Call Quality
      if (sv_genotype_quality_value > 0) {
        #print(sv_genotype_quality_value)
        sv_filtered_data <- sv_filtered_data[QUAL>=sv_genotype_quality_value]
      }
      
      # 16) Call filter
      if (sv_pass_variants_filter != "") {
        if (sv_pass_variants_filter == "PASS only variants") {
          sv_filtered_data <- sv_filtered_data[FILTER=="PASS"]
        } 
      }
      
      # 17) Allele balance (STILL NEEDS TO BE FIXED)
      
      # 18) SV annotation
      sv_relative_pos_patterns <- c("Exonic"="PREDICTED_LOF|PREDICTED_PARTIAL_EXON_DUP|PREDICTED_BREAKEND_EXONIC",
                                    "Intronic"="PREDICTED_INTRONIC","UTR"="PREDICTED_UTR","Promoter"="PREDICTED_PROMOTER","Intergenic"="PREDICTED_INTERGENIC|PREDICTED_NEAREST_TSS")
      if (length(sv_relative_pos) > 0) {
        sv_relative_pos_selected <- sv_relative_pos_patterns[sv_relative_pos]
        names(sv_relative_pos_selected) <- NULL
        sv_relative_pos_selected <- paste(sv_relative_pos_selected,collapse = "|")
        sv_filtered_data <- sv_filtered_data[grepl(sv_relative_pos_selected,CONSEQUENCE)]
        #print(sv_relative_pos_selected)
      }
      
      sv_consequences_patterns <- c("Loss of function (LoF)"="PREDICTED_LOF|PREDICTED_MSV_EXON_OVERLAP",
                                    "Copy Number Variation (CNV)"="PREDICTED_COPY_GAIN|PREDICTED_INTRAGENIC_EXON_DUP|PREDICTED_PARTIAL_EXON_DUP|PREDICTED_TSS_DUP|PREDICTED_DUP_PARTIAL|PREDICTED_MSV_EXON_OVERLAP",
                                    "Whole gene inversion"="PREDICTED_INV_SPAN","Regulatory and Non-coding variants"="PREDICTED_NONCODING_SPAN|PREDICTED_NONCODING_BREAKPOINT")
      if (length(sv_consequences) > 0) {
        sv_consequences_selected <- sv_consequences_patterns[sv_consequences]
        names(sv_consequences_selected) <- NULL
        sv_consequences_selected <- paste(sv_consequences_selected,collapse = "|")
        sv_filtered_data <- sv_filtered_data[grepl(sv_consequences_selected,CONSEQUENCE)]
        #print(sv_consequences)
      }
      
      # c("Loss of function (LoF)", "Copy Number Variation (CNV)", "Whole gene inversion", "Regulatory and Non-coding variants")
      
      # if (length(shortlisted_ids) > 0) {
      #   print(names(snv_filtered_data_shortlisted))
      #   print(names(sv_filtered_data_shortlisted))
      #   print(names(snv_filtered_data))
      # }
      
      if (length(shortlisted_ids) > 0) {
        split_shortlisted_ids <- unlist(strsplit(shortlisted_ids,"; "))
        found_shortlisted_ids <- split_shortlisted_ids[split_shortlisted_ids %in% c(snv_filtered_data$ID,sv_filtered_data$ID)]
        not_found_shortlisted_ids <- split_shortlisted_ids[!(split_shortlisted_ids %in% c(snv_filtered_data$ID,sv_filtered_data$ID))]
        snv_filtered_data[ID %in% found_shortlisted_ids,Color:="#ACF3AE"]
        snv_filtered_data[ID %in% found_shortlisted_ids,PRIORITY:=1]
        sv_filtered_data[ID %in% found_shortlisted_ids,Color:="#ACF3AE"]
        sv_filtered_data[ID %in% found_shortlisted_ids,PRIORITY:=1]
        snv_filtered_data_shortlisted <- snv_filtered_data_shortlisted[ID %in% not_found_shortlisted_ids]
        snv_filtered_data_shortlisted[,PANEL_APP:=NA]
        sv_filtered_data_shortlisted <- sv_filtered_data_shortlisted[ID %in% not_found_shortlisted_ids]
        sv_filtered_data_shortlisted[,PANEL_APP:=NA]
        snv_filtered_data <- rbind(snv_filtered_data_shortlisted,sv_filtered_data_shortlisted,snv_filtered_data)
        
      }
      
      # Blacklisted variants
      blacklisted_ids <- current_blacklisted_ids()
      if (length(blacklisted_ids) > 0) {
        split_blacklisted_ids <- unlist(strsplit(blacklisted_ids,"; "))
        snv_filtered_data[ID %in% split_blacklisted_ids,Color:="#FA6B84"]
        snv_filtered_data[ID %in% split_blacklisted_ids,PRIORITY:=-1]
        sv_filtered_data[ID %in% split_blacklisted_ids,Color:="#FA6B84"]
        sv_filtered_data[ID %in% split_blacklisted_ids,PRIORITY:=-1]
      }
      
      all_filtered_data <- rbind(snv_filtered_data,sv_filtered_data)
      
      if (inheritance_filter=="Compound Heterozygous") {
        selected_cols <- c("ID", "GENE_SYMBOL", grep("^alt_allele_count", names(all_filtered_data), value = TRUE))
        selected_data <- all_filtered_data[, .SD, .SDcols = selected_cols]
        split_gene <- selected_data[,tstrsplit(GENE_SYMBOL,",",fixed=TRUE)]
        split_gene <- cbind(selected_data[, setdiff(selected_cols,"GENE_SYMBOL"),with=FALSE], split_gene)
        split_gene <- melt(split_gene, id.vars = setdiff(selected_cols,"GENE_SYMBOL"), value.name = "GENE_SYMBOL", na.rm = TRUE)
        split_gene <- split_gene[, .SD, .SDcols = selected_cols][(alt_allele_count_1==1)]
        compunt_hets <- split_gene[,.(VAR_COUNT=.N,SUM_1=sum(as.integer(alt_allele_count_1)),SUM_2=sum(as.integer(alt_allele_count_2)),SUM_3=sum(as.integer(alt_allele_count_3))),by=GENE_SYMBOL]
        all_filtered_data <- all_filtered_data[ID %in% split_gene[GENE_SYMBOL %in% compunt_hets[VAR_COUNT>1 & SUM_2 > 0 & SUM_3>0,GENE_SYMBOL],ID]]
      }
      all_filtered_data[,FILTER:=as.factor(FILTER)]
      #snv_filtered_data[,GENE_SYMBOL:=as.factor(GENE_SYMBOL)]
      
      # Phenotype data
      hpo_terms_list <- current_phenotype_terms()
      if (length(hpo_terms_list) > 0) {
        split_hpo_terms_list <- unlist(strsplit(hpo_terms_list,"; "))
        hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list][,.(HPO_ID=hpo_id,GENE_SYMBOL=gene_symbol)]
        # Identify the columns to use as ID columns
        id_cols <- setdiff(names(all_filtered_data), "GENE_SYMBOL")
        all_filtered_data_long <- all_filtered_data[, .(GENE_SYMBOL = unlist(strsplit(GENE_SYMBOL, ","))), by = id_cols]
        all_filtered_data_long <- merge(all_filtered_data_long, hpo_terms_data, by = "GENE_SYMBOL", all.x = TRUE)
        all_filtered_data <- all_filtered_data_long[, .(
          GENE_SYMBOL = paste(unique(GENE_SYMBOL), collapse = ","),
          HPO_ID = paste(unique(HPO_ID[!is.na(HPO_ID)]), collapse = ",")
        ), by = id_cols]
        all_filtered_data[,HPO_COUNT:=str_count(HPO_ID,"HP:")]
        #print(head(all_filtered_data))
      } else {
        all_filtered_data[,HPO_ID:=NA]
        all_filtered_data[,HPO_COUNT:=0]
      }
      
      filtered_table_output(as.data.frame(all_filtered_data))
      hide_spinner()
    })
    
    # Trigger the apply_filter button click event when the app launches
    observe({
      #show_spinner()
      #print(sprintf("before: %s",initialisation())) 
      if (initialisation() == TRUE) {
        print("here")
        updateSelectInput(session, "pathogenicity", selected = "Pathogenic/Likely pathogenic")
        req(input$clinvar_checkboxes)
        updateSelectInput(session, "annotation", selected = "Moderate to high impact")
        req(input$consequence_checkboxes)
        shinyjs::delay(100, shinyjs::click("apply_filter"))
        initialisation(FALSE)
      }
      #print(sprintf("after: %s",initialisation())) 
      #hide_spinner()
    })
    
    return(filtered_table_output)
    
  })
}
