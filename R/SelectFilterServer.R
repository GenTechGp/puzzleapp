# Metadata

metaServer <- function(output, ns, pedigree) {
  peditab <- pedigree[, 1:4]
  opts <- c("affected", "unaffected")

  for (i in seq_len(dim(pedigree)[1])) {
    id = paste0("status_", pedigree$sample_id[i])
    selected = pedigree$status[i]
    peditab$status[i] <- as.character(selectInput(ns(id), NULL, opts, selected))
  }
  names(peditab) <- c("Sample ID", "Kinship", "Status", "Sex")
  output$meta <- renderTable(peditab, sanitize.text.function = function(x) x)
}

read_search_files <- function(directory, type=NULL) {
  
  file_pattern <- if (!is.null(type)) {
    paste0(".*\\.", type, "_search.tsv$")
  } else {
    ".*_search\\.tsv$"
  }
  
  # List all TSV files in the directory
  files <- list.files(directory, pattern = file_pattern, full.names = TRUE)
  
  # Initialize an empty list to store results
  search_data <- list()
  
  # Iterate over each file
  for (file in files) {
    # Read the file into a dataframe
    df <- read.delim(file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
    
    # Convert the data to a named list
    file_data <- setNames(as.list(df$Value), df$Key)
    
    # Extract the Label field as the key
    label <- file_data[["Label"]]
    
    if (!is.null(label)) {
      # Store the data inside the main list, using label as key
      search_data[[label]] <- file_data
    } else {
      warning(sprintf("Skipping file %s as it has no 'Label' field", file))
    }
  }
  
  return(search_data)
}

alleleCount <- function(inher, status, sex) {
  if (inher == "Homozygous Recessive") {
    if (status == "affected")
      "2"
    else
      "0-1"
  } else if (inher == "Dominant/De Novo") {
    if (status == "affected")
      "1-2"
    else
      "0"
  } else if (inher == "Compound Heterozygous") {
    if (status == "affected")
      "1"
    else
      "0-1"
  } else if (inher == "X-Linked Recessive") {
    if (sex == "male") {
      if (status == "affected")
        "1"
      else
        "0"
    } else {
      if (status == "affected")
        "2"
      else
        "0-1"
    }
  } else {
    ""
  }
}

alleleTable <- function(pedigree, alleles_FUN) {
  tab <- data.frame(id = pedigree$sample_id, alleles = NA)
  tab$alleles <- sapply(pedigree$sample_id, alleles_FUN)
  names(tab) <- c("Sample ID", "Allele Count")
  tab
}

alleleCustomTable <- function(ns, pedigree) {
  FUN <- function(x) {
    boxId <- paste0("allele_", x)
    names <- c("None", "0", "0-1", "1", "1-2", "2")
    vals <- c("", "0", "0-1", "1", "1-2", "2")
    as.character(radioButtons(ns(boxId), NULL, selected = "", inline = TRUE,
                              choiceNames = names, choiceValues = vals))
  }
  alleleTable(pedigree, FUN)
}

alleleServer <- function(input, output, ns, pedigree, allele_tab) {
  # Init allele table and custom checkbox table
  allele_tab(alleleTable(pedigree, function(x) ""))
  allele_custom_tab <- alleleCustomTable(ns, pedigree)

  n <- dim(pedigree)[1]

  alleles <- reactiveValues()
  for (i in seq_len(n)) {
    id <- pedigree$sample_id[i]
    sid <- paste0("status_", id)
    alleles[[id]] <- reactive(
      if (is.null(input[[sid]])) {
        alleleCount(input$inher, pedigree$status[i], pedigree$sex[i])
      } else {
        alleleCount(input$inher, input[[sid]], pedigree$sex[i])
      }
    )
  }

  tab <- reactive({
    if (input$inher == "") {
      NULL
    } else if (input$inher == "Custom") {
      allele_custom_tab
    } else {
      tmp <- isolate(allele_tab())
      for (i in seq_len(n)) {
        id <- tmp[i, "Sample ID"]
        tmp[i, "Allele Count"] <- alleles[[id]]()
      }
      allele_tab(tmp)
      tmp
    }
  })
  output$allele <- renderTable(tab(), sanitize.text.function = function(x) x)

  # Update allele table on custom checkbox event
  for (id in pedigree$sample_id) {
    boxId <- paste0("allele_", id)
    observeEvent(input[[boxId]], {
      tab <- allele_tab()
      cnt <- c(input[[boxId]])
      tab[tab["Sample ID"] == id, "Allele Count"] <- cnt
      allele_tab(tab)
    })
  }
}

# Main

selectFiltersServer <- function(id, dataset, pedigree, panel_app_genes, vep_consequences, phenotype_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_spinner()

    number_of_individuals <- reactiveVal(dim(pedigree)[1])

    allele_tab <- reactiveVal()

    green_genes <- reactiveVal()
    red_genes <- reactiveVal()
    amber_genes <- reactiveVal()
    unclassified_genes <- reactiveVal()

    phenos <- reactiveVal()

    filtered_table_output <- reactiveVal(dataset)

    igv_coord_box <- reactiveVal(NULL)

    initialisation <- reactiveVal(TRUE)

    metaServer(output, ns, pedigree)

    flagged_rows_reactive <- reactiveVal(data.table(ID = character(0), PRIORITY = numeric(0), NOTES = character(0)))

    observe({
      shinyjs::enable(ns("inher"))
      shinyjs::enable(ns("pathogenicity"))
      shinyjs::enable(ns("pre_saved_search"))
      shinyjs::enable(ns("apply_filter"))
    })

    alleleServer(input, output, ns, pedigree, allele_tab)
    
    # Load available searches on startup
    available_searches <- reactive({
      read_search_files("/g/data/kr68/andre/puzzleapp/data","snv")
    })
    
    # Update UI dropdown with available searches
    observe({
      choices <- c("", names(available_searches()))
      updateSelectizeInput(session, "pre_saved_search",
                        choices = choices, selected = "")
    })
    
    observeEvent(input$pre_saved_search, {
      
      print(input$pre_saved_search)
      
      selected_search <- input$pre_saved_search
      
      # If no search is selected, return
      if (is.null(selected_search) || selected_search == "") {
        return()
      }
      
      # Retrieve the corresponding search settings
      search_params <- available_searches()[[selected_search]]
      
      # Debugging print
      print(paste("Loading pre-saved search:", selected_search))
      
      # Update Mode of Inheritance
      if (!is.null(search_params$Inheritance)) {
        updateSelectInput(session, "inher", selected = search_params$Inheritance)
      }
      
      # Update Annotation Filters
      if (!is.null(search_params$Annotation)) {
        updatePrettyCheckboxGroup(session, "conseq_checkboxes", 
                                  selected = unlist(strsplit(search_params$Annotation, ";")))
      }
      
      # Update Pathogenicity
      if (!is.null(search_params$Pathogenicity)) {
        updatePrettyCheckboxGroup(session, "clinvar_checkboxes", 
                                  selected = unlist(strsplit(search_params$Pathogenicity, ";")))
      }
      
      # Update SpliceAI Score
      if (!is.null(search_params$`SpliceAI score`)) {
        updateNumericInput(session, "spliceai_score", 
                           value = as.numeric(search_params$`SpliceAI score`))
      }
      
      # Update gnomAD AF
      if (!is.null(search_params$`gnomADv4 AF`)) {
        updateSelectInput(session, "af", 
                          selected = as.numeric(search_params$`gnomADv4 AF`))
      }
      
      # Update Affected Only Checkbox
      if (!is.null(search_params$`Affected only`)) {
        updateMaterialSwitch(session, "affected_switch", 
                             value = as.logical(search_params$`Affected only`))
      }
      
      # Update Allele Balance
      if (!is.null(search_params$`Allele balance`)) {
        updateSliderInput(session, "allele_balance", 
                          value = as.numeric(search_params$`Allele balance`))
      }
      
      # Update Genotype Quality
      if (!is.null(search_params$`Genotype quality`)) {
        updateSliderInput(session, "genotype_quality", 
                          value = as.numeric(search_params$`Genotype quality`))
      }
      
      # Update Filter Value (PASS only vs Show all)
      if (!is.null(search_params$`Filter value`)) {
        updateSelectInput(session, "pass_variants", 
                          selected = search_params$`Filter value`)
      }
      
    })
    

    observeEvent(input$pathogenicity, {
      if (input$pathogenicity == "Pathogenic/Likely pathogenic") {
        select <- c("Pathogenic", "Likely pathogenic")
      } else if (input$pathogenicity == "Not benign") {
        select <- c("Pathogenic", "Likely pathogenic", "VUS", "Conflicting")
      } else if (input$pathogenicity == "") {
        select <- NULL
      } else {
        return
      }
      updatePrettyCheckboxGroup(session, "clinvar_checkboxes", selected = select)
    })


    observeEvent(input$annotation, {
      if (input$annotation == "High impact") {
        select <- c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                    "Frameshift variant")
      } else if (input$annotation == "Moderate to high impact") {
        select <- c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                    "Frameshift variant", "Missense variant", "In-frame variant")
      } else if (input$annotation == "") {
        select <- NULL
      } else {
        return
      }
      updatePrettyCheckboxGroup(session, "conseq_checkboxes", selected = select)
    })

    observeEvent(input$panelapp, {
      tmp <- panel_app_genes[Level4 %in% input$panelapp]
      green_genes(sort(unique(tmp[Sources == "Green", Entity_Name])))
      red_genes(sort(unique(tmp[Sources == "Red", Entity_Name])))
      amber_genes(sort(unique(tmp[Sources == "Amber", Entity_Name])))
      unclassified_genes(sort(unique(tmp[!(Sources %in% c("Green", "Red", "Amber")),
                                         Entity_Name])))
    }, ignoreNULL = FALSE)

    # Render UI outputs for each gene category
    output$green_genes <- renderText({ paste(green_genes(), collapse = "; ") })
    output$red_genes <- renderText({ paste(red_genes(), collapse = "; ") })
    output$amber_genes <- renderText({ paste(amber_genes(), collapse = "; ") })
    output$unclassified_genes <- renderText({
      paste(unclassified_genes(), collapse = "; ")
    })

    ##################### Phenotype

    # Add multiple IDs to the phenotype list
    observeEvent(input$phenotype_add, {
      new_ids <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))
      new_ids <- trimws(new_ids)
      print(new_ids)

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

    hide_spinner()

    ##################### Filtering

    observeEvent(input$apply_filter, {

      showNotification("Filtering...", duration = NULL, id = ns("notify_filter"),
                       type = "message")

      # Define filters
      snv_filters <- list(
        clinvar_filter = input$clinvar_checkboxes,
        af_value = as.numeric(input$af),
        annotation_filter = input$conseq_checkboxes,
        revel_value = input$revel,
        sift_filter = input$sift,
        polyphen_filter = input$polyphen,
        spliceai_filter = input$spliceai_score,
        inheritance_filter = input$inher,
        panelapp_filter = input$panelapp,
        genotype_quality_value = input$genotype_quality,
        allele_balance_value = input$allele_balance,
        hpo_terms_list = phenos()
      )

      # Define the dynamic filter function
      snv_filter_dataset <- function(data, filters) {


        # Helper function
        compare_allele_count <- function(col, values) {
          values <- as.numeric(unlist(strsplit(values, "-")))
          if (length(values) == 1) {
            filtered_rows <- (col == values)
          } else if (length(values) == 2) {
            filtered_rows <- (col >= values[1] & col <= values[2])
          } else {
            filtered_rows <- rep(TRUE, length(col))
          }
          return(filtered_rows)
        }

        # Initialise the filter expression
        filter_expression <- "TRUE"  # Start with TRUE to simplify appending conditions

        global_filters_expression <- "TRUE"

        # Inheritance filter
        if (!is.null(filters$inheritance_filter) && filters$inheritance_filter != "") {
          allele_count <- allele_tab()
          names(allele_count) <- c("sample_id", "allele_count")
          allele_count <- merge(pedigree, allele_count, by = "sample_id")
          allele_count[, col_name := paste0("alt_allele_count_", code), by = sample_id]

          inheritance_conditions <- vector("character")

          for (i in seq_len(nrow(allele_count))) {
            col_name <- allele_count[i, col_name]
            val <- allele_count[i, allele_count]
            condition <- sprintf("compare_allele_count(get('%s'), '%s')",col_name,val)
            inheritance_conditions <- c(inheritance_conditions, condition)
          }

          # Combine inheritance conditions into a single OR expression
          if (length(inheritance_conditions) > 0) {
            inheritance_expression <- paste(inheritance_conditions, collapse = " & ")
            filter_expression <- paste(filter_expression, inheritance_expression, sep = " & ")
            global_filters_expression <- paste(global_filters_expression, inheritance_expression, sep = " & ")
          }
        }

        # Panel app gene lists
        if (length(filters$panelapp_filter) > 0) {
          genes <- panel_app_genes[Level4 %in% filters$panelapp_filter,.(PANEL_APP=Level4,GENE_SYMBOL=Entity_Name,INHERITANCE=Model_Of_Inheritance)]
          genes <- genes[,.(PANEL_APP=paste(PANEL_APP,collapse=";"),INHERITANCE=paste(INHERITANCE,collapse=";")),by=GENE_SYMBOL]
          genes_search <- genes[,GENE_SYMBOL]
          panel_app_condition <- "GENE_SYMBOL %in% genes_search"
          filter_expression <- paste(filter_expression,panel_app_condition, sep = " & ")
          global_filters_expression <- paste(global_filters_expression, panel_app_condition, sep = " & ")

        }

        # VEP annotation filter
        if (length(filters$annotation_filter) > 0) {
          vep_search_terms <- vep_consequences[consequence %in% filters$annotation_filter, term]
          vep_condition <- sprintf("grepl('%s', CONSEQUENCE, ignore.case = TRUE)", 
                                   paste(vep_search_terms, collapse = "|"))
          filter_expression <- paste(filter_expression, vep_condition, sep = " & ")
        }

        # Allele frequency (AF) filter
        if (!is.null(filters$af_value) && filters$af_value < 1) {
          af_condition <- sprintf("(is.na(AF) | AF <= %f)", filters$af_value)
          filter_expression <- paste(filter_expression, af_condition, sep = " & ")
        }

        # Call Quality filter
        if (!is.null(filters$genotype_quality_value) && filters$genotype_quality_value > 0) {
          quality_condition <- sprintf("QUAL >= %f", filters$genotype_quality_value)
          filter_expression <- paste(filter_expression, quality_condition, sep = " & ")
        }

        # Allele Balance filter
        if (!is.null(filters$allele_balance_value) && filters$allele_balance_value > 0) {
          vaf_vars <- grep("^VAF_", colnames(data), value = TRUE)

          if (!is.null(filters$affected_switch) && filters$affected_switch) {
            vaf_vars <- vaf_vars[grep(paste0(pedigree_data[status == "affected", code], collapse = "|"), vaf_vars)]
          }

          allele_balance_conditions <- sapply(vaf_vars, function(var) {
            sprintf("get('%s') >= %f", var, filters$allele_balance_value)
          })

          allele_balance_expression <- paste(allele_balance_conditions, collapse = " & ")
          filter_expression <- paste(filter_expression, allele_balance_expression, sep = " & ")
        }

        # REVEL score
        if (!is.null(filters$revel_value) && filters$revel_value > 0) {
          revel_condition <- sprintf("REVEL >= %f", filters$revel_value)
          filter_expression <- paste(filter_expression, revel_condition, sep = " & ")
        }

        # SIFT filter
        if (!is.null(filters$sift_filter) && length(filters$sift_filter) > 0) {
          sift_condition <- sprintf("grepl('%s', SIFT, ignore.case = TRUE)", 
                                    paste(filters$sift_filter, collapse = "|"))
          filter_expression <- paste(filter_expression, sift_condition, sep = " & ")
        }

        # PolyPhen filter
        if (!is.null(filters$polyphen_filter) && length(filters$polyphen_filter) > 0) {
          polyphen_search <- gsub(" ", "_", filters$polyphen_filter)
          polyphen_condition <- sprintf("grepl('%s', PolyPhen, ignore.case = TRUE)", 
                                        paste(polyphen_search, collapse = "|"))
          filter_expression <- paste(filter_expression, polyphen_condition, sep = " & ")
        }

        # ClinVar filter and override
        clinvar_override_condition <- NULL
        if (!is.null(filters$clinvar_filter) && length(filters$clinvar_filter) > 0) {
          # Main ClinVar condition
          clinvar_filter_updated <- gsub("VUS", "uncertain", filters$clinvar_filter)
          clinvar_pattern <- paste(sapply(clinvar_filter_updated, function(x) paste0("\\\\b", x, "\\\\b")), collapse = "|")

          # Add condition for "Not available" (i.e., NA values in CLINVAR)
          if ("Not available" %in% filters$clinvar_filter) {
            clinvar_condition <- sprintf("(%s | is.na(CLINVAR))", 
                                         paste0("grepl('", clinvar_pattern, "', CLINVAR, ignore.case = TRUE)"))
          } else {
            clinvar_condition <- sprintf("grepl('%s', CLINVAR, ignore.case = TRUE)", clinvar_pattern)
          }
          #clinvar_condition <- sprintf("grepl('%s', CLINVAR, ignore.case = TRUE)", clinvar_pattern)
          filter_expression <- paste(filter_expression, clinvar_condition, sep = " & ")

          # ClinVar override for specific terms
          override_patterns <- c()
          if ("Pathogenic" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\bPathogenic\\\\b")
          if ("Likely pathogenic" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\bLikely pathogenic\\\\b")
          if ("uncertain" %in% filters$clinvar_filter) override_patterns <- c(override_patterns, "\\\\buncertain\\\\b")

          if (length(override_patterns) > 0) {
            override_pattern <- paste(override_patterns, collapse = "|")
            clinvar_override_condition <- sprintf(
              "(grepl('%s', CLINVAR, ignore.case = TRUE) & (is.na(AF) | AF < 0.05))",
              override_pattern
            )
          }
        }

        # SpliceAI override
        spliceai_override_condition <- NULL
        if (!is.null(filters$spliceai_filter) && filters$spliceai_filter > 0) {
          spliceai_override_condition <- sprintf(
            "(Donor_Loss > %f | Donor_Gain > %f | Acceptor_Loss > %f | Acceptor_Gain > %f)",
            filters$spliceai_filter,
            filters$spliceai_filter,
            filters$spliceai_filter,
            filters$spliceai_filter
          )
        }


        all_conditions <- list(
          if (!is.null(filters$inheritance_filter) && filters$inheritance_filter == "X-Linked Recessive") {
            sprintf("(%s & CHROM == 'chrX')", filter_expression)
          } else {
            sprintf("(%s)", filter_expression)
          },
          # Add SpliceAI override with conditional CHROM filter
          if (!is.null(spliceai_override_condition)) {
            if (!is.null(filters$inheritance_filter) && filters$inheritance_filter == "X-Linked Recessive") {
              sprintf("(%s & %s & CHROM == 'chrX')", global_filters_expression, spliceai_override_condition)
            } else {
              sprintf("(%s & %s)", global_filters_expression, spliceai_override_condition)
            }
          } else {
            NULL
          },

          # Add ClinVar override with conditional CHROM filter
          if (!is.null(clinvar_override_condition)) {
            if (!is.null(filters$inheritance_filter) && filters$inheritance_filter == "X-Linked Recessive") {
              sprintf("(%s & %s & CHROM == 'chrX')", global_filters_expression, clinvar_override_condition)
            } else {
              sprintf("(%s & %s)", global_filters_expression, clinvar_override_condition)
            }
          } else {
            NULL
          }
        )
        #combined_expression <- paste(na.omit(all_conditions), collapse = " | ")
        combined_expression <- paste(Filter(Negate(is.null), all_conditions), collapse = " | ")
        print(combined_expression)

        # Special case for X-Linked Recessive
        if (filters$inheritance_filter == "X-Linked Recessive") {
          combined_expression <- sprintf("(%s & CHROM == 'chrX')",  combined_expression)
        }

        # Evaluate the combined expression on the data.table
        filtered_data <- data[eval(parse(text = combined_expression))]


        # Add Panel app information
        if (length(filters$panelapp_filter) > 0) {
          setkey(filtered_data, GENE_SYMBOL)
          setkey(genes, GENE_SYMBOL)
          filtered_data <- merge(filtered_data,genes,by = "GENE_SYMBOL",all.x = TRUE)
        } else {
          filtered_data[,PANEL_APP:=NA]
          filtered_data[,INHERITANCE:=NA]
        }


        # Add HPO information
        if (!is.null(filters$hpo_terms_list) && length(filters$hpo_terms_list) > 0) {
          split_hpo_terms_list <- unlist(strsplit(filters$hpo_terms_list,"; "))
          hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list & gene_symbol %in% filtered_data$GENE_SYMBOL][,.(HPO_ID=hpo_id,GENE_SYMBOL=gene_symbol)]
          hpo_terms_summary <- hpo_terms_data[, .(HPO_ID = paste(HPO_ID, collapse = ";"), HPO_COUNT = .N), by = GENE_SYMBOL]
          setkey(hpo_terms_summary, GENE_SYMBOL)
          filtered_data <- merge(filtered_data,hpo_terms_summary,by = "GENE_SYMBOL",all.x = TRUE)
          filtered_data[is.na(HPO_COUNT), HPO_COUNT := 0]
        } else {
          filtered_data[,HPO_ID:=NA]
          filtered_data[,HPO_COUNT:=0]
        }

        filtered_data[, clinvar_override := FALSE]
        filtered_data[, spliceai_override := FALSE]
        
        if (!is.null(clinvar_override_condition)) {
          filtered_data[eval(parse(text = clinvar_override_condition)),
                        clinvar_override := TRUE]
        }
        
        if (!is.null(spliceai_override_condition)) {
          filtered_data[eval(parse(text = spliceai_override_condition)),
                        spliceai_override := TRUE]
        }

        return(filtered_data)
      }


      total_time <- system.time({
        snv_filtered_data <- snv_filter_dataset(dataset[CATEGORY=="SNV & Indel"], snv_filters)
      })
      cat(paste("Total execution time:", format_time(total_time), "\n"))


      # Handle PRIORITY and NOTES
      filtered_data <- data.table(filtered_table_output())

      if (!is.null(filtered_data) && all(c("PRIORITY", "NOTES") %in% colnames(filtered_data))) {
        # Extract flagged rows
        current_flagged_rows <- filtered_data[,
          .(ID, CURRENT_PRIORITY=PRIORITY,CURENT_NOTES=NOTES)
        ]

        # Check for changes in flagged rows
        previous_flagged_rows <- flagged_rows_reactive()
        if (!is.null(previous_flagged_rows)) {
          # Identify new or updated rows
          merged_flagged_rows <- merge(
            previous_flagged_rows, current_flagged_rows,
            by = "ID", all = TRUE)
        }
        current_flagged_rows <- merged_flagged_rows[,.(ID,
            PRIORITY = fifelse(is.na(PRIORITY) | (PRIORITY != CURRENT_PRIORITY & !is.na(CURRENT_PRIORITY)), CURRENT_PRIORITY, PRIORITY),
            NOTES = fifelse(is.na(NOTES) | (NOTES != CURENT_NOTES & !is.na(CURENT_NOTES)), CURENT_NOTES, NOTES))]

        # Update flagged rows reactive value
        flagged_rows_reactive(copy(current_flagged_rows))

        # Merge flagged rows back into the filtered data
        snv_filtered_data <- merge(current_flagged_rows, snv_filtered_data, by = "ID", all.y = TRUE)
        snv_filtered_data[is.na(PRIORITY), PRIORITY := 0]
        snv_filtered_data[PRIORITY > 0, PRIORITYFlag := TRUE]
        snv_filtered_data[PRIORITY < 0, PRIORITYFlag := FALSE]
      } else {
        # Fallback if no PRIORITY or NOTES columns exist
        snv_filtered_data <- data.table(PRIORITY = 0, NOTES = "", snv_filtered_data)
      }

      filtered_table_output(copy(snv_filtered_data))

      removeNotification(ns("notify_filter"))
    }) 

    return(filtered_table_output)
  })
}
