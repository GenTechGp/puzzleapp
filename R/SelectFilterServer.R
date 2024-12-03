# Metadata

metaServer <- function(output, pedigree) {
  peditab <- pedigree[, 1:4]
  names(peditab) <- c("Sample ID", "Kinship", "Status", "Sex")
  output$meta <- renderTable(peditab)
}

alleleCount <- function(inher, p) {
  if (inher == "Homozygous Recessive") {
    if (p$status == "affected")
      "2"
    else
      "0, 1"
  } else if (inher == "Dominant/De Novo") {
    if (p$status == "affected")
      "1, 2"
    else
      "0"
  } else if (inher == "Compound Heterozygous") {
    if (p$status == "affected")
      "1"
    else
      "0, 1"
  } else if (inher == "X-Linked Recessive") {
    if (p$sex == "male") {
      if (p$status == "affected")
        "1"
      else
        "0"
    } else {
      if (p$status == "affected")
        "2"
      else
        "0, 1"
    }
  } else if (inher == "Custom") {
    id = paste0("allele-box-", p$sample_id)
    as.character(checkboxGroupInput(id, NULL, c("0", "1", "2"), inline = TRUE))
  }
}

alleleTable <- function(inher, pedigree) {
  tab <- data.frame(id = pedigree$sample_id, alleles = NA)
  for (id in pedigree$sample_id) {
    p <- pedigree[pedigree$sample_id == id, ]
    tab[tab$id == id, "alleles"] <- alleleCount(inher, p)
  }
  names(tab) <- c("Sample ID", "Allele Count")
  tab
}

alleleUI <- function(inher, pedigree, allele_tab) {
  if (inher == "")
    return()

  allele_tab(alleleTable(inher, pedigree))
  renderTable(allele_tab(), sanitize.text.function = function(x) x)
}

alleleServer <- function(input, output, pedigree, allele_tab) {
  ui <- eventReactive(input$inher, alleleUI(input$inher, pedigree, allele_tab))
  output$allele <- renderUI(ui())
}

# Main

selectFiltersServer <- function(id, dataset, pedigree, panel_app_genes, vep_consequences, phenotype_data = phenotype_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    show_spinner()

    number_of_individuals <- reactiveVal(dim(pedigree)[1])

    allele_tab <- reactiveVal()

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

    metaServer(output, pedigree)

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
      shinyjs::enable(ns("inher"))
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

    alleleServer(input, output, pedigree, allele_tab)

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
      } else if (input$annotation == "") {
        updatePrettyCheckboxGroup(session, inputId = "consequence_checkboxes",
                                  choiceNames = consequence_options_display,
                                  choiceValues = consequence_options,
                                  selected = NULL,inline = TRUE)
      }
      #hide_spinner()
    })

    observe({
      #show_spinner()
      output$sv_features <- renderUI({
        sv_features_checkboxes_list <- list(prettyCheckboxGroup(ns("sv_features_checkboxes"), "SV type:",
                                                                choiceNames = c("Insertion", "Deletion", "Duplication", "Inversion","Translocation"),
                                                                choiceValues = c("Insertion", "Deletion", "Duplication", "Inversion","Translocation"),
                                                                selected = NULL,inline = FALSE))
        do.call(tagList,sv_features_checkboxes_list)
      })

      output$sv_relative_pos <- renderUI({
        sv_relative_pos_checkboxes_list <- list(prettyCheckboxGroup(ns("sv_relative_pos_checkboxes"), "Location:",
                                                                    choiceNames = c("Exonic", "Intronic", "UTR", "Promoter", "Intergenic"),
                                                                    choiceValues = c("Exonic", "Intronic", "UTR", "Promoter", "Intergenic"),
                                                                    selected = NULL,inline = FALSE))
        do.call(tagList,sv_relative_pos_checkboxes_list)
      })

      output$sv_consequence <- renderUI({
        sv_consequence_checkboxes_list <- list(prettyCheckboxGroup(ns("sv_consequence_checkboxes"), "Predicted consequences:",
                                                                   choiceNames = c("Loss of function (LoF)", "Copy Number Variation (CNV)", "Whole gene inversion", "Regulatory and Non-coding variants"),
                                                                   choiceValues = c("Loss of function (LoF)", "Copy Number Variation (CNV)", "Whole gene inversion", "Regulatory and Non-coding variants"),
                                                                   selected = "Loss of function (LoF)",inline = FALSE))
        do.call(tagList,sv_consequence_checkboxes_list)
      })

      # Function to dynamically render gene lists based on their source (e.g., "Green", "Red", "Amber", or "Unclassified").
      renderGeneOutput <- function(source, outputId, reactiveStore) {
        renderUI({
          genes <- ""
          if (!is.null(input$panelapp)) {
            if (source == "Unclassified") {
              genes <- sort(unique(panel_app_genes[Level4 %in% input$panelapp & !(Sources %in% c("Green", "Red", "Amber")), Entity_Name]))
            } else {
              genes <- sort(unique(panel_app_genes[Level4 %in% input$panelapp & Sources == source, Entity_Name]))
            }
          }
          reactiveStore(genes)  # Update the reactive store (if used)
          genes <- paste(genes, collapse = "; ")
          p(genes)  # Return the content as a paragraph
        })
      }

      # Render UI outputs for each gene category using the helper function
      output$green_genes <- renderGeneOutput("Green", "green_genes", green_genes)
      output$red_genes <- renderGeneOutput("Red", "red_genes", red_genes)
      output$amber_genes <- renderGeneOutput("Amber", "amber_genes", amber_genes)
      output$unclassified_genes <- renderGeneOutput("Unclassified", "unclassified_genes", unclassified_genes)

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
      variant_coord
    })


    ################# Short List

    # Add ID to the short list
    observeEvent(input$shortlist_add, {
      new_id <- input$shortlist_var
      if (new_id != "" & new_id %in% dataset$ID) {
        current_ids <- current_shortlisted_ids()
        if (!(new_id %in% current_ids)) {
          current_shortlisted_ids(c(current_ids, new_id))
          updateTextInput(session, "shortlist_var", value = "")
        } else {
          updateTextInput(session, "shortlist_var", value = "")
        }
      }
    })

    # Remove ID from the short list
    observeEvent(input$shortlist_remove, {
      #print("remove-button")
      id_to_remove <- input$shortlist_var
      if (id_to_remove != "") {
        current_ids <- current_shortlisted_ids()
        if (id_to_remove %in% current_ids) {
          current_shortlisted_ids(current_ids[current_ids != id_to_remove])
          updateTextInput(session, "shortlist_var", value = "")
        } else {
          updateTextInput(session, "shortlist_var", value = "")
        }
      }
    })

    # Display the list of shortlisted IDs
    output$shortlist <- renderUI({
      if (length(current_shortlisted_ids()) > 0) {
        variant_ids <- paste(current_shortlisted_ids(),collapse="; ")
      } else {
        variant_ids <- ""
      }
      variant_ids
    })

    ##################### Black List

    # Add ID to the black list
    observeEvent(input$blacklist_add, {
      new_id <- input$blacklist_var
      if (new_id != "" & new_id %in% dataset$ID) {
        current_ids <- current_blacklisted_ids()
        if (!(new_id %in% current_ids)) {
          current_blacklisted_ids(c(current_ids, new_id))
          updateTextInput(session, "blacklist_var", value = "")
        } else {
          updateTextInput(session, "blacklist_var", value = "")
        }
      }
    })

    # Remove ID from the black list
    observeEvent(input$blacklist_remove, {
      #print("remove-button")
      id_to_remove <- input$blacklist_var
      if (id_to_remove != "") {
        current_ids <- current_blacklisted_ids()
        if (id_to_remove %in% current_ids) {
          current_blacklisted_ids(current_ids[current_ids != id_to_remove])
          updateTextInput(session, "blacklist_var", value = "")
        } else {
          updateTextInput(session, "blacklist_var", value = "")
        }
      }
    })

    # Display the list of blacklisted IDs
    output$blacklist <- renderUI({
      if (length(current_blacklisted_ids()) > 0) {
        variant_ids <- paste(current_blacklisted_ids(),collapse="; ")
      } else {
        variant_ids <- ""
      }
      variant_ids
    })

    ##################### Phenotype

    # Add multiple IDs to the phenotype list
    observeEvent(input$phenotype_add, {
      new_ids <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))
      new_ids <- trimws(new_ids)
      print(new_ids)

      if (length(new_ids) > 0) {
        current_ids <- current_phenotype_terms()
        valid_new_ids <- new_ids[new_ids %in% phenotype_data$hpo_id & !(new_ids %in% current_ids)]

        if (length(valid_new_ids) > 0) {
          current_phenotype_terms(c(current_ids, valid_new_ids))
        }

        updateTextInput(session, "phenotype_var", value = "")
      }
    })


    observeEvent(input$phenotype_remove, {
      ids_to_remove <- unlist(strsplit(input$phenotype_var, split = "\\s+|,|;"))  # Split by spaces, commas, or semicolons

      if (length(ids_to_remove) > 0) {
        current_ids <- current_phenotype_terms()
        ids_to_remove <- trimws(ids_to_remove)  # Remove leading/trailing spaces
        ids_to_remove <- ids_to_remove[ids_to_remove != ""]  # Remove empty strings

        # Remove the terms that are in the current list
        updated_ids <- setdiff(current_ids, ids_to_remove)

        # Update the current phenotype terms list
        current_phenotype_terms(updated_ids)

        # Clear the input field
        updateTextInput(session, "phenotype_var", value = "")
      }
    })

    # Display the list of HPO terms
    output$phenotype <- renderUI({
      if (length(current_phenotype_terms()) > 0) {
        hpo_ids <- paste(current_phenotype_terms(),collapse="; ")
      } else {
        hpo_ids <- ""
      }
      hpo_ids
    })

    hide_spinner()

    ##################### Filtering

    observeEvent(input$apply_filter, {
      total_time <- system.time({
        show_spinner()
        print("Started filtering")



        # Read selected filters
        filter_time <- system.time({
          inheritance_filter <- input$inher
          pathogenicity_filter <- input$pathogenicity
          clinvar_filter <- input$clinvar_checkboxes
          annotation_filter <- input$consequence_checkboxes
          revel_value <- input$revel
          sift_filter <- input$sift
          polyphen_filter <- input$polyphen
          af_value <- input$af
          # pass_variants_filter <- input$pass_variants
          genotype_quality_value <- input$genotype_quality
          allele_balance_value <- input$allele_balance
          panelapp_filter <- input$panelapp
          spliceai_filter <- input$spliceai_score
          sv_types <- input$sv_features_checkboxes
          sv_min_svlen <- input$min_svlen
          sv_max_svlen <- input$max_svlen
          sv_genotype_quality_value <- input$sv_genotype_quality
          sv_pass_variants_filter <- input$sv_pass_variants
          sv_relative_pos <- input$sv_relative_pos_checkboxes
          sv_consequences <- input$sv_consequence_checkboxes
          sv_allele_balance_value <- input$sv_allele_balance

          filtered_ids <- dataset[, ID]
          id_vars <- names(dataset)
        })
        cat(paste("Filter reading time:", format_time(filter_time),"\n"))

        ## GLOBAL FILTERS (Inheritance and PanelApp)
        # 1) by inheritance

        inheritance_time <- system.time({
          if (inheritance_filter != "") {
            compare_allele_count <- function(col, values) {
              values <- as.numeric(unlist(strsplit(values, "-")))
              if (length(values) == 1) {
                filtered_rows <- (col == values)
              } else if (length(values) == 2) {
                filtered_rows <- (col >= values[1] & col <= values[2])
              }
              return(filtered_rows)
            }

            allele_count <- allele_tab()
            names(allele_count) <- c("sample_id", "allele_count")
            allele_count <- merge(pedigree, allele_count, by = "sample_id")
            allele_count[, col_name := paste0("alt_allele_count_", code), by = sample_id]
            #print(allele_count)

            for (i in seq_len(nrow(allele_count))) {
              col_name <- allele_count[i, col_name]
              val <- allele_count[i, allele_count]
              filtered_ids <- intersect(dataset[compare_allele_count(get(col_name),val),ID],filtered_ids)
            }
            if (inheritance_filter == "X-Linked Recessive") {
              filtered_ids <- dataset[ID %in% filtered_ids & CHROM =="chrX",]
              print(length(filtered_ids))
            }
          }
        })
        cat(paste("Inheritance time:", format_time(inheritance_time),"\n"))

        # by PanelApp genes
        panelapp_time <- system.time({
          if (length(panelapp_filter) > 0) {
            genes <- panel_app_genes[Level4 %in% panelapp_filter,.(PANEL_APP=Level4,GENE_SYMBOL=Entity_Name,INHERITANCE=Model_Of_Inheritance)]
            genes <- genes[,.(PANEL_APP=paste(PANEL_APP,collapse=";"),INHERITANCE=paste(INHERITANCE,collapse=";")),by=GENE_SYMBOL]
            selected_vars <- c("ID","GENE_SYMBOL")
            print(dim(genes))
            print(dim(dataset[(ID %in% filtered_ids) & !is.na(GENE_SYMBOL),c(selected_vars),with=FALSE][, c(GENE_SYMBOL=strsplit(GENE_SYMBOL, ",")), by=setdiff(selected_vars,"GENE_SYMBOL")]))
            if (nrow(dataset[(ID %in% filtered_ids) & !is.na(GENE_SYMBOL), c(selected_vars), with = FALSE]) > 0) {
              genes <- merge(
                dataset[(ID %in% filtered_ids) & !is.na(GENE_SYMBOL), c(selected_vars), with = FALSE][, c(GENE_SYMBOL = strsplit(GENE_SYMBOL, ",")), by = setdiff(selected_vars, "GENE_SYMBOL")],
                genes, by = "GENE_SYMBOL"
              )
              filtered_ids <- intersect(filtered_ids,genes[,ID])
            } else {
              genes[,ID:=NA]
              warning("The filtered dataset is empty. Merge operation will be skipped.")
            }
          }
        })
        cat(paste("PanelApp time:", format_time(panelapp_time),"\n"))

        # check short list
        # Shortlisted variants
        shortlisted_ids <- current_shortlisted_ids()
        split_shortlisted_ids <- c()
        if (length(shortlisted_ids) > 0) {
          split_shortlisted_ids <- unlist(strsplit(shortlisted_ids,"; "))
        }

        ## SNV-specific FILTERS (Pathogenicity, Annotation, in silico filters, call quality, frequency)
        snv_filtered_ids <- dataset[(ID %in% filtered_ids) & (CATEGORY =="SNV & Indel"),ID]
        sv_filtered_ids <- dataset[(ID %in% filtered_ids) & (CATEGORY =="SV"),ID]

        # Clinvar filter
        clinvar_time <- system.time({
          clinvar_override <- c()
          if (length(clinvar_filter) > 0) {
            clinvar_filter <- gsub("VUS", "uncertain", clinvar_filter)
            clinvar_pattern <- paste(sapply(clinvar_filter, function(x) paste0("\\b", x, "\\b")), collapse = "|")
            if ("Not available" %in% clinvar_filter) {
              na_snv_filtered_ids <- intersect(snv_filtered_ids,dataset[ID %in% snv_filtered_ids][is.na(CLINVAR),ID])
              #print("Not available - 1")
            }
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[ID %in% snv_filtered_ids][grep(clinvar_pattern, str_replace(CLINVAR,"_"," "), ignore.case = TRUE),ID])
            if ("Not available" %in% clinvar_filter) {
              snv_filtered_ids <- c(snv_filtered_ids,na_snv_filtered_ids)
              #print("Not available - 2")
            }
            override_patterns <- c()
            if ("Pathogenic" %in% clinvar_filter) {
              override_patterns <- c(override_patterns, "\\bPathogenic\\b")
              #clinvar_override <- unique(c(dataset[(ID %in% filtered_ids) & (CATEGORY =="SNV & Indel")][grep(paste0("\\b", "Pathogenic", "\\b"), str_replace(CLINVAR,"_"," "), ignore.case = TRUE),ID],clinvar_override))
            }
            if ("Likely pathogenic" %in% clinvar_filter) {
              override_patterns <- c(override_patterns, "\\bLikely pathogenic\\b")
              #clinvar_override <- unique(c(dataset[(ID %in% filtered_ids) & (CATEGORY =="SNV & Indel")][grep(paste0("\\b", "Likely pathogenic", "\\b"), str_replace(CLINVAR,"_"," "), ignore.case = TRUE),ID],clinvar_override))
            }
            if ("uncertain" %in% clinvar_filter) {
              override_patterns <- c(override_patterns, "\\buncertain\\b")
              #clinvar_override <- unique(c(dataset[(ID %in% filtered_ids) & (CATEGORY =="SNV & Indel")][grep(paste0("\\b", "uncertain", "\\b"), str_replace(CLINVAR,"_"," "), ignore.case = TRUE),ID],clinvar_override))
            }
            if (length(override_patterns) > 0) {
              override_pattern <- paste(override_patterns, collapse = "|")
              clinvar_override <- unique(dataset[ID %in% filtered_ids & CATEGORY == "SNV & Indel" & grepl(override_pattern, str_replace(CLINVAR, "_", " "), ignore.case = TRUE), ID])
              if (length(clinvar_override) > 0) {
                clinvar_override <- dataset[(ID %in% clinvar_override) & AF < 0.05,ID]
              }
            }
          }
        })
        cat(paste("ClinVar time:", format_time(clinvar_time),"\n"))

        # Annotation filter
        annotation_time <- system.time({
          if (length(annotation_filter) > 0) {
            vep_search_terms <- vep_consequences[consequence %in% annotation_filter,term]
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[ID %in% snv_filtered_ids][grep(paste(vep_search_terms, collapse = '|'), CONSEQUENCE, ignore.case = TRUE),ID])
          }
        })
        cat(paste("Annotation time:", format_time(annotation_time),"\n"))

        spliceai_time <- system.time({
          # SpliceAI filter
          spliceAI_override <- c()
          if (spliceai_filter > 0) {
            if (spliceai_filter >= 0.2) {
              spliceAI_override <- unique(c(spliceAI_override,dataset[(ID %in% filtered_ids) & (CATEGORY =="SNV & Indel")][(Donor_Loss>spliceai_filter) | (Donor_Gain>spliceai_filter) | (Acceptor_Loss>spliceai_filter) | (Acceptor_Gain>spliceai_filter),ID]))
            }
          }
        })
        cat(paste("SpliceAI time:", format_time(spliceai_time),"\n"))

        insilicofilters_time <- system.time({
          # REVEL score
          if (revel_value > 0) {
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[ID %in% snv_filtered_ids][REVEL>=revel_value,ID])
          }

          # SIFT
          if (length(sift_filter) > 0) {
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[ID %in% snv_filtered_ids][grep(paste(sift_filter, collapse = '|'), SIFT, ignore.case = TRUE),ID])
          }

          # Polyphen
          if (length(polyphen_filter) > 0) {
            polyphen_search <- str_replace_all(polyphen_filter," ","_")
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[ID %in% snv_filtered_ids][grep(paste(polyphen_search, collapse = '|'), PolyPhen, ignore.case = TRUE),ID])
          }
        })
        cat(paste("In Silico filters time:", format_time(insilicofilters_time),"\n"))

        # Allele Frequency
        if (af_value < 1) {
          if (af_value > 0) {
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[(ID %in% snv_filtered_ids)][AF<=af_value | is.na(AF),ID])
          } else if (af_value == 0) {
            snv_filtered_ids <- intersect(snv_filtered_ids,dataset[(ID %in% snv_filtered_ids)][is.na(AF),ID])
            print("AF:")
            print(length(snv_filtered_ids))
          }
        }

        # Call Quality
        if (genotype_quality_value > 0) {
          snv_filtered_ids <- intersect(snv_filtered_ids,dataset[(ID %in% snv_filtered_ids)][QUAL>=genotype_quality_value,ID])
        }

        # Call Quality
        if (allele_balance_value > 0) {
          ad_vars <- grep("^AD_", id_vars, value = TRUE)
          vaf_vars <- gsub("AD","VAF",ad_vars)
          if (input$affected_switch) {
            vaf_vars  <- vaf_vars[grep(pedigree_data[status=="affected",code],vaf_vars)]
          }
          print(vaf_vars)
          print(allele_balance_value)
          vaf <- dataset[(ID %in% snv_filtered_ids),c("ID",vaf_vars),with=FALSE]
          vaf[vaf<allele_balance_value] <- NA
          vaf <- vaf[complete.cases(vaf)]
          snv_filtered_ids <- intersect(snv_filtered_ids,vaf$ID)
        }

        ## SV-specific FILTERS

        # SV type
        sv_type_options <- c("Insertion"="INS", "Deletion"="DEL", "Duplication"="DUP", "Inversion"="INV","Translocation"="TRA")
        if (length(sv_types) > 0) {
          sv_type_selected <- sv_type_options[sv_types]
          names(sv_type_selected) <- NULL
          sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (VAR_TYPE %in% sv_type_selected),ID])
        }

        # SV annotation
        sv_relative_pos_patterns <- c("Exonic"="PREDICTED_LOF|PREDICTED_PARTIAL_EXON_DUP|PREDICTED_BREAKEND_EXONIC",
                                      "Intronic"="PREDICTED_INTRONIC","UTR"="PREDICTED_UTR","Promoter"="PREDICTED_PROMOTER","Intergenic"="PREDICTED_INTERGENIC|PREDICTED_NEAREST_TSS")
        if (length(sv_relative_pos) > 0) {
          sv_relative_pos_selected <- sv_relative_pos_patterns[sv_relative_pos]
          names(sv_relative_pos_selected) <- NULL
          sv_relative_pos_selected <- paste(sv_relative_pos_selected,collapse = "|")
          sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (grepl(sv_relative_pos_selected,CONSEQUENCE)),ID])
        }

        sv_consequences_patterns <- c("Loss of function (LoF)"="PREDICTED_LOF|PREDICTED_MSV_EXON_OVERLAP",
                                      "Copy Number Variation (CNV)"="PREDICTED_COPY_GAIN|PREDICTED_INTRAGENIC_EXON_DUP|PREDICTED_PARTIAL_EXON_DUP|PREDICTED_TSS_DUP|PREDICTED_DUP_PARTIAL|PREDICTED_MSV_EXON_OVERLAP",
                                      "Whole gene inversion"="PREDICTED_INV_SPAN","Regulatory and Non-coding variants"="PREDICTED_NONCODING_SPAN|PREDICTED_NONCODING_BREAKPOINT")
        if (length(sv_consequences) > 0) {
          sv_consequences_selected <- sv_consequences_patterns[sv_consequences]
          names(sv_consequences_selected) <- NULL
          sv_consequences_selected <- paste(sv_consequences_selected,collapse = "|")
          sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (grepl(sv_consequences_selected,CONSEQUENCE)),ID])
        }

        # SV length
        if (sv_min_svlen > 0) {
          sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (VAR_LENGTH >= sv_min_svlen),ID])
        }
        if (sv_max_svlen > 0) {
          sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (VAR_LENGTH <= sv_max_svlen),ID])
        }

        # SV Call Quality
        if (sv_genotype_quality_value > 0) {
          sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (QUAL>=sv_genotype_quality_value),ID])
        }

        # SV allele fraction
        if (sv_allele_balance_value > 0) {
          ad_vars <- grep("^AD_", id_vars, value = TRUE)
          vaf_vars <- gsub("AD","VAF",ad_vars)
          if (input$sv_affected_switch) {
            vaf_vars  <- vaf_vars[grep(pedigree_data[status=="affected",code],vaf_vars)]
          }
          vaf <- dataset[(ID %in% sv_filtered_ids),c("ID",vaf_vars),with=FALSE]
          vaf[vaf<allele_balance_value] <- NA
          vaf <- vaf[complete.cases(vaf)]
          sv_filtered_ids <- intersect(sv_filtered_ids,vaf$ID)
        }

        # SV Call filter
        if (sv_pass_variants_filter != "") {
          if (sv_pass_variants_filter == "PASS only variants") {
            sv_filtered_ids <- intersect(sv_filtered_ids,dataset[(ID %in% sv_filtered_ids) & (FILTER=="PASS"),ID])
          }
        }

        finaltable_time <- system.time({
          final_filtered_ids <- unique(c(snv_filtered_ids,sv_filtered_ids,split_shortlisted_ids,clinvar_override,spliceAI_override))

          # Prepare filtered dataset to send over to Variants tab
          # Variables to take care of are PANEL_APP, PRIORITY, COLOR, HPO_TERMS, HPO_COUNT
          filtered_dataset <- data.table(PRIORITY = 0,NOTES="", dataset[ID %in% final_filtered_ids])

          if (length(panelapp_filter) > 0) {

            if (nrow(filtered_dataset) > 0) {
              print("here")
              filtered_dataset <- merge(filtered_dataset,genes[,.(ID,PANEL_APP,INHERITANCE)],by="ID")
            } else {
              filtered_dataset[,PANEL_APP:=NA]
              filtered_dataset[,INHERITANCE:=NA]
            }

            filtered_dataset[, `:=`(PANEL_APP = paste(unique(PANEL_APP), collapse = ";"),
                                    INHERITANCE = paste(unique(INHERITANCE), collapse = ";")), by = ID]
            filtered_dataset <- unique(filtered_dataset)
          } else {
            filtered_dataset[,PANEL_APP:=NA]
            filtered_dataset[,INHERITANCE:=NA]
          }
        })
        cat(paste("Final filtered table time:", format_time(finaltable_time),"\n"))

        # Check HPO terms
        hpo_terms_list <- current_phenotype_terms()
        if (length(hpo_terms_list) > 0) {
          split_hpo_terms_list <- unlist(strsplit(hpo_terms_list,"; "))
          hpo_terms_data <- phenotype_data[hpo_id %in% split_hpo_terms_list][,.(HPO_ID=hpo_id,GENE_SYMBOL=gene_symbol)]
          hpo_terms_data <- unique(merge(filtered_dataset[,.(ID,GENE_SYMBOL)][, c(GENE_SYMBOL=strsplit(GENE_SYMBOL, ",")), by=ID],hpo_terms_data,by="GENE_SYMBOL")[,.(ID,HPO_ID)])
          hpo_terms_data <- hpo_terms_data[,.(HPO_ID=paste(HPO_ID,collapse=";")),by=ID]
          hpo_terms_data[,HPO_COUNT:=str_count(HPO_ID,"HP:")]
          filtered_dataset <- merge(filtered_dataset,hpo_terms_data,by="ID",all.x =TRUE)
        } else {
          filtered_dataset[,HPO_ID:=NA]
          filtered_dataset[,HPO_COUNT:=0]
        }

        # I want color to be the last variable I set
        filtered_dataset <- data.table(filtered_dataset,Color="#FFFFFF")
        filtered_dataset[CATEGORY=="SNV & Indel" & is.na(AF),AF:=0]
        if (length(shortlisted_ids) > 0 | length(clinvar_override) > 0 | length(spliceAI_override) > 0) {
          #split_shortlisted_ids <- c(split_shortlisted_ids,clinvar_override,spliceAI_override)
          filtered_dataset[ID %in% split_shortlisted_ids,Color:="#ACF3AE"]
          filtered_dataset[ID %in% clinvar_override,Color:="#FFA50099"]
          filtered_dataset[ID %in% spliceAI_override,Color:="#FFFF0099"]
          filtered_dataset[ID %in% split_shortlisted_ids,PRIORITY:=1]
        }
        blacklisted_ids <- current_blacklisted_ids()
        if (length(blacklisted_ids) > 0) {
          split_blacklisted_ids <- unlist(strsplit(blacklisted_ids,"; "))
          filtered_dataset[ID %in% split_blacklisted_ids,Color:="#FA6B84"]
          filtered_dataset[ID %in% split_blacklisted_ids,PRIORITY:=-1]
        }


        if (inheritance_filter=="Compound Heterozygous") {
          selected_cols <- c("ID", "GENE_SYMBOL", grep("^alt_allele_count", names(filtered_dataset), value = TRUE))
          selected_data <- filtered_dataset[, .SD, .SDcols = selected_cols]
          split_gene <- selected_data[,tstrsplit(GENE_SYMBOL,",",fixed=TRUE)]
          split_gene <- cbind(selected_data[, setdiff(selected_cols,"GENE_SYMBOL"),with=FALSE], split_gene)
          split_gene <- melt(split_gene, id.vars = setdiff(selected_cols,"GENE_SYMBOL"), value.name = "GENE_SYMBOL", na.rm = TRUE)
          split_gene <- split_gene[, .SD, .SDcols = selected_cols][(alt_allele_count_1==1)]
          compunt_hets <- split_gene[,.(VAR_COUNT=.N,SUM_1=sum(as.integer(alt_allele_count_1)),SUM_2=sum(as.integer(alt_allele_count_2)),SUM_3=sum(as.integer(alt_allele_count_3))),by=GENE_SYMBOL]
          filtered_dataset <- filtered_dataset[ID %in% split_gene[GENE_SYMBOL %in% compunt_hets[VAR_COUNT>1 & SUM_2 > 0 & SUM_3>0,GENE_SYMBOL],ID]]
        }

        filtered_table_output(as.data.frame(filtered_dataset))
        hide_spinner()
      })
      cat(paste("Total execution time:", format_time(total_time), "\n"))
      cat("\n")
    })

    return(filtered_table_output)

  })
}
