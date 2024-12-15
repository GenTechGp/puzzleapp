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
      "0-1"
  } else if (inher == "Dominant/De Novo") {
    if (p$status == "affected")
      "1-2"
    else
      "0"
  } else if (inher == "Compound Heterozygous") {
    if (p$status == "affected")
      "1"
    else
      "0-1"
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

updateAlleleTable <- function(inher, pedigree, allele_tab) {
  tab <- allele_tab()
  FUN <- function(x) alleleCount(inher, pedigree[pedigree$sample_id == x, ])
  tab["Allele Count"] <- sapply(tab["Sample ID"], FUN)
  allele_tab(tab)
}

alleleUI <- function(inher, allele_tab, allele_custom_tab) {
  if (inher == "")
    return()

  if (inher == "Custom")
    tab <- allele_custom_tab
  else
    tab <- allele_tab()

  renderTable(tab, sanitize.text.function = function(x) x)
}

alleleServer <- function(input, output, ns, pedigree, allele_tab) {
  # Init allele table and custom checkbox table
  allele_tab(alleleTable(pedigree, function(x) ""))
  allele_custom_tab <- alleleCustomTable(ns, pedigree)

  # Update allele table and table UI on inheritance dropdown event
  observeEvent(input$inher, {
    updateAlleleTable(input$inher, pedigree, allele_tab)
    ui <- alleleUI(input$inher, allele_tab, allele_custom_tab)
    output$allele <- renderUI(ui)
  })

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

    condition_status <- pedigree$status
    names(condition_status) <- pedigree$sample_id
    condition_status <- reactiveValues(values=condition_status)

    green_genes <- reactiveVal()
    red_genes <- reactiveVal()
    amber_genes <- reactiveVal()
    unclassified_genes <- reactiveVal()

    phenos <- reactiveVal()

    filtered_table_output <- reactiveVal(dataset)

    igv_coord_box <- reactiveVal(NULL)

    initialisation <- reactiveVal(TRUE)

    metaServer(output, pedigree)


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

    alleleServer(input, output, ns, pedigree, allele_tab)

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

    ################# IGV


    observeEvent(input$coords_button, {
      if (input$igv_var_id %in% dataset$ID) {

        x <- dataset[dataset$ID == input$igv_var_id, ]
        chrom <- x$CHROM
        pos <- x$POS
        len <- x$VAR_LENGTH
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

    output$igv_coord_box <- renderText(igv_coord_box())


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
      total_time <- system.time({
        show_spinner()
        print("Started filtering")



        # Read selected filters
        filter_time <- system.time({
          inheritance_filter <- input$inher
          pathogenicity_filter <- input$pathogenicity
          clinvar_filter <- input$clinvar_checkboxes
          annotation_filter <- input$conseq_checkboxes
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
              } else {
                filtered_rows <- rep(TRUE, length(col))
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
          final_filtered_ids <- unique(c(snv_filtered_ids,sv_filtered_ids,clinvar_override,spliceAI_override))

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
        hpo_terms_list <- phenos()
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
        if (length(clinvar_override) > 0 | length(spliceAI_override) > 0) {
          filtered_dataset[ID %in% clinvar_override,Color:="#FFA50099"]
          filtered_dataset[ID %in% spliceAI_override,Color:="#FFFF0099"]
        }


        if (inheritance_filter=="Compound Heterozygous") {
          selected_cols <- c("ID", "GENE_SYMBOL", grep("^alt_allele_count", names(filtered_dataset), value = TRUE))
          selected_data <- filtered_dataset[, .SD, .SDcols = selected_cols]
          split_gene <- selected_data[,tstrsplit(GENE_SYMBOL,",",fixed=TRUE)]
          split_gene <- cbind(selected_data[, setdiff(selected_cols,"GENE_SYMBOL"),with=FALSE], split_gene)
          split_gene <- melt(split_gene, id.vars = setdiff(selected_cols,"GENE_SYMBOL"), value.name = "GENE_SYMBOL", na.rm = TRUE)
          split_gene <- split_gene[, .SD, .SDcols = selected_cols][(alt_allele_count_1==1)]
          # TODO: below fails for datasets with 1-2 individuals
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
