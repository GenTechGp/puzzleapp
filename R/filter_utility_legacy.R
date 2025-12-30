read_search_files <- function(directory, flag_all=TRUE) {
  file_pattern <- ".*\\.tsv$"
  files_defaults <- character(0)
  files_work_dir <- character(0)
  if (flag_all) {
    directory_default <- system.file("extdata", "pre_saved_filters", package = "puzzleapp")
    files_defaults <- list.files(directory_default, pattern = file_pattern, full.names = TRUE)
  }
  if (!is.null(directory)) {
    files_work_dir <- list.files(directory, pattern = file_pattern, full.names = TRUE)
  }
  files <- unique(c(files_work_dir, files_defaults))  # Combine and remove duplicates
  # Initialize an empty list to store results
  search_data <- list()
  # Iterate over each file
  for (file in files) {
    # Read the file into a dataframe
    df <- read.delim(file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
    # Convert the data to a named list
    file_data <- setNames(as.list(df$Value), df$Key)
    # Extract the name of the file without extension to use as label
    label <- tools::file_path_sans_ext(basename(file))
    search_data[[label]] <- file_data
  }
  return(search_data)
}

list_files <- function(dir) {
  if(is.null(dir)) return(character(0))
  log_info(sprintf("[filtServer] Listing files in directory: %s", dir))
  if (dir.exists(dir)) {
    list.files(dir, full.names = FALSE)
  } else {
    character(0)
  }
}

extractCustomAllelCount <- function(pedigree, input) {
  res <- lapply(pedigree, function(sample) input[[paste0("allele_", sample$sample_id)]])
  names(res) <- sapply(pedigree, `[[`, "sample_id")
  res
}

getAlleleCounts <- function(pedigree, input) {
  counts <- list()
  if (length(pedigree) > 0) {
    if (input$inher == "Custom") {
      counts <- extractCustomAllelCount(pedigree, input)  # named list
      cat("Custom allele counts:\n")
      for (sid in names(counts)) {
        cat(sprintf("  %s: %s\n", sid, counts[[sid]] %||% ""))
      }
    } else if (input$inher != "") {
      counts <- puzzlecore_compute_allele_table(pedigree, input$inher)  # named list
      cat("Allele table counts:\n")
      for (sid in names(counts)) {
        cat(sprintf("  %s: %s\n", sid, counts[[sid]]))
      }
    }
  }
  counts
}

get_snv_filters <- function(input, phenos) {
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
    custom_genes = parse_gene_list(input$custom_genes),
    substract_panelapp_gene_lists_filter = input$substract_panelapp_gene_lists,
    substract_panelapp_genes_filter = parse_gene_list(input$substract_panelapp_genes),
    treat_negative = input$treat_negative,
    inheritance_panelapp_gene = input$inheritance_panelapp_gene,
    genotype_quality_value = input$genotype_quality,
    allele_balance_value = input$allele_balance,
    hpo_terms_list = phenos,
    affected_only = input$affected_switch,
    use_af = input$use_af
  )
  return(snv_filters)
}

get_sv_filters <- function(input, phenos) {
  sv_filters <- list(
    inheritance_filter = input$inher,
    panelapp_filter = input$panelapp,
    custom_genes = parse_gene_list(input$custom_genes),
    substract_panelapp_gene_lists_filter = input$substract_panelapp_gene_lists,
    substract_panelapp_genes_filter = parse_gene_list(input$substract_panelapp_genes),
    treat_negative = input$treat_negative,
    inheritance_panelapp_gene = input$inheritance_panelapp_gene,
    annotation_filter = input$sv_conseq_checkboxes,
    sv_features = input$sv_features_checkboxes,
    min_svlen = input$min_svlen,
    max_svlen = input$max_svlen,
    genotype_quality_value = input$sv_genotype_quality,
    allele_balance_value = input$sv_allele_balance,
    af_value = as.numeric(input$sv_af),
    hpo_terms_list = phenos,
    affected_only = input$sv_affected_switch
  )
  return(sv_filters)
}

capture_filters <- function(input, phenos, samples, flag_save_samples=FALSE, flag_save_hpo_panelapp=FALSE, flag_save_presaved_filter=FALSE) {
  # SNV filters (excluding shared)
  snv_filters <- list(
    "Annotation" = if (!is.null(input$conseq_checkboxes)) paste(input$conseq_checkboxes, collapse = ";") else "",
    "Pathogenicity" = if (!is.null(input$clinvar_checkboxes)) paste(input$clinvar_checkboxes, collapse = ";") else "",
    "SpliceAI score" = input$spliceai_score,
    "REVEL" = input$revel,
    "AlphaMissense" = input$alpha_missense,
    "SIFT" = input$sift,
    "PolyPhen" = input$polyphen,
    "gnomADv4 AF" = input$af,
    "Affected only" = input$affected_switch,
    "Allele balance" = input$allele_balance,
    "Genotype quality" = input$genotype_quality,
    "Use_AF" = input$use_af
  )

  # SV filters (excluding shared)
  sv_filters <- list(
    "Annotation" = if (!is.null(input$sv_conseq_checkboxes)) paste(input$sv_conseq_checkboxes, collapse = ";") else "",
    "SV type" = if (!is.null(input$sv_features_checkboxes)) paste(input$sv_features_checkboxes, collapse = ";") else "",
    "Min SV Length" = input$min_svlen,
    "Max SV Length" = input$max_svlen,
    "gnomADv4 AF" = input$sv_af,
    "Affected only" = input$sv_affected_switch,
    "Allele balance" = input$sv_allele_balance,
    "Genotype quality" = input$sv_genotype_quality
  )

  # Shared filters
  shared_filters <- list(
    "Inheritance" = input$inher
  )

  # if Inheritace is Custom then we need to save the Allele counts for each sample
  if (input$inher == "Custom") {
    # ped <- convert_samples_to_pedigree(samples)
    ped <- samples
    allele_counts <- getAlleleCounts(ped, input)
    # paste allele counts into a single string with sample_id:count;sample_id:count;...
    allele_counts_str <- paste(sapply(names(allele_counts), function(sid) {
      count <- allele_counts[[sid]]
      if (is.null(count)) count <- ""
      paste0(sid, ":", count)
    }), collapse = ";")
    if (flag_save_samples) {
      shared_filters[["Custom_Allele_Counts"]] <- allele_counts_str
    }
  }

  if (input$save_panelapp_hpo || flag_save_hpo_panelapp) {
    shared_filters[["PanelApp_Genes"]] <- if (!is.null(input$panelapp)) paste(input$panelapp, collapse = ";") else ""
    shared_filters[["Substract_PanelApp_Gene_Lists"]] <- if (!is.null(input$substract_panelapp_gene_lists)) paste(input$substract_panelapp_gene_lists, collapse = ";") else ""
    shared_filters[["Substract_PanelApp_Genes"]] <- input$substract_panelapp_genes
    shared_filters[["Custom_Genes"]] <- input$custom_genes
    shared_filters[["Treat_Negative"]] <- input$treat_negative
    shared_filters[["Inheritance_PanelApp_Gene"]] <- input$inheritance_panelapp_gene
    shared_filters[["HPO_Terms"]] <- paste(phenos, collapse = "; ")
  }

  if (flag_save_presaved_filter) {
    if (input$pre_saved_filters != "") {
      shared_filters[["PreSaved_Filter"]] <- input$pre_saved_filters
    }
  }

  # Prefix SNV and SV keys
  snv_prefixed <- setNames(snv_filters, paste0("SNV_", names(snv_filters)))
  sv_prefixed <- setNames(sv_filters, paste0("SV_", names(sv_filters)))

  # Combine all into one named list (order: SNV, SV, shared)
  all_filters <- c(snv_prefixed, sv_prefixed, shared_filters)

  # Return as a data.table
  data.table(
    Variable = names(all_filters),
    Value = vapply(all_filters, function(x) if (is.null(x)) "" else as.character(x), FUN.VALUE = character(1))
  )
}

# Helper: Single-key SV_Classification parser and applier (minimal coupling to UI)
update_sv_classification <- function(session, value) {
  # Known child sets
  mobile_subtypes <- c("line","sine","sva","retroposon","dna_transposon","ltr")
  tandem_subtypes <- c("str","vntr","tr","homo")

  # Normalize incoming tokens into codes (supports labels and hierarchical strings)
  normalize_token <- function(tok) {
    tok <- trimws(as.character(tok))
    if (tok == "") return("")

    # Map top-level labels to codes
    map_top <- c(
      "Non-repetitive"            = "nonrep",
      "Repetitive/Mobile"         = "rep_mobile",
      "Repetitive/Tandem"         = "rep_tandem",
      "Repetitive/Mixed"          = "rep_mixed",
      "Repetitive/Mobile/subtype" = "rep_mobile_subtype",
      "Repetitive/Tandem/subtype" = "rep_tandem_subtype",
      "Repetitive/Mixed/subtype"  = "rep_mixed_subtype"
    )
    if (tok %in% names(map_top)) tok <- map_top[[tok]]

    # Hierarchical label paths, e.g., "Repetitive/Mobile/SINE"
    if (grepl("^Repetitive/", tok) && grepl("/", tok)) {
      parts <- unlist(strsplit(tok, "/", fixed = TRUE))
      if (length(parts) >= 3) {
        parent_label <- paste(parts[1:2], collapse = "/")
        subtype_label <- parts[3]
        parent_code <- switch(tolower(parent_label),
          "repetitive/mobile" = "rep_mobile_subtype",
          "repetitive/tandem" = "rep_tandem_subtype",
          "repetitive/mixed"  = "rep_mixed_subtype",
          parent_label
        )
        # Map subtype labels to codes (case-insensitive)
        lab_to_code <- c(
          "line" = "line", "sine" = "sine", "sva" = "sva",
          "retroposon" = "retroposon", "dna transposon" = "dna_transposon", "ltr" = "ltr",
          "str" = "str", "vntr" = "vntr", "tr" = "tr", "homo" = "homo"
        )
        sub_code <- lab_to_code[[tolower(subtype_label)]]
        if (is.null(sub_code)) sub_code <- tolower(subtype_label)
        tok <- paste0(parent_code, "/", sub_code)
      }
    }

    # Already-coded child tokens like "rep_mobile_subtype/sine" pass through
    tok
  }
  # replace space with underscore in value
  value <- gsub(" ", "_", value, fixed = TRUE)
  # make to lowercase
  value <- tolower(value)
  tokens <- unlist(strsplit(as.character(value), ";", fixed = TRUE))
  tokens <- tokens[nzchar(tokens)]
  tokens <- vapply(tokens, normalize_token, FUN.VALUE = character(1))

  # Parent presence: consider plain flag OR any scoped child token for subtype parents
  present <- list(
    nonrep            = any(tokens == "nonrep"),
    rep_mobile        = any(tokens == "rep_mobile"),
    rep_tandem        = any(tokens == "rep_tandem"),
    rep_mixed         = any(tokens == "rep_mixed"),
    rep_mobile_subtype = any(tokens == "rep_mobile_subtype"  | grepl("^rep_mobile_subtype/",  tokens)),
    rep_tandem_subtype = any(tokens == "rep_tandem_subtype"  | grepl("^rep_tandem_subtype/",  tokens)),
    rep_mixed_subtype  = any(tokens == "rep_mixed_subtype"   | grepl("^rep_mixed_subtype/",   tokens))
  )

  # Collect child selections per context (normalize to lowercase)
  mm_sel <- sub("^rep_mobile_subtype/", "", tokens[grepl("^rep_mobile_subtype/", tokens)])
  tt_sel <- sub("^rep_tandem_subtype/", "", tokens[grepl("^rep_tandem_subtype/", tokens)])
  mx_children <- sub("^rep_mixed_subtype/", "", tokens[grepl("^rep_mixed_subtype/", tokens)])
  mx_mobile_sel <- mx_children[mx_children %in% mobile_subtypes]
  mx_tandem_sel <- mx_children[mx_children %in% tandem_subtypes]

  # Defaults: select all if subtype flag present but no specific child tokens
  if (present$rep_mobile_subtype && length(mm_sel) == 0) mm_sel <- mobile_subtypes
  if (present$rep_tandem_subtype && length(tt_sel) == 0) tt_sel <- tandem_subtypes
  if (present$rep_mixed_subtype && length(mx_mobile_sel) == 0 && length(mx_tandem_sel) == 0) {
    mx_mobile_sel <- mobile_subtypes
    mx_tandem_sel <- tandem_subtypes
  }

  # Apply parents (explicit TRUE/FALSE so state matches SV_Classification exactly)
  updateCheckboxInput(session, "nonrep",             value = present$nonrep)
  updateCheckboxInput(session, "rep_mobile",         value = present$rep_mobile)
  updateCheckboxInput(session, "rep_tandem",         value = present$rep_tandem)
  updateCheckboxInput(session, "rep_mixed",          value = present$rep_mixed)
  updateCheckboxInput(session, "rep_mobile_subtype", value = present$rep_mobile_subtype)
  updateCheckboxInput(session, "rep_tandem_subtype", value = present$rep_tandem_subtype)
  updateCheckboxInput(session, "rep_mixed_subtype",  value = present$rep_mixed_subtype)

  # Apply children per context (clear when parent flag absent)
  updateCheckboxGroupInput(session, "class_mobile_mobile",
                           selected = if (present$rep_mobile_subtype) unique(mm_sel) else character(0))
  updateCheckboxGroupInput(session, "class_tandem_tandem",
                           selected = if (present$rep_tandem_subtype) unique(tt_sel) else character(0))
  updateCheckboxGroupInput(session, "class_mobile_mixed",
                           selected = if (present$rep_mixed_subtype) unique(mx_mobile_sel) else character(0))
  updateCheckboxGroupInput(session, "class_tandem_mixed",
                           selected = if (present$rep_mixed_subtype) unique(mx_tandem_sel) else character(0))
}

update_filters_params <- function(search_params, session) {
  # print (search_params)
  # Mapping table: for each base param, SNV and SV update info, and shared update info for 3 params
  param_mapping <- list(
    "Annotation" = list(
      snv = list(func = updateCheckboxGroupInput, id = "conseq_checkboxes", selected = TRUE, split = TRUE),
      sv  = list(func = updateCheckboxGroupInput, id = "sv_conseq_checkboxes", selected = TRUE, split = TRUE)
    ),
    "Pathogenicity" = list(
      snv = list(func = updateCheckboxGroupInput, id = "clinvar_checkboxes", selected = TRUE, split = TRUE),
      sv = list(func = updateCheckboxGroupInput, id = "sv_clinvar_checkboxes", selected = TRUE, split = TRUE)
    ),
    "SpliceAI score" = list(
      snv = list(func = updateNumericInput, id = "spliceai_score", value = TRUE, as_numeric = TRUE)
    ),
    "REVEL" = list(
      snv = list(func = updateNumericInput, id = "revel", value = TRUE, as_numeric = TRUE)
    ),
    "AlphaMissense" = list(
      snv = list(func = updateNumericInput, id = "alpha_missense", value = TRUE, as_numeric = TRUE)
    ),
    "SIFT" = list(
      snv = list(func = updateSelectInput, id = "sift", selected = TRUE)
    ),
    "PolyPhen" = list(
      snv = list(func = updateSelectInput, id = "polyphen", selected = TRUE)
    ),
    "gnomADv4 AF" = list(
      snv = list(func = updateSelectInput, id = "af", selected = TRUE, as_numeric = FALSE),
      sv  = list(func = updateSelectInput, id = "sv_af", selected = TRUE, as_numeric = FALSE)
    ),
    "gnomADv4_AF" = list(
      snv = list(func = updateSelectInput, id = "af", selected = TRUE, as_numeric = FALSE),
      sv  = list(func = updateSelectInput, id = "sv_af", selected = TRUE, as_numeric = FALSE)
    ),
    "Use_AF" = list(
      snv = list(func = updateCheckboxInput, id = "use_af", value = TRUE, as_logical = TRUE)
    ),
    "SV type" = list(
      sv  = list(func = updateCheckboxGroupInput, id = "sv_features_checkboxes", selected = TRUE, split = TRUE)
    ),
    "SV_type" = list(
      sv  = list(func = updateCheckboxGroupInput, id = "sv_features_checkboxes", selected = TRUE, split = TRUE)
    ),
    "Min SV Length" = list(
      sv  = list(func = updateNumericInput, id = "min_svlen", value = TRUE, as_numeric = TRUE)
    ),
    "Min_SV_Length" = list(
      sv  = list(func = updateNumericInput, id = "min_svlen", value = TRUE, as_numeric = TRUE)
    ),
    "Max SV Length" = list(
      sv  = list(func = updateNumericInput, id = "max_svlen", value = TRUE, as_numeric = TRUE)
    ),
    "Max_SV_Length" = list(
      sv  = list(func = updateNumericInput, id = "max_svlen", value = TRUE, as_numeric = TRUE)
    ),
    "Affected only" = list(
      snv = list(func = updateCheckboxInput, id = "affected_switch", value = TRUE, as_logical = TRUE),
      sv  = list(func = updateCheckboxInput, id = "sv_affected_switch", value = TRUE, as_logical = TRUE)
    ),
    "Affected_only" = list(
      snv = list(func = updateCheckboxInput, id = "affected_switch", value = TRUE, as_logical = TRUE),
      sv  = list(func = updateCheckboxInput, id = "sv_affected_switch", value = TRUE, as_logical = TRUE)
    ),
    "Allele balance" = list(
      snv = list(func = updateSliderInput, id = "allele_balance", value = TRUE, as_numeric = TRUE),
      sv  = list(func = updateSliderInput, id = "sv_allele_balance", value = TRUE, as_numeric = TRUE)
    ),
    "Allele_balance" = list(
      snv = list(func = updateSliderInput, id = "allele_balance", value = TRUE, as_numeric = TRUE),
      sv  = list(func = updateSliderInput, id = "sv_allele_balance", value = TRUE, as_numeric = TRUE)
    ),
    "Genotype quality" = list(
      snv = list(func = updateSliderInput, id = "genotype_quality", value = TRUE, as_numeric = TRUE),
      sv  = list(func = updateSliderInput, id = "sv_genotype_quality", value = TRUE, as_numeric = TRUE)
    ),
    "Genotype_quality" = list(
      snv = list(func = updateSliderInput, id = "genotype_quality", value = TRUE, as_numeric = TRUE),
      sv  = list(func = updateSliderInput, id = "sv_genotype_quality", value = TRUE, as_numeric = TRUE)
    ),
    "Inheritance" = list(
      shared = list(func = updateRadioButtons, id = "inher", selected = TRUE)
    ),
    "PanelApp_Genes" = list(
      shared = list(func = updateSelectInput, id = "panelapp", selected = TRUE, split = TRUE)
    ),
    # "HPO_Terms" = list(
    #   shared = list(func = updateCheckboxGroupInput, id = "phenotype", selected = TRUE, split = TRUE)
    # ),
    "Custom_Genes" = list(
      shared = list(func = updateTextInput, id = "custom_genes", value = TRUE)
    ),
    "Treat_Negative" = list(
      shared = list(func = updateCheckboxInput, id = "treat_negative", value = TRUE, as_logical = TRUE)
    ),
    "Substract_PanelApp_Gene_Lists" = list(
      shared = list(func = updateSelectInput, id = "substract_panelapp_gene_lists", selected = TRUE, split = TRUE)
    ),
    "Substract_PanelApp_Genes" = list(
      shared = list(func = updateTextInput, id = "substract_panelapp_genes", value = TRUE)
    ),
    "Inheritance_PanelApp_Gene" = list(
      shared = list(func = updateCheckboxInput, id = "inheritance_panelapp_gene", value = TRUE, as_logical = TRUE)
    ),
    "SVlog_Annotation" = list(
      sv  = list(func = updateCheckboxGroupInput, id = "svlog_conseq_checkboxes", selected = TRUE, split = TRUE)
    ),
    "SVlog_min_recip_overlap" = list(
      sv  = list(func = updateSliderInput, id = "sv_reciprocal_overlap_fraction", value = TRUE, as_numeric = TRUE)
    ),
    "SVlog_max_break_distance" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_breakpoint_distance", value = TRUE, as_numeric = FALSE)
    ),
    "SVlog_max_abs_dlen" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_delta_length", value = TRUE, as_numeric = FALSE)
    ),
    "SVlog_internal_max_carriers" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_carriers_internal", value = TRUE, as_numeric = FALSE)
    ),
    "SVlog_internal_max_families" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_families", value = TRUE, as_numeric = FALSE)
    ),
    "SVlog_1000G_max_carriers" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_carriers_1000", value = TRUE, as_numeric = FALSE)
    ),
    "SVlog_Advanced_Keeping" = list(
      sv  = list(func = updateSelectInput, id = "svlog_keeping", selected = TRUE, as_numeric = FALSE)
    ),
    "SVlog_Advanced_Filtering_out" = list(
      sv  = list(func = updateSelectInput, id = "svlog_filtering", selected = TRUE, as_numeric = FALSE)
    ),
    "Keeping" = list(
      sv  = list(func = updateCheckboxGroupInput, id = "svlog_keeping_checkboxes", selected = TRUE, split = TRUE)
    ),
    "Filtering_out" = list(
      sv  = list(func = updateCheckboxGroupInput, id = "svlog_filtering_checkboxes", selected = TRUE, split = TRUE)
    ),
    "intronic_splice_max_dist" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_distance_to_splice_site", value = TRUE, as_numeric = FALSE)
    ),
    "intronic_min_len_intron_ratio" = list(
      sv  = list(func = updateNumericInput, id = "sv_min_ratio_sv_length_intron_length", value = TRUE, as_numeric = FALSE)
    ),
    "tad_max_dist" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_distance_to_nearest_tad_boundary", value = TRUE, as_numeric = FALSE)
    ),
    "enhancer_max_dist" = list(
      sv  = list(func = updateNumericInput, id = "sv_max_distance_to_nearest_enhancer", value = TRUE, as_numeric = FALSE)
    ),
    "intra_tad_only" = list(
      sv  = list(func = updateCheckboxInput, id = "sv_intra_tad_boundary", value = TRUE, as_logical = TRUE)
    ),
    "inter_tad_only" = list(
      sv  = list(func = updateCheckboxInput, id = "sv_inter_tad_boundary", value = TRUE, as_logical = TRUE)
    )
  )

  # If search_params is NULL or empty, do nothing
  if (is.null(search_params) || length(search_params) == 0) return(invisible())

  for (param in names(search_params)) {
    value <- search_params[[param]]
    # Determine type by prefix
    # Special-case: SV classification driven by a single key carrying parent and child selections
    if (startsWith(param, "SV_")) {
      base_param <- substring(param, 4)

      if (identical(base_param, "Classification")) {
        update_sv_classification(session, value)
        next
      }

      # Default SV handling via mapping table
      mapping_entry <- param_mapping[[base_param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$sv)) next
      update_info <- mapping_entry$sv

    } else if (startsWith(param, "SNV_")) {
      base_param <- substring(param, 5)
      mapping_entry <- param_mapping[[base_param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$snv)) next
      update_info <- mapping_entry$snv
    } else if (startsWith(param, "SV_")) {
      base_param <- substring(param, 4)
      mapping_entry <- param_mapping[[base_param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$sv)) next
      update_info <- mapping_entry$sv
    } else {
      # Shared params: extend allowlist to include the new three
      if (!param %in% c("Inheritance", "PanelApp_Genes", "Custom_Genes", "Treat_Negative", "Substract_PanelApp_Gene_Lists", "Substract_PanelApp_Genes", "Inheritance_PanelApp_Gene")) next
      mapping_entry <- param_mapping[[param]]
      if (is.null(mapping_entry) || is.null(mapping_entry$shared)) next
      update_info <- mapping_entry$shared
    }

    # Conversion logic
    if (!is.null(update_info$as_numeric)) value <- as.numeric(value)
    if (!is.null(update_info$as_logical)) value <- as.logical(value)
    if (!is.null(update_info$split)) value <- unlist(strsplit(value, ";"))

    # Apply updates explicitly based on argument type
    if (!is.null(update_info$selected)) {
      update_info$func(session, update_info$id, selected = value)
    } else if (!is.null(update_info$value)) {
      update_info$func(session, update_info$id, value = value)
    }
  }
}

update_custom_alleles <- function(search_params, session) {
  # --- Handle Custom inheritance allele counts ---
  if (!is.null(search_params[["Inheritance"]]) && search_params[["Inheritance"]] == "Custom" && !is.null(search_params[["Custom_Allele_Counts"]])) {
    # Example string:
    # "LRS00112-00-RF-01:0-1;LRS00112-01-RF-01:0-1;LRS00112-03-RF-01:0;LRS00112-04-RF-01:1"
    allele_str <- search_params[["Custom_Allele_Counts"]]
    # Split into per-sample pairs
    allele_pairs <- unlist(strsplit(allele_str, ";"))
    # Loop through each and update the corresponding radio input
    for (pair in allele_pairs) {
      kv <- unlist(strsplit(pair, ":", fixed = TRUE))
      if (length(kv) >= 1) {
        sid <- kv[1]
        val <- if (length(kv) == 2) kv[2] else ""
        # log_info(sprintf("[update_filters_params] Updating custom allele count for sample %s to %s", sid, val))
        input_id <- paste0("allele_", sid)
        if (!is.null(session$input[[input_id]])) {
          updateRadioButtons(session, inputId = input_id, selected = val)
        } else {
          log_error(sprintf("[update_filters_params] Skipped %s - not yet rendered or sample IDs are different\n", input_id))
        }
      }
    }
    log_info(sprintf("[update_filters_params] Updated custom allele counts for %d samples", length(allele_pairs)))
  }
}

save_session_data <- function(input, session_name, sessions_dir, snvs_data, svs_data, phenos, samples, sticky) {
  session_dir <- sprintf("%s", sessions_dir)
  create_safe_dir(session_dir, sticky = sticky)

  session_dir <- sprintf("%s/%s", sessions_dir, session_name)
  log_info(sprintf("[filtServer] Saving session: %s", session_name))

  session_dir <- create_safe_dir(session_dir, sticky = FALSE)

  # Capture current filter states
  filters_dt <- capture_filters(input, phenos, samples, flag_save_samples=TRUE, flag_save_hpo_panelapp=TRUE, flag_save_presaved_filter=TRUE)

  # Save filter tables try catch
  tryCatch({
    fwrite(filters_dt, file = file.path(session_dir, "filters.tsv"), sep = "\t", quote = FALSE, col.names = FALSE)
  }, error = function(e) {
    log_error(sprintf("[filtServer] Failed to save filters: %s", e$message))
    showNotification(sprintf("Failed to save filters: %s", e$message), type = "error")
    # return 
    return()
  })

  # Save flagged rows if any
  save_flagged_rows <- function(data, data_type) {
    if (!is.null(data) && all(c("PRIORITY", "NOTES") %in% colnames(data))) {
      tryCatch({
        fwrite(data[PRIORITY != 0 | NOTES != "", .(ID, PRIORITY, NOTES)], file = file.path(session_dir, sprintf("flagged_rows_%s.tsv", data_type)), sep = "\t", quote = FALSE, col.names = TRUE)
      }, error = function(e) {
        log_error(sprintf("[filtServer] Failed to save flagged rows for %s: %s", data_type, e$message))
        showNotification(sprintf("Failed to save flagged rows for %s: %s", data_type, e$message), type = "error")
        return(NULL)
      })
      cat(sprintf("Flagged rows for %s saved to %s\n", data_type, file.path(session_dir, sprintf("flagged_rows_%s.tsv", data_type))))
    } else {
      cat(sprintf("No PRIORITY or NOTES column in %s data. Skipping flagged rows save.\n", data_type))
    }
  }
  save_flagged_rows(snvs_data, "snvs_data")
  save_flagged_rows(svs_data, "svs_data")

  return(TRUE)
}

save_filters <- function(input, phenos, file_path, samples, sticky) {
  filter_save_name <- input$filters_save_name
  filters_dt <- capture_filters(input, phenos, samples, flag_save_samples=TRUE, flag_save_hpo_panelapp=TRUE, flag_save_presaved_filter=FALSE)

  file_path <- create_safe_dir(file_path, sticky = sticky)

  # file_name = file_path + filter_save_name + .tsv
  file_name <- file.path(file_path, paste0(filter_save_name, ".tsv"))
  # add Label as the first row
  # try fwrite and catch errors
  tryCatch({
    fwrite(filters_dt, file = file_name, sep = "\t", quote = FALSE, col.names = FALSE)
  }, error = function(e) {
    log_error(sprintf("[filtServer] Failed to save filters: %s", e$message))
    showNotification(sprintf("Failed to save filters: %s", e$message), type = "error")
    return(NULL)
  })

}

delete_filters <- function(filter_name, file_path) {
  file_name <- file.path(file_path, paste0(filter_name, ".tsv"))
  if (file.exists(file_name)) {
    file.remove(file_name)
    message(sprintf("Deleted filter file: %s", file_name))
    return(TRUE)
  } else {
    message(sprintf("Filter file does not exist: %s", file_name))
    return(FALSE)
  }
}

update_flagged_rows <- function(original_dt, flagged_rows_file) {
  stopifnot(is.data.table(original_dt))  # must be data.table
  # Check required columns
  required_cols <- c("ID", "PRIORITY", "NOTES")
  missing_cols <- setdiff(required_cols, names(original_dt))
  if (length(missing_cols) > 0) {
    stop(sprintf("Original data is missing required columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Reset PRIORITY and NOTES in the original data.table
  original_dt[, PRIORITY := 0L]
  original_dt[, NOTES := NA_character_]
  original_dt[, PRIORITYFlag := as.logical(NA)]
  
  # Return early if no file
  if (!file.exists(flagged_rows_file)) {
    message("Flagged rows file does not exist. Returning original data.")
    return(copy(original_dt))
  }
  
  # Read flagged rows as data.table
  flagged_rows_dt <- fread(flagged_rows_file, sep = "\t", header = TRUE, na.strings = NULL, nThread = 8)
  flagged_rows_dt <- setDT(flagged_rows_dt)
  if (nrow(flagged_rows_dt) == 0) {
    message("Flagged rows file is empty. Returning original data.")
    return(copy(original_dt))
  }
  log_info(sprintf("Read %d flagged rows from file: %s", nrow(flagged_rows_dt), flagged_rows_file))
  
  # Type consistency
  if (!is.numeric(flagged_rows_dt$PRIORITY)) {
    flagged_rows_dt[, PRIORITY := as.numeric(PRIORITY)]
  }
  if (!is.character(flagged_rows_dt$NOTES)) {
    flagged_rows_dt[, NOTES := as.character(NOTES)]
    flagged_rows_dt[is.na(NOTES), NOTES := ""]
  }
  
  # Update only overlapping IDs and merge to retain only the original_dts rows
  # original_dt <- merge(
  #   original_dt[, !c("PRIORITY", "NOTES"), with = FALSE],
  #   flagged_rows_dt[, .(ID, PRIORITY, NOTES)],
  #   by = "ID",
  #   all.x = TRUE
  # )
  # Update only overlapping IDs and keep PRIORITY/NOTES for others
  original_dt[flagged_rows_dt, 
              `:=`(PRIORITY = i.PRIORITY, NOTES = i.NOTES), 
              on = "ID"]
  
  original_dt[, PRIORITYFlag := fifelse(
    is.na(PRIORITY) | PRIORITY == 0, NA,
    fifelse(PRIORITY > 0, TRUE, FALSE)
  )]

  log_info(sprintf("Updated flagged rows from file: %s", flagged_rows_file))
  return(original_dt)
}

load_session_data <- function(input, session_name, sessions_dir, snvs_data, svs_data) {
  session_to_load <- sprintf("%s/%s", sessions_dir, session_name)
  log_info(sprintf("[filtServer] Loading session: %s", session_to_load))
  if (! dir.exists(session_to_load)) {
    showNotification("Session directory does not exist", type = "error")
    return(NULL)
  }
  filters_file <- file.path(session_to_load, "filters.tsv")
  if (!file.exists(filters_file)) {
    showNotification("filters.tsv file does not exist in session directory", type = "error")
    return(NULL)
  }
  snv_flagged_rows_file <- file.path(session_to_load, "flagged_rows_snvs_data.tsv")
  sv_flagged_rows_file <- file.path(session_to_load, "flagged_rows_svs_data.tsv")
  snv_flagged_rows_exists <- file.exists(snv_flagged_rows_file)
  sv_flagged_rows_exists <- file.exists(sv_flagged_rows_file)

  filters_df <- read.delim(filters_file, header = FALSE, col.names = c("Key", "Value"), sep = "\t", quote = "", stringsAsFactors = FALSE)
  filters_df <- setNames(as.list(filters_df$Value), filters_df$Key)

  log_info("[filtServer][filter_dataset] Updating flagged rows for SNV data")
  snvs_data <- update_flagged_rows(snvs_data, snv_flagged_rows_file)
  log_info("[filtServer][filter_dataset] Updating flagged rows for SV data")
  svs_data <- update_flagged_rows(svs_data, sv_flagged_rows_file)
  return(list(filters=filters_df, snvs_data=snvs_data, svs_data=svs_data))

}

delete_session_data <- function(session_name, sessions_dir) {
  session_to_delete <- sprintf("%s/%s", sessions_dir, session_name)
  log_info(sprintf("[filtServer] Deleting session: %s", session_to_delete))
  if (dir.exists(session_to_delete)) {
    unlink(session_to_delete, recursive = TRUE)
    log_info(sprintf("Deleted session directory: %s", session_to_delete))
    return(TRUE)
  } else {
    log_info(sprintf("Session directory does not exist: %s", session_to_delete))
    return(FALSE)
  }
}

clear_input_fields <- function(session) {
  updateSelectizeInput(session, "pre_saved_filters", selected = character(0))
  updateTextInput(session, "filters_save_name", value = "")
  updateSelectizeInput(session, "delete_pre_saved_filters", selected = character(0))
  updateSelectizeInput(session, "available_sessions", selected = character(0))
  updateTextInput(session, "session_name", value = "")
  updateSelectizeInput(session, "delete_sessions", selected = character(0))
}