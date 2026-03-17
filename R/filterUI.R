# ---- UI building functions ----
# Annotation UI
AnnotUI <- function(ns, id_prefix = "snv") {
  is_snv <- (id_prefix == "snv")
  if (id_prefix != "snv") {
    id_prefix <- paste0(id_prefix, "_")
  } else {
    id_prefix <- ""
  }
  # consequence_select_opts <- c("None", "High impact", "Moderate to high impact")
  consequence_select_opts <- c("None", "HIGH", "MODERATE", "LOW", "MODIFIER")
  # vep_conseq_opts <- c(
  #   "Stop gained", "Start lost", "Stop lost", "Splice variant",
  #   "Frameshift variant", "Missense variant", "In-frame variant",
  #   "Synonymous variant", "5'UTR variant", "3'UTR variant",
  #   "Intron variant", "Intergenic variant", "Regulatory variant", "Other"
  # )
  vep_conseq_opts <- c("frameshift_variant", "transcript_ablation", "transcript_amplification", "feature_elongation", "feature_truncation", "splice_acceptor_variant", "splice_donor_variant", "start_lost", "stop_gained", "stop_lost", 
    "inframe_insertion", "inframe_deletion", "missense_variant", "protein_altering_variant", 
    "incomplete_terminal_codon_variant", "start_retained_variant", "stop_retained_variant", "splice_donor_5th_base_variant", "splice_region_variant", "splice_donor_region_variant", "splice_polypyrimidine_tract_variant", "synonymous_variant", 
    "3_prime_UTR_variant", "5_prime_UTR_variant", "intergenic_variant", "intron_variant", "coding_sequence_variant", "mature_miRNA_variant", "non_coding_transcript_exon_variant", "NMD_transcript_variant", "non_coding_transcript_variant", "coding_transcript_variant", "upstream_gene_variant", "downstream_gene_variant", "TFBS_ablation", "TFBS_amplification", "TF_binding_site_variant", "regulatory_region_ablation", "sequence_variant", "regulatory_region_amplification", "regulatory_region_variant")
   ui_elements <- list(
    tags$style(HTML("
      .always-show-scrollbar {
        overflow-y: scroll !important;
      }
      /* WebKit browsers (Chrome, Safari, Edge) */
      .always-show-scrollbar::-webkit-scrollbar {
        -webkit-appearance: none;
        width: 8px;
      }
      .always-show-scrollbar::-webkit-scrollbar-thumb {
        background-color: rgba(0, 0, 0, 0.4);
        border-radius: 4px;
      }
      .always-show-scrollbar::-webkit-scrollbar-track {
        background-color: rgba(0, 0, 0, 0.1);
        border-radius: 4px;
      }
    ")),
    h4("Annotation"),
    selectInput(ns(paste0(id_prefix, "consequence")), "VEP Consequence:", choices = consequence_select_opts, selected = "", multiple = TRUE, ),
    div(
      class = "always-show-scrollbar",
      style = "display: flex; flex-direction: column; gap: 5px; max-height: 400px; width: 80%;",
      do.call(checkboxGroupInput, list(ns(paste0(id_prefix, "conseq_checkboxes")), NULL, choices = vep_conseq_opts))
    ),
    # breakline
    br()
    
  )
  if (is_snv) {
    ui_elements <- append(
      ui_elements,
      list(numericInput(ns("spliceai_score"), "SpliceAI score:", 0, 0, 1, 0.05))
    )
  }
  tagList(ui_elements)
}

# Pathogenicity UI
PathoUI <- function(ns, id_prefix = "snv") {
  if (id_prefix != "snv") {
    id_prefix <- paste0(id_prefix, "_")
  } else {
    id_prefix <- ""
  }
  pathogenicity_clinvar_select_opts <- c("None", "Pathogenic/Likely pathogenic", "Not benign")
  pathogenicity_clinvar_opts <- c(
    "Pathogenic", "Likely pathogenic", "VUS", "Conflicting",
    "Benign", "Likely benign", "Not available", "Other"
  )
    tagList(
    h4("Pathogenicity"),
    selectInput(ns(paste0(id_prefix, "pathogenicity")), "Clinvar:", choices = pathogenicity_clinvar_select_opts, selected = "None"),
    checkboxGroupInput(ns(paste0(id_prefix, "clinvar_checkboxes")), NULL, choices = pathogenicity_clinvar_opts)
  )
}

# In silico UI
snvInsilicoUI <- function(ns) {
  snv_insilico_sift_opts <- c("", "Deleterious", "Tolerated")
  snv_insilico_polyphen_opts <- c("", "Probably damaging", "Possibly damaging", "Benign")
  tagList(
    h4("In silico predictions"),
    numericInput(ns("revel"), "Revel score:", 0, 0, 1, 0.05),
    numericInput(ns("alpha_missense"), "AlphaMissense score:", 0, 0, 1, 0.05),
    selectInput(ns("sift"), "Sift:", choices = snv_insilico_sift_opts, selected = "", selectize = TRUE),
    selectInput(ns("polyphen"), "Polyphen:", choices = snv_insilico_polyphen_opts, selected = "", selectize = TRUE)
  )
}

# Quality UI
QualityUI <- function(ns, id_prefix = "snv") {
  if (id_prefix != "snv") {
    id_prefix <- paste0(id_prefix, "_")
  } else {
    id_prefix <- ""
  }
  tagList(
    h4("Per-sample Call quality filters"),
    sliderInput(ns(paste0(id_prefix, "genotype_quality")), "Genotype quality:", 0, 100, 0, ticks = FALSE),
    sliderInput(ns(paste0(id_prefix, "allele_balance")), "Minimum Allele fraction:", 0, 1, 0, ticks = FALSE),
    numericInput(ns(paste0(id_prefix, "min_read_depth")), "Minimum read depth:", value = NULL, min = 0, step = 1),
    checkboxInput(ns(paste0(id_prefix, "affected_switch")), "Consider 'affected' samples only", value = FALSE)
  )
}

# Frequency UI
FreqUI <- function(ns, id_prefix = "snv") {
  is_snv <- (id_prefix == "snv")
  if (id_prefix != "snv") {
    id_prefix <- paste0(id_prefix, "_")
  } else {
    id_prefix <- ""
  }
  filter_freqs <- c(
    0, seq(0.0001, 0.0005, by = 0.0004),
    0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 1
  )
  tagList(
    h4("Frequency"),
    selectInput(ns(paste0(id_prefix, "af")), "gnomADv4 AF:", choices = filter_freqs, selected = 1),
    if (is_snv) {
      checkboxInput(ns(paste0(id_prefix, "use_af")), "Use for clinvar and spliceAI override filters", value = FALSE)
    }
  )
}

# Structural Variant Features UI
svFeatsUI <- function(ns) {
  sv_type_opts <- c("Insertion", "Deletion", "Duplication", "Inversion", "Translocation")
  tagList(
    h4("SV properties"),
    checkboxGroupInput(ns("sv_features_checkboxes"), "SV type", choices = sv_type_opts),
    numericInput(ns("min_svlen"), "Min SV Length:", value = NULL, min = 0, step = 1),
    numericInput(ns("max_svlen"), "Max SV Length:", value = NULL, min = 0, step = 1)
  )
}

svscannerUI <- function(ns) {
  # Child subtype sets (unchecked by default)
  choices_class <- c(
    "Non-repetitive" = "non_repetitive",
    "LINE"           = "line",
    "SINE"           = "sine",
    "Retroposon"     = "retroposon",
    "DNA transposon" = "dna",
    "LTR"            = "ltr",
    "STR"  = "str",
    "VNTR" = "vntr",
    "TR"   = "tr",
    "HOMO" = "homo",
    "Other" = "other"
  )
  choices_reciprocal <- c(
    "Full"      = "full",
    "Partial"   = "partial"
  )
  tagList(
    h4("SVscanner Classifications"),
    checkboxGroupInput(ns("svscanner_class"), "Class:", choices = choices_class),
    checkboxGroupInput(ns("svscanner_reciprocal"), "Reciprocal:", choices = choices_reciprocal)
  )
  
}

InherUI <- function(ns) {
  inher_opts <- c(
    "None" = "",
    "Homozygous Recessive" = "Homozygous Recessive",
    "X-Linked Recessive" = "X-Linked Recessive",
    "Compound Heterozygous" = "Compound Heterozygous",
    "Dominant/De Novo" = "Dominant/De Novo",
    "Custom" = "Custom"
  )
  tagList(
    # selectInput(ns("inher"), "", inher_opts, selected = ""),
    radioButtons(
      ns("inher"),
      label = NULL,
      choices = inher_opts,
      selected = ""
    ),
    uiOutput(ns("allele_ui"))
  )
}

# SNVs and Indels UI
snvOptsUI <- function(ns) {
  fluidRow(
    column(1),
    column(2, AnnotUI(ns, "snv")),
    column(2, QualityUI(ns, "snv")),
    column(2, FreqUI(ns, "snv")),
    column(2, PathoUI(ns, "snv")),
    column(2, snvInsilicoUI(ns)),
    column(1),
  )
}

PopulationUI_0 <- function(ns) {
  choices <- c("todo", "todo2")  # Placeholder for actual population choices
  choices_similarity_criteria <- c("High", "Moderate", "Low", "None")
  tagList(
    h4("Population Evidence"),
    selectInput(ns("sv_population_similarity_criteria"), "Similarity/Matching criteria:", choices = choices_similarity_criteria, selected = "None"),
    sliderInput(ns("sv_reciprocal_overlap_fraction"), "Reciprocal overlap fraction - DEL,DUP,INV:", 0, 1, 0, ticks = FALSE),
    numericInput(ns("sv_max_breakpoint_distance"), "Max position distance (bp) - INS:", value = NULL, min = 0, step = 1),
    numericInput(ns("sv_max_delta_length"), "Max |delta len| (bp) - INS:", value = NULL, min = 0, step = 1)
  )
}
PopulationUI_1 <- function(ns) {
  choices <- c("todo", "todo2")  # Placeholder for actual population choices
  filter_freqs <- c(
    0, seq(0.0001, 0.0005, by = 0.0004),
    0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 1
  )
  tagList(
    selectInput(ns("sv_af"), "gnomADv4 AF:", choices = filter_freqs, selected = 1),
    strong("ONT 1000 Genomes"),
    h5("Max carriers (HOM + HET)"),
    numericInput(ns("sv_max_carriers_1000"), label = NULL, value = NULL, min = 0, step = 1),
    strong("Internal Cohort"),
    h5("Max carriers (HOM + HET)"),
    numericInput(ns("sv_max_carriers_internal"), label = NULL, value = NULL, min = 0, step = 1),
    h5("Max families:"),
    numericInput(ns("sv_max_families"), label = NULL, value = NULL, min = 0, step = 1)
  )
}

SVlog_conseqUI <- function(ns) {
  annotation_select_opts <- c("None", "High impact", "Moderate to high impact")
  svlog_conseq_opts <- c("affects_cds", "affects_only_promoter", "affects_tad_boundary", "affects_utr", "intronic", "intergenic", "splice_altering", "other")
  ui_elements <- list(
    h4("SVlog annotation"),
    selectInput(ns("svlog_consequence"), "SVlog Consequence:", choices = annotation_select_opts, selected = "None"),
    checkboxGroupInput(ns("svlog_conseq_checkboxes"), NULL, choices = svlog_conseq_opts)
  )
  tagList(ui_elements)
}

SVlogUI <- function(ns) {
  choices_keeping_tier <- c(
    "0" = "0",
    "1" = "1",
    "2" = "2",
    "3" = "3"
  )
  choices_filtering_tier <- c(
    "1" = "1",
    "2" = "2",
    "3" = "3"
  )
  choices_keeping <- c("prioritise_lof_any", "prioritise_lof_high", "prioritise_lof_mendeliome", "prioritise_lof_mod", "prioritise_str_novel")
  choices_filtering <- c("common_1kg", "common_gnomad", "common_internal", "common_intergenic")
  tagList(
    fluidRow(
      column(6, checkboxGroupInput(ns("svlog_keeping"), "Keeping:", choices = choices_keeping_tier)),
      column(6, checkboxGroupInput(ns("svlog_filtering"), "Filtering out:", choices = choices_filtering_tier))
    ),
    checkboxGroupInput(ns("svlog_keeping_checkboxes"), "Keeping criteria:", choices = choices_keeping),
    checkboxGroupInput(ns("svlog_filtering_checkboxes"), "Filtering out criteria:", choices = choices_filtering)
  )
}

GenomicContextUI <- function(ns) {
  tagList(
    h4("Genomic Context"),
    numericInput(ns("sv_max_distance_to_splice_site"), "Max distance to splice site (bp) - intronic:", value = NULL, min = 0, step = 1),
    numericInput(ns("sv_min_ratio_sv_length_intron_length"), "Min ratio SV length/intron length - intronic:", value = NULL, min = 0, step = 0.01),
    numericInput(ns("sv_max_distance_to_nearest_tad_boundary"), "Max distance to nearest TAD boundary (bp):", value = NULL, min = 0, step = 1),
    checkboxInput(ns("sv_intra_tad_boundary"), "Intra TAD boundary", value = FALSE),
    checkboxInput(ns("sv_inter_tad_boundary"), "Inter TAD boundary", value = FALSE),
    numericInput(ns("sv_max_distance_to_nearest_enhancer"), "Max distance to nearest enhancer (bp):", value = NULL, min = 0, step = 1)
  )
}

svOptsUI <- function(ns) {
  tagList(
    fluidRow(
      column(3, PopulationUI_0(ns), PopulationUI_1(ns)),
      column(2, PathoUI(ns, "sv"), AnnotUI(ns, "sv")),
      column(3, svFeatsUI(ns), GenomicContextUI(ns), QualityUI(ns, "sv")),
      column(2, SVlog_conseqUI(ns), SVlogUI(ns)),
      column(2, svscannerUI(ns))
    )
  )
}

panelBox <- function(id, label, color = "black", width = "100%", height = "30vh") {
  boxStyle <- paste("overflow-y: auto; max-height: calc(100% - 30px); color:", color, ";")
  panelStyle <- paste("width:", width, "; height:", height, "; padding: 5px;")
  if (is.null(label)) {
    labelUI <- NULL
  } else {
    labelStyle <- "font-weight: bold; margin-bottom: 5px;"
    labelUI <- div(style = labelStyle, label)
  }
  wellPanel(labelUI, div(style = boxStyle, textOutput(id)), style = panelStyle)
}

panelAppOptsUI <- function(ns) {
  tagList(
    selectizeInput(ns("panelapp"), "PanelApp Gene list:", choices = character(0), selected = "", multiple = TRUE, options = list(plugins = c("drag_drop")), width = "100%"),
    selectizeInput(ns("substract_panelapp_gene_lists"), "Subtract gene lists from PanelApp Gene list:", choices = character(0), selected = "", multiple = TRUE, options = list(plugins = c("drag_drop")), width = "100%"),
    textInput(ns("substract_panelapp_genes"), "Subtract genes from PanelApp Gene list (semi-colon separated):", value = "", width = "100%"),
    br(),
    fluidRow(
      column(3, panelBox(ns("unclassified_genes"), "Unclassified genes:", "gray")),
      column(3, panelBox(ns("green_genes"), "Green genes:", "green")),
      column(3, panelBox(ns("red_genes"), "Red genes:", "red")),
    column(3, panelBox(ns("amber_genes"), "Amber genes:", "#FFBF00"))
    ),
    fluidRow(
      column(12, textInput(ns("custom_genes"), "Custom genes (semi-colon separated):", value = "", width = "100%"))
    ),
    fluidRow(
      column(6, checkboxInput(ns("treat_negative"), "Consider PanelApp genes and Custom genes as negative", value = FALSE, width = "100%")),
      column(6, checkboxInput(ns("inheritance_panelapp_gene"), "Filter panel app gene lists by mode of inheritance", value = FALSE, width = "100%"))
    )
  )
}

phenotypeOptsUI <- function(ns) {
  tagList(
    fluidRow(
      column(4, 
      br(),
      textInput(ns("phenotype_var"), "HPO term:", ""),
      actionButton(ns("phenotype_add"), NULL, icon("plus")),
      actionButton(ns("phenotype_remove"), NULL, icon("minus")),
      br(),
      panelBox(ns("phenotype"), NULL, height = "10vh")
      )
    )
  )
}

header_UI <- function(ns) {
  tagList(
    fluidRow(
      column(3, 
        selectizeInput(ns("pre_saved_filters"), "Pre-saved filters found in working dir:", choices = NULL, selected=NULL),
        # selectizeInput(ns("sv_pre_saved_search"), "SVs pre-saved search:", choices = NULL, selected=NULL),
        div(style = "display: flex; align-items: center;",
          textInput(ns("filters_save_name"), "Name filters:", value = ""),
          actionButton(ns("btn_save_filters"), "save", class = "btn-primary",style = "margin-left: 25px; margin-top: 10px;")
        ),
        checkboxInput(ns("save_panelapp_hpo"), "Save PanelApp and HPO Options", value = TRUE),
        div(style = "display: flex; align-items: center;",
          selectizeInput(ns("delete_pre_saved_filters"), "Delete filters:", choices = NULL, selected=NULL, options = list(create = FALSE)),
          actionButton(ns("btn_delete_pre_saved_filters"), "delete", class = "btn-danger",style = "margin-left: 25px; margin-top: 10px;"),
        ),
      ),
      column(3,   
        div(style = "display: flex; align-items: center;",
          selectizeInput(ns("available_sessions"), "Saved sessions:", choices = NULL, selected=NULL, options = list(create = FALSE)),
          actionButton(ns("btn_load_session"), "load", class = "btn-primary",style = "margin-left: 25px; margin-top: 10px;"),
        ),
        div(style = "display: flex; align-items: center;",
          textInput(ns("session_name"), "Name session:", value = ""),
          actionButton(ns("btn_save_session"), "save", class = "btn-primary",style = "margin-left: 25px; margin-top: 10px;")
        ),
        div(style = "display: flex; align-items: center;",
          selectizeInput(ns("delete_sessions"), "Delete session:", choices = NULL, selected=NULL, options = list(create = FALSE)),
          actionButton(ns("btn_delete_session"), "delete", class = "btn-danger",style = "margin-left: 25px; margin-top: 10px;"),
        ),
      ),
      column(6,
        shiny::strong("Inheritance model options:"),
        InherUI(ns)
      )
    )
  )
}


#' Filter Tab UI
#' @param id Module ID
#' @return Shiny UI object
#' @export
#' @import shiny
selectFiltersUI <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(1, actionButton(ns("btn_reset"), "Reset all filters", class = "btn-secondary")),
      column(1, actionButton(ns("btn_apply_filters"), "Apply filters", class = "btn-primary")),
      column(5, textOutput(ns("num_variants_after_filtering"))),
      column(5)
    ),
    br(),
    header_UI(ns),
    br(),
    shiny::tags$hr(style = "border: 0; border-top: 1px solid #808080; margin-top: 20px;"),
    
    # div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_inher", "+", style="padding:0 6px;min-width:30px;"), span("Show Inheritance Options", id="toggle_inher_label")),
    # div(id="inher_container", style="display:none;margin-top:10px;", inherOptsUI(ns)),
    # tags$script(HTML("$('#toggle_inher').on('click',function(){var c=$('#inher_container');var b=$('#toggle_inher');var l=$('#toggle_inher_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide Inheritance Options');}else{b.text('+');l.text('Show Inheritance Model Options');}});")),
    # br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_panelapp", "+", style="padding:0 6px;min-width:50px;"), span("Show PanelApp Options", id="toggle_panelapp_label")),
    div(id="panelapp_container", style="max-height:0; overflow:hidden; transition:max-height 0.3s ease;", panelAppOptsUI(ns)),
    tags$script(HTML("$('#toggle_panelapp').on('click',function(){var c=$('#panelapp_container'); var b=$('#toggle_panelapp'); var l=$('#toggle_panelapp_label'); if(c.css('max-height')=='0px'){c.css('max-height','2000px'); b.text('-'); l.text('Hide PanelApp Options');} else {c.css('max-height','0px'); b.text('+'); l.text('Show PanelApp Options');}});")),
    br(),
    shiny::tags$hr(style = "border: 0; border-top: 1px solid #808080; margin-top: 20px;"),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_phenotype", "+", style="padding:0 6px;min-width:50px;"), span("Show Phenotype Options", id="toggle_phenotype_label")),
    div(id="phenotype_container", style="max-height:0; overflow:hidden; transition:max-height 0.3s ease;", phenotypeOptsUI(ns)),
    tags$script(HTML("$('#toggle_phenotype').on('click',function(){var c=$('#phenotype_container'); var b=$('#toggle_phenotype'); var l=$('#toggle_phenotype_label'); if(c.css('max-height')=='0px'){c.css('max-height','2000px'); b.text('-'); l.text('Hide Phenotype Options');} else {c.css('max-height','0px'); b.text('+'); l.text('Show Phenotype Options');}});")),
    br(),
    shiny::tags$hr(style = "border: 0; border-top: 1px solid #808080; margin-top: 20px;"),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_snv", "+", style="padding:0 6px;min-width:50px;"), span("Show SNVs and Indels Filters", id="toggle_snv_label")),
    div(id="snv_container", style="display:none;margin-top:10px;", snvOptsUI(ns)),
    tags$script(HTML("$('#toggle_snv').on('click',function(){var c=$('#snv_container');var b=$('#toggle_snv');var l=$('#toggle_snv_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide SNVs and Indels Filters');}else{b.text('+');l.text('Show SNVs and Indels Filters');}});")),
    br(),
    shiny::tags$hr(style = "border: 0; border-top: 1px solid #808080; margin-top: 20px;"),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_sv", "+", style="padding:0 6px;min-width:50px;"), span("Show SVs Filters", id="toggle_sv_label")),
    div(id="sv_container", style="display:none;margin-top:10px;", svOptsUI(ns)),
    tags$script(HTML("$('#toggle_sv').on('click',function(){var c=$('#sv_container');var b=$('#toggle_sv');var l=$('#toggle_sv_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide SVs Filters');}else{b.text('+');l.text('Show SVs Filters');}});")),
    br(),
    shiny::tags$hr(style = "border: 0; border-top: 1px solid #808080; margin-top: 20px;")
  )
}
