#' Filter Tab UI
#' @param id Module ID
#' @export
#' @import shiny

# ---- UI building functions ----
# Annotation UI
AnnotUI <- function(ns, type = "snv") {
  annotation_select_opts <- c("None", "High impact", "Moderate to high impact")
  annotation_conseq_opts <- c(
    "Stop gained", "Start lost", "Stop lost", "Splice variant",
    "Frameshift variant", "Missense variant", "In-frame variant",
    "Synonymous variant", "5'UTR variant", "3'UTR variant",
    "Intron variant", "Other"
  )
  id_prefix <- if (type == "sv") "sv_" else ""
  ui_elements <- list(
    h4("Annotation"),
    selectInput(ns(paste0(id_prefix, "annotation")), "Functional consequence:", choices = annotation_select_opts, selected = "None"),
    checkboxGroupInput(ns(paste0(id_prefix, "conseq_checkboxes")), NULL, choices = annotation_conseq_opts)
  )
  if (type == "snv") {
    ui_elements <- append(
      ui_elements,
      list(numericInput(ns("spliceai_score"), "SpliceAI score:", 0, 0, 1, 0.05))
    )
  }
  tagList(ui_elements)
}

# Pathogenicity UI
snvPathoUI <- function(ns) {
  snv_pathogenicity_clinvar_select_opts <- c("None", "Pathogenic/Likely pathogenic", "Not benign")
  snv_pathogenicity_clinvar_opts <- c(
    "Pathogenic", "Likely pathogenic", "VUS", "Conflicting",
    "Benign", "Likely benign", "Not available"
  )
  tagList(
    h4("Pathogenicity"),
    selectInput(ns("pathogenicity"), "Clinvar:", choices = snv_pathogenicity_clinvar_select_opts, selected = "None"),
    checkboxGroupInput(ns("clinvar_checkboxes"), NULL, choices = snv_pathogenicity_clinvar_opts)
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
QualityUI <- function(ns, label = "snv") {
  callquality_filter_opts <- c("PASS only variants", "All variants")
  prefix <- ifelse(label == "sv", "sv_", "")
  tagList(
    h4("Call quality"),
    selectInput(ns(paste0(prefix, "pass_variants")), "Filter:", choices = callquality_filter_opts, selected = "All variants"),
    sliderInput(ns(paste0(prefix, "genotype_quality")), "Genotype quality:", 0, 100, 0, ticks = FALSE),
    sliderInput(ns(paste0(prefix, "allele_balance")), "Minimum Allele fraction:", 0, 1, 0, ticks = FALSE),
    checkboxInput(ns(paste0(prefix, "affected_switch")), "Affected only", value = FALSE)
  )
}

# Frequency UI
FreqUI <- function(ns, label) {
  filter_freqs <- c(
    0, seq(0.0001, 0.0005, by = 0.0004),
    0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 1
  )
  prefix <- ifelse(label == "sv", "sv_", "")
  tagList(
    h4("Frequency"),
    selectInput(ns(paste0(prefix, "af")), "gnomADv4 AF:", choices = filter_freqs, selected = 1)
  )
}

# Structural Variant Features UI
svFeatsUI <- function(ns) {
  sv_type_opts <- c("Insertion", "Deletion", "Duplication", "Inversion", "Translocation")
  tagList(
    h4("SV Features"),
    checkboxGroupInput(ns("sv_features_checkboxes"), NULL, choices = sv_type_opts),
    numericInput(ns("min_svlen"), "Min Length:", 0, NA, NA),
    numericInput(ns("max_svlen"), "Max Length:", 0, NA, NA)
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
    column(2, snvPathoUI(ns)),
    column(2, snvInsilicoUI(ns)),
    column(1),
  )
}

# SVs UI
svOptsUI <- function(ns) {
  fluidRow(
    column(1),
    column(2, AnnotUI(ns, "sv")),
    column(2, QualityUI(ns, "sv")),
    column(2, FreqUI(ns, "sv")),
    column(2, svFeatsUI(ns)),
    column(3),
  )
}

inherOptsUI <- function(ns) {
  fluidRow(
    column(1),
    column(4, InherUI(ns)),
    column(7)
  )
}


selectFiltersUI <- function(id) {
  ns <- NS(id)

  tagList(
  
    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_inher", "+", style="padding:0 6px;min-width:30px;"), span("Show Inheritance Options", id="toggle_inher_label")),
    div(id="inher_container", style="display:none;margin-top:10px;", inherOptsUI(ns)),
    tags$script(HTML("$('#toggle_inher').on('click',function(){var c=$('#inher_container');var b=$('#toggle_inher');var l=$('#toggle_inher_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide Inheritance Options');}else{b.text('+');l.text('Show Inheritance Model Options');}});")),
    br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_snv", "+", style="padding:0 6px;min-width:30px;"), span("Show SNVs and Indels Filters", id="toggle_snv_label")),
    div(id="snv_container", style="display:none;margin-top:10px;", snvOptsUI(ns)),
    tags$script(HTML("$('#toggle_snv').on('click',function(){var c=$('#snv_container');var b=$('#toggle_snv');var l=$('#toggle_snv_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide SNVs and Indels Filters');}else{b.text('+');l.text('Show SNVs and Indels Filters');}});")),
    br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_sv", "+", style="padding:0 6px;min-width:30px;"), span("Show SVs Filters", id="toggle_sv_label")),
    div(id="sv_container", style="display:none;margin-top:10px;", svOptsUI(ns)),
    tags$script(HTML("$('#toggle_sv').on('click',function(){var c=$('#sv_container');var b=$('#toggle_sv');var l=$('#toggle_sv_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide SVs Filters');}else{b.text('+');l.text('Show SVs Filters');}});")),
    br(),
    actionButton(ns("apply"), "Apply filters", class = "btn-primary"),

  )
}
