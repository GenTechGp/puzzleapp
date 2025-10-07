#' Filter Tab UI
#' @param id Module ID
#' @export
#' @import shiny
#' @importFrom shiny selectizeInput

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
    br(),
    fluidRow(
      column(3, panelBox(ns("unclassified_genes"), "Unclassified genes:", "gray")),
      column(3, panelBox(ns("green_genes"), "Green genes:", "green")),
      column(3, panelBox(ns("red_genes"), "Red genes:", "red")),
    column(3, panelBox(ns("amber_genes"), "Amber genes:", "#FFBF00"))
    ),
    fluidRow(
      column(12, textInput(ns("custom_genes"), "Custom genes (comma/semi-colon/tab/space-separated):", value = "", width = "100%"))
    ),
    fluidRow(
      column(12, checkboxInput(ns("treat_negative"), "Consider PanelApp genes and Custom genes as negative", value = FALSE, width = "100%"))
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

selectFiltersUI <- function(id) {
  ns <- NS(id)

  tagList(
    br(),
    fluidRow(
      column(1, actionButton(ns("btn_reset"), "Reset all filters", class = "btn-secondary")),
      column(1, actionButton(ns("btn_apply_filters"), "Apply filters", class = "btn-primary")),
      column(10)
    ),
    br(),
    header_UI(ns),
    br(),
    
    # div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_inher", "+", style="padding:0 6px;min-width:30px;"), span("Show Inheritance Options", id="toggle_inher_label")),
    # div(id="inher_container", style="display:none;margin-top:10px;", inherOptsUI(ns)),
    # tags$script(HTML("$('#toggle_inher').on('click',function(){var c=$('#inher_container');var b=$('#toggle_inher');var l=$('#toggle_inher_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide Inheritance Options');}else{b.text('+');l.text('Show Inheritance Model Options');}});")),
    # br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_panelapp", "+", style="padding:0 6px;min-width:30px;"), span("Show PanelApp Options", id="toggle_panelapp_label")),
    div(id="panelapp_container", style="max-height:0; overflow:hidden; transition:max-height 0.3s ease;", panelAppOptsUI(ns)),
    tags$script(HTML("$('#toggle_panelapp').on('click',function(){var c=$('#panelapp_container'); var b=$('#toggle_panelapp'); var l=$('#toggle_panelapp_label'); if(c.css('max-height')=='0px'){c.css('max-height','2000px'); b.text('-'); l.text('Hide PanelApp Options');} else {c.css('max-height','0px'); b.text('+'); l.text('Show PanelApp Options');}});")),
    br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_phenotype", "+", style="padding:0 6px;min-width:30px;"), span("Show Phenotype Options", id="toggle_phenotype_label")),
    div(id="phenotype_container", style="max-height:0; overflow:hidden; transition:max-height 0.3s ease;", phenotypeOptsUI(ns)),
    tags$script(HTML("$('#toggle_phenotype').on('click',function(){var c=$('#phenotype_container'); var b=$('#toggle_phenotype'); var l=$('#toggle_phenotype_label'); if(c.css('max-height')=='0px'){c.css('max-height','2000px'); b.text('-'); l.text('Hide Phenotype Options');} else {c.css('max-height','0px'); b.text('+'); l.text('Show Phenotype Options');}});")),
    br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_snv", "+", style="padding:0 6px;min-width:30px;"), span("Show SNVs and Indels Filters", id="toggle_snv_label")),
    div(id="snv_container", style="display:none;margin-top:10px;", snvOptsUI(ns)),
    tags$script(HTML("$('#toggle_snv').on('click',function(){var c=$('#snv_container');var b=$('#toggle_snv');var l=$('#toggle_snv_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide SNVs and Indels Filters');}else{b.text('+');l.text('Show SNVs and Indels Filters');}});")),
    br(),

    div(style="display:flex;align-items:center;gap:6px;", actionButton("toggle_sv", "+", style="padding:0 6px;min-width:30px;"), span("Show SVs Filters", id="toggle_sv_label")),
    div(id="sv_container", style="display:none;margin-top:10px;", svOptsUI(ns)),
    tags$script(HTML("$('#toggle_sv').on('click',function(){var c=$('#sv_container');var b=$('#toggle_sv');var l=$('#toggle_sv_label');c.toggle();if(c.is(':visible')){b.text('-');l.text('Hide SVs Filters');}else{b.text('+');l.text('Show SVs Filters');}});")),
    br(),
  )
}
