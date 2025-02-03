# Metadata
metaUI <- function(ns) {
  ui <- div(
    tableOutput(ns("meta"))
  )

  collapseUI("meta_collapse", "Metadata", "primary", ui)
}

# Saved searches
preSavedSearchesUI <- function(ns) {
  collapseUI("pre_saved_searches_collapse", "Saved searches", "info",
             selectizeInput(ns("pre_saved_search"), "SNVs & Indels pre-saved search:", choices = NULL, selected=NULL),
             selectizeInput(ns("sv_pre_saved_search"), "SVs pre-saved search:", choices = NULL, selected=NULL)
  )
}

saveSessionsUI <- function(ns) {
  collapseUI("save_sessions_collapse", "Sessions", "info",
             fluidRow(
               column(2, textInput(ns("session_name"), "Name session:", value = "")),
               column(1, actionButton(ns("save_session"), "save", class = "btn-primary",style = "margin-top: 25px;"))
             ),
             fluidRow(
               column(2, selectizeInput(ns("available_sessions"), "Saved sessions:", choices = NULL, selected=NULL, options = list(create = FALSE))),
               column(1, actionButton(ns("load_session"), "load", class = "btn-primary",style = "margin-top: 25px;"))
             )
  )
}

# Global Options

inherOptsUI <- function(ns) {
  choices <- c("", "Homozygous Recessive", "X-Linked Recessive",
               "Compound Heterozygous", "Dominant/De Novo", "Custom")
  collapseUI("inheritance_collapse", "Inheritance", "info",
    selectInput(ns("inher"), "Mode of inheritance:", choices, ""),
    tableOutput(ns("allele"))
  )
}

panelBox <- function(id, label, color = "black", width = "100%",
                     height = "30vh") {
  boxStyle <- paste("overflow-y: auto; max-height: calc(100% - 30px); color:",
                    color, ";")
  panelStyle <- paste("width:", width, "; height:", height, "; padding: 5px;")

  if (is.null(label)) {
    labelUI <- NULL
  } else {
    labelStyle <- "font-weight: bold; margin-bottom: 5px;"
    labelUI <- div(style = labelStyle, label)
  }

  wellPanel(
    labelUI,
    div(style = boxStyle, textOutput(id)),
    style = panelStyle
  )
}

geneOptsUI <- function(ns, panel_app_genes) {
  locs <- c("", unique(panel_app_genes$Level4))

  ui <- div(
    selectInput(ns("panelapp"), "Gene list:", locs, "", TRUE),
    fluidRow(
      column(3,
        panelBox(ns("unclassified_genes"), "Unclassified genes:", "gray")
      ),
      column(3, panelBox(ns("green_genes"), "Green genes:", "green")),
      column(3, panelBox(ns("red_genes"), "Red genes:", "red")),
      column(3, panelBox(ns("amber_genes"), "Amber genes:", "#FFBF00"))
    )
  )

  collapseUI("panelapp_collapse", "Panel App", "info", ui)
}

listUI <- function(ns, id, label, prompt, box_label) {
  buttonUI <- div(
    actionButton(ns(paste0(id, "_add")), NULL, icon("plus")),
    actionButton(ns(paste0(id, "_remove")), NULL, icon("minus"))
  )

  ui <- fluidRow(
    column(4,
      textInput(ns(paste0(id, "_var")), prompt, ""),
      buttonUI,
      br(),
      panelBox(ns(id), box_label, height = "10vh")
    )
  )

  collapseUI(paste0(id, "_collapse"), label, "info", ui)
}

phenoUI <- function(ns) {
  listUI(ns, "phenotype", "Phenotype", "HPO term:", NULL)
}

globalOptsUI <- function(ns, panel_app_genes) {
  collapseUI("global_options_collapse", "Global options", "primary",
    div(
      saveSessionsUI(ns),
      preSavedSearchesUI(ns),
      inherOptsUI(ns),
      geneOptsUI(ns, panel_app_genes),
      phenoUI(ns)
    )
  )
}

# SNVs and Indels

snvPathoUI <- function(ns) {
  clinvar_opts <- c("Pathogenic", "Likely pathogenic", "VUS", "Conflicting",
                    "Benign", "Likely benign", "Not available")

  ui <- div(
    selectInput(ns("pathogenicity"), "Clinvar:",
                c("", "Pathogenic/Likely pathogenic", "Not benign"), ""),
    prettyCheckboxGroup(ns("clinvar_checkboxes"), NULL, clinvar_opts,
                        selected = NULL)
  )

  collapseUI("pathogenicity_collapse", "Pathogenicity", "info", ui)
}

AnnotUI <- function(ns, type = "snv") {
  conseq_opts <- c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                   "Frameshift variant", "Missense variant", "In-frame variant",
                   "Synonymous variant", "5'UTR variant", "3'UTR variant",
                   "Intron variant", "Other")

  # Adjust namespace prefix based on type
  id_prefix <- if (type == "sv") "sv_" else ""

  ui_elements <- list(
    selectInput(ns(paste0(id_prefix, "annotation")), "Functional consequence:",
                c("", "High impact", "Moderate to high impact"), ""),
    prettyCheckboxGroup(ns(paste0(id_prefix, "conseq_checkboxes")), NULL, conseq_opts,
                        selected = NULL)
  )

  # Only add SpliceAI score if type is SNV
  if (type == "snv") {
    ui_elements <- append(ui_elements, 
                          list(numericInput(ns("spliceai_score"), "SpliceAI score:", 0, 0, 1, 0.05))
    )
  }

  ui <- div(ui_elements)

  collapseUI(paste0(id_prefix, "annotation_collapse"), "Annotation", "info", ui)
}

snvInsilicoUI <- function(ns) {
  ui <- div(
    numericInput(ns("revel"), "Revel score:", 0, 0, 1, 0.05),
    numericInput(ns("alpha_missense"), "AlphaMissense score:", 0, 0, 1, 0.05),
    selectInput(ns("sift"), "Sift:", c("", "Deleterious", "Tolerated"),
                "", TRUE),
    selectInput(ns("polyphen"), "Polyphen:",
                c("", "Probably damaging", "Possibly damaging", "Benign"), "",
                TRUE)
  )

  collapseUI("insilico_filters_collapse", "In silico filters", "info", ui)
}

QualityUI <- function(ns, label = "snv") {
  prefix <- ifelse(label == "sv", "sv_", "")

  ui <- div(
    selectInput(ns(paste0(prefix, "pass_variants")), "Filter:",
                c("", "PASS only variants", "All variants"), ""),
    sliderInput(ns(paste0(prefix, "genotype_quality")), "Genotype quality:", 0, 100, 0,
                ticks = FALSE),
    sliderInput(ns(paste0(prefix, "allele_balance")), "Minimum Allele fraction:", 0, 1, 0,
                ticks = FALSE),
    materialSwitch(ns(paste0(prefix, "affected_switch")), label = tags$b("Affected only:"))
  )

  collapseUI(paste0(prefix, "quality_collapse"), "Call quality", "info", ui)
}

FreqUI <- function(ns,label) {
  freqs <- c(0, seq(0.0001, 0.0005, by = 0.0004), 0.001, 0.005, 0.01, 0.02,
             0.03, 0.04, 0.05, 0.1, 1)

  ui <- div(
    selectInput(ns(label), "gnomADv4 AF:", freqs, 1)
  )

  collapseUI("frequency_collapse", "Frequency", "info", ui)
}

snvOptsUI <- function(ns) {
  ui <- div(
    fluidRow(
      column(3, AnnotUI(ns,"snv")),
      column(2, snvPathoUI(ns)),
      column(2, snvInsilicoUI(ns)),
      column(2, QualityUI(ns,"snv")),
      column(2, FreqUI(ns,"af"))
    )
  )

  collapseUI("snvs_indels_collapse", "SNVs and Indels", "primary", ui)
}

# SVs

svFeatsUI <- function(ns) {
  choices <- c("Insertion", "Deletion", "Duplication", "Inversion", "Translocation")
  ui <- div(
    prettyCheckboxGroup(ns("sv_features_checkboxes"), "SV type:", choices,
                        NULL, inline = FALSE),
    numericInput(ns("min_svlen"), "Min Length:", 0, NA, NA),
    numericInput(ns("max_svlen"), "Max Length:", 0, NA, NA)
  )

  collapseUI("sv_features_collapse", "Features", "info", ui)
}

svOptsUI <- function(ns) {
  ui <- fluidRow(
    column(3, AnnotUI(ns,"sv")),
    column(2, svFeatsUI(ns)),
    column(2, QualityUI(ns,"sv")),
    column(2, FreqUI(ns,"sv_af"))
  )

  collapseUI("svs_collapse", "SVs", "primary", ui)
}

# Main

selectFiltersUI <- function(id, panel_app_genes) {
  ns <- NS(id)

  fluidPage(
    useShinyjs(),
    fluidRow(metaUI(ns)),
    fluidRow(globalOptsUI(ns, panel_app_genes)),
    fluidRow(snvOptsUI(ns)),
    fluidRow(svOptsUI(ns)),
    actionButton(ns("apply_filter"), "Apply filters", class = "btn-primary")
  )
}
