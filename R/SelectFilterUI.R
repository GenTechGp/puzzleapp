# Metadata

metaUI <- function(ns) {
  tableOutput(ns("meta"))
}

# Global Options

inherOptsUI <- function(ns) {
  choices <- c("", "Homozygous Recessive", "X-Linked Recessive",
               "Compound Heterozygous", "Dominant/De Novo", "Custom")
  collapseUI("inheritance_collapse", "Inheritance", "info",
    selectInput(ns("inher"), "Select inheritance:", choices, ""),
    uiOutput(ns("allele"))
  )
}

panelBox <- function(id, label, color = "black", width = "100%",
                     height = "30vh") {
  labelStyle <- "font-weight: bold; margin-bottom: 10px;"
  boxStyle <- paste("overflow-y: auto; max-height: calc(100% - 30px); color:",
                    color, ";")
  panelStyle <- paste("width:", width, "; height:", height, ";")

  wellPanel(
    div(style = labelStyle, label),
    div(style = boxStyle, textOutput(id)),
    style = panelStyle
  )
}

geneOptsUI <- function(ns, panel_app_genes) {
  locs <- c("", unique(panel_app_genes$Level4))
  locUI <- fluidRow(column(12,
    selectInput(ns("panelapp"), "Select location:", locs, "", TRUE),
    panelBox(ns("unclassified_genes"), "Unclassified genes:", "gray", "80%",
             "20vh")
  ))

  ui <- div(style = "height: 33vh",
    fluidRow(
      column(3, locUI),
      column(3, panelBox(ns("green_genes"), "Green genes:", "green")),
      column(3, panelBox(ns("red_genes"), "Red genes:", "red")),
      column(3, panelBox(ns("amber_genes"), "Amber genes:", "#FFBF00"))
    )
  )

  collapseUI("panelapp_collapse", "Panel App", "info", ui)
}

igvOptsUI <- function(ns) {
  inputUI <- fluidRow(
    column(6,
      textInput(ns("igv_var_id"), "Variant ID:", value = ""),
      numericInput(ns("igv_max_window"), "Max window size:", 10000, 0)
    ),
    column(6,
      numericInput(ns("igv_flanking"), "Flanking size:", 200, 0)
    )
  )

  ui <- div(style = "height: 33vh",
    inputUI,
    fluidRow(column(4, actionButton(ns("coords_button"), "get coords"))),
    fluidRow(column(12, br(), panelBox(ns("igv_coord_box"), "", height = "7vh")))
  )

  collapseUI("igv_collapse", "IGV", "info", ui)
}

listUI <- function(ns, id, label, prompt, box_label) {
  buttonUI <- fluidRow(
    column(3, actionButton(ns(paste0(id, "_add")), NULL, icon("plus"))),
    column(3, actionButton(ns(paste0(id, "_remove")), NULL, icon("minus")))
  )

  ui <- fluidRow(
    column(4,
      textInput(ns(paste0(id, "_var")), prompt, ""),
      buttonUI
    ),
    column(8, panelBox(ns(id), box_label, height = "10vh"))
  )

  collapseUI(paste0(id, "_collapse"), label, "info", ui)
}

phenoUI <- function(ns) {
  listUI(ns, "phenotype", "Phenotype", "HPO term:", NULL)
}

globalOptsUI <- function(ns, panel_app_genes) {
  collapseUI("global_options_collapse", "Global options", "primary",
    fluidRow(column(12, inherOptsUI(ns))),
    fluidRow(
      column(9, geneOptsUI(ns, panel_app_genes)),
      column(3, igvOptsUI(ns))
    ),
    fluidRow(
      column(4, phenoUI(ns))
    )
  )
}

# SNVs and Indels

checkboxDisplay <- function(x) {
  tags$div(style = "width: 140px;", x)
}

snvPathoUI <- function(ns) {
  clinvar_opts <- c("Pathogenic", "Likely pathogenic", "VUS", "Conflicting",
                    "Benign", "Likely benign", "Not available")
  clinvar_opts_display <- lapply(clinvar_opts, checkboxDisplay)

  ui <- div(style = "height: 30vh",
    selectInput(ns("pathogenicity"), "Select Clinvar:",
                c("", "Pathogenic/Likely pathogenic", "Not benign"), ""),
    prettyCheckboxGroup(ns("clinvar_checkboxes"), NULL,
                        choiceNames = clinvar_opts_display,
                        choiceValues = clinvar_opts, selected = NULL,
                        inline = TRUE)
  )

  collapseUI("pathogenicity_collapse", "Pathogenicity", "info", ui)
}

snvAnnotUI <- function(ns) {
  conseq_opts <- c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                   "Frameshift variant", "Missense variant", "In-frame variant",
                   "Synonymous variant", "5'UTR variant", "3'UTR variant",
                   "Intron variant", "Other")
  conseq_opts_display <- lapply(conseq_opts, checkboxDisplay)

  ui <- div(style = "height: 30vh",
    selectInput(ns("annotation"), "Select Annotation:",
                c("", "High impact", "Moderate to high impact"), ""),
    prettyCheckboxGroup(ns("conseq_checkboxes"), NULL,
                        choiceNames = conseq_opts_display,
                        choiceValues = conseq_opts, selected = NULL,
                        inline = TRUE),
    numericInput(ns("spliceai_score"), "SpliceAI score:", 0, 0, 1, 0.05)
  )

  collapseUI("annotation_collapse", "Annotation", "info", ui)
}

snvInsilicoUI <- function(ns) {
  ui <- div(style = "height: 30vh",
    numericInput(ns("revel"), "Enter Revel:", 0, 0, 1, 0.05),
    selectInput(ns("sift"), "Select Sift:", c("", "Deleterious", "Tolerated"),
                "", TRUE),
    selectInput(ns("polyphen"), "Select Polyphen:",
                c("", "Probably damaging", "Possibly damaging", "Benign"), "",
                TRUE)
  )

  collapseUI("insilico_filters_collapse", "In silico filters", "info", ui)
}

snvQualityUI <- function(ns) {
  ui <- div(style = "height: 30vh",
    # selectInput(ns("pass_variants"), "Select Variant:",
    #             c("", "PASS only variants", "All variants"), ""),
    sliderInput(ns("genotype_quality"), "Genotype quality:", 0, 100, 0,
                ticks = FALSE),
    sliderInput(ns("allele_balance"), "Minimum Allele fraction:", 0, 1, 0,
                ticks = FALSE),
    materialSwitch(ns("affected_switch"), label = tags$b("Affected only:"))
  )

  collapseUI("quality_collapse", "Call quality", "info", ui)
}

snvFreqUI <- function(ns) {
  freqs <- c(0, seq(0.0001, 0.0005, by = 0.0004), 0.001, 0.005, 0.01, 0.02,
             0.03, 0.04, 0.05, 0.1, 1)

  ui <- div(style = "height: 30vh",
    selectInput(ns("af"), "gnomADv4 AF:", freqs, 1)
  )

  collapseUI("frequency_collapse", "Frequency", "info", ui)
}

snvOptsUI <- function(ns) {
  ui <- fluidRow(
    column(2, snvPathoUI(ns)),
    column(4, snvAnnotUI(ns)),
    column(2, snvInsilicoUI(ns)),
    column(2, snvQualityUI(ns)),
    column(2, snvFreqUI(ns))
  )

  collapseUI("snvs_indels_collapse", "SNVs and Indels", "primary", ui)
}

# SVs

svFeatsUI <- function(ns) {
  choices <- c("Insertion", "Deletion", "Duplication", "Inversion", "Translocation")
  ui <- div(style = "height: 20vh",
    fluidRow(
      column(6,
        prettyCheckboxGroup(ns("sv_features_checkboxes"), "SV type:", choices,
                            NULL, inline = FALSE)
      ),
      column(6,
        numericInput(ns("min_svlen"), "Min Length:", 0, NA, NA),
        numericInput(ns("max_svlen"), "Max Length:", 0, NA, NA)
      )
    )
  )

  collapseUI("sv_features_collapse", "Features", "info", ui)
}

svConseqUI <- function(ns) {
  rel_choices <- c("Exonic", "Intronic", "UTR", "Promoter", "Intergenic")
  conseq_choices <- c("Loss of function (LoF)", "Copy Number Variation (CNV)",
                      "Whole gene inversion",
                      "Regulatory and Non-coding variants")
  ui <- div(style = "height: 20vh",
    fluidRow(
      column(4,
        prettyCheckboxGroup(ns("sv_relative_pos_checkboxes"), "Location:",
                            rel_choices, NULL, inline = FALSE)
      ),
      column(8,
        prettyCheckboxGroup(ns("sv_consequence_checkboxes"),
                            "Predicted consequences:", conseq_choices,
                            "Loss of function (LoF)", inline = FALSE)
      )
    )
  )

  collapseUI("sv_consequence_collapse", "Annotation", "info", ui)
}

svQualityUI <- function(ns) {
  ui <- div(style = "height: 20vh",
    fluidRow(
      column(6,
        selectInput(ns("sv_pass_variants"), "Select Variant:",
                    c("","PASS only variants","All variants"), ""),
        sliderInput(ns("sv_genotype_quality"), "Genotype quality:", 0, 100, 0,
                    ticks = FALSE),
      ),
      column(6,
        sliderInput(ns("sv_allele_balance"), "Minimum Allele fraction:", 0, 1,
                    0, ticks = FALSE),
        materialSwitch(ns("sv_affected_switch"), tags$b("Affected only:"))
      )
    )
  )

  collapseUI("sv_quality_collapse", "Call quality", "info", ui)
}

svOptsUI <- function(ns) {
  ui <- fluidRow(
    column(2, svFeatsUI(ns)),
    column(3, svConseqUI(ns)),
    column(3, svQualityUI(ns))
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
