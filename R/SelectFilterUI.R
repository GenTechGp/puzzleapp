# TODO:
# - replace all uiOutput with reactive variables?

collapseUI <- function(id, title, style, ...) {
  box_id <- paste0(id, "_box")
  bsCollapse(open = box_id, multiple = TRUE,
    bsCollapsePanel(title = title, value = box_id, style = style, ...)
  )
}

inherOptsUI <- function(ns) {
  choices <- c("", "Homozygous Recessive", "X-Linked Recessive",
               "Compound Heterozygous", "Dominant/De Novo", "Custom")
  collapseUI("inheritance_collapse", "Inheritance", "info",
    selectInput(ns("inheritance"), "Select inheritance:", choices, ""),
    uiOutput(ns("additional_rows"))
  )
}

panelBox <- function(ns, outputId, label, color = "black",width = "100%", height = "30vh") {
  wellPanel(div(style = "font-weight: bold; margin-bottom: 10px;", label),
    div(style = paste("overflow-y: auto; max-height: calc(100% - 30px); color:", color, ";"),
      uiOutput(ns(outputId))),
    style = paste("width:", width, "; height:", height, ";")
  )
}

geneOptsUI <- function(ns, panel_app_genes) {
  locs <- c("", unique(panel_app_genes$Level4))
  locUI <- fluidRow(column(12,
             selectInput(ns("panelapp"), "Select location:", locs, "", TRUE),
             uiOutput(ns("unclassified_genes"))
           ))

  collapseUI("panelapp_collapse", "Panel App", "info",
    div(style = "height: 33vh",
      fluidRow(
        column(3,
          fluidRow(
            column(12,
              selectInput(ns("panelapp"), "Select location:",
                choices = c("", unique(panel_app_genes$Level4)),
                selected = "", multiple = TRUE),
              panelBox(ns, "unclassified_genes", paste("Unclassified", "genes:"), color = "gray",width = "80%", height = "20vh")
            )
          )
        ),
        column(3,panelBox(ns, "green_genes", paste("Green", "genes:"), color = "green")),
        column(3,panelBox(ns, "red_genes", paste("Red", "genes:"), color = "red")),
        column(3,panelBox(ns, "amber_genes", paste("Amber", "genes:"), color = "#FFBF00")),
      )
    )
  )
}

igvOptsUI <- function(ns) {
  inputUI <- fluidRow(
    column(6,
      textInput(ns("igv_var_id"), "Variant ID:", value = ""),
      numericInput(ns("igv_max_window"), "Max window size:", value = 10000,
                   min = 0)
    ),
    column(6,
      numericInput(ns("igv_flanking"), "Flanking size:",
                   value = 200, min = 0)
    )
  )

  collapseUI("igv_collapse", "IGV", "info",
    div(style = "height: 33vh",
      inputUI,
      fluidRow(column(4, actionButton(ns("coords_button"), "get coords"))),
      fluidRow(column(12, br(), panelBox(ns, "igv_coord_box", "", height = "7vh")))
    )
  )
}

shortlistUI <- function(ns) {
 ui <- fluidRow(
    column(4,
      textInput(ns("shortlisted_var"), "Variant ID:", value = ""),
      fluidRow(
        column(3, actionButton(ns("shortlisted_add"), label = NULL, icon = icon("plus"))),
        column(3, actionButton(ns("shortlisted_remove"), label = NULL, icon = icon("minus")))
      )
    ),
    column(8, panelBox(ns, "shortlist", "Shortlist:", height = "10vh"))
  )

  collapseUI("short_list_collapse", "Shortlisted Variants", "info", ui)
}

blacklistUI <- function(ns) {
  collapseUI("black_list_collapse", "Blacklisted Variants", "info",
    fluidRow(
      column(4,
        textInput(ns("blacklisted_var"), "Variant ID:", value = ""),
        fluidRow(
          column(3, actionButton(ns("blacklisted_add"), label = NULL, icon = icon("plus"))),
          column(3, actionButton(ns("blacklisted_remove"), label = NULL, icon = icon("minus")))
        )
      ),
      column(8, panelBox(ns, "blacklist", "Blacklist:", height = "10vh"))
    )
  )
}

phenotypeUI <- function(ns) {
  collapseUI("phenotype_collapse", "Phenotype", "info",
    fluidRow(
      column(4,
        textInput(ns("phenotype_var"), "HPO term:", value = ""),
        fluidRow(
          column(3, actionButton(ns("phenotype_add"), label = NULL, icon = icon("plus"))),
          column(3, actionButton(ns("phenotype_remove"), label = NULL, icon = icon("minus")))
        )
      ),
      column(8, panelBox(ns, "phenotype", "HPO terms:", height = "10vh"))
    )
  )
}

globalOptsUI <- function(ns, panel_app_genes) {
  collapseUI("global_options_collapse", "Global options", "primary",
    fluidRow(column(12, inherOptsUI(ns))),
    fluidRow(
      column(9, geneOptsUI(ns, panel_app_genes)),
      column(3, igvOptsUI(ns))
    ),
    fluidRow(
      column(4, shortlistUI(ns)),
      column(4, blacklistUI(ns)),
      column(4, phenotypeUI(ns))
    )
  )
}

snvOptsUI <- function(ns) {
  collapseUI("snvs_indels_collapse", "SNVs and Indels", "primary",
    fluidRow(
      column(2,
        collapseUI("pathogenicity_collapse", "Pathogenicity", "info",
          div(style = "height: 30vh",
            selectInput(ns("pathogenicity"), "Select Clinvar:",
                        c("","Pathogenic/Likely pathogenic","Not benign"), ""),
            uiOutput(ns("clinvar"))
          )
        )
      ),
      column(4,
        collapseUI("annotation_collapse", "Annotation", "info",
          div(style = "height: 30vh",
            selectInput(ns("annotation"), "Select Annotation:",
                        c("","High impact","Moderate to high impact"), ""),
            uiOutput(ns("consequences")),
            numericInput(ns("spliceai_score"), "SpliceAI score:", value = 0, min = 0, max = 1, step = 0.05)
          )
        )
      ),
      column(2,
        collapseUI("insilico_filters_collapse", "In silico filters", "info",
          div(style = "height: 30vh",
            numericInput(ns("revel"), "Enter Revel:", value = 0, min = 0, max = 1, step = 0.05),
            selectInput(ns("sift"), "Select Sift:",
                        c("","Deleterious","Tolerated"), "", TRUE),
            selectInput(ns("polyphen"), "Select Polyphen:",
                        c("","Probably damaging","Possibly damaging","Benign"),
                        "", TRUE)
          )
        )
      ),
      column(2,
        collapseUI("quality_collapse", "Call quality", "info",
          div(style = "height: 30vh",
            # selectInput(ns("pass_variants"), "Select Variant:",
            #             c("","PASS only variants","All variants"), ""),
            sliderInput(ns("genotype_quality"), label = "Genotype quality:",
              min = 0, max = 100, ticks = FALSE, value = 0
            ),
            sliderInput(ns("allele_balance"), label = "Minimum Allele fraction:",
              min = 0, max = 1,ticks = FALSE, value = 0
            ),
            materialSwitch(ns("affected_switch"), label = tags$b("Affected only:"))
          )
        )
      ),
      column(2,
        collapseUI("frequency_collapse", "Frequency", "info",
          div(style = "height: 30vh",
            selectInput(ns("af"), "gnomADv4 AF:", c(0, seq(0.0001, 0.0005, by = 0.0004),
                        0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 1), 1),
          )
        )
      ),
    )
  )
}

svOptsUI <- function(ns) {
  collapseUI("svs_collapse", "SVs", "primary",
    fluidRow(
      column(2,
        collapseUI("sv_features_collapse", "Features", "info",
          div(style = "height: 20vh",
            fluidRow(
              column(6, uiOutput(ns("sv_features"))),
              column(6,
                numericInput(ns("min_svlen"), "Min Length:", value = 0, min = NA, max = NA),
                numericInput(ns("max_svlen"), "Max Length:", value = 0, min = NA, max = NA)
              )
            )
          )
        )
      ),
      column(3,
        collapseUI("sv_consequence_collapse", "Annotation", "info",
          div(style = "height: 20vh",
            fluidRow(
              column(4, uiOutput(ns("sv_relative_pos"))),
              column(8, uiOutput(ns("sv_consequence")))
            )
          )
        )
      ),
      column(3,
        collapseUI("sv_quality_collapse", "Call quality", "info",
          div(style = "height: 20vh",
            fluidRow(
              column(6,
                selectInput(ns("sv_pass_variants"), "Select Variant:",
                            c("","PASS only variants","All variants"), ""),
                sliderInput(ns("sv_genotype_quality"), label = "Genotype quality:",
                  min = 0, max = 100, ticks = FALSE, value = 0
                ),
              ),
              column(6,
                sliderInput(ns("sv_allele_balance"), label = "Minimum Allele fraction:",
                  min = 0, max = 1, ticks = FALSE, value = 0
                ),
                materialSwitch(ns("sv_affected_switch"), label = tags$b("Affected only:"))
              )
            )
          )
        )
      )
    )
  )
}

selectFiltersUI <- function(id, panel_app_genes) {
  ns <- NS(id)

  fluidPage(
    useShinyjs(),  # Initialize shinyjs
    fluidRow(globalOptsUI(ns, panel_app_genes)),
    fluidRow(snvOptsUI(ns)),
    fluidRow(svOptsUI(ns)),
    actionButton(ns("apply_filter"), "Apply filters", class = "btn-primary")
  )
}
