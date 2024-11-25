# TODO:
# - replace all uiOutput with reactive variables?

globalOptsInherUI <- function(ns) {
  bsCollapse(id = "inheritance_collapse", open = "inheritance_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "Inheritance", value = "inheritance_collapse_box", style = "info",
      selectInput(ns("inheritance"), "Select inheritance:",
        choices = c("", c("Homozygous Recessive","X-Linked Recessive","Compound Heterozygous","Dominant/De Novo","Custom")),
        selected = ""),
      uiOutput(ns("additional_rows"))
    )
  )
}

globalOptsPanelAppUI <- function(ns, panel_app_genes) {
  bsCollapse(id = "panelapp_collapse", open = "panelapp_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "Panel App", value = "panelapp_collapse_box", style = "info",
      div(style = "height: 33vh",
        fluidRow(
          column(3,
            fluidRow(
              column(12,
                selectInput(ns("panelapp"), "Select location:",
                  choices = c("", unique(panel_app_genes$Level4)),
                  selected = "", multiple = TRUE),
                uiOutput(ns("unclassified_genes"))
              )
            )
          ),
          column(3, uiOutput(ns("green_genes"))),
          column(3, uiOutput(ns("red_genes"))),
          column(3, uiOutput(ns("amber_genes")))
        )
      )
    )
  )
}

globalOptsIgvUI <- function(ns) {
  bsCollapse(id = "igv_collapse", open = "igv_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "IGV", value = "igv_collapse_box", style = "info",
      div(style = "height: 33vh",
        fluidRow(
          column(6,
            textInput(ns("igv_var_id"), "Variant ID:", value = ""),
            numericInput(ns("igv_max_window"), "Max window size:", value = 10000, min = 0)
          ),
          column(6, numericInput(ns("igv_flanking"), "Flanking size:", value = 200, min = 0))
        ),
        fluidRow(column(4, actionButton(ns("coords_button"), "get coords"))),
        fluidRow(column(12, br(), uiOutput(ns("igv_coord_box"))))
      )
    )
  )
}

shortlistUI <- function(ns) {
  bsCollapse(id = "short_list_collapse", open = "short_list_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "Shortlisted Variants", value = "short_list_collapse_box", style = "info",
      fluidRow(
        column(4,
          textInput(ns("shortlisted_var"), "Variant ID:", value = ""),
          fluidRow(
            column(3, actionButton(ns("shortlisted_add"), label = NULL, icon = icon("plus"))),
            column(3, actionButton(ns("shortlisted_remove"), label = NULL, icon = icon("minus")))
          )
        ),
        column(8, uiOutput(ns("shortlist")))
      )
    )
  )
}

blacklistUI <- function(ns) {
  bsCollapse(id = "black_list_collapse", open = "black_list_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "Blacklisted Variants", value = "black_list_collapse_box", style = "info",
      fluidRow(
        column(4,
          textInput(ns("blacklisted_var"), "Variant ID:", value = ""),
          fluidRow(
            column(3, actionButton(ns("blacklisted_add"), label = NULL, icon = icon("plus"))),
            column(3, actionButton(ns("blacklisted_remove"), label = NULL, icon = icon("minus")))
          )
        ),
        column(8, uiOutput(ns("blacklist")))
      )
    )
  )
}

phenotypeUI <- function(ns) {
  bsCollapse(id = "phenotype_collapse", open = "phenotype_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "Phenotype", value = "phenotype_collapse_box", style = "info",
      fluidRow(
        column(4,
          textInput(ns("phenotype_var"), "HPO term:", value = ""),
          fluidRow(
            column(3, actionButton(ns("phenotype_add"), label = NULL, icon = icon("plus"))),
            column(3, actionButton(ns("phenotype_remove"), label = NULL, icon = icon("minus")))
          )
        ),
        column(8, uiOutput(ns("phenotype")))
      )
    )
  )
}

globalOptsUI <- function(ns, panel_app_genes) {
  bsCollapse(id = "global_options_collapse", open = "global_options_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "Global options", value = "global_options_collapse_box", style = "primary",
      fluidRow(column(12, globalOptsInherUI(ns))),
      fluidRow(
        column(9, globalOptsPanelAppUI(ns, panel_app_genes)),
        column(3, globalOptsIgvUI(ns))
      ),
      fluidRow(
        column(4, shortlistUI(ns)),
        column(4, blacklistUI(ns)),
        column(4, phenotypeUI(ns))
      )
    )
  )
}

snvOptsUI <- function(ns) {
  bsCollapse(id = "snvs_indels_collapse", open = "snvs_indels_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "SNVs and Indels", value = "snvs_indels_collapse_box",style = "primary",
      fluidRow(
        column(2,
          bsCollapse(id = "pathogenicity_collapse", open = "pathogenicity_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Pathogenicity", value = "pathogenicity_collapse_box",style = "info",
              div(style = "height: 30vh",
                selectInput(ns("pathogenicity"), "Select Clinvar:",
                  choices = c("","Pathogenic/Likely pathogenic","Not benign"),
                  selected = ""
                ),
                uiOutput(ns("clinvar"))
              )
            )
          )
        ),
        column(4,
          bsCollapse(id = "annotation_collapse", open = "annotation_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Annotation", value = "annotation_collapse_box",style = "info",
              div(style = "height: 30vh",
                selectInput(ns("annotation"), "Select Annotation:",
                  choices = c("","High impact","Moderate to high impact"),
                  selected = ""
                ),
                uiOutput(ns("consequences")),
                numericInput(ns("spliceai_score"), "SpliceAI score:", value = 0, min = 0, max = 1, step = 0.05)
              )
            )
          )
        ),
        column(2,
          bsCollapse(id = "insilico_filters_collapse", open = "insilico_filters_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "In silico filters", value = "insilico_filters_collapse_box",style = "info",
              div(style = "height: 30vh",
                numericInput(ns("revel"), "Enter Revel:", value = 0, min = 0, max = 1, step = 0.05),
                selectInput(ns("sift"), "Select Sift:",
                  choices = c("","Deleterious","Tolerated"),
                  selected = "", multiple = TRUE),
                selectInput(ns("polyphen"), "Select Polyphen:",
                  choices = c("","Probably damaging","Possibly damaging","Benign"),
                  selected = "", multiple = TRUE
                )
              )
            )
          )
        ),
        column(2,
          bsCollapse(id = "quality_collapse", open = "quality_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Call quality", value = "quality_collapse_box",style = "info",
              div(style = "height: 30vh",
                # selectInput(ns("pass_variants"), "Select Variant:",
                #   choices = c("","PASS only variants","All variants"), selected = ""
                # ),
                sliderInput(ns("genotype_quality"), label = "Genotype quality:",
                  min = 0, max = 100, ticks = FALSE, value = 0
                ),
                sliderInput(ns("allele_balance"), label = "Minimum Allele fraction:",
                  min = 0, max = 1,ticks = FALSE, value = 0
                ),
                materialSwitch(ns("affected_switch"), label = tags$b("Affected only:"))
              )
            )
          )
        ),
        column(2,
          bsCollapse(id = "frequency_collapse", open = "frequency_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Frequency", value = "frequency_collapse_box",style = "info",
              div(style = "height: 30vh",
                selectInput(ns("af"), "gnomADv4 AF:", choices= c(0, seq(0.0001, 0.0005, by = 0.0004),
                  0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 1), selected = 1
                ),
              )
            )
          )
        ),
      )
    )
  )
}

svOptsUI <- function(ns) {
  bsCollapse(id = "svs_collapse", open = "svs_collapse_box", multiple = TRUE,
    bsCollapsePanel(title = "SVs", value = "svs_collapse_box",style = "primary",
      fluidRow(
        column(2,
          bsCollapse(id = "sv_features_collapse", open = "sv_features_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Features", value = "sv_features_collapse_box",style = "info",
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
          )
        ),
        column(3,
          bsCollapse(id = "sv_consequence_collapse", open = "sv_consequence_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Annotation", value = "sv_consequence_collapse_box",style = "info",
              div(style = "height: 20vh",
                fluidRow(
                  column(4, uiOutput(ns("sv_relative_pos"))),
                  column(8, uiOutput(ns("sv_consequence")))
                )
              )
            )
          )
        ),
        column(3,
          bsCollapse(id = "sv_quality_collapse", open = "sv_quality_collapse_box", multiple = TRUE,
            bsCollapsePanel(title = "Call quality", value = "sv_quality_collapse_box",style = "info",
              div(style = "height: 20vh",
                fluidRow(
                  column(6,
                    selectInput(ns("sv_pass_variants"), "Select Variant:",
                      choices = c("","PASS only variants","All variants"),
                      selected = ""
                    ),
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
