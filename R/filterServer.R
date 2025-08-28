#' Filter Tab Server
#' @param id Module ID
#' @export
#' @import shiny

source("R/filter_utility.R")
source("R/inheritance_utility.R")

selectFiltersServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    snv_filters <- reactive({
      list(
        # Frequency
        af            = input$af,
        # Quality
        pass_only     = isTRUE(input$pass_variants == "PASS only variants"),
        affected_only = isTRUE(input$affected_switch),
        genotype_quality = input$genotype_quality,
        allele_balance   = input$allele_balance,
        # Annotation
        conseq_checkboxes = input$conseq_checkboxes,
        spliceai_score    = input$spliceai_score,
        # Pathogenicity
        clinvar_checkboxes = input$clinvar_checkboxes,
        # In silico
        revel           = input$revel,
        alpha_missense  = input$alpha_missense,
        sift            = input$sift,
        polyphen        = input$polyphen
      )
    })

    sv_filters <- reactive({
      list(
        # Frequency
        af            = input$sv_af,
        # Quality
        pass_only     = isTRUE(input$sv_pass_variants == "PASS only variants"),
        affected_only = isTRUE(input$sv_affected_switch),
        genotype_quality = input$sv_genotype_quality,
        allele_balance   = input$sv_allele_balance,
        # Annotation
        conseq_checkboxes = input$sv_conseq_checkboxes,
        # SV features
        sv_features_checkboxes = input$sv_features_checkboxes,
        sv_min_len = input$min_svlen,
        sv_max_len = input$max_svlen
      )
    })

    samples <- reactive({ shared_data$samples })
    pedigree <- reactive({ convert_samples_to_pedigree(samples()) })

    alleleCustomTable <- function(ns, pedigree) {
      tagList(
        tags$table(
          tags$thead(tags$tr(tags$th(style = "padding: 0.5em 1em; min-width: 120px;", "Sample ID"),tags$th(style = "padding: 0.5em 1em; min-width: 120px;", "Allele Count"))),
          tags$tbody(
            lapply(pedigree, function(sample) {
              sid <- sample$sample_id
              radio_id <- ns(paste0("allele_", sid))
              tags$tr(
                tags$td(style = "vertical-align: baseline; padding-right: 1em;", sid),
                tags$td(radioButtons(inputId = radio_id,label = NULL,choices = c("None" = "", "0", "0-1", "1", "1-2", "2"),inline = TRUE,selected = "")))}))))
    }

    output$allele_ui <- renderUI({
      req(input$inher)
      ped <- pedigree()  # Reactive
      if (input$inher == "") return(NULL)
      if (input$inher == "Custom") {
        # Render input radio buttons for custom allele counts
        alleleCustomTable(ns, ped)
      } else {
        # Compute allele counts (named list) and convert to data.frame for display
        counts <- compute_allele_table(ped, input$inher)
        if (length(counts) == 0) return(NULL)
        tbl <- data.frame(
          Sample_ID = names(counts),
          Allele_Count = unlist(counts, use.names = FALSE),
          stringsAsFactors = FALSE
        )
        DT::datatable(tbl, rownames = FALSE, options = list(dom = 't', paging = FALSE))
      }
    })
    outputOptions(output, "allele_ui", suspendWhenHidden = FALSE)

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
          counts <- compute_allele_table(pedigree, input$inher)  # named list
          cat("Allele table counts:\n")
          for (sid in names(counts)) {
            cat(sprintf("  %s: %s\n", sid, counts[[sid]]))
          }
        }
      }
      counts
    }

    observeEvent(input$apply, {
      snvf <- snv_filters()
      svf  <- sv_filters()
      ped <- pedigree()
      allele_counts <- getAlleleCounts(ped, input)
      snv_filtered <- apply_filters(pedigree=ped, allele_counts=allele_counts, dt=shared_data$snvs_data, filters=snvf, type="snv", vep_consequences=shared_data$vep_consequences)
      sv_filtered <- apply_filters(pedigree=ped, allele_counts=allele_counts, dt=shared_data$svs_data, filters=svf, type="sv", vep_consequences=shared_data$vep_consequences)
      #cat("class of snv_filtered:", class(snv_filtered), "\n")
      shared_data$snvs_data_filtered <- snv_filtered
      shared_data$svs_data_filtered  <- sv_filtered
      showNotification(sprintf("snv_filtered to %s rows", nrow(snv_filtered)), type = "message")
      showNotification(sprintf("sv_filtered to %s rows", nrow(sv_filtered)), type = "message")
      # print(allele_counts)

    })

    updateAnnotationSelection <- function(selected_option, checkbox_id, session) {
      cat(sprintf("Annotation selection changed: %s\n", selected_option))
      annotation_map <- list(
        "High impact" = c("Stop gained", "Start lost", "Stop lost", "Splice variant", "Frameshift variant"),
        "Moderate to high impact" = c("Stop gained", "Start lost", "Stop lost", "Splice variant",
                                      "Frameshift variant", "Missense variant", "In-frame variant")
      )
      select <- annotation_map[[selected_option]]
      if (is.null(select)) select <- character(0)
      updateCheckboxGroupInput(session, checkbox_id, selected = select)
    }

    observeEvent(input$annotation, {
      updateAnnotationSelection(input$annotation, "conseq_checkboxes", session)
    })

    observeEvent(input$sv_annotation, {
      updateAnnotationSelection(input$sv_annotation, "sv_conseq_checkboxes", session)
    })

    observeEvent(input$pathogenicity, {
      cat(sprintf("[homeServer] Pathogenicity filter changed: %s\n", input$pathogenicity))
      pathogenicity_map <- list(
        "Pathogenic/Likely pathogenic" = c("Pathogenic", "Likely pathogenic"),
        "Not benign" = c("Pathogenic", "Likely pathogenic", "VUS", "Conflicting")
      )
      select <- pathogenicity_map[[input$pathogenicity]]
      if (is.null(select)) select <- character(0)
      updateCheckboxGroupInput(session, "clinvar_checkboxes", selected = select)
    })

  })


}
