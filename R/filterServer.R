#' Filter Tab Server
#' @param id Module ID
#' @export
#' @import shiny

source("R/filter_utility.R")
source("R/inheritance_utility.R")
source("R/filter_utility_legacy.R")

selectFiltersServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    phenos <- reactiveVal()

    observeEvent(input$apply, {
      ped <- pedigree()
      allele_counts <- getAlleleCounts(ped, input)
      cat("class of allele_counts:", class(allele_counts), "\n")
      cat("Allele counts:\n")
      print(allele_counts)

      # Legacy filtering function
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
        genotype_quality_value = input$genotype_quality,
        allele_balance_value = input$allele_balance,
        hpo_terms_list = phenos()
      )

      sv_filters <- list(
        inheritance_filter = input$inher,
        panelapp_filter = input$panelapp,
        annotation_filter = input$sv_conseq_checkboxes,
        sv_features = input$sv_features_checkboxes,
        min_svlen = input$min_svlen,
        max_svlen = input$max_svlen,
        genotype_quality_value = input$sv_genotype_quality,
        allele_balance_value = input$sv_allele_balance,
        af_value = as.numeric(input$sv_af),
        hpo_terms_list = phenos()
      )
      if (exists("processed_data", envir = .GlobalEnv)) {
        processed_data <- get("processed_data", envir = .GlobalEnv)
        cat("Applying legacy filter_dataset on processed_data with", nrow(processed_data), "rows\n")
        cat("class of processed_data:", class(processed_data), "\n")
        cat("colnames of processed_data:", paste(colnames(processed_data), collapse=", "), "\n")
        # browser()
        allele_counts_dt <- data.table(
          sample_id = names(allele_counts),
          allele_count = unlist(allele_counts, use.names = FALSE)
        )
        pedigree_dt <- rbindlist(ped, fill = TRUE)
        d <- processed_data
        cat("nrow(d):", nrow(d), "\n")
        filtered_data <- apply_filter_legacy(data=d,snv_filters=snv_filters, sv_filters=sv_filters,pedigree=pedigree_dt, allele_tab=allele_counts_dt, panel_app_genes=NULL, vep_consequences=shared_data$vep_consequences, phenotype_data=NULL)
        shared_data$legacy_snvs_data_filtered <- filtered_data$snv_filtered_data
        shared_data$legacy_svs_data_filtered <- filtered_data$sv_filtered_data
      } else {
        warning("processed_data not available yet in GlobalEnv")
      }
      # legacy end

      # new filtering function
      # snv_filtered_0 <- apply_filter_0(dt=shared_data$snvs_data, input$inher)
      # sv_filtered_0  <- apply_filter_0(dt=shared_data$svs_data, input$inher)
      # snv_filtered <- apply_filters(pedigree=ped, allele_counts=allele_counts, dt=snv_filtered_0, filters=snvf, type="snv", vep_consequences=shared_data$vep_map)
      # sv_filtered <- apply_filters(pedigree=ped, allele_counts=allele_counts, dt=sv_filtered_0, filters=svf, type="sv", vep_consequences=shared_data$vep_map)
      # shared_data$snvs_data_filtered <- snv_filtered
      # shared_data$svs_data_filtered  <- sv_filtered
      # showNotification(sprintf("snv_filtered to %s rows", nrow(snv_filtered)), type = "message")
      # showNotification(sprintf("sv_filtered to %s rows", nrow(sv_filtered)), type = "message")
      showNotification(sprintf("legacy_snvs_data_filtered to %s rows", nrow(shared_data$legacy_snvs_data_filtered)), type = "message")
      showNotification(sprintf("legacy_svs_data_filtered to %s rows", nrow(shared_data$legacy_svs_data_filtered)), type = "message")
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
      cat(sprintf("[homeServer] Pathogenicity filter chan`ged: %s\n", input$pathogenicity))
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
