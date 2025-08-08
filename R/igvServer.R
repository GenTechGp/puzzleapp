igvServer <- function(id, dataset, snps_vcf_file, svs_vcf_file, bam_file, assembly, kinship, selected_igv_id) {
  moduleServer(id, function(input, output, session) {

    cat(sprintf("[igvServer] Module initialized\n"))
    
    ns <- session$ns

    # Parse input coordinates
    parseCoords <- function(co) {
      p <- "^(chr[0-9XYM]+)([:-]|\\s)([0-9]+)(([-]|\\s)([0-9]+))?$"
      if (!grepl(p, co)) {
        showNotification("Invalid coordinates: Expected chr:start-end", type = "error")
        return(NULL)
      }
      m <- regmatches(co, regexec(p, co))[[1]]
      chr <- m[2]
      start <- as.numeric(m[4])
      if (m[7] != "") {
        end <- as.numeric(m[7])
      } else {
        start <- as.numeric(m[4]) - 20
        end <- as.numeric(m[4]) + 20
      }
      if (start > end) {
        showNotification("Invalid coordinates: start > end", type = "error")
        return(NULL)
      }
      return(paste0(chr, ":", start, "-", end))
    }

    parseCoordsList <- function(co) {
      cl <- strsplit(co, " ")[[1]]
      cps <- vector(mode = "character", length = length(cl))
      for (i in seq(1, length(cl))) {
        cp <- parseCoords(cl[i])
        if (is.null(cp)) {
          if (i > 1) {
            showNotification(paste("Invalid coordinates at position", i), type = "error")
          }
          return(NULL)
        }
        cps[i] <- cp
      }
      return(paste(cps, collapse = " "))
    }

    # Parse a single region
    parseRegion <- function(reg) {
      if (reg == "all") {
        return(NULL)
      }
      p1 <- strsplit(reg, ":")[[1]]
      chr <- p1[1]
      p2 <- strsplit(p1[2], "-")[[1]]
      start <- as.numeric(p2[1])
      end <- as.numeric(p2[2])
      tryCatch({
        GRanges(seqnames = chr, ranges = IRanges(start = start, end = end), strand = "*")
      }, error = function(e) {
        showNotification(e$message, type = "error")
        return(NULL)
      })
    }

    # Parse regions for loading igv tracks
    parseRegionList <- function(reg) {
      regs <- strsplit(reg, " ")[[1]]
      gr_list <- lapply(regs, parseRegion)
      #print(do.call(c, gr_list))
      do.call(c, gr_list)
    }

    # Helper function to update the IGV viewer
    updateIgvViewer <- function(reg, assembly) {
      if (is.null(reg) || reg == "") {
        return()
      }
      cat(sprintf("[igvServer] Updating IGV Viewer: %s\n", reg))
      region(parseRegionList(reg))
      showNotification("Loading IGV...", duration = NULL,
                       id = ns("notify_igv"), type = "message")
      genomeOptions <- parseAndValidateGenomeSpec(
        genomeName = assembly,
        initialLocus = reg,
        stockGenome = TRUE,
        dataMode = "localFiles"
      )
      output$igvShiny_0 <- renderIgvShiny({
        igvShiny(genomeOptions, displayMode = "SQUISHED")
      })
      removeNotification(ns("notify_igv"))
    }

    updateIgv <- function(co) {
      ct <- trimws(co)
      if (is.null(ct) || ct == "") {
        cat("[igvServer] Initializing IGV with default coordinates\n")
        updateIgvViewer("all", assembly)
        return()
      }
      cat(sprintf("[igvServer] Parsing coordinates: %s\n", co))
      cp <- parseCoordsList(co)
      updateIgvViewer(cp, assembly)
    }

    # Utility function to load BAM track
    loadBAMTrack <- function(kinship_label, track_name) {
      cat(sprintf("[igvServer] Loading BAM track: %s\n", kinship_label))
      tags_to_extract <- c("PS", "HP")
      param <- ScanBamParam(which = region(), what = "seq",tag=tags_to_extract)
      current_bam <- unlist(bam_file[kinship==kinship_label])
      if (file.exists(current_bam)) {
        showNotification("Loading BAM...", duration = NULL,
                         id = ns("notify_bam"), type = "message")
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = track_name, data = bam, displayMode = "EXPANDED")
        removeNotification(ns("notify_bam"))
      } else {
        showNotification("File missing: BAM", type = "error")
      }
    }

    region <- reactiveVal()
    # igv_id <- isolate(input$igv_var_id)
    # if (is.null(igv_id) || igv_id == "") {
    #   init_coords <- isolate(input$genome_coords)
    #   updateIgv(init_coords)
    # }
    # 
    
    observe({
      shinyjs::enable(ns("coords_button"))
    })
    
    #igv_trigger <- reactiveVal(NULL)
    
    # observeEvent(selected_igv_id(),{
    #   clicked_id <- as.character(selected_igv_id())
    #   cat(sprintf("[igvServer] ID clicked: %s\n",clicked_id))
    #   updateTextInput(session, inputId = "igv_var_id", value = clicked_id)
    #   igv_trigger(clicked_id)
    # },ignoreInit = TRUE)
    # 
    # # Ensure button is clicked **after** igv_var_id is updated
    # observeEvent(igv_trigger(), {
    #   print("test")
    #   req(input$igv_var_id)
    #   igv_id <- input$igv_var_id
    #   clicked_id <- selected_igv_id()
    #   if (input$igv_var_id == igv_trigger()) {
    #     cat(sprintf("[igvServer] Automatically clicking on search button\n"))
    #     selected_igv_id(NULL)
    #     igv_trigger(NULL)
    #     shinyjs::click("coords_button")
    #   }
    # },ignoreInit = TRUE)
    
    pending_igv_id <- reactiveVal(NULL)
    
    observeEvent(selected_igv_id(), {
      clicked_id <- as.character(selected_igv_id())
      cat(sprintf("[igvServer] ID clicked: %s\n", clicked_id))
      pending_igv_id(clicked_id)  # flag the pending value
      updateTextInput(session, "igv_var_id", value = clicked_id)
      selected_igv_id(NULL)
    })
    
    observeEvent(input$igv_var_id, {
      if (!is.null(pending_igv_id()) && input$igv_var_id == pending_igv_id()) {
        cat("[igvServer] input$igv_var_id updated — clicking search button\n")
        shinyjs::click("coords_button")
        pending_igv_id(NULL)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$coords_button, {
      req(input$igv_var_id)
      selected_id <- input$igv_var_id
      if (!(selected_id %in% dataset$ID)) {
        cat("[igvServer] Error: Invalid variant ID\n")
        showNotification("Invalid variant ID", type = "error")
        return()
      }
      cat(sprintf("[igvServer] Variant selected: %s\n", selected_id))

      x <- dataset[dataset$ID == selected_id, ]
      chrom <- x$CHROM
      pos <- x$POS
      len <- x$VAR_LENGTH
      flanking <- input$igv_flanking
      max_window <- input$igv_max_window

      start <- pos - flanking
      end <- pos + len + flanking

      if ((end - start) > max_window) {
        split_start <- paste0(chrom, ":", start, "-", (start + max_window))
        split_end <- paste0(chrom, ":", (end - max_window), "-", end)
        coords <- paste(split_start, split_end)
      } else {
        coords <- paste0(chrom, ":", start, "-", end)
      }
      cat(sprintf("[igvServer] Updating IGV with new coordinates: %s\n", coords))
      updateTextInput(session, inputId = "genome_coords", value = coords)
      coords_p <- parseCoordsList(coords)
      updateIgvViewer(coords_p, assembly)
    }, ignoreInit = TRUE)

    # Validate genome coordinates and update IGV viewer
    observeEvent(input$coords_manual_button,{
      cat("[igvServer] Manual genome coordinates update triggered\n")
      updateIgv(input$genome_coords)
    }, ignoreInit = TRUE)
    
    # Observe changes to `region` to load VCF tracks
    observeEvent(input$igvReady, {
      req(region())
      if (file.exists(snps_vcf_file)) {
        showNotification("Loading SNVs/Indels...", duration = NULL,
                         id = ns("notify_snv"), type = "message")
        vcf1 <- tryCatch(
          {
            readVcf(snps_vcf_file, genome = assembly, param = ScanVcfParam(which = region()))
          },
          error = function(e) {
            showNotification("Error loading SNVs/Indels: Region may not be in VCF", type = "error")
            return(NULL)  # Return NULL to avoid further processing
          }
        )
        if (!is.null(vcf1)) {
          cat("[igvServer] Loading SNVs/Indels VCF track\n")
          loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SNVs/Indels VCF", vcf1)
        }
        removeNotification(ns("notify_snv"))
      } else {
        showNotification("File missing: SNVs/Indels VCF", type = "error")
      }
      if (file.exists(svs_vcf_file)) {
        showNotification("Loading SVs...", duration = NULL,
                         id = ns("notify_sv"), type = "message")
        vcf2 <- tryCatch(
          {
            readVcf(svs_vcf_file, genome = assembly, param = ScanVcfParam(which = region()))
          },
          error = function(e) {
            showNotification("Error loading SVs: Region may not be in VCF", type = "error")
            return(NULL)
          }
        )
        if (!is.null(vcf2)) {
          cat("[igvServer] Loading SVs VCF track\n")
          loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SVs VCF", vcf2)
        }
        removeNotification(ns("notify_sv"))
      } else {
        showNotification("File missing: SVs VCF", type = "error")
      }
      lapply(pedigree_data$kinship, function(x) loadBAMTrack(x, sprintf("%s BAM",x)))
      region(NULL)
    }, ignoreInit = TRUE)
    
  })
}
