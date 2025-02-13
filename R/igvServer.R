igvServer <- function(id, dataset, snps_vcf_file, svs_vcf_file, bam_file, assembly, kinship) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Parse input coordinates
    parseCoords <- function(co) {
      p <- "^(chr[0-9XY]+)([:-]|\\s)([0-9]+)(([-]|\\s)([0-9]+))?$"
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
      print(do.call(c, gr_list))
      do.call(c, gr_list)
    }

    # Helper function to update the IGV viewer
    updateIgvViewer <- function(reg, assembly) {
      if (is.null(reg) || reg == "") {
        return()
      }
      region(parseRegionList(reg))
      genomeOptions <- parseAndValidateGenomeSpec(
        genomeName = assembly,
        initialLocus = reg,
        stockGenome = TRUE,
        dataMode = "localFiles"
      )
      output$igvShiny_0 <- renderIgvShiny({
        igvShiny(genomeOptions, displayMode = "SQUISHED")
      })
    }

    updateIgv <- function(co) {
      ct <- trimws(co)
      if (is.null(ct) || ct == "") {
        updateIgvViewer("all", assembly)
        return()
      }
      cp <- parseCoords(co)
      req(!is.null(cp))
      updateIgvViewer(cp, assembly)
    }

    # Utility function to load BAM track
    loadBAMTrack <- function(kinship_label, track_name) {
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

    init_coords <- isolate(input$genome_coords)
    region <- reactiveVal()
    updateIgv(isolate(init_coords))

    observeEvent(input$coords_button, {
      req(input$igv_var_id)
      if (!(input$igv_var_id %in% dataset$ID)) {
        showNotification("Invalid variant ID", type = "error")
        return()
      }

      x <- dataset[dataset$ID == input$igv_var_id, ]
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
      updateTextInput(session, inputId = "genome_coords", value = coords)
      coords_p <- parseCoords(coords)
      req(!is.null(coords_p))
      updateIgvViewer(coords_p, assembly)
    })

    # Validate genome coordinates and update IGV viewer
    observeEvent(input$coords_manual_button, updateIgv(input$genome_coords))

    # Observe changes to `region` to load VCF tracks
    observeEvent(input$igvReady, {
      req(region())
      if (file.exists(snps_vcf_file)) {
        showNotification("Loading SNVs/Indels...", duration = NULL,
                         id = ns("notify_snv"), type = "message")
        vcf1 <- readVcf(snps_vcf_file, genome = assembly, param = ScanVcfParam(which = region()))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SNVs/Indels VCF", vcf1)
        removeNotification(ns("notify_snv"))
      } else {
        showNotification("File missing: SNVs/Indels VCF", type = "error")
      }
      if (file.exists(svs_vcf_file)) {
        showNotification("Loading SVs...", duration = NULL,
                         id = ns("notify_sv"), type = "message")
        vcf2 <- readVcf(svs_vcf_file, genome = assembly, param = ScanVcfParam(which = region()))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SVs VCF", vcf2)
        removeNotification(ns("notify_sv"))
      } else {
        showNotification("File missing: SVs VCF", type = "error")
      }
      lapply(pedigree_data$kinship, function(x) loadBAMTrack(x, sprintf("%s BAM",x)))
    })
  })
}
