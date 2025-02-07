igvServer <- function(id, dataset, snps_vcf_file, svs_vcf_file, bam_file, assembly, kinship) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Utility function for parsing regions
    parseRegions <- function(region_string) {
      regions <- strsplit(region_string, " ")[[1]]
      gr_list <- lapply(regions, function(region) {
        parts <- strsplit(region, "[:-]")[[1]]
        if (length(parts) != 3) {
          stop("Invalid region format. Use chr:start-end")
        }
        chr <- parts[1]
        start <- as.numeric(parts[2])
        end <- as.numeric(parts[3])
        GRanges(seqnames = chr, ranges = IRanges(start = start, end = end), strand = "*")
      })
      do.call(c, gr_list)
    }

    # Helper function to update the IGV viewer
    updateIgvViewer <- function(region_of_interest, assembly) {
      if (!is.null(region_of_interest) && region_of_interest != "") {
        genomeOptions <- parseAndValidateGenomeSpec(
          genomeName = assembly,
          initialLocus = region_of_interest,
          stockGenome = TRUE,
          dataMode = "localFiles"
        )
        output$igvShiny_0 <- renderIgvShiny({
          cat("--- starting renderIgvShiny\n")
          igvShiny(genomeOptions, displayMode = "SQUISHED")
        })
      }
      current_region(region_of_interest)
    }

    # Store the current region for use
    current_region <- reactiveVal(NULL)

    observe({
      if (is.null(current_region())) {
        genomeOptions <- parseAndValidateGenomeSpec(
          genomeName = assembly,
          initialLocus = "all",
          stockGenome = TRUE,
          dataMode = "localFiles"
        )
        output$igvShiny_0 <- renderIgvShiny({
          igvShiny(genomeOptions, displayMode = "SQUISHED")
        })
      }
    })

    observeEvent(input$coords_button, {
      print(input$igv_var_id)
      if (input$igv_var_id %in% dataset$ID) {

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
      } else {
        showNotification("Invalid variant ID", type = "error")
      }
    })

    # Reactive expression for condition
    shouldUpdateRegion <- reactive({
      current <- current_region()
      coords <- input$genome_coords
      if (is.null(current)) {
        !is.null(coords) && coords != ""
      } else {
        !is.null(coords) && coords != "" && coords != current
      }
    })

    # Validate genome coordinates and update IGV viewer
     observeEvent(input$genome_coords, {
       if (shouldUpdateRegion()) {
          coords <- trimws(input$genome_coords)
          pattern <- "^(chr[0-9XY]+)([:-]|\\s)([0-9]+)(([-]|\\s)([0-9]+))?$"
          if (grepl(pattern, coords)) {
            matches <- regmatches(coords, regexec(pattern, coords))[[1]]
            chr <- matches[2]
            start <- as.numeric(matches[4])
            if (matches[7] != "") {
              end <- as.numeric(matches[7])
            } else {
              start <- as.numeric(matches[4]) - 20
              end <- as.numeric(matches[4]) + 20
            }
            region_of_interest <- paste0(chr, ":", start, "-", end)
            updateIgvViewer(region_of_interest, assembly)
          } else {
            showNotification("Invalid coordinates: Expected 'chr:start-end' or 'chr start end'",
                             type = "error")
          }
       }
     })


    # Observe changes to `current_region` to load VCF tracks
    observeEvent(input$igvReady, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        region.GRanges <- parseRegions(region_of_interest)
        vcf1 <- readVcf(snps_vcf_file, genome = assembly, param = ScanVcfParam(which = region.GRanges))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SNVs/Indels VCF", vcf1)
        vcf2 <- readVcf(svs_vcf_file, genome = assembly, param = ScanVcfParam(which = region.GRanges))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SVs VCF", vcf2)
        lapply(pedigree_data$kinship, function(x) loadBAMTrack(x, sprintf("%s BAM",x)))
      }
    })

    # Utility function to load BAM track
    loadBAMTrack <- function(kinship_label, track_name) {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        region.GRanges <- parseRegions(region_of_interest)
        tags_to_extract <- c("PS", "HP")
        param <- ScanBamParam(which = region.GRanges, what = "seq",tag=tags_to_extract)
        current_bam <- unlist(bam_file[kinship==kinship_label])
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = track_name, data = bam, displayMode = "EXPANDED")
      }
    }

  })
}
