igvServer <- function(id, snps_vcf_file, svs_vcf_file, bam_file, assembly, kinship) {
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
    
    # Function to capitalise word
    capitalize_word <- function(word) {
      paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
    }
    
    # Dynamic buttons based on kinship
    output$dynamicButtons <- renderUI({
      kinship_labels <- c("proband", "mother", "father", "brother", "uncle")
      
      buttons <- lapply(seq_along(kinship_labels), function(i) {
        if (kinship_labels[i] %in% kinship) {
          tags$div(actionButton(ns(sprintf("add%sBAMTrackButton",capitalize_word(kinship_labels[i]))), sprintf("%s BAM",capitalize_word(kinship_labels[i]))), style = "margin-bottom: 10px;")
        }
      })
      tagList(buttons)
    })

    # Store the current region for use
    current_region <- reactiveVal(NULL)

    # Handle region search
    observeEvent(input$genome_coords_search, {
      region_of_interest <- input$genome_coords
      print(region_of_interest)
      current_region(input$genome_coords)
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
    
    # Add BAM track buttons for different kinship labels
    observeEvent(input$addProbandBAMTrackButton, { loadBAMTrack("proband", "Proband BAM") })
    observeEvent(input$addMotherBAMTrackButton, { loadBAMTrack("mother", "Mother BAM") })
    observeEvent(input$addFatherBAMTrackButton, { loadBAMTrack("father", "Father BAM") })
    observeEvent(input$addBrotherBAMTrackButton, { loadBAMTrack("brother", "Brother BAM") })
    observeEvent(input$addUncleBAMTrackButton, { loadBAMTrack("uncle", "Uncle BAM") })
  
    # Load SNV/Indel VCF Track
    observeEvent(input$snvs_vcf, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        region.GRanges <- parseRegions(region_of_interest)
        vcf <- readVcf(snps_vcf_file, genome = assembly, param = ScanVcfParam(which = region.GRanges))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SNVs/Indels VCF", vcf)
      }
    })

    # Load SV VCF Track
    observeEvent(input$svs_vcf, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        region.GRanges <- parseRegions(region_of_interest)
        vcf <- readVcf(svs_vcf_file, genome = assembly, param = ScanVcfParam(which = region.GRanges))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SVs VCF", vcf)
      }
    })

  })
}
