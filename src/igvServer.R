igvServer <- function(id,input, output, session,snps_vcf_file,svs_vcf_file,bam_file,assembly,chain_file,kinship) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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

    # # Generate dynamic buttons based on kinship vector
    # output$dynamicButtons <- renderUI({
    #   buttons <- tagList()
    #   if ("proband" %in% kinship) {
    #     buttons <- tagAppendChild(buttons,
    #                               tags$div(actionButton(ns("addProbandBAMTrackButton"), "Proband BAM"),
    #                                        style = "margin-bottom: 10px;"))
    #   }
    #   if ("mother" %in% kinship) {
    #     buttons <- tagAppendChild(buttons,
    #                               tags$div(actionButton(ns("addMotherBAMTrackButton"), "Mother BAM"),
    #                                        style = "margin-bottom: 10px;"))
    #   }
    #   if ("father" %in% kinship) {
    #     buttons <- tagAppendChild(buttons,
    #                               tags$div(actionButton(ns("addFatherBAMTrackButton"), "Father BAM"),
    #                                        style = "margin-bottom: 10px;"))
    #   }
    #   buttons
    # })

    output$dynamicButtons <- renderUI({
      buttons <- tagList()
      if ("proband" %in% kinship) {
        buttons <- tagAppendChild(buttons,
                                  tags$div(actionButton(ns("addProbandBAMTrackButton"), "Proband BAM"),
                                           style = "margin-bottom: 10px;"))
      }
      if ("mother" %in% kinship) {
        buttons <- tagAppendChild(buttons,
                                  tags$div(actionButton(ns("addMotherBAMTrackButton"), "Mother BAM"),
                                           style = "margin-bottom: 10px;"))
      }
      if ("father" %in% kinship) {
        buttons <- tagAppendChild(buttons,
                                  tags$div(actionButton(ns("addFatherBAMTrackButton"), "Father BAM"),
                                           style = "margin-bottom: 10px;"))
      }
      if ("brother" %in% kinship) {
        buttons <- tagAppendChild(buttons,
                                  tags$div(actionButton(ns("addBrotherBAMTrackButton"), "Brother BAM"),
                                           style = "margin-bottom: 10px;"))
      }
      if ("uncle" %in% kinship) {
        buttons <- tagAppendChild(buttons,
                                  tags$div(actionButton(ns("addUncleBAMTrackButton"), "Uncle BAM"),
                                           style = "margin-bottom: 10px;"))
      }
      buttons
    })


    current_region <- reactiveVal(NULL)
    # chr1:1000-2000 chr1:3000:4000

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

    observeEvent(input$addProbandBAMTrackButton, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        print(region_of_interest)
        region.GRanges <- parseRegions(region_of_interest)
        tags_to_extract <- c("PS", "HP")
        param <- ScanBamParam(which = region.GRanges, what = "seq",tag=tags_to_extract)
        current_bam <- unlist(bam_file[kinship=="proband"])
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = "Proband BAM", data = bam, displayMode = "EXPANDED")
      }
    })

    observeEvent(input$addMotherBAMTrackButton, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        print(region_of_interest)
        region.GRanges <- parseRegions(region_of_interest)
        tags_to_extract <- c("PS", "HP")
        param <- ScanBamParam(which = region.GRanges, what = "seq",tag=tags_to_extract)
        current_bam <- unlist(bam_file[kinship=="mother"])
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = "Mother BAM", data = bam, displayMode = "EXPANDED")
      }
    })

    observeEvent(input$addFatherBAMTrackButton, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        print(region_of_interest)
        region.GRanges <- parseRegions(region_of_interest)
        tags_to_extract <- c("PS", "HP")
        param <- ScanBamParam(which = region.GRanges, what = "seq",tag=tags_to_extract)
        current_bam <- unlist(bam_file[kinship=="father"])
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = "Father BAM", data = bam, displayMode = "EXPANDED")
      }
    })

    # Observe the Brother BAM button click event
    observeEvent(input$addBrotherBAMTrackButton, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        print(region_of_interest)
        region.GRanges <- parseRegions(region_of_interest)
        tags_to_extract <- c("PS", "HP")
        param <- ScanBamParam(which = region.GRanges, what = "seq", tag = tags_to_extract)
        current_bam <- unlist(bam_file[kinship == "brother"])
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = "Brother BAM", data = bam, displayMode = "EXPANDED")
      }
    })

    # Observe the Uncle BAM button click event
    observeEvent(input$addUncleBAMTrackButton, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        print(region_of_interest)
        region.GRanges <- parseRegions(region_of_interest)
        tags_to_extract <- c("PS", "HP")
        param <- ScanBamParam(which = region.GRanges, what = "seq", tag = tags_to_extract)
        current_bam <- unlist(bam_file[kinship == "uncle"])
        bam <- readGAlignments(current_bam, use.names = TRUE, param = param)
        loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = "Uncle BAM", data = bam, displayMode = "EXPANDED")
      }
    })

    observeEvent(input$snvs_vcf, {
      region_of_interest <- current_region()
      if (!is.null(region_of_interest)) {
        region.GRanges <- parseRegions(region_of_interest)
        vcf <- readVcf(snps_vcf_file, genome = assembly, param = ScanVcfParam(which = region.GRanges))
        loadVcfTrack(session, id = ns("igvShiny_0"), trackName = "SNVs/Indels VCF", vcf)
      }
    })

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