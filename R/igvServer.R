#' IGV Tab Server
#' 
#' @param id Module ID
#' @param shared_store A reactiveValues object to share data across modules
#' @param shared_rx A list of reactive values for cross-module communication
#' @export
#' @import shiny
#' @import igvShiny
igv_server <- function(id, shared_store, shared_rx) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Defaults (can be moved to config later)
    default_flanking <- 200L
    default_max_window <- 10000L
    assembly_val <- reactiveVal("hg38")
    custom_genome <- reactiveVal(NULL)
    use_custom_genome <- reactiveVal(FALSE)

    region <- reactiveVal(NULL)
    `%||%` <- function(x, y) if (is.null(x)) y else x

    make_locus <- function(chrom, pos, len) {
      #if input$igv_max_window is set, use that instead of default_max_window
      flanking <- input$igv_flanking %||% default_flanking
      max_window <- input$igv_max_window %||% default_max_window
      log_info(sprintf("flanking: %d  max_window: %d", flanking, max_window))
      pos <- as.numeric(pos)
      start <- max(1, pos - flanking)
      end   <- max(start, pos + len + flanking)
      if ((end - start) > max_window) {
        paste(
          paste0(chrom, ":", start, "-", (start + max_window)),
          paste0(chrom, ":", max(start, end - max_window), "-", end)
        )
      } else {
        paste0(chrom, ":", start, "-", end)
      }
    }

    parseRegion <- function(reg) {
      if (identical(reg, "all")) return(NULL)
      parts <- strsplit(reg, ":", fixed = TRUE)[[1]]
      if (length(parts) != 2) return(NULL)
      chr <- parts[1]
      se <- strsplit(parts[2], "-", fixed = TRUE)[[1]]
      if (length(se) != 2) return(NULL)
      start <- suppressWarnings(as.numeric(se[1]))
      end   <- suppressWarnings(as.numeric(se[2]))
      if (is.na(start) || is.na(end)) return(NULL)
      tryCatch({
        GenomicRanges::GRanges(seqnames = chr, ranges = IRanges::IRanges(start = start, end = end), strand = "*")
      }, error = function(e) NULL)
    }

    parseRegionList <- function(reg) {
      regs <- strsplit(trimws(reg), "[,[:space:]]+")[[1]]
      regs <- regs[nzchar(regs)]
      if (!length(regs)) return(NULL)
      gr_list <- lapply(regs, parseRegion)
      gr_list <- Filter(Negate(is.null), gr_list)
      if (!length(gr_list)) return(NULL)
      do.call(c, gr_list)
    }

    updateIgvViewer <- function(locus, assembly) {
      if (is.null(locus) || !nzchar(locus)) return()
      region(parseRegionList(locus))
      log_info(sprintf("[igvServer] Updating IGV viewer to locus: %s (assembly: %s)", locus, assembly))
      if (is.null(region())) {
        showNotification("Invalid locus specification", type = "error")
        return()
      }
      showNotification("Loading IGV...", duration = NULL, id = ns("notify_igv"), type = "message")
      use_custom_genome_flag <- use_custom_genome()
      genomeOptions <- NULL
      
      if (use_custom_genome_flag) {
        cg <- custom_genome()
        genomeOptions <- igvShiny::parseAndValidateGenomeSpec(
          genomeName   = assembly,
          initialLocus = locus,
          stockGenome  = FALSE,
          dataMode = "localFiles",
          fasta = cg$fasta,
          fastaIndex = cg$index
        )
      } else{
        genomeOptions <- igvShiny::parseAndValidateGenomeSpec(
          genomeName   = assembly,
          initialLocus = locus,
          stockGenome  = TRUE,
          dataMode     = "localFiles"
        )
      }
      output$igvShiny_0 <- igvShiny::renderIgvShiny({
        igvShiny::igvShiny(genomeOptions, displayMode = "SQUISHED")
      })
      removeNotification(ns("notify_igv"))
    }

    # ---- File validations (clear user errors, cheap checks) ----
    validate_vcf_or_notify <- function(path, label) {
      if (is.null(path) || !nzchar(path)) {
        showNotification(sprintf("%s path not provided", label), type = "error"); return(FALSE)
      }
      if (!file.exists(path)) {
        showNotification(sprintf("%s file missing: %s", label, path), type = "error"); return(FALSE)
      }
      idx <- paste0(path, ".tbi")
      if (!file.exists(idx)) {
        showNotification(sprintf("%s index (.tbi) missing: %s", label, idx), type = "error"); return(FALSE)
      }
      TRUE
    }

    validate_bam_or_notify <- function(path, label) {
      if (is.null(path) || !nzchar(path)) {
        showNotification(sprintf("BAM path not provided for %s", label), type = "error"); return(FALSE)
      }
      if (!file.exists(path)) {
        showNotification(sprintf("BAM file missing: %s", path), type = "error"); return(FALSE)
      }
      idx1 <- paste0(path, ".bai")
      idx2 <- sub("\\.bam$", ".bai", path)
      if (!file.exists(idx1) && !file.exists(idx2)) {
        showNotification(sprintf("BAM index (.bai) missing for: %s", path), type = "error"); return(FALSE)
      }
      TRUE
    }

    make_locus_from_alt <- function(alt) {
      # ALT format: N[chrX:12345[ or ]chrX:12345]N
      m <- regexec("[][](chr[^:]+):(\\d+)[][]", alt)
      matches <- regmatches(alt, m)
      if (length(matches) == 0 || length(matches[[1]]) < 3) {
        showNotification(sprintf("Cannot parse ALT field for locus: %s", alt), type = "error")
        return(NULL)
      }
      chrom <- matches[[1]][2]
      pos <- as.numeric(matches[[1]][3])
      locus <- make_locus(chrom, pos, 100)
      log_info(sprintf("[igvServer] Parsed locus from ALT %s: %s", alt, locus))
      locus
    }

    # ---- Load tracks when IGV is ready ----
    observeEvent(input$igvReady, {
      log_info("[igvServer] IGV is ready, loading tracks...")
      gr <- region(); req(gr)

      assembly <- assembly_val()
      igv_data <- shared_store$igv_data %||% list()
      snv_vcf  <- igv_data$snvs_vcf %||% NULL
      sv_vcf   <- igv_data$svs_vcf  %||% NULL

      # Load any configured VCFs
      load_vcf <- function(path, track_name) {
        if (!validate_vcf_or_notify(path, track_name)) return(invisible(NULL))
        nid <- ns(paste0("notify_", gsub("\\s+", "_", tolower(track_name))))
        showNotification(sprintf("Loading %s...", track_name), duration = NULL, id = nid, type = "message")
        vcf <- tryCatch({
          readVcf(path, genome = assembly, param = ScanVcfParam(which = gr))
        }, error = function(e) {
          showNotification(sprintf("Error loading %s: %s", track_name, e$message), type = "error"); NULL
        })
        if (!is.null(vcf)) {
          loadVcfTrack(session, id = ns("igvShiny_0"), trackName = track_name, vcf)
        }
        removeNotification(nid)
      }

      if (!is.null(snv_vcf)) load_vcf(snv_vcf, "SNVs/Indels VCF")
      if (!is.null(sv_vcf))  load_vcf(sv_vcf,  "SVs VCF")

      # Load BAMs for all samples
      smp <- shared_store$samples
      if (is.list(smp) && length(smp)) {
        for (i in seq_along(smp)) {
          s <- smp[[i]]
          bam_path <- s$bam %||% ""
          track_label <- if (!is.null(s$kinship) && nzchar(s$kinship)) {
            paste0(s$kinship, " BAM")
          } else if (!is.null(s$sample_id) && nzchar(s$sample_id)) {
            paste0(s$sample_id, " BAM")
          } else {
            paste0("Sample#", i, " BAM")
          }
          if (!validate_bam_or_notify(bam_path, track_label)) next

          nid <- ns(paste0("notify_bam_", i))
          showNotification("Loading BAM...", duration = NULL, id = nid, type = "message")
          param <- ScanBamParam(which = gr, what = "seq", tag = c("PS", "HP"))
          bam <- tryCatch(
            readGAlignments(bam_path, use.names = TRUE, param = param),
            error = function(e) { showNotification(sprintf("BAM error: %s", e$message), type = "error"); NULL }
          )
          if (!is.null(bam)) {
            loadBamTrackFromLocalData(session, id = ns("igvShiny_0"), trackName = track_label, data = bam, displayMode = "EXPANDED")
          }
          removeNotification(nid)
        }
      }

      region(NULL)
    }, ignoreInit = TRUE)

    # Entry point: react to clicked ID
    observeEvent(shared_rx$igv_version(), {
      if (shared_rx$igv_version() == 0L) return()
      igv_info <- shared_store$igv_data$igv_info %||% list()
      if (length(igv_info) == 0) {
        showNotification("IGV info not available", type = "error")
        return()
      }
      log_info(sprintf("[igvServer] Received IGV version bump: %d", shared_rx$igv_version()))
      updateTextInput(session, "igv_var_id", value = igv_info$ID)
      log_info(sprintf("[igvServer] IGV info: %s", paste(names(igv_info), unlist(igv_info), sep = "=", collapse = ", ")))
      if (igv_info$VAR_TYPE == "BND" || igv_info$VAR_TYPE == "TRA") {
        locus1 <- make_locus(igv_info$CHROM, igv_info$POS, 100)
        locus2 <- make_locus_from_alt(igv_info$ALT)
        locus <- paste(locus1, locus2, sep = ", ")
      } else {
        locus <- make_locus(igv_info$CHROM, igv_info$POS, igv_info$VAR_LENGTH %||% 100)
      }
      updateTextInput(session, "genome_coords", value = locus)
      updateIgvViewer(locus, assembly_val())
    }, ignoreInit = FALSE)

    # Manual coordinate entry
    observeEvent(input$update_button, {
      locus <- input$genome_coords
      # Convert all "chr:pos" (not already ranges) to "chr:pos-pos"
      locus <- gsub("([^: ]+):(\\d+)\\b(?!-)", "\\1:\\2-\\2", locus, perl = TRUE)
      updateTextInput(session, "genome_coords", value = locus)
      updateIgvViewer(locus, assembly_val())
    }, ignoreInit = TRUE)

    # Helper function: find a variant ID in a data.table
    find_variant_in_table <- function(dt, id, source_name) {
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      rows <- dt[ID == id]
      if (nrow(rows) == 0) return(NULL)

      if (nrow(rows) > 1) {
        showNotification(sprintf("Multiple entries found for %s ID %s; using the first one.", source_name, id), type = "warning")
      }
      rows[1, ]  # return the first row
    }

    observeEvent(input$search_button, {
      id <- input$igv_var_id
      if (is.null(id) || id == "") {
        showNotification("Please enter a variant ID.", type = "error")
        return()
      }
      # Try SNV first
      var_row <- find_variant_in_table(shared_store$data_for_data$SNV, id, "SNV")
      # If not found, try SV
      if (is.null(var_row)) {
        var_row <- find_variant_in_table(shared_store$data_for_data$SV, id, "SV")
      }
      if (is.null(var_row)) {
        showNotification(sprintf("Variant ID not found: %s", id), type = "error")
        return()
      }
      if (is.null(var_row$VAR_TYPE)){
        showNotification(sprintf("VAR_TYPE missing for variant ID: %s", id), type = "error")
        return()
      }
      log_info(sprintf("[igvServer] Found variant %s at %s:%d", id, var_row$CHROM, var_row$POS))
      if (var_row$VAR_TYPE == "BND" || var_row$VAR_TYPE == "TRA") {
        locus1 <- make_locus(var_row$CHROM, var_row$POS, 100)
        locus2 <- make_locus_from_alt(var_row$ALT)
        locus <- paste(locus1, locus2, sep = ", ")
      } else{
        locus <- make_locus(var_row$CHROM, var_row$POS, var_row$VAR_LENGTH %||% 0)
      }
      updateTextInput(session, "genome_coords", value = locus)

      updateIgvViewer(locus, assembly_val())
    }, ignoreInit = TRUE)

    observeEvent(shared_rx$data_version(), {
      log_info(sprintf("[igvServer] Data version updated: %d", shared_rx$data_version()))
      default_assembly <- shared_store$igv_data$igv_genome %||% "hg38"
      assembly_val(default_assembly)
      custom_genome(shared_store$igv_data$custom_genome %||% NULL)
      use_custom_genome(shared_store$igv_data$use_custom_genome %||% FALSE)
      log_info(sprintf("[igvServer] IGV genome set to: %s", default_assembly))
    }, ignoreInit = TRUE)

  })
}

# igvShiny::get_css_genomes()
# Supported genomes
#  [1] "hs1"             "chm13v1.1"       "hg38"            "hg38_1kg"       
#  [5] "hg19"            "hg18"            "mm39"            "mm10"           
#  [9] "mm9"             "rn7"             "rn6"             "gorGor6"        
# [13] "gorGor4"         "panTro6"         "panTro5"         "panTro4"        
# [17] "macFas5"         "GCA_011100615.1" "panPan2"         "canFam6"        
# [21] "canFam5"         "canFam4"         "canFam3"         "bosTau9"        
# [25] "bosTau8"         "susScr11"        "galGal6"         "GCF_016699485.2"
# [29] "danRer11"        "danRer10"        "ce11"            "dm6"            
# [33] "dm3"             "dmel_r5.9"       "sacCer3"         "ASM294v2"       
# [37] "ASM985889v3"     "tair10"          "GCA_003086295.2" "GCF_001433935.1"
# [41] "NC_016856.1"     "GCA_000182895.1"