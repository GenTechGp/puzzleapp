#' QC Plots UI
#' @param id Module id
#' @return Shiny UI elements
#' @importFrom shiny NS tagList tags
#' @export
qcPlots <- function(id) {
  ns <- NS(id)

  tagList(
    tags$iframe(
      id = ns("coverage_iframe"),
      src = "",
      style = "width:100%; height:1500px; border:none;"
    ),

    tags$iframe(
      id = ns("somalier_iframe"),
      src = "",
      style = "width:100%; height:1000px; border:none;"
    ),

    ## --- JS to just update src ---
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateIframeSrc', function(msg) {
        var iframe = document.getElementById(msg.id);
        if (iframe) {
          iframe.src = msg.src;
        }
      });
    "))
  )
}

#' QC Plots Server
#' @param id Module id
#' @param shared_store Shared values list
#' @param shared_rx Shared reactive values list
#' @return Shiny server logic
#' @importFrom shiny moduleServer observe req
#' @export
qcPlotsServer <- function(id, shared_store, shared_rx) {
  moduleServer(
    id,
    function(input, output, session) {

      observe({
        req(shared_rx$data_version())
        req(shared_store$html$coverage_path,
            shared_store$html$somalier_path)

        coverage_path  <- shared_store$html$coverage_path
        somalier_path  <- shared_store$html$somalier_path

        log_info(sprintf("coverage_path: %s\n", coverage_path))
        log_info(sprintf("somalier_path: %s\n", somalier_path))

        ts_coverage <- if (file.exists(coverage_path)) {
          as.integer(file.info(coverage_path)$mtime)
        } else {
          as.integer(Sys.time())
        }

        ts_somalier <- if (file.exists(somalier_path)) {
          as.integer(file.info(somalier_path)$mtime)
        } else {
          as.integer(Sys.time())
        }

        session$sendCustomMessage(
          "updateIframeSrc",
          list(
            id  = session$ns("coverage_iframe"),
            src = paste0(coverage_path, "?v=", ts_coverage)
          )
        )

        session$sendCustomMessage(
          "updateIframeSrc",
          list(
            id  = session$ns("somalier_iframe"),
            src = paste0(somalier_path, "?v=", ts_somalier)
          )
        )
      })
    }
  )
}
