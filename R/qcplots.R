#' QC Plots UI
#' @param id Module id
#' @return Shiny UI elements
#' @importFrom shiny NS tagList tags
#' @export
qcPlots <- function(id) {
  ns <- NS(id)

  tagList(
    tags$iframe(
      id = ns("somalier_iframe"),
      src = "",
      style = "width:100%; border:none; display:none;"
    ),
    tags$iframe(
      id = ns("coverage_iframe"),
      src = "",
      style = "width:100%; border:none; display:none;"
    ),

    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateIframe', function(msg) {
        var iframe = document.getElementById(msg.id);
        if (!iframe) return;

        if (msg.src && msg.src !== '') {
          iframe.src = msg.src;
          iframe.removeAttribute('srcdoc');
          iframe.style.display = 'block';
          if (msg.height) iframe.style.height = msg.height;
        } else if (msg.html) {
          iframe.src = '';
          iframe.srcdoc = msg.html;
          iframe.style.display = 'block';
          if (msg.height) iframe.style.height = msg.height;
        } else {
          iframe.src = '';
          iframe.removeAttribute('srcdoc');
          iframe.style.display = 'none';
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
  moduleServer(id, function(input, output, session) {

    observe({
      req(shared_rx$data_version())
      cat("Updating QC plots iframes...\n")

      ## Coverage
      if (!is.null(shared_store$html$coverage_path) &&
          shared_store$html$coverage_path != "") {

        ts_coverage <- if (file.exists(shared_store$html$coverage_path)) {
          as.integer(file.info(shared_store$html$coverage_path)$mtime)
        } else {
          as.integer(Sys.time())
        }

        cat("Coverage path:", shared_store$html$coverage_path, "\n")

        session$sendCustomMessage(
          "updateIframe",
          list(
            id     = session$ns("coverage_iframe"),
            src    = paste0(shared_store$html$coverage_path, "?v=", ts_coverage),
            height = "1500px"
          )
        )
      } else {
        cat("No valid coverage path found.\n")

        session$sendCustomMessage(
          "updateIframe",
          list(
            id     = session$ns("coverage_iframe"),
            html   = "<p style='padding:20px; color:#666; font-size:16px;'>
                        Coverage QC plot not found.
                      </p>",
            height = "80px"
          )
        )
      }

      ## Somalier
      if (!is.null(shared_store$html$somalier_path) &&
          shared_store$html$somalier_path != "") {

        ts_somalier <- if (file.exists(shared_store$html$somalier_path)) {
          as.integer(file.info(shared_store$html$somalier_path)$mtime)
        } else {
          as.integer(Sys.time())
        }

        cat("Somalier path:", shared_store$html$somalier_path, "\n")

        session$sendCustomMessage(
          "updateIframe",
          list(
            id     = session$ns("somalier_iframe"),
            src    = paste0(shared_store$html$somalier_path, "?v=", ts_somalier),
            height = "1000px"
          )
        )
      } else {
        cat("No valid somalier path found.\n")

        session$sendCustomMessage(
          "updateIframe",
          list(
            id     = session$ns("somalier_iframe"),
            html   = "<p style='padding:20px; color:#666; font-size:16px;'>
                        Somalier QC plot not found.
                      </p>",
            height = "80px"
          )
        )
      }
    })
  })
}
