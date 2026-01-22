#' @importFrom shiny
#'   NS tagList tags HTML
#'   textOutput verbatimTextOutput renderText renderUI
#'   selectInput selectizeInput checkboxGroupInput radioButtons
#'   numericInput sliderInput textInput checkboxInput actionButton
#'   downloadButton downloadHandler
#'   updateSelectInput updateSelectizeInput updateCheckboxGroupInput
#'   updateCheckboxInput updateRadioButtons updateSliderInput
#'   updateTextInput updateNumericInput
#'   moduleServer reactiveVal reactive eventReactive
#'   observe observeEvent isolate invalidateLater
#'   showModal modalDialog modalButton showNotification
#'   uiOutput fluidRow column div span h4 br
#'   req getDefaultReactiveDomain onStop
#'   runApp
#'
#' @importFrom shinybusy add_busy_spinner
#' 
#' @importFrom DT datatable renderDT dataTableProxy replaceData DTOutput updateSearch
#'
#' @importFrom htmlwidgets JS
#'
#' @importFrom htmltools strong singleton
#'
#' @importFrom jsonlite toJSON fromJSON
#'
#' @importFrom data.table
#'   data.table as.data.table copy is.data.table haskey key
#'   setkey set setDT setcolorder rbindlist fread fwrite tstrsplit
#'   fifelse := setnames melt %chin%
#'
#' @importFrom stringr str_extract str_remove
#'
#' @importFrom tools file_path_sans_ext file_ext
#'
#' @importFrom lgr
#'   get_log_levels get_logger
#'   AppenderConsole LayoutFormat
#'   AppenderFile    LayoutJson
#'
#' @importFrom yaml read_yaml
#'
#' @importFrom lobstr obj_addr
#'
#' @importFrom utils packageVersion capture.output modifyList read.delim str write.table read.table
#'
#' @importFrom stats setNames density
#' 
#' @importFrom bit64 as.integer64
#' 
#' @importFrom igvShiny igvShinyOutput renderIgvShiny parseAndValidateGenomeSpec loadVcfTrack loadBamTrackFromLocalData
#' 
#' @importFrom GenomicRanges GRanges
#' 
#' @importFrom IRanges IRanges
#' 
#' @importFrom VariantAnnotation readVcf ScanVcfParam
#' 
#' @importFrom GenomicAlignments readGAlignments
#' 
#' @importFrom Rsamtools ScanBamParam
#' 
#' @importFrom plotly plot_ly layout
#' 
#' @importFrom magrittr %>%
#' 
#' @importFrom RColorBrewer brewer.pal
#' 
#' @importFrom dplyr filter group_by summarise left_join mutate
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
