#
# editTable <- function(tbl){
#   DF <<- tbl
#   # browser()
#   #
#   ui <- miniUI::miniPage(miniUI::miniContentPanel(
#     miniUI::gadgetTitleBar("Edit the Table if you wish so...."),
#     shiny::fillRow(
#       shiny::ann
#       shiny::wellPanel(
#         rhandsontable::rHandsontableOutput("hot")
#       ), height = "500px"
#     )
#
#   ))
#
#   server <- function(input,output, session){
#     values = shiny::reactiveValues()
#     setHot = function(x) values[["hot"]] = DF
#     output$hot <- renderRHandsontable(
#       rhandsontable(DF, readOnly = FALSE, useTypes = FALSE, colHeaders = TRUE,
#                     allowRowEdit = TRUE))
#     shiny::observeEvent(input$done, {
#       nrows <- length(input$hot$data)
#       ncols <- unique(lengths(input$hot$data))
#
#       # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
#       DF    <- data.frame(matrix(unlist(input$hot$data),
#                                  nrow = nrows, byrow = TRUE))
#       shiny::stopApp(returnValue = DF)
#     })
#
#     shiny::observeEvent(input$cancel, {
#       shiny::stopApp()
#     })
#   }
#   shiny::runGadget(ui, server,
#                    viewer = dialogViewer("aaAAAaaaa"),
#                    stopOnCancel = FALSE)
#
# }
