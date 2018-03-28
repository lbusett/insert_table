

editTable <- function(tbl){
  # DF <<- tbl
  # browser()
  #
  # ui <- miniUI::miniPage(miniUI::miniContentPanel(
  #   shiny::fillRow(
  #     wellPanel(
  #     rHandsontableOutput("hot"),
  #     actionButton(inputId = "Done", label="Done")
  #   ))
  # ),
  # miniUI::gadgetTitleBar(NULL)
  # )

#   server <- function(input,output, session, tbl){
#       values = shiny::reactiveValues()
#       setHot = function(x) values[["hot"]] = DF
#       # output$hot <- renderRHandsontable(rhandsontable(DF,
#       #                                                 readOnly=FALSE))
#       output$hot <- renderRHandsontable({
#         data <- reactiveData()
#         if (isErrorMessage(data))
#           return(NULL)
#
#         if (is.null(input$hot))
#           DF = data
#         else
#           DF = hot_to_r(input$hot)
#
#         setHot(DF)
#         rhandsontable(DF) %>%
#           hot_table(highlightCol = TRUE, highlightRow = TRUE)
#       })
#
#       #
#       #     observeEvent(input$enter, {
#       #       DF=hot_to_r(input$hot)
#       #       print(DF)
#       #     })
#
#       shiny::observeEvent(input$done, {
#         DF <- hot_to_r(input$hot)
#         shiny::stopApp(returnValue = DF)
#       })
#       shiny::observeEvent(input$cancel, {
#         shiny::stopApp()
#       })
#     }
#   # )
# shiny::runGadget(ui, server,
#                  stopOnCancel = FALSE,
#                  viewer = shiny::dialogViewer('Edit the table if you wish so',
#                                               height = 50)
#
#                  )
#                  library(shiny)
  library(rhandsontable)

  ui = shinyUI(fluidPage(
    fluidRow(wellPanel(
      rHandsontableOutput("hot"),
      actionButton(inputId="enter",label="enter")
    ))
  ))


  server=function(input,output){

    DF=data.frame(Code=c(1,2,3),Amount=as.character(c(NA,NA,NA)))

    output$hot=renderRHandsontable(rhandsontable(DF, readOnly = FALSE) %>%
                                     hot_col("Amount", type = "numeric"))

    observeEvent(input$enter, {
      DF=hot_to_r(input$hot)
      print(DF)
    })
  }
  shinyApp(ui = ui, server = server)
}
