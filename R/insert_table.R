#' @title insert_table
#' @description Function and RStudio add-in allowing to quickly and automatically
#'  generate the code needed to render a table in a RMarkdown document using different
#'  formats (kable, DT and rhandsontable are currently implemented).
#' @param nrows `numeric` number of rows of the generated empty table, Default: 1
#'  (ignored if calling the addin from an empty Rmd line)
#' @param ncols `numeric` number of columns of the generated empty table, Default: 1
#'  (ignored if calling the addin from an empty Rmd line)
#' @param tbl_format `character` [`kable` | `DT` | `rhandsontable`] format required
#'  for the table to be created (ignored if calling as an addin)
#' @param tbl_name `character` name required for the table to be created
#'  (ignored if calling as an addin)
#' @return returns the code required to create a table in a Rmd file with the
#'  required format. \cr
#'  When calling as an add-in:
#'    * if the call is done when the cursor is on a empty selection the user can
#'      enter also the number of rows and columns and the code to generate a empty
#'      tribble with the specified dimensions is also created;
#'    * if the call is done when the cursor is on a non-empty selection the user can
#'      only select the output format, and the add-in returns the code needed to
#'      create a table named as the selected text, with the specified format
#'
#'  When called as a function:
#'    * the code to generate a empty tribble with the specified dimensions is
#'      created (defaults are used if any parameter is not passed), followed by
#'      the code needed to create a table with the specified format. The results
#'      are sent back to the console.
#' @examples
#' \dontrun{
#'  # From the console, use:
#'  insert_table(nrows = 4, ncols = 3, tbl_format = "DT")
#'
#'  # From a "Rmd" file and within RStudio, place the cursor on a empty line or
#'  # select the name a data.frame within a "R" chunk, then click on "Addins"
#'  # and select "Insert Table"
#'  }
#' @rdname insert_table
#' @export
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom shiny runGadget fillRow numericInput selectInput observeEvent stopApp dialogViewer
#' @importFrom miniUI miniPage miniContentPanel gadgetTitleBar
#' @importFrom datapasta tribble_construct
#' @importFrom assertthat assert_that
#'
insert_table = function(nrows      = 3,
                        ncols      = 3,
                        tbl_format = "kable",
                        tbl_name   = NULL
){

  # Get the text selected when the addin was called
  context    <- rstudioapi::getActiveDocumentContext()
  text       <- context$selection[[1]]$text
  is_console <- context[["id"]] == "#console"
  if (is.null(tbl_name)) {
    tbl_name <- "mytbl"
  }

  # create an empty table to initialize the GUI
  DF <- data.frame(matrix(data = "", ncol = 3, nrow = 3))

  if (!is_console) {
    if (text == "") {

      # If function called as addin from an empty line, ask user to define number
      # of rows and columns and format of table he wishes to create and create a empty data
      # frame

      out_tbl = local({
        ui <- miniUI::miniPage(miniUI::miniContentPanel(
          miniUI::gadgetTitleBar("Select output format and edit the Table if
                                 you wish so"),
          shiny::fillRow(
            shiny::selectInput('format', 'Select Output Format',
                               c('kable', 'DT', 'rhandsontable')),
            height = '70px'
          ),
          h4("Edit Table or cut and paste from spreadsheet", align = "left"),
          div(""),
          div("* The first row will be used as column names.\n", style = "bold"),
          div("* Right click on a row/column header to add more lines or columns", style = "bold"),
          shiny::fillRow(
            shiny::wellPanel(
              rhandsontable::rHandsontableOutput("hot")
            ), height = "500px"
          )

        ))

        server <- function(input,output, session){
          values = shiny::reactiveValues()
          setHot = function(x) values[["hot"]] = DF
          output$hot <- renderRHandsontable(
            rhandsontable(DF, readOnly = FALSE, useTypes = FALSE, colHeaders = TRUE,
                          allowRowEdit = TRUE))
          shiny::observeEvent(input$done, {
            nrows <- length(input$hot$data)
            ncols <- unique(lengths(input$hot$data))

            # https://stackoverflow.com/questions/4227223/r-list-to-data-frame
            data_tbl <- unlist(input$hot$data)
            if (is.null(data_tbl)) data_tbl <- rep(NA, nrows * ncols)
            DF    <- data.frame(matrix(data_tbl,
                                       nrow = nrows, byrow = TRUE),
                                stringsAsFactors = FALSE)
            out_tbl <- list(DF, input$format)
            shiny::stopApp(returnValue = out_tbl)
          })

          shiny::observeEvent(input$cancel, {
            shiny::stopApp()
          })
        }
        shiny::runGadget(ui, server,
                         viewer = dialogViewer("Insert Table Add-In"),
                         stopOnCancel = FALSE)
      })

    } else {

      output_tibble_str <- ""
      tbl_name          <- text

      out_tbl = local({
        ui <- miniUI::miniPage(miniUI::miniContentPanel(
          miniUI::gadgetTitleBar("Select output format"),
          shiny::fillRow(
            shiny::selectInput('format', 'Format',
                               c('kable', 'DT', 'rhandsontable')),
            height = '70px'
          )
        ))

        server = function(input, output, session) {
          shiny::observeEvent(input$done, {
            shiny::stopApp(returnValue = list("", input$format))
          })
          shiny::observeEvent(input$cancel, {
            shiny::stopApp()
          })
        }

        shiny::runGadget(ui, server,
                         viewer = dialogViewer("Insert Table Add-In"),
                         stopOnCancel = FALSE)
      })
    }
  } else {
    # If called from console, check that all parameters were passed and are
    # correct

    assertthat::assert_that(!any(is.null(nrows), is.null(ncols), is.null(tbl_format)),
                            msg = strwrap("Please specify the number of rows and
                                          the output format. Aborting!",
                                          width = 100))

    assertthat::assert_that(
      tbl_format %in% c("kable", "DT", "rhandsontable"),
      msg = strwrap("`format` must be equal to `kable`, `DT` or `rhandsontable`. Please
                   correct. Aborting!", width = 100))

    out_tbl  <- create_empty_df(nrows, ncols)
    out_tbl  <- list(out_tbl, tbl_format)

  }

  get_table_code(out_tbl,
                 tbl_name,
                 is_console,
                 context)
}
