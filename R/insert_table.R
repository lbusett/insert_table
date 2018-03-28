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
insert_table = function(nrows      = 1,
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

  # taken from https://stackoverflow.com/a/39084576/6871135
  create_empty_df <- function(num_rows, num_cols) {
    dframe <- data.frame(matrix("", nrow = num_rows, ncol = num_cols),
                         stringsAsFactors = FALSE)
    names(dframe) <- paste0("col_", seq_len(num_cols))
    return(dframe)
  }

  if (!is_console) {
    if (text == "") {

      # If function called as addin from an empty line, ask user to define number
      # of rows and columns and format of table he wishes to create and create a empty data
      # frame

      out_tbl = local({
        shiny::runGadget(
          miniUI::miniPage(miniUI::miniContentPanel(
            shiny::fillRow(
              shiny::numericInput('ncols', 'Number of Columns', 2, 1, 100, 1),
              shiny::numericInput('nrows', 'Number of Rows',    2, 1, 100, 1),
              shiny::selectInput('format', 'Format',
                                 c('kable', 'DT', 'rhandsontable')),
              height = '70px'
            ),
            miniUI::gadgetTitleBar(NULL)
          )),
          server = function(input, output, session) {
            shiny::observeEvent(input$done, {

              # If "Done" is clicked, create an empty data frame, then return
              # the empty data frame along with the selected table format
              out_df <- create_empty_df(input$nrows, input$ncols)

              shiny::stopApp(returnValue = list(out_df, input$format))
            })
            shiny::observeEvent(input$cancel, {
              shiny::stopApp()
            })
          },
          stopOnCancel = FALSE,
          viewer = shiny::dialogViewer('Add empty table to a Rmd document',
                                       width = 500, height = 50)
        )
      })

      # output_tibble_str <- paste0(
      #   tbl_name, " <- ",
      #   suppressWarnings(datapasta::tribble_construct(out_tbl[[1]])))
    } else {

      output_tibble_str <- ""
      tbl_name          <- text

      out_tbl = local({
        shiny::runGadget(
          miniUI::miniPage(miniUI::miniContentPanel(
            shiny::fillRow(
              shiny::selectInput('format', 'Format',
                                 c('kable', 'DT', 'rhandsontable')),
              height = '70px'
            ),
            miniUI::gadgetTitleBar(NULL)
          )),
          server = function(input, output, session) {
            shiny::observeEvent(input$done, {
              shiny::stopApp(returnValue = list("", input$format))
            })
            shiny::observeEvent(input$cancel, {
              shiny::stopApp()
            })
          },
          stopOnCancel = FALSE,
          viewer = shiny::dialogViewer('Add table to a Rmd document from an
                                     existing data frame', height = 50)
        )
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

    out_df <- create_empty_df(nrows, ncols)
    out_tbl <- list(out_df, tbl_format)
    # output_tibble_str <- paste0(
    #   tbl_name, " <- ",
    #   suppressWarnings(datapasta::tribble_construct(out_tbl[[1]])))

  }
# Sys.sleep(1)
 # browser()
get_table_code(out_tbl,
               tbl_name,
               is_console,
               context)
}
