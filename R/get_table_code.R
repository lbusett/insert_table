#' @title get_table_code
#' @description Accessory function used to generate the code needed to generate
#'  the table in the selcted otuput format
#' @param out_tbl `list` passed from `insert_table` and containing 4 elements:
#'  1: data.frame to be used to generate the table, 2: context of the call,
#'  3: column names (optional) and 4: table name
#' @param is_console `logical` if TRUE, the insert_table function was called
#'  from the console, otherwise from an Rmd file using the addin
#' @param context context of the call (tells if from console or file, and if
#'  from file allows to retrieve  the lines, etcetera)
#' @return returns the code needed to generate the table, either by creating
#'  new lines in the Rmd, or by printing it to the console (if is.console = TRUE)
#' @rdname get_table_code
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom anytime anydate
#' @importFrom datapasta tribble_construct
#' @importFrom rstudioapi insertText
#'
get_table_code <- function(out_tbl,
                           is_console,
                           context) {



  #   ____________________________________________________________________________
  #   Create code to generate tribble                                         ####
  header   <- out_tbl[[3]]
  tbl_name <- out_tbl[[4]]

  if (is.data.frame(out_tbl[[1]])) {

    out_tbl_data <- out_tbl[[1]]

    #  replace column names with the first row of the table.
    #  If the first row is empty, use col_1, col_2....
    if (header) {
      if (length(unique(as.character(out_tbl_data[1, ]))) == ncol(out_tbl_data)) {
        colnames            <- out_tbl_data[1, ]
        out_tbl_data        <- out_tbl_data[-1, ]
        names(out_tbl_data) <- colnames
      } else {
        if (!unique(out_tbl_data[ ,1]) == "") {
          stop("Non-unique column names found! Aborting! ")
        } else {
          names(out_tbl_data) <- paste0("Col_", seq_len(ncol(out_tbl_data)))
        }
      }
    } else {
      names(out_tbl_data) <- paste0("Col_", seq_len(ncol(out_tbl_data)))
    }

    # convert columns to numeric if possible
    for (col in seq_len(ncol(out_tbl_data))) {

      if (!any(is.na(suppressWarnings(as.numeric(out_tbl_data[, col]))))) {
        out_tbl_data[, col] <- as.numeric(out_tbl_data[, col])
      } else {
        # convert columns to "standard" date representation if possible
        # (i.e., YYYY-mm-dd)
        if (!any(is.na(anytime::anydate(out_tbl_data[, col])))) {
          out_tbl_data[, col] <- anytime::anydate(out_tbl_data[, col])
          out_tbl_data  <- as.character(out_tbl_data)
        }
      }

    }

    output_tibble_str <- paste0(
      tbl_name, " <- ",
      suppressWarnings(datapasta::tribble_construct(out_tbl_data)))
  } else {
    # In case the add-in was fired while the name of a object was selected,
    # or from the console, no need to add the tibble: just create code to
    # render a table in the specifed name using the selected format
    output_tibble_str <- ""
  }

  #   __________________________________________________________________________
  #   Create code to generate table in specified format                     ####

  if (out_tbl[[2]] == "kableExtra") {

    output_table_str <-
      paste0("require(knitr)\n",
             "require(kableExtra)\n",
             "kable_styling(
              kable(", tbl_name, ", digits = 3, row.names = FALSE, align = \"c\",
              caption = NULL, format = \"html\"),
        bootstrap_options = c(\"striped\", \"hover\", \"condensed\"),
        position = \"center\", full_width = FALSE) ")
  } else {
    if (out_tbl[[2]] == "DT") {
      output_table_str <-
        paste0("require(DT)\n",
               "datatable(", tbl_name, ", rownames = FALSE, caption = NULL,
               filter = \"top\", escape = FALSE, style = \"default\")")
    } else {
      if (out_tbl[[2]] == "rhandsontable") {
        output_table_str <-
          paste0("require(rhandsontable)\n",
                 "rhandsontable(", tbl_name, ", rowHeaders = NULL,
               digits = 3, useTypes = FALSE, search = FALSE)")
      } else {
        output_table_str <- ""
      }
    }
  }

  # create the final text string to be added to the Rmd or pasted to console
  output_str <- paste0(output_tibble_str, "\n", output_table_str, "\n")

  # find the first empty line below that from which the call was made to
  # avoid breaking the Rmd (only if not called from console)

  if (!is_console) {

    insert_line <- NULL
    curr_line <- context[["selection"]][[1]][["range"]][["end"]][[1]]
    for (line in (curr_line):length(context$contents)) {
      if (context["contents"][[1]][line] == "") {
        insert_line <- line
        break()
      }
      if (context["contents"][[1]][line] == "```") {
        insert_line <- line - 1
        break()
      }
    }
    if (is.null(insert_line)) insert_line <- length(context$contents) - 1
    rstudioapi = rstudioapi::insertText(c(insert_line, 1, insert_line, 1),
                                        output_str)
  } else {
    rstudioapi = rstudioapi::insertText(output_str)
  }
}
