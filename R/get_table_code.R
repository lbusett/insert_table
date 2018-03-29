get_table_code <- function(out_tbl,
                           tbl_name,
                           is_console,
                           context) {

  #   ____________________________________________________________________________
  #   Create code to generate tribble                                         ####


  if (is.data.frame(out_tbl[[1]])) {
    # In case the add-in was fired from an empty line, build the code to create
    # the tribble corresponding to the edited table

    #  replace column names with the first row of the table.
    #  If the first row is empty, use col_1, col_2....
    out_tbl_data <- out_tbl[[1]][-1,]
    names(out_tbl_data) <- out_tbl[[1]][1,]
    if (unique(names(out_tbl_data)) == "") {
      names(out_tbl_data) <- paste0("Col_", seq_len(ncol(out_tbl_data)))
    }
    # browser()
    # convert to numeric if possible
    for (col in seq_len(ncol(out_tbl_data))) {

      if (!any(is.na(suppressWarnings(as.numeric(out_tbl_data[, col]))))) {

        out_tbl_data[, col] <- as.numeric(out_tbl_data[, col])

      } else {

        if (!any(is.na(anytime::anydate(out_tbl_data[, col])))) {

          out_tbl_data[, col] <- anytime::anydate(out_tbl_data[, col]) %>%
            as.character()

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

  if (out_tbl[[2]] == "kable") {

    output_table_str <-
      paste0("require(knitr)\n",
             "kable(", tbl_name, ", digits = 3, row.names = FALSE, caption = NULL)")
  } else {
    if (out_tbl[[2]] == "DT") {
      output_table_str <-
        paste0("require(DT)\n",
               "datatable(", tbl_name, ", rownames = FALSE, caption = NULL,
               filter = \"top\", escape = FALSE, style = \"default\")")
    } else {
      output_table_str <-
        paste0("require(rhandsontable)\n",
               "rhandsontable(", tbl_name, ", rowHeaders = NULL,
               digits = 3, useTypes = FALSE, search = FALSE)")
    }

  }

  # create the final text string to be added to the Rmd or pasted to console
  output_str <- paste0(output_tibble_str, "\n", output_table_str, "\n")

  # find the first empty line below that from which the call was made to
  # avoid breaking the Rmd (only if not called from console)

  if (!is_console) {

    insert_line <- NULL
    curr_line <- context[["selection"]][[1]][["range"]][["end"]][[1]]
    for (line in (curr_line + 1):length(context$contents)) {
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
