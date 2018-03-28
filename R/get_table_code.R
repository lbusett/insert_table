get_table_code <- function(out_tbl,
                           tbl_name,
                           is_console,
                           context) {

  out_tbl[[1]] <- editTable(out_tbl[[1]])


  output_tibble_str <- paste0(
    tbl_name, " <- ",
    suppressWarnings(datapasta::tribble_construct(out_tbl[[1]])))

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

  # create the string to be added
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
