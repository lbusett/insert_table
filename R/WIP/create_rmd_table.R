create_rmd_table <- function(table_data = NULL,
                            nrows      = NULL,
                            ncols      = NULL,
                            colnames   = NULL,
                            tbl_format = "kable") {


  #   ____________________________________________________________________________
  #   Get the data from "table_data" (if provided) and check arguments        ####

  if (!is.null(table_data)) {
    assertthat::assert_that(
      is.data.frame(table_data),
      msg = strwrap("`table_data` must be a `data.frame` or something inheriting
                    from a `data.frame` (e.g., a `data.table` or a `tibble`.
                    Aborting!", width = 100))
    nrows    <- nrow(table_data)
    ncols    <- ncol(table_data)
    colnames <- names(table_data)
  } else {
    assertthat::assert_that(!any(is.null(nrows), is.null(ncols)),
                            msg = strwrap("Please specify the number of rows and
                                          columns. Aborting!", width = 100))
    if (!is.null(colnames)) {
      assertthat::assert_that(is.character(colnames) & length(colnames = ncols),
                              msg = "`colnames` must be a character array of
                              length equal to `ncols`. Aborting", width = 100)
    } else {
      colnames <- paste("column", seq_len(ncols), sep = "_")
    }

  }

  assertthat::assert_that(
    tbl_format %in% c("kable", "DT", "rhandson"),
    msg = strwrap("`format` must be equal to `kable`, `DT` or `rhandson`. Please
                   correct. Aborting!", width = 100))



  if (tbl_format == "kable") {

    table_code = paste0("knitr::kable(table_data",
                        " col.names = ",
                        paste0("c(",
                               paste(lapply(colnames, FUN = function(x) paste0("'", x, "'")),
                                     collapse = ", "), ")\n"))
  } else {
    if (tbl_format == "DT") {

    } else {

    }



  }
  table_code <- paste0(datapasta::tribble_construct(table_data), "\n", table_code)

  rstudioapi::insertText(table_code)
}
