#' Wrappers for openxlsx package functions
#'
#' Add data to sheet
#'
#' A wrapper function to use within the openxlsx workbook building.
#' This adds additional functionality to override within the function and provide an output that can be piped.
#'
#' @details These should be applied to a string starting with
#'   [openxlsx::createWorkbook()] then piped through with the functions below.
#'
#' @param wb A Workbook object to attach the new worksheet and table
#' @param data A dataframe.
#' @param sheetname The worksheet to write to. Can be the worksheet index or
#'   name.
#' @param tableStyle Any excel table style name or "none".
#' @param bandedRows Logical. If TRUE, rows are colour banded
#' @param bandedCols Logical. If TRUE, the columns are colour banded
#' @param ... Additional arguments passed to openxlsx::writeDataTable that are
#'   not already listed
#' @param override Logical.  If TRUE, will delete the sheetname (if present)
#'
#' @export

add_data_sheet <- function(wb, data, sheetname,
                           tableStyle = "TableStyleLight8", bandedRows = TRUE,
                           bandedCols = TRUE, ...,
                           override = TRUE)
{
  require_namespace("openxlsx")

  sns <- wb$sheet_names

  if (length(sns)) {
    snsl <- tolower(sns)
    sn_in <- snsl %in% tolower(sheetname)
    if (override & any(sn_in)) {
      openxlsx::removeWorksheet(wb, sns[sn_in])
    }
  }

  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::writeDataTable(wb, sheetname, x = data,
                           tableStyle = tableStyle,
                           bandedRows = bandedRows,
                           bandedCols = bandedCols,
                           ...)
  invisible(wb)
}

#' Add image to sheet
#'
#' A wrapper function to..
#'
#' @param wb A Workbook object to attach the new worksheet and table
#' @param file An image file. Valid file types are: jpeg, png, bmp
#' @param sheetname The worksheet to write to. Can be the worksheet index or name.
#' @param ... Additional arguments passed to openxlsx::insertImage
#' @param override Logical.  If TRUE, will delete the sheetname (if present)
#'
#' @export

add_image_sheet <- function(wb, file, sheetname, ..., override = TRUE)
{
  require_namespace("openxlsx")

  sns <- wb$sheet_names

  if (length(sns)) {
    snsl <- tolower(sns)
    sn_in <- snsl %in% tolower(sheetname)
    if (override & any(sn_in)) {
      openxlsx::removeWorksheet(wb, sns[sn_in])
    }
  }

  openxlsx::addWorksheet(wb, sheetname)
  openxlsx::insertImage(wb, sheetname, file = file, ...)
  invisible(wb)
}
