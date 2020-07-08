#' Evaluate a  Named Chunk
#'
#' Evaluate a named chunk from an Rmd file.
#'
#' @param rmd_file Absolute path to rmd file
#' @param label_name Name of label
#' @param ... Additional arguments passed to [base::eval()]
#'
#' @importFrom tools file_ext
#'
#' @export
#'
#' @examples
#' \dontrun{
#' temp_rmd <- tempfile(fileext = ".rmd")
#'
#' text <- '
#' ```{r not this label}
#' print("that is wrong")
#' ```
#'
#' ```{r hello label}
#' text <- "hello, world"
#' print(text)
#' ```
#'
#' ```{r another label}
#' warning("wrong label")
#' ```
#' '
#'
#' writeLines(text, con = temp_rmd)
#'
#' read_named_chunk(temp_rmd, "hello label")
#' }

eval_named_chunk <- function(rmd_file, label_name, ...) {
  stopifnot(tolower(file_ext(rmd_file)) == "rmd")

  lines <- readLines(rmd_file)
  label_line <- grep(paste0("\\{r ", label_name), lines)[1]

  stopifnot(!is.na(label_line))

  lines <- lines[(label_line + 1):length(lines)]
  exp <- lines[1:(grep("[```]", lines)[1]-1)]
  eval(parse(text = exp), ...)
}
