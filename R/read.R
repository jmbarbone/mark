# from https://github.com/alistaire47/read.so/blob/master/R/md.R
# NOTE read.so is not in CRAN

mark_read_md <- function(
  file,
  sep = "|",
  strip.white = TRUE, # nolint: object_name_linter.
  ...
) {
  if (inherits(file, "AsIs")) {
    file <- textConnection(as.character(file))
    on.exit(if (isOpen(file)) close(file))
  }

  lines <- readLines(file)
  lines <- grep(
    "^[\\:\\s\\+\\-\\=\\_\\|]*$",
    lines,
    perl = TRUE,
    invert = TRUE,
    value = TRUE
  )
  lines <- gsub("(^\\s*?\\|)|(\\|\\s*?$)", "", lines)
  lines <- paste(lines, collapse = "\n")
  utils::read.delim(
    text = lines,
    sep = "|",
    strip.white = strip.white,
    ...
  )
}
