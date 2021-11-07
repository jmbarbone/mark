row_bind <- function(...) {
  ls <- if (...length() == 1) ..1 else list(...)

  stopifnot(all(mark::vap_lgl(ls, is.data.frame)))

  names <- lapply(ls, names)
  all_names <- unique(unlist(names))

  cinds <- lapply(
    names,
    function(x, y) match(y, x, nomatch = NA_integer_),
    all_names
  )

  res <- mapply(
    function(a, b) {
      w <- which(is.na(b))

      if (length(w)) {
        a <- mark::insert(a, w, NA)
        names(a) <- all_names
        a <- mark::quick_df(a)
      }

      a
    },
    a = ls,
    b = cinds,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  utils::type.convert(do.call(rbind2, res), as.is = TRUE)
}

rbind2 <- function(...) {
  ls <- list(...)

  res <- list()

  for (i in seq_along(ls[[1]])) {
    # res[[i]] <- Reduce(c, lapply(ls, `[[`, i))
    res[[i]] <- Reduce(c, lapply(ls, `[[`, i))
  }

  names(res) <- names(ls[[1]])
  mark::quick_df(res)
}
