between_more <- function(x, left, right, type = "gele") {
  if(!type %in% c("gele", "gel", "gle", "gl")) stop("Not a valid type!", call. = F)
  if(left > right) warning("`left` > `right`", call. = F)
  dplyr::case_when(
    type == "gele" ~ x >= left & x <= right,
    type == "gel" ~ x >= left & x < right,
    type == "gle" ~ x > left & x <= right,
    type == "gl" ~ x > left & x < right
  )
}
