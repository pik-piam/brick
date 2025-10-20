#' Convert named list to data.frame
#'
#' @param x named list with keys as column names
#' @param split character, split names by this pattern and assume same value for
#'   each separated item
#' @returns data.frame with column for each key
#'
#' @author Robin Hasse
#'

listToDf <- function(x, split = NULL) {
  x <- as.data.frame(x)
  if (!is.null(split)) {
    splitCols <- dplyr::where(is.character)
    x <- dplyr::mutate(x, dplyr::across(!!splitCols, ~ strsplit(., split)))
    x <- tidyr::unnest_longer(x, col = dplyr::where(is.list))
  }
  x
}
