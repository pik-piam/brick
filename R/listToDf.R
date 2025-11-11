#' Convert named list to data.frame
#'
#' @param x named list with keys as column names
#' @param split character, split names by this pattern and assume same value for
#'   each separated item
#' @returns data.frame with column for each key
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr mutate across
#' @importFrom tidyr unnest_longer

listToDf <- function(x, split = NULL) {
  x <- as.data.frame(x)
  if (!is.null(split)) {
    splitCols <- dplyr::where(is.character)
    x <- mutate(x, across(!!splitCols, ~ strsplit(., split)))
    x <- unnest_longer(x, col = dplyr::where(is.list))
  }
  x
}
