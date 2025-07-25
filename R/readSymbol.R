#' Read symbol from gams container
#'
#' @author Robin Hasse
#'
#' @param x gams Container, Parameter, Variable or Set
#' @param symbol character, name of gams object if x is a Container else NULL
#' @param selectArea logical, select area quantity and remove this dimension
#' @param stringAsFactor logical, keep default factors from gams
#'
#' @importFrom dplyr %>% filter select .data

readSymbol <- function(x, symbol = NULL, selectArea = TRUE,
                       stringAsFactor = TRUE) {

  tDims <- c("ttot", "tall", "ttot2", "t", "thist", "tinit", "tcalib")

  # get gams Parameter, Variable or Set
  if (class(x)[1] == "Container") {
    if (length(symbol) != 1) {
      stop("Only a single string is allowed as symbol.")
    }
    obj <- x$getSymbols(symbol)[[1]]
  } else if (class(x)[1] %in% c("Parameter", "Variable", "Set")) {
    if (!is.null(symbol)) {
      warning("symbol is ignored as x is a ", tolower(class(x)[1]), " already.")
    }
    obj <- x
  } else {
    stop("x is of an unsupported type.")
  }

  data <- obj$records
  if (is.null(data)) {
    data <- data.frame(matrix(ncol = obj$dimension, nrow = 0))
  }

  colnames(data)[1:obj$dimension] <- obj$domainNames

  # remove columns
  switch(class(obj)[1],
    Variable = {
      data <- data %>%
        select(-"marginal", -"lower", -"upper", -"scale") %>%
        rename(value = "level")
    },
    Set = {
      data[["element_text"]] <- NULL
      if (identical(colnames(data), "*")) {
        data <- getElement(data, "*")
        # For one-dimensional time sets: Convert to numeric
        if (symbol %in% tDims) data <- as.numeric(as.character(data))
      }
    }
  )

  # select area quantity
  if ("qty" %in% colnames(data) && selectArea) {
    data <- data %>%
      filter(.data[["qty"]] == "area") %>%
      select(-"qty")
  }

  # convert factors to character
  if (!stringAsFactor) {
    for (dim in setdiff(colnames(data), "value")) {
      data[[dim]] <- as.character(data[[dim]])
    }
  }

  # make temporal dimensions numeric
  tDims <- intersect(
    colnames(data),
    tDims
  )
  for (tDim in tDims) {
    data[[tDim]] <- as.numeric(as.character(data[[tDim]]))
  }

  return(data)
}
