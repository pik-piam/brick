#' get Period from config
#'
#' extract different periods from a given BRICK config
#'
#' @author Robin Hasse
#'
#' @param config named list with run configuration
#' @param periodType type of period(s)
#' @returns numeric vector with periods

periodFromConfig <- function(config, periodType) {

  startyear <- config[["startyear"]]
  ttot <- sort(unique(config[["periods"]]))
  tinit <- config[["tinit"]]

  if (startyear <= min(ttot)) {
    stop("startyear cannot be equal or before the first period. ",
         "There has to be at least one historic period.")
  }

  if (!tinit %in% ttot) {
    stop("tinit has to be part of the periods.")
  }
  if (tinit != min(ttot)) {
    warning("tinit should be the first period but isn't. ",
            "This can lead to unexpected behaviour ",
            "in the consideration of the standing life time.")
  }

  t <- ttot[which(ttot >= startyear)]

  if (periodType == "tcalib") {
    calibperiods <- config[["calibperiods"]]
    if (!all(calibperiods %in% ttot)) {
      stop("Calibration periods have to be a subset of all periods.")
    }
  } else {
    calibperiods <- min(t)
  }

  switch(periodType,
    ttot = ttot,
    tall = min(ttot):max(ttot),
    startyear = startyear,
    t = t,
    thist = setdiff(ttot, t),
    tinit = tinit,
    t0 = min(t),
    tcalib = calibperiods[which(calibperiods >= startyear)],
    stop("unknown type of period: ", periodType)
  )
}
