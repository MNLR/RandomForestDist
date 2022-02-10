#' Daily rainfall records for a selected subset of 9 meteorological stations
#' from Experiment 1 of the COST action VALUE (http://www.value-cost.eu).(A description of the
#' experiment and the data can be found in Widmann et al. 2019, \url{https://doi.org/10.1002/joc.6024}).
#' Only days with positive rain are included (>1mm).
#' It contains a list of length 9 with an element for each station. Each element is divided into 5 folds,
#' each with \code{$train.y} and \code{$test.y} (the predictands); and \code{$train.x}, \code{$test.x}
#'  (the predictors)
#' @name VALUE9
#' @docType data
#' @usage data(VALUE9)
#' @author Mikel N. Legasa \email{mikel.legasa@unican.es}
#' @references The predictands dataset is provided by the European Climate Assessment &
#' Dataset  project (ECA&D). The predictors are ERA-Interim Reanalysis (see Dee D. et Al, The
#'  ERA-Interim reanalysis: configuration and performance of the data assimilation system,
#'  Royal Meteorological Society, 137, 553-597)
#' @keywords datasets
"VALUE9"

#' Daily rainfall records for the 86 meteorological stations
#' from Experiment 1 of the COST action VALUE (http://www.value-cost.eu).(A description of the
#' experiment and the data can be found in Widmann et al. 2019, \url{https://doi.org/10.1002/joc.6024}).
#' Only days with positive rain are included (>1mm).
#' It contains a list of length 86 with an element for each station. Each element is divided into 5 folds,
#' each with \code{$train.y} and \code{$test.y} (the predictands); and \code{$train.x}, \code{$test.x}
#'  (the predictors)
#' @name VALUE86
#' @docType data
#' @usage data(VALUE86)
#' @author Mikel N. Legasa \email{mikel.legasa@unican.es}
#' @references The predictands dataset is provided by the European Climate Assessment &
#' Dataset  project (ECA&D). The predictors are ERA-Interim Reanalysis (see Dee D. et Al, The
#'  ERA-Interim reanalysis: configuration and performance of the data assimilation system,
#'  Royal Meteorological Society, 137, 553-597)
#' @keywords datasets
"VALUE86"
