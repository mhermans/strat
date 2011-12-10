
# Mandatory:
# - $oug [Occupational Unit Group-identifier]
#
# Optional:
# - $nem [number of employees 0-n]
# - $sem [self-empoyed 1/0]
# - $sup [supervising 1/0]
#   <-> sup [minimum number of supervised people]


#' Recode occupational and stratification schemas.
#'
#' \code{recode} provides an uniform function for converting 
#' to and from different occupational and stratification schemas.
#'
#' @param data a single value, vector or dataframe containing occupational or
#'        stratification data that one wishes to recode. See Details.
#' @param format a string indicating the output format. See 
#'        Details for possible values.
#' @param detail the level of detail in returned classfication. 
#'        The default of 0 returns the most detailed form.
#' @param labels return a factor with value labels instead of the numeric
#'  	    values.
#' @keywords manip
#' @export
#' @examples
#' isco <- c(1200, 3131, 9110)
#' recode(isco, format="isco88")
recode <- function(data, format="esec", labels=FALSE) {
  # TODO: support list of outformats
  
  x <- data.frame(data)
  colnames(x) <- c("oug", "sem", "nem", "sup")[1:ncol(x)]

  #switch(informat,
  #    isco88 = switch(outformat,
  #        isei  = r.isco88.isei(x, detail),
  #        siops = r.isco88.siops(x, detail),
  #        egp   = r.isco88.egp(x, detail),
  #        guveli = r.isco88.guveli(x, detail),
  #        esec  = r.isco88.esec(x, detail, labels),
  #        oesch = r.isco88.oesch(x, detail),
  #        icam = r.isco88.icam(x, detail)
  #    ),
  #    isco69 = switch(outformat,
  #        oesch = r.isco88.oesch(r.isco69.isco88(x), detail),
  #        esec  = r.isco88.esec(r.isco69.isco88(x), detail)
  #    )
  #)
}


#' Recode occupational characteristics to ESeC.
#'
#' \code{recode} provides an uniform function for converting 
#' to and from different occupational and stratification schemas.
#'
#' @param data a single value, vector or dataframe containing occupational or
#'        stratification data that one wishes to recode. See Details.
#' @param details a string indicating the input format. See 
#'        Details for possible values.
#' @param labels the level of detail in returned classfication. 
#'        The default of 0 returns the most detailed form.
#' @return vector of ESeC codes/labels
#' @keywords manip
#' @export
#' @examples
#' isco <- c(1200, 3131, 9110)
#' esec(isco, details=3, labels=TRUE)
esec <- function(data, details=0, labels=FALSE) {
  data <- data.frame(data)
  if (dim(data)[2] == 1) { names(data) <- c('oug')} # label single vector
  esec <- isco88_esec(data, details, labels)
  
  esec
}
