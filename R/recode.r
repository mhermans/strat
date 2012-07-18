
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


#' Recode occupational characteristics into the ESeC socioeconomic classification.
#'
#' \code{esec} provides an uniform function for converting 
#' to and from different occupational and stratification schemas.
#'
#' @param data a single value, vector or dataframe containing occupational or
#'        stratification data that one wishes to recode. See Details.
#' @param detail a string indicating the input format. See 
#'        Details for possible values.
#' @param labels the level of detail in returned classfication. 
#'        The default of 0 returns the most detailed form.
#' @return vector of ESeC codes/labels
#' @keywords manip
#' @export
#' @examples
#' isco <- c(1200, 3131, 9110)
#' esec(isco, details=3, labels=TRUE)
esec <- function(data, detail=0, labels=FALSE) {
  data <- format_input(data)
  data$isco88 <- isco88_isco88(data$isco88, detail=3)
  esec <- isco88_esec(data, detail, labels)
  
  esec
}

#' Recode occupational characteristics to the ISEI socioeconomic scale.
#' @param data a single value, vector or dataframe containing occupational or
#'        stratification data that one wishes to recode. See Details.
#' @export
#' @examples
#' isco <- c(1200, 3131, 9110)
#' isei(isco)
isei <- function(data) {
  data <- format_input(data)
  data$isco88 <- isco88_isco88(data$isco88, detail=4)
  isei <- isco88_isei(data)
  
  isei
}


#' Recode occupational characteristics to the SIOPS prestige scale.
#' @param data a single value, vector or dataframe containing occupational or
#'        stratification data that one wishes to recode. See Details.
#' @export
#' @examples
#' isco <- c(1200, 3131, 9110)
#' siops(isco)
siops <- function(data) {
  data <- format_input(data)
  data$isco88 <- isco88_isco88(data$isco88, detail=4)
  siops <- isco88_siops(data)
  
  siops
}


#' Select and format variables as expected by the conversion functions
format_input <- function(data) {
  # possible inputs: dataframe|datamatrix|vector|list|single element
  #   => dataframe
  
  if (is.vector(data)) { # vector/single value
    data <- data.frame(isco88=data) # assue isco88 (detect?)
  }
  
  # lower names
  names(data) <- tolower(names(data)) 
  
  #subset(data, subset=TRUE, select=selection)?
  
  data
}
