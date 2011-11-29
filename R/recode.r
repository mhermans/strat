
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
#' @param informat a string indicating the input format. See 
#'        Details for possible values.
#' @param outformat a string indicating the output format. See 
#'        Details for possible values.
#' @param detail the level of detail in returned classfication. 
#'        The default of 0 returns the most detailed form.
#' @param labels return a factor with value labels instead of the numeric
#'  	    values.
#' @keywords manip
#' @export
#' @examples
#' isco <- c(1200, 3131, 9110)
#' recode(isco, informat="isco88", outformat="isei")
recode <- function(data, informat="isco88", outformat="esec", detail=0, labels=FALSE) {

  x <- data.frame(data)
  colnames(x) <- c("oug", "sem", "nem", "sup")[1:ncol(x)]

  switch(informat,
      isco88 = switch(outformat,
          isei  = r.isco88.isei(x, detail),
          siops = r.isco88.siops(x, detail),
          egp   = r.isco88.egp(x, detail),
          guveli = r.isco88.guveli(x, detail),
          esec  = r.isco88.esec(x, detail),
          oesch = r.isco88.oesch(x, detail),
          icam = r.isco88.icam(x, detail)
      ),
      isco69 = switch(outformat,
          oesch = r.isco88.oesch(r.isco69.isco88(x), detail),
          esec  = r.isco88.esec(r.isco69.isco88(x), detail)
      )
  )
}

#' Ganzeboom's ISCO88/ISEI/SIOPS/EGP conversion table.
#' 
#' This table provides the mapping between the ISCO88, SIOPS, ISEI and EGP codes.
#' 
#' \itemize{
#'   \item isco88. ILO ISCO88  
#'   \item siops. Treimans SIOPS 
#'   \item isei. Ganzebooms ISEI
#'   \item egp. Goldtorp
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name ganzeboom
#' @usage ganzeboom
#' @format A data frame with XXX rows and 4 variables
#' @references Ganzeboom H. & Treiman D. (1996). Internationally Comparable Measures of Occupational Status	for the 1988 International Standard Classification of Occupations. \emph{Social Science Research} 25, pp. 201-239.
NULL
