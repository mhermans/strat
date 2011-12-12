isco88_isco88 <-function(data, detail=0, labels=FALSE, validate=FALSE) {
  
  # Should be character: numeric to character
  data <- as.character(data)
  
  # Select lower granularity if needed
  if (detail) {
    data <- substring(data,1,detail) # Make ISCO88 1/2/3-digit
  }  
  
  data
}
