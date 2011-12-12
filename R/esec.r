#' Recode ISCO88 (and optional job characteristics) to ESeC.
isco88_esec <- function(data, detail = 0, labels=FALSE) {
  # expects 3 digit ISCO88 codes.
  
  if (ncol(data) == 1) { # only data for the simple version
    message('Selecting simple ESeC algo')
    esec <- isco88_esec_simple(data) # recode
  }
    
  if (ncol(data) == 4) { # sufficient vars for full version
    # FAQ: should ESeC pre precoded by simple version?
    esec <- isco88_esec_full(data)
    message('Selecting full ESeC algo')
  }  
    
  if (detail != 0) { 
    message('ESeC: collapsing categories to data')
    esec <- collapse_esec(esec, detail) 
  }
    
  if (labels) { 
    esec <- label_esec(esec)
    message('ESeC: adding labels')
  }
    
  esec
}


#' Recode ISCO88 (3 digit) to ESeC according to the "simple" version
isco88_esec_simple <- function(data) {

  # ESeC class 1: 
  data$esec[data$isco88 %in% c(010,100,110,111,114,120,121,123,200,210,
                               211,212,213, 214,220,221,222, 231, 235,
                               240,241,242)] <- 1

  # ESeC class 2: 
  data$esec[data$isco88 %in% c(122, 223,230,232,233,234,243, 244, 
                               245, 246, 247, 310, 311,312, 314, 
                               320, 321, 322, 323, 334, 342, 344, 
                               345, 348, 521)] <- 2

  # ESeC class 3: 
  data$esec[data$isco88 %in% c(011,300,330,331,332,333,340, 341, 
                               343, 346, 347, 400, 410, 411, 412, 
                               419, 420)] <- 3

  # ESeC class 4: 
  data$esec[data$isco88 %in% c(130,131,911)] <- 4

  # ESeC class 5: 
  data$esec[data$isco88 %in% c(600,610,611,612,613,621)] <- 5

  # ESeC class 6: 
  data$esec[data$isco88 %in% c(313, 315, 730,731)] <- 6

  # ESeC class 7: 
  data$esec[data$isco88 %in% c(413, 421, 422, 500, 510, 511, 513, 
                               514, 516, 520, 522)] <- 7

  # ESeC class 8: 
  data$esec[data$isco88 %in% c(614, 615, 700, 710, 711, 712, 713, 
                               714, 720, 721, 722, 723, 724, 732, 
                               733, 734, 740, 741, 742, 743, 744, 
                               825, 831, 834)] <- 8

  # ESeC class 9: 
  data$esec[data$isco88 %in% c(414, 512, 800, 810, 811, 811, 812, 
                               813, 814, 815, 816, 817, 820, 821, 
                               822, 823, 824, 826, 827, 828, 829, 
                               830, 832, 833, 900, 910, 912, 913, 
                               914, 915, 916, 920, 921, 930, 931, 
                               932, 933)] <- 9  

  data$esec
}


#' Recode ISCO88 (3 digit) to ESeC using the full algorithm.
isco88_esec_full <- function(data) {
  # data expectations:
  # 1. ISCO88-code (3/4-digit)
  # 2. Employment
  #  1 = Employed => 0
  #  2 = Self-employed => 1
  # 3. Supervising
  #  1 = Yes => 1
  #  2 = No  => 0
  # 4. Number of employees
  #  1-...
  
  # Compute employment status categories
  # ------------------------------------

  # Status variable:
  # (0: missing info) 
  # 1: self-employed 10+ employees
  # 2: small employers <10
  # 3: self-employed, no employees 
  # 4: supervisors
  # 5: employee

  data$status[data$sem == 1 & data$nem > 9] <- 1	
  data$status[data$sem == 1 & data$nem %in% 1:9] <- 2 	# 
  data$status[data$sem == 1 & data$nem %in% c(0,99999)] <- 3  
  data$status[data$sem == 0 & data$sup == 1] <- 4
  data$status[data$sem == 0 & data$sup == 0] <- 5
   
  # Recode 5 employment groeps
  # --------------------------

  # a) Self-employed 10+ employees. Defaults to 1 
  data$esec[data$status == 1] <- 1
  data$esec[data$status == 1 & data$isco88 %in% c(344, 345)] <- 2
  data$esec[data$status == 1 & data$isco88 %in% c(011, 516)] <- 3
  data$esec[data$status == 1 & data$isco88 == 621] <- 5

  # b) Small employers <10. Defaults to 4
  data$esec[data$status == 2] <- 4
  data$esec[data$status == 2 & data$isco88 %in% c(010, 110, 111, 114, 200, 
    210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
  data$esec[data$status == 2 & data$isco88 %in% c(223, 230, 232, 233, 234, 
    243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 
    342, 344, 345, 348)] <- 2
  data$esec[data$status == 2 & data$isco88 %in% c(011, 516)] <- 3
  data$esec[data$status == 2 & data$isco88 %in% c(600, 610, 611, 612, 613, 
    614, 615, 621, 920, 921)] <- 5

  # c) Self-employed with no employees. Defaults to 4
  data$esec[data$status == 3] <- 4
  data$esec[data$status == 3 & data$isco88 %in% c(010, 110, 111, 114, 200, 
    210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
  data$esec[data$status == 3 & data$isco88 %in% c(223, 230, 232, 233, 234, 
    243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 
    342, 344, 345, 348)] <- 2
  data$esec[data$status == 3 & data$isco88 %in% c(011,516)] <- 3
  data$esec[data$status == 3 & data$isco88 %in% c(600, 610, 611, 612, 613, 
    614, 615, 621, 920, 921)] <- 5

  # d) Supervisors. Defaults to 6
  data$esec[data$status == 4] <- 6
  data$esec[data$status == 4 & data$isco88 %in% c(010, 100, 110, 111, 114, 
    120, 121, 123, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 
    240, 241, 242)] <- 1
  data$esec[data$status == 4 & data$isco88 %in% c(011, 122, 130,131, 223, 
    230, 232, 233, 234, 243, 244, 245, 246, 247, 300, 310, 311, 312, 313, 
    314, 320, 321, 322, 323, 330, 331, 332, 333, 334, 340,341, 342, 343, 
    344, 345, 346, 347, 348, 400, 410, 411, 412, 419, 420, 521)] <- 2
  data$esec[data$status == 4 & data$isco88 == 621] <- 5

  # e) Employees
  data$esec[data$status == 5 & data$isco88 %in% c(010, 100,110,111,114,120, 
    121, 123, 200, 210, 211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 
    241, 242)] <- 1
  data$esec[data$status == 5 & data$isco88 %in% c(122,130,131, 223, 230, 232, 
    233, 234, 243, 244, 245, 246, 247, 310, 311, 312, 314, 320, 321, 322, 323, 
    334, 342, 344, 345, 348, 521)] <- 2
  data$esec[data$status == 5 & data$isco88 %in% c(011, 300, 330, 331, 332, 333, 
    340, 341, 343, 346, 347, 400, 410, 411, 412, 419, 420)] <- 3
  data$esec[data$status == 5 & data$isco88 == 621] <- 5
  data$esec[data$status == 5 & data$isco88 %in% c(313, 315, 730,731)] <- 6
  data$esec[data$status == 5 & data$isco88 %in% c(413, 421, 422, 500, 510, 511, 
    513, 514, 516, 520, 522, 911)] <- 7
  data$esec[data$status == 5 & data$isco88 %in% c(600, 610, 611, 612, 613, 614, 
    615, 700, 710, 711, 712, 713, 714, 720, 721, 722, 723, 724, 732, 733, 734, 
    740, 741, 742, 743, 744, 825, 831, 834)] <- 8
  data$esec[data$status == 5 & data$isco88 %in% c(414, 512, 800, 810, 811,  812, 
    813, 814, 815, 816, 817, 820, 821, 822, 823, 824, 826, 827, 828, 829, 830, 832, 
    833,900, 910, 912, 913, 914, 915, 916, 920, 921, 930, 931, 932, 933)] <- 9

  data$esec
  
}

label_esec <- function(data) {
  # optional type of labels: full|short|numeral?
  
  nclasses <- length(names(table(data)))
  
  if (!(nclasses %in% c(3, 5, 6, 9, 10))) { 
    warning('Non-standard number of classes, no labels assigned')
    return(data)
  }

  short10 <- c(
    'Higher salariat',  # 1 -> 1
    'Lower salariat',   # 2 -> 1
    'Higher grade white collar workers', # 3
    'Petit bourgeoisie (AC)',  # 4
    'Petit bourgeoisie (non-AC)', # 5
    'Higher grade blue collar workers', # 6 
    'Lower grade white collar workers', # 7
    'Skilled workers', # 8
    'Semi/nonskilled workers', # 9
    'Unemployed' # 10
    )

  short6 <- c(
    'Salariat',                         # 1
    'Intermediate employee',            # 2 
    'Small/self employers',    # 3 
    'Higher grade blue collar workers', # 4
    'Skilled workers',                  # 5
    'Semi/nonskilled workers'           # 6
    )
  
  short5 <- c(
    'Salariat',                         # 1
    'Intermediate employee',            # 2 
    'Small/self employers',    # 3 
    'Higher grade blue collar workers', # 4
    'Lower technical/routine occupations'      # 5
    )  
  
  short3 <- c('Salariat', 'Intermediate employee', 'Working class')
  
  if (nclasses == 10){
    return(factor(data, levels=1:10, labels=short10))
  }

  if (nclasses == 9){
    return(factor(data, level=1:9, labels=short10[1:9]))
  }

  if (nclasses == 6){
    return(factor(data, level=1:6, labels=short6))
  }
  
  if (nclasses == 5){
    return(factor(data, level=1:5, labels=short5))
  }

  if (nclasses == 3){    
    return(factor(data, level=1:3, labels=short3))
  }
    

}

collapse_esec <- function(data, detail=0) {

  # Harrison & Rose, 2006, "(ESeC) User Guide", pg. 9-10
  # ----------------------------------------------------
  # The 10 class model may be collapsed to 6, 5 or 3 classes. 
  #
  # - 6-class: 
  #  - classes 1 and 2 = class 1 "the salariat"
  #  - classes 3 and 6 = "intermediate employee" class 2
  #  - classes 4 and 5 =  class 3 "small employers and self-employed"
  #  - class 7 becomes class 4;
  #  - class 8 becomes class 5;
  #  - class 9 becomes class 6.
  #
  # - 5-class model:
  #  - classes 5 and 6 in the sidata class model: 
  #      "lower technical and routine occupations".
  #
  # - the three class model:
  #  - classes 1 and 2=salariat;
  #  - 3, 4, 5 and 6=intermediate;
  #  - 7, 8 and 9=working class.
  #
  # - Class 10 may be added as an additional in any of the models, 
  #      if desired.

  # Collapse 6 class
  if (detail == 6) {
    data[data %in% 1:2] <- 1
    data[data %in% c(3,6)] <- 2
    data[data %in% c(4,5)] <- 3
    data[data == 7] <- 4
    data[data == 8] <- 5
    data[data == 9] <- 6
  }  

  # Collapse 5 class
  if (detail == 6) {
    data[data %in% 1:2] <- 1
    data[data %in% c(3,6)] <- 2
    data[data %in% c(4,5)] <- 3
    data[data == 7] <- 4
    data[data %in% 8:9] <- 5
  }  

  # Collapse 3 class
  if (detail == 3) {
    data[data %in% 1:2] <- 1
    data[data %in% 3:6] <- 2
    data[data %in% 7:9] <- 3
  }

  # if detail is not equal to 6|5|3, just return the input (no collapse)
  data

}
