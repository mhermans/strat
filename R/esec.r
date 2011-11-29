r.simple.esec <- function(x) {
  # Recode ISCO88, 3 digit to ESeC according to the "simple" version

  # ESeC class 1: 
  x$esec[x$isco88 %in% c(010,100,110,111,114,120,121,123,200,210,211,212,
    213, 214,220,221,222, 231, 235,240,241,242)] <- 1

  # ESeC class 2: 
  x$esec[x$isco88 %in% c(122, 223,230,232,233,234,243, 244, 245, 246, 247, 
    310, 311,312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348, 
    521)] <- 2

  # ESeC class 3: 
  x$esec[x$isco88 %in% c(011,300,330,331,332,333,340, 341, 343, 346, 347, 
    400, 410, 411, 412, 419, 420)] <- 3

  # ESeC class 4: 
  x$esec[x$isco88 %in% c(130,131,911)] <- 4

  # ESeC class 5: 
  x$esec[x$isco88 %in% c(600,610,611,612,613,621)] <- 5

  # ESeC class 6: 
  x$esec[x$isco88 %in% c(313, 315, 730,731)] <- 6

  # ESeC class 7: 
  x$esec[x$isco88 %in% c(413, 421, 422, 500, 510, 511, 513, 514, 516, 520, 
    522)] <- 7

  # ESeC class 8: 
  x$esec[x$isco88 %in% c(614, 615, 700, 710, 711, 712, 713, 714, 720, 721, 
    722, 723, 724, 732, 733, 734, 740, 741, 742, 743, 744, 825, 831, 
    834)] <- 8

  # ESeC class 9: 
  x$esec[x$isco88 %in% c(414, 512, 800, 810, 811, 811, 812, 813, 814, 815, 
    816, 817, 820, 821, 822, 823, 824, 826, 827, 828, 829, 830, 832, 833, 
    900, 910, 912, 913, 914, 915, 916, 920, 921, 930, 931, 932, 933)] <- 9  

  x
}

collapse.esec <- function(x, detail, labels) {

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
  #  - classes 5 and 6 in the six class model: 
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
    x$esec[x$esec %in% 1:2] <- 1
    x$esec[x$esec %in% c(3,6)] <- 2
    x$esec[x$esec %in% c(4,5)] <- 3
    x$esec[x$esec == 7] <- 4
    x$esec[x$esec == 8] <- 5
    x$esec[x$esec == 9] <- 6
  }  

  # Collapse 5 class
  if (detail == 6) {
    x$esec[x$esec %in% 1:2] <- 1
    x$esec[x$esec %in% c(3,6)] <- 2
    x$esec[x$esec %in% c(4,5)] <- 3
    x$esec[x$esec == 7] <- 4
    x$esec[x$esec %in% 8:9] <- 5
  }  

  # Collapse 3 class
  if (detail == 3) {
    x$esec[x$esec %in% 1:2] <- 1
    x$esec[x$esec %in% 3:6] <- 2
    x$esec[x$esec %in% 7:9] <- 3
  }

 # if detail is not equal to 6|5|3, just return the input (no collapse)
 x

}

r.isco88.esec <- function(x, detail = 0) {
  # 1. ISCO88-code (3/4-digit)
  # 2. Employment
  #  1 = Employed => 0
  #  2 = Self-employed => 1
  # 3. Supervising
  #  1 = Yes => 1
  #  2 = No	=> 0
  # 4. Number of employees
  #  1-...

  #x <- data.frame(x)
  
  # STEP 1: fill with simple version
  # --------------------------------

  x$isco88 <- substring(x[,1],1,3) # Make ISCO88 3-digit
  #print(head(x))

  x <- r.simple.esec(x)

  # Return simple version if only one column (oug) provided
  #print('stap1')

  #print('pre step')
  if (ncol(x)-1 == 2) { 
    x <- collapse.esec(x, detail)
    return(x$esec) 
  } 
  #print('stap2')

  # STEP 2: compute employment status categories
  # --------------------------------------------

  # Status variable:
  # (0: missing info) 
  # 1: self-employed 10+ employees
  # 2: small employers <10
  # 3: self-employed, no employees 
  # 4: supervisors
  # 5: employee

  x$status[x$sem == 1 & x$nem > 9] <- 1	
  x$status[x$sem == 1 & x$nem %in% 1:9] <- 2 	# 
  x$status[x$sem == 1 & x$nem %in% c(0,99999)] <- 3  
  x$status[x$sem == 0 & x$sup == 1] <- 4
  x$status[x$sem == 0 & x$sup == 0] <- 5
   
  # STEP 3: recode 5 employment groeps
  # ----------------------------------

  # a) Self-employed 10+ employees. Defaults to 1 
  x$esec[x$status == 1] <- 1
  x$esec[x$status == 1 & x$isco88 %in% c(344, 345)] <- 2
  x$esec[x$status == 1 & x$isco88 %in% c(011, 516)] <- 3
  x$esec[x$status == 1 & x$isco88 == 621] <- 5

  # b) Small employers <10. Defaults to 4
  x$esec[x$status == 2] <- 4
  x$esec[x$status == 2 & x$isco88 %in% c(010, 110, 111, 114, 200, 210, 211, 212, 213, 214, 
    220, 221, 222, 231, 235, 240, 241, 242)] <- 1
  x$esec[x$status == 2 & x$isco88 %in% c(223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 
    310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348)] <- 2
  x$esec[x$status == 2 & x$isco88 %in% c(011, 516)] <- 3
  x$esec[x$status == 2 & x$isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 621, 920, 921)] <- 5

  # c) Self-employed with no employees. Defaults to 4
  x$esec[x$status == 3] <- 4
  x$esec[x$status == 3 & x$isco88 %in% c(010, 110, 111, 114, 200, 210, 211, 212, 213, 214, 
    220, 221, 222, 231, 235, 240, 241, 242)] <- 1
  x$esec[x$status == 3 & x$isco88 %in% c(223, 230, 232, 233, 234, 243, 244, 245, 246, 247, 
    310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348)] <- 2
  x$esec[x$status == 3 & x$isco88 %in% c(011,516)] <- 3
  x$esec[x$status == 3 & x$isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 621, 920, 921)] <- 5

  # d) Supervisors. Defaults to 6
  x$esec[x$status == 4] <- 6
  x$esec[x$status == 4 & x$isco88 %in% c(010, 100, 110, 111, 114, 120, 121, 123, 200, 210, 
    211, 212, 213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
  x$esec[x$status == 4 & x$isco88 %in% c(011, 122, 130,131, 223, 230, 232, 233, 234, 243, 244, 
    245, 246, 247, 300, 310, 311, 312, 313, 314, 320, 321, 322, 323, 330, 331, 332, 333, 334, 
    340,341, 342, 343, 344, 345, 346, 347, 348, 400, 410, 411, 412, 419, 420, 521)] <- 2
  x$esec[x$status == 4 & x$isco88 == 621] <- 5

  # e) Employees
  x$esec[x$status == 5 & x$isco88 %in% c(010, 100,110,111,114,120, 121, 123, 200, 210, 211, 212, 
    213, 214, 220, 221, 222, 231, 235, 240, 241, 242)] <- 1
  x$esec[x$status == 5 & x$isco88 %in% c(122,130,131, 223, 230, 232, 233, 234, 243, 244, 245, 246, 
    247, 310, 311, 312, 314, 320, 321, 322, 323, 334, 342, 344, 345, 348, 521)] <- 2
  x$esec[x$status == 5 & x$isco88 %in% c(011, 300, 330, 331, 332, 333, 340, 341, 343, 346, 347, 400, 
    410, 411, 412, 419, 420)] <- 3
  x$esec[x$status == 5 & x$isco88 == 621] <- 5
  x$esec[x$status == 5 & x$isco88 %in% c(313, 315, 730,731)] <- 6
  x$esec[x$status == 5 & x$isco88 %in% c(413, 421, 422, 500, 510, 511, 513, 514, 516, 520, 522, 911)] <- 7
  x$esec[x$status == 5 & x$isco88 %in% c(600, 610, 611, 612, 613, 614, 615, 700, 710, 711, 712, 713, 
    714, 720, 721, 722, 723, 724, 732, 733, 734, 740, 741, 742, 743, 744, 825, 831, 834)] <- 8
  x$esec[x$status == 5 & x$isco88 %in% c(414, 512, 800, 810, 811,  812, 813, 814, 815, 816, 817, 820, 
    821, 822, 823, 824, 826, 827, 828, 829, 830, 832, 833,900, 910, 912, 913, 914, 915, 916, 920, 921, 
    930, 931, 932, 933)] <- 9


  return(x$esec)
}
