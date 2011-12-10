# ########################################## #
# TEST LABELS FOR OUG/STRATIFICATION SCHEMAS #
# ########################################## #

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


test_that('ESeC 3 class labels work', {
  expect_equal(label_esec(c(3,NA,1,2,2,2,3,NA)),
    factor(c(3,NA,1,2,2,2,3,NA), level=1:3, 
      label=c('Salariat', 'Intermediate employee', 'Working class')))
  
})

test_that('ESeC 10 class labeling works', {
  expect_equal(label_esec(1:10),factor(1:10, level=1:10, label=short10))
})

test_that('ESeC 10 class labeling fails', {
  expect_warning(label_esec(1:11), 'Non-standard number of classes, no labels assigned')
})

test_that('ESeC class labeling fails with single class', {
  expect_warning(label_esec(rep(1,10)), 'Non-standard number of classes, no labels assigned')
})

test_that('ESeC 6 class labeling works', {
  expect_equal(label_esec(c(5,6,1:6,2,NA)),
    factor(c(5,6,1:6,2,NA), level=1:6, label=short6))
})

test_that('ESeC 5 class labeling works', {
  expect_equal(label_esec(c(1:5)),
    factor(c(1:5), level=1:5, label=short5))
})  
  
