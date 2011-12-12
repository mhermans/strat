data.ganzeboom <- read.table(system.file("data/ganzeboom.txt.gz", package = "strat"),
                             header=T)

#data.ess <- read.table(file.path('C:/Users/MaHermans/Documents/projects/stable/strat/',
#                                 'testdata/ESS5_occupationvars.dat')

# TEST BASIC RECODINGS
# --------------------

# 

test_that("ISCO88 to SIOPS recoding", {
  # single column data-frame, "oug"
  siops <- isco88_siops(data.frame(isco88=data.ganzeboom$ISCO88))
  expect_equal(data.ganzeboom$SIOPS, siops)
})

test_that("ISCO88 to ISEI recoding", {
  isei <- isco88_isei(data.frame(isco88=data.ganzeboom$ISCO88))
  
  # TODO:
  # Mismatch Appendix A en iskoisei.sps
  # recode @isko (6134=28) into @isei.
  # recode @isko (7520=39) into @isei.
  # 46  38   8  7520   [SKILLED WORKERS NFS]
  # 30  23  10	6134   [Skilled farm workers nfs]
  # fix missings according to Appendix:
  isei[297] <- 23 
  isei[402] <- 38
  
  expect_equal(data.ganzeboom$ISEI, isei)
})

test_that("ISCO88 to EGP recoding (simple)", {
  egp <- isco88_egp(data.frame(isco88=data.ganzeboom$ISCO88))

  # SIOPS ISEI EGP    ISCO884
  #   36   51  3(4?)  4122 
  #   38   34  8(9?) 	7000

  expect_equal(data.ganzeboom$EGP, egp)
})

test_that("ISCO88 to ICAM recoding", {
  #TODO: quid missigns voor default ISCO88-codes?
  icam <- isco88_icam(data.frame(isco88=data.ganzeboom$ISCO88))
  expect_equal(icam[1:5], c(65.07, 69.02, 70.82, 70.84, 49.86))
})

test_that('ISCO88 to ESec, simple function', {
  isco88_3d <- substring(data.ganzeboom$ISCO88,1,3)
  isco88 <- data.frame(isco88=isco88_3d)
  esec <- isco88_esec_simple(isco88)
})

test_that("ISCO88 to ESeC recoding (simple)", {
  #TODO: quid missigns voor default ISCO88-codes?
  input <- isco88_isco88(data.ganzeboom$ISCO88, detail=3)
  input <- data.frame(isco88=input)
  esec <- isco88_esec(input)
  expect_equal(esec[1:3], rep(1,3))
})

#test_that("ISCO88 to ESeC recoding (full)", {
#  expect_equal(1,1)
#})

# TEST HIGHER LEVEL RECODE FUNCTIONS
# ----------------------------------

test_that('isei() works', {
  i <- isei(data.ganzeboom$ISCO88)
  expect_equal(i, data.ganzeboom$ISEI)
})

test_that('esec() works', {
  e <- esec(data.ganzeboom$ISCO88)
  expect_equal(e[1:3], rep(1,3))
})

test_that('formatting works', {
  # vectors to dataframes
  expect_equal(format_input(1228:1230), data.frame(isco88=1228:1230))
  expect_equal(format_input(2030), data.frame(isco88=2030))
  
  #expect_equal(
  #  format_input(c(1228, 1230, 9830), detail=3), 
  #  data.frame(isco88=c('122','123', '983'),stringsAsFactors=F)
  #)
})

test_that('main ISCO88-ESeC function works', {
  # direct 3digit ISCO88 should work OK
  expect_equal(isco88_esec(data.frame(isco88=c(743, 413, 344))), c(8, 7, 2))
})
