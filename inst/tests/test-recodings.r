data.ganzeboom <- read.table(system.file("data/ganzeboom.txt.gz", package = "strat"),
                             header=T)

test_that("ISCO88 to SIOPS recoding", {
  # single column data-frame, "oug"
  siops <- r.isco88.siops(data.frame(oug=data.ganzeboom$ISCO88))
  expect_equal(data.ganzeboom$SIOPS, siops)
})

test_that("ISCO88 to ISEI recoding", {
  isei <- r.isco88.isei(data.frame(oug=data.ganzeboom$ISCO88))
  
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
  egp <- r.isco88.egp(data.frame(oug=data.ganzeboom$ISCO88))

  # SIOPS ISEI EGP    ISCO884
  #   36   51  3(4?)  4122 
  #   38   34  8(9?) 	7000

  expect_equal(data.ganzeboom$EGP, egp)
})

test_that("ISCO88 to ICAM recoding", {
  #TODO: quid missigns voor default ISCO88-codes?
  icam <- r.isco88.icam(data.frame(oug=data.ganzeboom$ISCO88))
  expect_equal(icam[1:5], c(65.07, 69.02, 70.82, 70.84, 49.86))
})

test_that("ISCO88 to ESeC recoding (simple)", {
  #TODO: quid missigns voor default ISCO88-codes?
  esec <- isco88_esec(data.frame(oug=data.ganzeboom$ISCO88))
  expect_equal(esec[1:3], rep(1,3))
})