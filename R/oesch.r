
r.isco88.oesch <- function(x, detail) {

  # 1. Large employers
  # 2. Self-employed professionals
  # 3. Petite bourgeoisie w employees
  # 4. Petite bourgeoisie wo employees
  # 5. Technical experts
  # 6. Technicians
  # 7. Skilled craft
  # 8. Higher-grade managers and administrators
  # 9. Accociate managers and administrators
  # 10. Skilled office
  # 11. Sociocultural professionals
  # 12. Sociocultural semi-professionals
  # 13. Skilled service
  # 14. Routine operatives
  # 15. Routine agreculture
  # 16. Routine office
  # 17. Routine service

  # Large employers (more then 9 empl and selfempl)
  x$oesch[x$sem == 1 & x$nem > 9] <- 1

  # Self-employed professionals
  x$oesch[x$sem == 1 & x$nem < 10 & x$oug %in% 2000:2470] <- 2
  
  # Petite bourgeoisie w employees
  x$oesch[x$sem == 1 & x$nem < 10 & !(x$oug %in% 2000:2470)] <- 3
  
  # Petite bourgeoisie wo employees
  x$oesch[x$sem == 1 & (x$nem == 0 | is.na(x$nem)) 
	& !(x$oug %in% 2000:2470)] <- 4

  x$oesch[x$sem == 0 & x$oug %in% 2100:2213] <- 5
  x$oesch[x$sem == 0 & x$oug %in% c(3100:3213, 3471)] <- 6
  x$oesch[x$sem == 0 & x$oug %in% c(110, 7120:7142, 7200:7233, 7240:7423, 7430:7520, 8311, 
    8324, 8333)] <- 7
  x$oesch[x$sem == 0 & x$oug %in% c(1000:1251, 2410:2419, 2441, 2470)] <- 8
  x$oesch[x$sem == 0 & x$oug %in% c(1252:1319, 3410:3449, 3452)] <- 9
  x$oesch[x$sem == 0 & x$oug %in% c(4000:4112, 4114:4141, 4143, 4190:4210, 4213:4221)] <- 10
  x$oesch[x$sem == 0 & x$oug %in% c(2220:2323, 2350:2351, 2359, 2420:2440, 2442:2443, 2445, 
    2451, 2460)] <- 11
  x$oesch[x$sem == 0 & x$oug %in% c(2330:2340, 2352, 2444, 2446:2450, 2452:2455, 3220, 
    3222:3224, 3226, 3229:3232, 3240:3400, 3450:3451, 3460:3470, 3472:3480)] <- 12
  x$oesch[x$sem == 0 & x$oug %in% c(3221, 3225, 3227:3228, 5122, 5141, 5143, 5110:5113, 
   5150:5163, 5200:5210, 8323)] <- 13
  x$oesch[x$sem == 0 & x$oug %in% c(7100:7113, 7129:7130, 7143, 7234, 7424, 8000:8310, 
   8312, 8334:8400, 9160:9162, 9300:9333)] <- 14
  x$oesch[x$sem == 0 & x$oug %in% c(6010:6210, 8330:8332, 9200:9213)] <- 15
  x$oesch[x$sem == 0 & x$oug %in% c(4113, 4142, 4144, 4211:4212, 4222:4223)] <- 16
  x$oesch[x$sem == 0 & x$oug %in% c(5120:5121, 5123:5130, 5131:5140, 5142, 5149, 5169, 
   5220:5230, 8320:8322, 9100:9153)] <- 17

  # militair, 3digit?
  x$oesch[x$oug %in% c(110, 111, 112)] <- 8 

  x$oesch
}
