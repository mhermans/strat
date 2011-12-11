
r.isco88.guveli <- function(data, detail) {

  data$egp <- recode(data, informat="isco88", outformat="egp")  
  data$guveli <- data$egp + 2 # shift categories from 3, lower 
  data$guveli[data$guveli %in% 1:4] <- NA

  # Ia. High-grade Technocrats 
  data$guveli[data$egp == 1 & data$oug %in% c(1000, 1100, 1110, 1120, 1200, 
    1210, 1220, 1222, 1223, 1224, 1225, 1226, 1227, 1228, 1229,
    1230, 1231, 1232, 1233, 1234, 1235, 1236, 1237, 1239, 1250, 
    1251, 2000, 2100, 2110, 2111, 2112, 2113, 2114, 2120, 2121,
    2122, 2130, 2131, 2140, 2142, 2143, 2144, 2145, 2146, 2147, 
    2411, 2420, 2443, 3143, 3144)] <- 1

  # Ib. High-grade Social and Cultural Specialists
  data$guveli[data$egp == 1 & data$oug %in% c(2141, 2213, 2220, 2221, 2222, 
    2223, 2224, 2229, 2310, 2350, 2351, 2352, 2400, 2421, 2422, 
    2429, 2440, 2441, 2442, 2445)] <- 2

  # IIa. Low-grade Technocrats 
  data$guveli[data$egp == 2 & data$oug %in% c(1130, 1140, 1141, 1142, 1143, 
    1240, 1252, 1300, 1310, 1312, 1313, 1314, 1315, 1316, 1317, 
    1318, 1319, 2132, 2139, 2148, 2410, 3000, 3100, 3110, 3111, 
    3112, 3113, 3114, 3115, 3116, 3117, 3118, 3119, 3120, 3121, 
    3122, 3123, 3130, 3132, 3133, 3139, 3140, 3141, 3142, 3145, 
    3150, 3151, 3152, 3211, 3212, 3213, 3220, 3221, 3222, 3223,
    3224, 3225, 3226, 3227, 3228, 3400, 3410, 3411, 3412, 3413, 
    3414, 3415, 3416, 3417, 3419, 3420, 3421, 3422, 3423, 3429,
    3431, 3432, 3434, 3440, 3441, 3442, 3443, 3444, 3449, 3450, 
    3451, 3475)] <- 3
  
  # IIb. Low-grade Social and Cultural Specialists
  data$guveli[data$egp == 2 & data$oug %in% c(2230, 2300, 2320, 2321, 
    2322, 2323, 2330, 2331, 2332, 2340, 2359, 2412, 2419, 2430, 
    2431, 2432, 2444, 2446, 2450, 2451, 2452, 2453, 2454, 2455, 
    2460, 2470, 3131, 3200, 3210, 3229, 3240, 3241, 3242, 3470, 
    3471, 3472, 3473, 3474, 5150, 5151, 5152)] <- 4
  
  #lbls <- c(	'High-grade Technocrats',
  #			'High-grade Social and Cultural Specialists',
  #			'Low-grade Technocrats',
  #			'Low-grade Social and Cultural Specialists',
  #			...

  data$guveli
}
