
isco88_icam <- function(data, detail=0) {
	#ISCO88->ICAM, versie april 14 2010
	
	data$icam[data$oug == 1000] <- 65.07
	data$icam[data$oug == 1100] <- 69.02
	data$icam[data$oug == 1110] <- 70.82
	data$icam[data$oug == 1120] <- 70.84
	data$icam[data$oug == 1130] <- 49.86
	data$icam[data$oug == 1140] <- 64.05
	data$icam[data$oug == 1141] <- 64.05
	data$icam[data$oug == 1142] <- 64.05
	data$icam[data$oug == 1143] <- 64.05
	data$icam[data$oug == 1200] <- 67.59
	data$icam[data$oug == 1210] <- 66.87
	data$icam[data$oug == 1220] <- 62.86
	data$icam[data$oug == 1221] <- 41.55
	data$icam[data$oug == 1222] <- 60.13
	data$icam[data$oug == 1223] <- 60.13
	data$icam[data$oug == 1224] <- 58.52
	data$icam[data$oug == 1225] <- 58.52
	data$icam[data$oug == 1226] <- 58.52
	data$icam[data$oug == 1227] <- 58.52
	data$icam[data$oug == 1228] <- 60.54
	data$icam[data$oug == 1229] <- 60.13
	data$icam[data$oug == 1230] <- 69.14
	data$icam[data$oug == 1231] <- 67.11
	data$icam[data$oug == 1232] <- 67.11
	data$icam[data$oug == 1233] <- 67.11
	data$icam[data$oug == 1234] <- 67.11
	data$icam[data$oug == 1235] <- 67.11
	data$icam[data$oug == 1236] <- 67.11
	data$icam[data$oug == 1237] <- 76.04
	data$icam[data$oug == 1239] <- 67.11
	data$icam[data$oug == 1300] <- 57.81
	data$icam[data$oug == 1310] <- 57.27
	data$icam[data$oug == 1311] <- 41.55
	data$icam[data$oug == 1312] <- 54.51
	data$icam[data$oug == 1313] <- 56.18
	data$icam[data$oug == 1314] <- 56.18
	data$icam[data$oug == 1315] <- 56.18
	data$icam[data$oug == 1316] <- 54.51
	data$icam[data$oug == 1317] <- 56.18
	data$icam[data$oug == 1318] <- 54.51
	data$icam[data$oug == 1319] <- 56.18
	data$icam[data$oug == 2000] <- 70.89
	data$icam[data$oug == 2100] <- 75.42
	data$icam[data$oug == 2110] <- 81.92
	data$icam[data$oug == 2111] <- 80.22
	data$icam[data$oug == 2112] <- 80.22
	data$icam[data$oug == 2113] <- 80.22
	data$icam[data$oug == 2114] <- 80.22
	data$icam[data$oug == 2120] <- 85.27
	data$icam[data$oug == 2121] <- 85.27
	data$icam[data$oug == 2122] <- 85.27
	data$icam[data$oug == 2130] <- 75.15
	data$icam[data$oug == 2131] <- 75.39
	data$icam[data$oug == 2132] <- 72.17
	data$icam[data$oug == 2139] <- 75.39
	data$icam[data$oug == 2140] <- 73.00
	data$icam[data$oug == 2141] <- 73.00
	data$icam[data$oug == 2142] <- 73.00
	data$icam[data$oug == 2143] <- 73.00
	data$icam[data$oug == 2144] <- 73.00
	data$icam[data$oug == 2145] <- 73.00
	data$icam[data$oug == 2146] <- 73.00
	data$icam[data$oug == 2147] <- 73.00
	data$icam[data$oug == 2148] <- 73.00
	data$icam[data$oug == 2149] <- 73.00
	data$icam[data$oug == 2200] <- 70.25
	data$icam[data$oug == 2210] <- 68.98
	data$icam[data$oug == 2211] <- 68.98
	data$icam[data$oug == 2212] <- 68.98
	data$icam[data$oug == 2213] <- 68.98
	data$icam[data$oug == 2220] <- 78.57
	data$icam[data$oug == 2221] <- 78.57
	data$icam[data$oug == 2222] <- 78.57
	data$icam[data$oug == 2223] <- 78.57
	data$icam[data$oug == 2224] <- 78.57
	data$icam[data$oug == 2229] <- 78.57
	data$icam[data$oug == 2230] <- 63.21
	data$icam[data$oug == 2300] <- 69.75
	data$icam[data$oug == 2310] <- 82.71
	data$icam[data$oug == 2320] <- 71.89
	data$icam[data$oug == 2330] <- 63.79
	data$icam[data$oug == 2331] <- 63.79
	data$icam[data$oug == 2332] <- 63.79
	data$icam[data$oug == 2340] <- 73.49
	data$icam[data$oug == 2350] <- 68.47
	data$icam[data$oug == 2351] <- 68.47
	data$icam[data$oug == 2352] <- 68.47
	data$icam[data$oug == 2359] <- 68.47
	data$icam[data$oug == 2400] <- 74.02
	data$icam[data$oug == 2410] <- 68.40
	data$icam[data$oug == 2411] <- 68.40
	data$icam[data$oug == 2412] <- 68.40
	data$icam[data$oug == 2419] <- 68.40
	data$icam[data$oug == 2420] <- 80.43
	data$icam[data$oug == 2421] <- 80.43
	data$icam[data$oug == 2422] <- 80.43
	data$icam[data$oug == 2429] <- 80.43
	data$icam[data$oug == 2430] <- 72.95
	data$icam[data$oug == 2431] <- 72.95
	data$icam[data$oug == 2432] <- 72.95
	data$icam[data$oug == 2440] <- 76.83
	data$icam[data$oug == 2441] <- 76.83
	data$icam[data$oug == 2442] <- 76.83
	data$icam[data$oug == 2443] <- 76.83
	data$icam[data$oug == 2444] <- 76.83
	data$icam[data$oug == 2445] <- 76.83
	data$icam[data$oug == 2446] <- 76.83
	data$icam[data$oug == 2450] <- 77.15
	data$icam[data$oug == 2451] <- 80.08
	data$icam[data$oug == 2452] <- 73.32
	data$icam[data$oug == 2453] <- 73.32
	data$icam[data$oug == 2454] <- 73.32
	data$icam[data$oug == 2455] <- 73.32
	data$icam[data$oug == 2460] <- 73.02
	data$icam[data$oug == 3000] <- 61.26
	data$icam[data$oug == 3100] <- 56.68
	data$icam[data$oug == 3110] <- 54.25
	data$icam[data$oug == 3111] <- 53.17
	data$icam[data$oug == 3112] <- 53.53
	data$icam[data$oug == 3113] <- 53.17
	data$icam[data$oug == 3114] <- 53.17
	data$icam[data$oug == 3115] <- 53.53
	data$icam[data$oug == 3116] <- 53.17
	data$icam[data$oug == 3117] <- 53.17
	data$icam[data$oug == 3118] <- 53.17
	data$icam[data$oug == 3119] <- 53.17
	data$icam[data$oug == 3120] <- 63.23
	data$icam[data$oug == 3121] <- 63.01
	data$icam[data$oug == 3122] <- 63.01
	data$icam[data$oug == 3123] <- 63.01
	data$icam[data$oug == 3130] <- 61.73
	data$icam[data$oug == 3131] <- 61.73
	data$icam[data$oug == 3132] <- 61.73
	data$icam[data$oug == 3133] <- 61.73
	data$icam[data$oug == 3139] <- 61.73
	data$icam[data$oug == 3140] <- 56.89
	data$icam[data$oug == 3141] <- 56.89
	data$icam[data$oug == 3142] <- 56.89
	data$icam[data$oug == 3143] <- 56.89
	data$icam[data$oug == 3144] <- 56.89
	data$icam[data$oug == 3145] <- 56.89
	data$icam[data$oug == 3150] <- 50.50
	data$icam[data$oug == 3151] <- 50.50
	data$icam[data$oug == 3152] <- 50.50
	data$icam[data$oug == 3200] <- 59.23
	data$icam[data$oug == 3210] <- 56.58
	data$icam[data$oug == 3211] <- 56.58
	data$icam[data$oug == 3212] <- 56.58
	data$icam[data$oug == 3213] <- 56.58
	data$icam[data$oug == 3220] <- 68.70
	data$icam[data$oug == 3221] <- 60.63
	data$icam[data$oug == 3222] <- 58.95
	data$icam[data$oug == 3223] <- 58.95
	data$icam[data$oug == 3224] <- 58.95
	data$icam[data$oug == 3225] <- 58.95
	data$icam[data$oug == 3226] <- 60.63
	data$icam[data$oug == 3227] <- 58.95
	data$icam[data$oug == 3228] <- 58.95
	data$icam[data$oug == 3229] <- 60.63
	data$icam[data$oug == 3230] <- 58.79
	data$icam[data$oug == 3231] <- 58.79
	data$icam[data$oug == 3232] <- 58.79
	data$icam[data$oug == 3240] <- 48.10
	data$icam[data$oug == 3241] <- 48.10
	data$icam[data$oug == 3242] <- 48.10
	data$icam[data$oug == 3300] <- 62.76
	data$icam[data$oug == 3310] <- 66.16
	data$icam[data$oug == 3320] <- 57.73
	data$icam[data$oug == 3330] <- 66.05
	data$icam[data$oug == 3340] <- 64.70
	data$icam[data$oug == 3400] <- 60.89
	data$icam[data$oug == 3410] <- 60.99
	data$icam[data$oug == 3411] <- 59.95
	data$icam[data$oug == 3412] <- 59.95
	data$icam[data$oug == 3413] <- 59.95
	data$icam[data$oug == 3414] <- 59.95
	data$icam[data$oug == 3415] <- 59.95
	data$icam[data$oug == 3416] <- 54.30
	data$icam[data$oug == 3417] <- 59.95
	data$icam[data$oug == 3419] <- 54.30
	data$icam[data$oug == 3420] <- 60.84
	data$icam[data$oug == 3421] <- 60.84
	data$icam[data$oug == 3422] <- 60.84
	data$icam[data$oug == 3423] <- 60.84
	data$icam[data$oug == 3429] <- 60.84
	data$icam[data$oug == 3430] <- 59.99
	data$icam[data$oug == 3431] <- 58.22
	data$icam[data$oug == 3432] <- 60.33
	data$icam[data$oug == 3433] <- 58.22
	data$icam[data$oug == 3434] <- 60.33
	data$icam[data$oug == 3439] <- 58.22
	data$icam[data$oug == 3440] <- 61.24
	data$icam[data$oug == 3441] <- 57.79
	data$icam[data$oug == 3442] <- 57.79
	data$icam[data$oug == 3443] <- 57.79
	data$icam[data$oug == 3444] <- 57.79
	data$icam[data$oug == 3449] <- 55.23
	data$icam[data$oug == 3450] <- 53.56
	data$icam[data$oug == 3460] <- 61.32
	data$icam[data$oug == 3470] <- 63.04
	data$icam[data$oug == 3471] <- 67.45
	data$icam[data$oug == 3472] <- 61.96
	data$icam[data$oug == 3473] <- 61.96
	data$icam[data$oug == 3474] <- 61.96
	data$icam[data$oug == 3475] <- 61.96
	data$icam[data$oug == 3480] <- 64.85
	data$icam[data$oug == 4000] <- 55.54
	data$icam[data$oug == 4100] <- 55.33
	data$icam[data$oug == 4110] <- 59.26
	data$icam[data$oug == 4111] <- 59.26
	data$icam[data$oug == 4112] <- 59.26
	data$icam[data$oug == 4113] <- 59.26
	data$icam[data$oug == 4114] <- 59.26
	data$icam[data$oug == 4115] <- 59.26
	data$icam[data$oug == 4120] <- 56.87
	data$icam[data$oug == 4121] <- 55.80
	data$icam[data$oug == 4122] <- 55.80
	data$icam[data$oug == 4130] <- 44.97
	data$icam[data$oug == 4131] <- 44.97
	data$icam[data$oug == 4132] <- 44.97
	data$icam[data$oug == 4133] <- 44.97
	data$icam[data$oug == 4140] <- 48.77
	data$icam[data$oug == 4141] <- 48.77
	data$icam[data$oug == 4142] <- 48.77
	data$icam[data$oug == 4143] <- 48.77
	data$icam[data$oug == 4144] <- 48.77
	data$icam[data$oug == 4190] <- 56.27
	data$icam[data$oug == 4200] <- 52.33
	data$icam[data$oug == 4210] <- 50.16
	data$icam[data$oug == 4211] <- 50.16
	data$icam[data$oug == 4212] <- 50.16
	data$icam[data$oug == 4213] <- 50.16
	data$icam[data$oug == 4214] <- 50.16
	data$icam[data$oug == 4215] <- 50.16
	data$icam[data$oug == 4220] <- 54.96
	data$icam[data$oug == 4221] <- 54.96
	data$icam[data$oug == 4222] <- 54.96
	data$icam[data$oug == 4223] <- 54.96
	data$icam[data$oug == 5000] <- 43.65
	data$icam[data$oug == 5100] <- 43.44
	data$icam[data$oug == 5110] <- 51.45
	data$icam[data$oug == 5111] <- 51.45
	data$icam[data$oug == 5112] <- 51.45
	data$icam[data$oug == 5113] <- 51.45
	data$icam[data$oug == 5120] <- 38.29
	data$icam[data$oug == 5121] <- 38.29
	data$icam[data$oug == 5122] <- 38.29
	data$icam[data$oug == 5123] <- 38.29
	data$icam[data$oug == 5130] <- 45.77
	data$icam[data$oug == 5131] <- 45.77
	data$icam[data$oug == 5132] <- 45.77
	data$icam[data$oug == 5133] <- 45.77
	data$icam[data$oug == 5139] <- 45.77
	data$icam[data$oug == 5140] <- 46.24
	data$icam[data$oug == 5141] <- 46.16
	data$icam[data$oug == 5142] <- 25.51
	data$icam[data$oug == 5143] <- 43.35
	data$icam[data$oug == 5149] <- 43.35
	data$icam[data$oug == 5150] <- 33.43
	data$icam[data$oug == 5151] <- 33.43
	data$icam[data$oug == 5152] <- 33.43
	data$icam[data$oug == 5160] <- 45.99
	data$icam[data$oug == 5161] <- 44.73
	data$icam[data$oug == 5162] <- 44.73
	data$icam[data$oug == 5163] <- 44.73
	data$icam[data$oug == 5169] <- 44.73
	data$icam[data$oug == 5200] <- 44.41
	data$icam[data$oug == 5210] <- 49.36
	data$icam[data$oug == 5210] <- 49.36
	data$icam[data$oug == 5220] <- 45.26
	data$icam[data$oug == 5220] <- 45.26
	data$icam[data$oug == 5230] <- 35.48
	data$icam[data$oug == 5230] <- 35.48
	data$icam[data$oug == 6000] <- 36.49
	data$icam[data$oug == 6100] <- 38.31
	data$icam[data$oug == 6110] <- 38.20
	data$icam[data$oug == 6111] <- 32.13
	data$icam[data$oug == 6112] <- 36.35
	data$icam[data$oug == 6113] <- 36.35
	data$icam[data$oug == 6114] <- 33.15
	data$icam[data$oug == 6120] <- 42.35
	data$icam[data$oug == 6121] <- 42.17
	data$icam[data$oug == 6122] <- 42.17
	data$icam[data$oug == 6123] <- 42.17
	data$icam[data$oug == 6124] <- 42.17
	data$icam[data$oug == 6129] <- 42.17
	data$icam[data$oug == 6130] <- 41.55
	data$icam[data$oug == 6140] <- 30.08
	data$icam[data$oug == 6141] <- 26.19
	data$icam[data$oug == 6142] <- 13.19
	data$icam[data$oug == 6150] <- 21.98
	data$icam[data$oug == 6151] <- 33.33
	data$icam[data$oug == 6152] <- 19.64
	data$icam[data$oug == 6153] <- 19.64
	data$icam[data$oug == 6154] <- 13.19
	data$icam[data$oug == 6200] <- 13.19
	data$icam[data$oug == 6210] <- 13.19
	data$icam[data$oug == 6210] <- 13.19
	data$icam[data$oug == 7000] <- 34.89
	data$icam[data$oug == 7100] <- 32.92
	data$icam[data$oug == 7110] <- 29.42
	data$icam[data$oug == 7111] <- 26.39
	data$icam[data$oug == 7112] <- 26.39
	data$icam[data$oug == 7113] <- 26.39
	data$icam[data$oug == 7120] <- 31.41
	data$icam[data$oug == 7121] <- 30.68
	data$icam[data$oug == 7122] <- 30.68
	data$icam[data$oug == 7123] <- 26.14
	data$icam[data$oug == 7124] <- 30.68
	data$icam[data$oug == 7129] <- 30.68
	data$icam[data$oug == 7130] <- 39.00
	data$icam[data$oug == 7131] <- 39.00
	data$icam[data$oug == 7132] <- 39.00
	data$icam[data$oug == 7133] <- 39.00
	data$icam[data$oug == 7134] <- 39.00
	data$icam[data$oug == 7135] <- 39.00
	data$icam[data$oug == 7136] <- 39.00
	data$icam[data$oug == 7137] <- 39.00
	data$icam[data$oug == 7140] <- 32.95
	data$icam[data$oug == 7141] <- 32.24
	data$icam[data$oug == 7142] <- 32.94
	data$icam[data$oug == 7143] <- 32.94
	data$icam[data$oug == 7200] <- 37.97
	data$icam[data$oug == 7210] <- 33.30
	data$icam[data$oug == 7211] <- 33.30
	data$icam[data$oug == 7212] <- 33.30
	data$icam[data$oug == 7213] <- 33.30
	data$icam[data$oug == 7214] <- 33.30
	data$icam[data$oug == 7215] <- 33.30
	data$icam[data$oug == 7216] <- 33.30
	data$icam[data$oug == 7220] <- 35.65
	data$icam[data$oug == 7221] <- 35.65
	data$icam[data$oug == 7222] <- 35.65
	data$icam[data$oug == 7223] <- 35.65
	data$icam[data$oug == 7224] <- 35.65
	data$icam[data$oug == 7230] <- 39.82
	data$icam[data$oug == 7231] <- 39.43
	data$icam[data$oug == 7232] <- 50.81
	data$icam[data$oug == 7233] <- 35.44
	data$icam[data$oug == 7240] <- 43.57
	data$icam[data$oug == 7241] <- 42.58
	data$icam[data$oug == 7242] <- 42.58
	data$icam[data$oug == 7243] <- 46.59
	data$icam[data$oug == 7244] <- 42.58
	data$icam[data$oug == 7245] <- 42.58
	data$icam[data$oug == 7300] <- 43.86
	data$icam[data$oug == 7310] <- 46.82
	data$icam[data$oug == 7311] <- 45.98
	data$icam[data$oug == 7312] <- 45.98
	data$icam[data$oug == 7313] <- 45.98
	data$icam[data$oug == 7320] <- 32.12
	data$icam[data$oug == 7321] <- 32.12
	data$icam[data$oug == 7322] <- 32.12
	data$icam[data$oug == 7323] <- 32.12
	data$icam[data$oug == 7324] <- 32.12
	data$icam[data$oug == 7330] <- 39.72
	data$icam[data$oug == 7331] <- 38.27
	data$icam[data$oug == 7332] <- 38.27
	data$icam[data$oug == 7340] <- 47.39
	data$icam[data$oug == 7341] <- 47.39
	data$icam[data$oug == 7342] <- 47.39
	data$icam[data$oug == 7343] <- 47.39
	data$icam[data$oug == 7344] <- 47.39
	data$icam[data$oug == 7345] <- 47.39
	data$icam[data$oug == 7346] <- 47.39
	data$icam[data$oug == 7400] <- 32.57
	data$icam[data$oug == 7410] <- 31.22
	data$icam[data$oug == 7411] <- 28.43
	data$icam[data$oug == 7412] <- 31.36
	data$icam[data$oug == 7413] <- 31.36
	data$icam[data$oug == 7414] <- 28.43
	data$icam[data$oug == 7415] <- 28.43
	data$icam[data$oug == 7416] <- 31.22
	data$icam[data$oug == 7420] <- 34.59
	data$icam[data$oug == 7421] <- 28.25
	data$icam[data$oug == 7422] <- 35.48
	data$icam[data$oug == 7423] <- 28.25
	data$icam[data$oug == 7424] <- 28.25
	data$icam[data$oug == 7430] <- 34.28
	data$icam[data$oug == 7431] <- 31.58
	data$icam[data$oug == 7432] <- 31.58
	data$icam[data$oug == 7433] <- 31.58
	data$icam[data$oug == 7434] <- 34.44
	data$icam[data$oug == 7435] <- 34.44
	data$icam[data$oug == 7436] <- 31.58
	data$icam[data$oug == 7437] <- 31.58
	data$icam[data$oug == 7440] <- 25.96
	data$icam[data$oug == 7441] <- 22.20
	data$icam[data$oug == 7442] <- 25.30
	data$icam[data$oug == 8000] <- 32.80
	data$icam[data$oug == 8100] <- 33.31
	data$icam[data$oug == 8110] <- 36.94
	data$icam[data$oug == 8111] <- 32.92
	data$icam[data$oug == 8112] <- 36.75
	data$icam[data$oug == 8113] <- 36.75
	data$icam[data$oug == 8120] <- 27.00
	data$icam[data$oug == 8121] <- 27.00
	data$icam[data$oug == 8122] <- 27.00
	data$icam[data$oug == 8123] <- 27.00
	data$icam[data$oug == 8124] <- 27.00
	data$icam[data$oug == 8130] <- 25.76
	data$icam[data$oug == 8131] <- 25.76
	data$icam[data$oug == 8139] <- 25.76
	data$icam[data$oug == 8140] <- 31.66
	data$icam[data$oug == 8141] <- 31.66
	data$icam[data$oug == 8142] <- 31.66
	data$icam[data$oug == 8143] <- 31.66
	data$icam[data$oug == 8150] <- 37.27
	data$icam[data$oug == 8151] <- 37.27
	data$icam[data$oug == 8152] <- 37.27
	data$icam[data$oug == 8153] <- 37.27
	data$icam[data$oug == 8154] <- 37.27
	data$icam[data$oug == 8155] <- 37.27
	data$icam[data$oug == 8159] <- 37.27
	data$icam[data$oug == 8160] <- 36.70
	data$icam[data$oug == 8161] <- 37.80
	data$icam[data$oug == 8162] <- 32.97
	data$icam[data$oug == 8163] <- 32.97
	data$icam[data$oug == 8170] <- 37.92
	data$icam[data$oug == 8171] <- 37.92
	data$icam[data$oug == 8172] <- 37.92
	data$icam[data$oug == 8200] <- 32.71
	data$icam[data$oug == 8210] <- 30.77
	data$icam[data$oug == 8211] <- 30.77
	data$icam[data$oug == 8212] <- 30.77
	data$icam[data$oug == 8220] <- 33.41
	data$icam[data$oug == 8221] <- 28.95
	data$icam[data$oug == 8222] <- 28.95
	data$icam[data$oug == 8223] <- 28.95
	data$icam[data$oug == 8224] <- 45.26
	data$icam[data$oug == 8229] <- 28.95
	data$icam[data$oug == 8230] <- 28.18
	data$icam[data$oug == 8231] <- 28.18
	data$icam[data$oug == 8232] <- 28.18
	data$icam[data$oug == 8240] <- 26.09
	data$icam[data$oug == 8250] <- 40.54
	data$icam[data$oug == 8251] <- 40.88
	data$icam[data$oug == 8252] <- 40.88
	data$icam[data$oug == 8253] <- 40.88
	data$icam[data$oug == 8260] <- 30.00
	data$icam[data$oug == 8261] <- 24.65
	data$icam[data$oug == 8262] <- 24.65
	data$icam[data$oug == 8263] <- 29.14
	data$icam[data$oug == 8264] <- 29.14
	data$icam[data$oug == 8265] <- 24.65
	data$icam[data$oug == 8266] <- 24.65
	data$icam[data$oug == 8269] <- 24.65
	data$icam[data$oug == 8270] <- 28.38
	data$icam[data$oug == 8271] <- 28.34
	data$icam[data$oug == 8272] <- 28.34
	data$icam[data$oug == 8273] <- 28.34
	data$icam[data$oug == 8274] <- 28.34
	data$icam[data$oug == 8275] <- 28.34
	data$icam[data$oug == 8276] <- 28.34
	data$icam[data$oug == 8277] <- 28.34
	data$icam[data$oug == 8278] <- 28.34
	data$icam[data$oug == 8279] <- 28.34
	data$icam[data$oug == 8280] <- 37.52
	data$icam[data$oug == 8281] <- 37.52
	data$icam[data$oug == 8282] <- 37.52
	data$icam[data$oug == 8283] <- 37.52
	data$icam[data$oug == 8284] <- 37.52
	data$icam[data$oug == 8285] <- 37.52
	data$icam[data$oug == 8286] <- 37.52
	data$icam[data$oug == 8290] <- 34.52
	data$icam[data$oug == 8290] <- 34.52
	data$icam[data$oug == 8300] <- 34.75
	data$icam[data$oug == 8310] <- 37.45
	data$icam[data$oug == 8311] <- 37.45
	data$icam[data$oug == 8312] <- 37.45
	data$icam[data$oug == 8320] <- 36.43
	data$icam[data$oug == 8321] <- 24.76
	data$icam[data$oug == 8322] <- 35.67
	data$icam[data$oug == 8323] <- 35.67
	data$icam[data$oug == 8324] <- 35.67
	data$icam[data$oug == 8330] <- 31.86
	data$icam[data$oug == 8331] <- 31.86
	data$icam[data$oug == 8332] <- 31.86
	data$icam[data$oug == 8333] <- 31.86
	data$icam[data$oug == 8334] <- 31.86
	data$icam[data$oug == 8340] <- 43.00
	data$icam[data$oug == 9000] <- 27.85
	data$icam[data$oug == 9100] <- 28.70
	data$icam[data$oug == 9110] <- 33.37
	data$icam[data$oug == 9111] <- 35.05
	data$icam[data$oug == 9112] <- 30.41
	data$icam[data$oug == 9113] <- 35.05
	data$icam[data$oug == 9120] <- 25.69
	data$icam[data$oug == 9130] <- 26.71
	data$icam[data$oug == 9131] <- 25.01
	data$icam[data$oug == 9132] <- 25.01
	data$icam[data$oug == 9133] <- 24.69
	data$icam[data$oug == 9140] <- 35.41
	data$icam[data$oug == 9141] <- 35.42
	data$icam[data$oug == 9142] <- 29.68
	data$icam[data$oug == 9150] <- 33.34
	data$icam[data$oug == 9151] <- 33.34
	data$icam[data$oug == 9152] <- 33.34
	data$icam[data$oug == 9153] <- 33.34
	data$icam[data$oug == 9160] <- 21.07
	data$icam[data$oug == 9161] <- 21.07
	data$icam[data$oug == 9162] <- 21.07
	data$icam[data$oug == 9200] <- 22.45
	data$icam[data$oug == 9210] <- 23.43
	data$icam[data$oug == 9211] <- 23.43
	data$icam[data$oug == 9212] <- 23.43
	data$icam[data$oug == 9213] <- 23.43
	data$icam[data$oug == 9300] <- 28.16
	data$icam[data$oug == 9310] <- 26.18
	data$icam[data$oug == 9311] <- 26.12
	data$icam[data$oug == 9312] <- 26.12
	data$icam[data$oug == 9313] <- 24.99
	data$icam[data$oug == 9320] <- 30.71
	data$icam[data$oug == 9321] <- 30.71
	data$icam[data$oug == 9322] <- 30.71
	data$icam[data$oug == 9330] <- 31.24
	data$icam[data$oug == 9331] <- 23.60
	data$icam[data$oug == 9332] <- 23.60
	data$icam[data$oug == 9333] <- 29.48

	data$icam
}