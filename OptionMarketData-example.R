
# Option Market and Position data
# Ryan McCorvie
# January 2017

library( "lubridate")

# TODO is there any programmatic way to get this data?
# https://www.theice.com/marketdata/reports/170
# Oct 30 2017
ice.libor.str <- 
#TENOR	PUBLICATION TIME*	USD ICE LIBOR 26-OCT-2017
"Overnight	11:55:06 AM	1.18444
1 Week	11:55:06 AM	1.20517
1 Month	11:55:06 AM	1.24166
2 Month	11:55:06 AM	1.31212
3 Month	11:55:06 AM	1.37796
6 Month	11:55:06 AM	1.56447
1 Year	11:55:06 AM	1.84289"

  
# https://www.theice.com/marketdata/reports/180
ice.swaps.str <-
"TENOR	USD RATES 1100
26-OCT-2017
1 Year	1.64000
2 Years	1.83300
3 Years	1.96300
4 Years	2.05600
5 Years	2.13300
6 Years	2.20300
7 Years	2.26800
8 Years	2.32500
9 Years	2.37400
10 Years	2.41900
15 Years	2.56500
20 Years	2.63300
30 Years	2.66600"

# If the closeout price is non-zero then this option has already been exercised or expired
# If the closeout price is zero, then the option is live

position.table.str <-
"ID,Type,Strike,Expiration,Quantity,CostBasis,Closeout
TWTR170120P17,P,17,20-Jan-17,200,0.81,16.40
TWTR180119P14,P,14,19-Jan-18,100,0.91,0"

spot.price <- 21.68

option.prices <- c(
TWTR170120P17 = 0,
TWTR180119P14 = 0.05
)
option.prices

today <- mdy("10/30/2017")
#today <- today()
