
# Option Analysis Reports
# Ryan McCorvie
# January 2017

library( "ggplot2" )
library( "dplyr")
library( "reshape2")
library( "lubridate")
library( "stringr")
library( "scales")

# Replace this with your own option market data file
source("OptionMarketData-example.R")

lot.size <- 100


# Reads the positions csv file into a data.frame, and puts the date into a standard column
#
# Args:
#   file.name : name of the file
#   datecol : name of the column containing date strings
#   format : the strptime format of the date string
#
# Returns:
#   data frame with the formatted data

read.positiondata <- function( file.name = "string", datecol = "", format = "%Y%m%d" )
{
  data.dir <- "~/Desktop/Option reporting"
  
  if( file.name == "string")
  {
    con <- textConnection(position.table.str)
    out <- read.csv(con, stringsAsFactors =FALSE )
  }
  else
  {
    full.name <- paste( data.dir, file.name, sep = "/" )
    out <- read.csv( full.name, stringsAsFactors =FALSE )
  }
  
  # Convert date strings into dates
  if( datecol != "")
  {
    out[, paste(datecol,"Str",sep="")] <- as.character( out[, datecol] )
    out[,datecol]  <- as.Date( out[,datecol], format )
  }
  
  return( out )
}



# Computes the numerical term between two dates
#
# Args:
#   d1 : a date, or vector of dates
#   d2 : a date, or vector of dates
#   
# Returns:
#   A term (or vector of terms) between the given dates.  The convention 1=1 year (365 days)
#   Note this doesn't use actual financial datemath conventions (e.g. actual-360), its
#   just an approximation.  Also leap years will have a term longer than 1.

calc.term <- function( d2, d1 =today() )
{
  daycount <- 365
  
  return( as.numeric(d2-d1)/daycount)
}


# Black-Scholes pricer
#
# Args:
#   type : "C" for call or "P" for put
#   strike : strike price of option
#   expiration : expiration date
#   spot : spot price of underlying
#   volatility : volatility (per unit time) of lognormal process.  
#     The variance of the process is volatility^2 * term
#   df : discount factor to expiration date
#   today : today's date, for computing terms
#   
# Returns:
#   Price of a european option using the Black-Scholes assumptions.  
#   Setting DF=1 and spot = forward price of the underlying, this gives the forward option price

BSPrice <- function( type, strike, expiration, spot, volatility, df=1, pricing.date = today() )
{
  if( expiration <= pricing.date )
  {
    if( type == "C")
      return( df*max(spot-strike,0))
    else if(type=="P")
      return( df*max(strike-spot,0))
    else
      stop( paste("unknown type: ", type ))
 }
  
  forward <- spot / df

  tt    <- calc.term( expiration, pricing.date ) 
  vol.t <- volatility * sqrt(tt)
  dd    <- log(forward/strike)/(vol.t)
  d.plus  <- dd + 1/2 * vol.t
  d.minus <- dd - 1/2 * vol.t
  
  if( type == "C")
  {
    out <- forward * pnorm(d.plus) - strike * pnorm(d.minus)
  }
  else if( type == "P" )
  {
    out <- -forward * pnorm(-d.plus) + strike * pnorm(-d.minus)
  }
  else
  {
    stop( paste("unknown option type:", type ))
  }

  return( df * out )
}



# A pricer which accounts for closeouts
#
# Args:
#   closeout : closeout price or 0 for open positions
#   type : "C" for call or "P" for put
#   strike : strike price of option
#   expiration : expiration date
#   spot : spot price of underlying
#   volatility : volatility (per unit time) of lognormal process.  
#     The variance of the process is volatility^2 * term
#   df : discount factor to expiration date
#   today : today's date, for computing terms
#   

BSPrice.closeout <- function( closeout, type, strike, expiration, spot, volatility, df=1, pricing.date = today() )
{
  if( closeout == 0 )
    return( BSPrice(  type, strike, expiration, spot, volatility, df, pricing.date ))
  return( process.closeout( closeout, type, strike ) )
}

# Black-Scholes greeks calculation
#
# Args:
#   type : "C" for call or "P" for put
#   strike : strike price of option
#   expiration : expiration date
#   spot : spot price of underlying
#   volatility : volatility (per unit time) of lognormal process.  
#     The variance of the process is volatility^2 * term
#   df : discount factor to expiration date
#   today : today's date, for computing terms
#   
# Returns:
#   List of greeks of an option using the Black-Scholes assumptions.  


BSGreeks <- function( type, strike, expiration, spot, volatility, df, today = today() )
{
  
  if( type == "C")
    ff <- 1
  else if( type =="P" )
    ff <- -1
  else
    stop( paste("unknown type: ", type ))
  
  if( expiration <= today )
  {
    moneyness <- as.numeric( ff * ( spot -strike ) > 0 )
    return( list( moneyness =moneyness, delta=ff * moneyness, rho = 0, vega = 0, tau = 0, gamma = 0 ))
  }
  
  forward <- spot / df
  tt    <- calc.term( expiration, today ) 
  vol.t <- volatility * sqrt(tt)
  dd    <- log(forward/strike)/(vol.t)
  d.plus  <- dd + 1/2 * vol.t
  d.minus <- dd - 1/2 * vol.t
  
  upsilon <- df * strike * dnorm( d.minus )
  
  
  moneyness <- -ff * pnorm( ff * d.minus)
  delta <- ff * pnorm( ff * d.plus )
  rho   <- ff * tt * pnorm( ff * d.minus )
  vega  <- sqrt(tt) * upsilon

  rr  <- -log(df)/tt
  tau <- -ff * rr * strike * pnorm( ff * d.minus ) - volatility * upsilon / (2 * sqrt(tt))
  
  gamma <- upsilon / (spot^2 * volatility * sqrt(tt))
  
  return( list( moneyness = moneyness, delta = delta, rho = rho, vega = vega, tau = tau, gamma = gamma))
}


# Return a term corresponding to the relative date string
#
# Args:
#   periodStr : a string representing the relative time period
#
# Returns:
#   double corresponding to the term duration

calc.term.string <- function( period.str )
{
  
  period.labels <- c( "Overnight", 
     "1 Week", 
     "1 Month", 
     "2 Month",
     "3 Month", 
     "6 Month",
     "1 Year",
     "2 Years",
     "3 Years",
     "4 Years",
     "5 Years",
     "6 Years",
     "7 Years",
     "8 Years",
     "9 Years",
     "10 Years",
     "15 Years",
     "20 Years",
     "30 Years"
  )
  terms <- c( 1/365,7/365,1/12,2/12,3/12,6/12,1,2,3,4,5,6,7,8,9,10,15,20,30)
  
  idx <- grep( period.str, period.labels )
  if( !length(idx))
    stop( paste( "Unrecognzied period string:", period.str ) )
  
  return( terms[idx])
}


# Interpolate the discount factor curve assuming constant forward rates
# 
# Args:
#   df.curve : a df curve, such as output by bootstrap.df.curve
#   date : the date (or vector of dates) to interpolate to
#   today : base date (or vector of dates) to interpolate from
#   term : the term for calculation.  When specified, this overrides date and today
#
# Returns:
#   discount factor (or vector of discount factors) to the given date or term

discount.factor <- function( df.curve, date = NULL, term = NULL )
{
  if( is.null( term ))
  {
    if( is.null( date ))
      stop( "both 'date' and 'term' are null")
    
    ref.date <- df.curve$ref.date
    term <- calc.term( date, ref.date )
  }
  
  knots <- df.curve$knots
  return( exp( approx( knots$term, knots$df.log, term )$y ) )
}


# Compute forward discount factors, from some date other than the df.curve's ref.date
# 
# Args:
#   df.curve : a df curve, such as output by bootstrap.df.curve
#   forward.date : the forward date to start discounting from
#   date : the date (or vector of dates) to interpolate to
#   term : the term for calculation.  When specified, this overrides date.  This term is relative 
#    to the forward.date, not to the df.curve's reference date
#
# Returns:
#   forward discount factor (or vector of discount factors) from the given date or term to the forward.date


forward.discount.factor <- function( df.curve, forward.date, date = NULL, term = NULL )
{
  if( forward.date < df.curve$ref.date)
    stop( "can't compute forward discount factors before the reference date" )
  
  base.term <- calc.term( forward.date, df.curve$ref.date )
  base.df <- discount.factor(df.curve, term=base.term )
  if( !is.null( term ))
  {
    df <- discount.factor( df.curve, term = term+base.term)
    return( mapply( min, 1,df/base.df))
  }
    
  if( !is.null( date ))
  {
    df <- discount.factor( df.curve, date=date )
    return( mapply( min, 1,df/base.df))
  }
    
  stop( "both 'date' and 'term' are null")
}

# A series of tests for the various modes of computign discount factors

discount.factor.test <- function()
{
  df.curve <- bootstrap.df.curve()
  today <- today()
  print( discount.factor( df.curve, today ) )
  print( discount.factor( df.curve, today-365 ))
  print( discount.factor( df.curve, today+365 ))
  print( discount.factor( df.curve, term=0.45))
  
  print( discount.factor( df.curve, date = today+ seq(30,360,by=30)))
  print( discount.factor( df.curve, term = seq(1/12,1,by=1/12)))
}

# Calculates the value of a swap, from the perspective of a fixed receiver / floating payer
# 
# Args:
#   df.curve : a df curve, such as output by bootstrap.df.curve
#   expiration : expiration date of the swap
#   swap.rate : the fixed leg rate
#   coupon.frequency : how many coupons per year?
#   today : pricing date
#
# Returns:
#   NPV of swap


swap.price <- function( df.curve, expiration, swap.rate, coupon.frequency = 4, today=today() )
{
  coupon.term <- 1/coupon.frequency
  term  <- calc.term( expiration, today )
  terms <- seq( 0, term, coupon.term )
  dfs <- discount.factor( df.curve, term = terms )
  dt <- diff( terms )
  
}

# Create a discount factor curve, represented as a data.frame
#
# Returns:
#   data frame with columns "term" and "df" columns

bootstrap.df.curve <- function( ref.date = today() )
{ 
  lowest.date <- -100
  # Past discount factors are 1
  
  term <- c(lowest.date,0)
  df   <- c(1,1)
  
  con <- textConnection(ice.libor.str )
  libor.table <- read.table(con, header = FALSE, sep="\t",stringsAsFactors=FALSE)
  
  # FIXME: remove this hack for an extra rate 
  libor.table[nrow(libor.table)+1,] <- libor.table[nrow(libor.table),]
  libor.table[nrow(libor.table),1] <- "2 Year"
  
  term.libor <- sapply( libor.table[,1], calc.term.string)
  term <- c( term, term.libor )
  df   <- c( df, 1/(1+term.libor*libor.table[,3]/100))

  # TODO bootstrap from the swap rates too?
  # con <- textConnection(ice.swaps)
  # swap.table <- read.table(con, header = FALSE, sep="\t")
  
  df.log <- log( df )
  
  knots <- data.frame( term = term, df = df, df.log = df.log )
  
  return( list( ref.date = ref.date, knots = knots ))
}


# Return the forward price corresponding to a given spot price
#
# Args:
#  spot : numeric representing spot price today
#  df.curve : a discount factor curve, such as returned by bootstrap.df.curve
#  forward.date : a date or vector of dates to evaluate the forward at
#
# Returns:
#   forward price (or vector of forward prices)
#
# TODO make it fancy and incorporate dividends?

forward.price <- function( spot, df.curve, forward.date )
{
  return( spot / discount.factor( df.curve, forward.date ))
}



# A lambda to create a BSPrice which only depends on volatility to pass into the uniroot solver
#
# Args:
#  Same as BSPrice except
#  price : the price to be solve for, which is subtracted off
#
# Returns:
#   A function of volatility which returns BSPrice(params)-price
#

BSPrice.lambda <- function( type, strike, expiration, forward, df, price=0, pricing.date = today() )
{
  ff <- function( volatility )
  {
    return( BSPrice( type, strike, expiration, forward, volatility, df, pricing.date )-price )
  }
  return( ff )
}

# Find the implied volatility of an option given its price
#
# Args:
#  price : the price to be solve for
#  type : "C" for call or "P" for put
#  strike : the strike price of the option
#  expiration : expiration date of the option
#  forward : the forward price of the underlying
#  df : discount factor to the expiration date
#
# Returns:
#   The annualized implied volatility to recover the price.  If the vol is 0.5 that means 50% per year
#

implied.volatility <- function( price, type, strike, expiration, forward, df, pricing.date = today() )
{
  minvol <- 0.001
  maxvol <- 10
  if( expiration <= pricing.date )
    return( 0 )
  ff <- BSPrice.lambda( type, strike, expiration, forward, df, price, pricing.date )
  return( uniroot( ff, c( minvol, maxvol ))$root )
}



# Generate a risk report, showing dollar exposures
#
# Args:
#  positiondata : the position table
#  greeks : data frame of per-unit greeks
#  risk.measure.spec : a data.frame specifying the risk measures
#
# Returns:
#   data frame with the dollar risk measures, by position

risk.report <- function( positiondata, greeks, risk.measure.spec  )
{
  risk.report.df <- data.frame( ID = positiondata$ID )
  
  for( idx in 1:nrow(risk.measure.spec))
  {
    spec <- risk.measure.spec[idx,]
    risk.report.df[[spec$name]] <- greeks[[spec$greek]] * spec$delta * positiondata$Quantity * lot.size
  }
  
  return( risk.report.df )
}


# Generate a risk report, showing dollar exposures
#
# Args:
#  pos : the position data.frame
#  greeks : data frame of per-unit greeks
#  risk.measure.spec : a data.frame specifying the risk measures
#
# Returns:
#   data frame with the dollar risk measures, by position

pnl.report <- function( pos, spots, dates, df.curve )
{
  pnl.report.df <- NULL
  
  for( pricing.date in as.list( dates ))
  {
    df <- forward.discount.factor( df.curve, pricing.date, pos$Expiration )
    
    for(spot in spots)
    {
      prices <- mapply( BSPrice.closeout, pos$Closeout, pos$Type, pos$Strike, pos$Expiration, spot, pos$Volatility, df, pricing.date )

      NPV <- lot.size * pos$Quantity * prices
      PNL <- NPV - pos$NPV
      LTD.PNL <- NPV - pos$CostBasis * pos$Quantity * lot.size
      scenario.df <- data.frame( ID = pos$ID, price = prices, NPV = NPV, PNL = PNL, LTD.PNL = LTD.PNL, spot = spot, pricing.date = pricing.date )
      pnl.report.df <- rbind( pnl.report.df,  scenario.df )
    }
  }
  return( pnl.report.df )
}


# Handle closeouts when populating the option price vector
#
# Args:
#  price.closeout : 0 if not closed out, or the price corresponding to the closeout
#  type : "C" for call, "P" for put
#  strike : strike of the option
#  option.price : the market option price.  Defaults to 0 in the case that the function is
#   invoked when the closeout is known to be true
# Returns:
#   the market option price, or the price corresponding to the closeout

process.closeout <- function( price.closeout, type, strike, option.price = 0 )
{
  if( price.closeout == 0 )
    return( option.price )
  
  if( type == "C")
    return( max(price.closeout-strike,0))
  else if(type=="P")
    return( max(strike-price.closeout,0))
  else
    stop( paste("unknown type: ", type ))
}
  
# Do some datatype juggling because I dont' really understand the various types
#
# Args:
#  thingie : an array of lists (maybe?)
#
# Returns:
#   data.frame of vectors

df.convert<-function( thingie )
{
  row.names(thingie) <- NULL
  thingie <- data.frame(thingie)
  for( col in colnames(thingie ))
    thingie[[col]] <- unlist(thingie[[col]])
  
  return( thingie )
}


# The main function which does all the calibrations and runs the reports
#
# Args:
#   spot.price : current spot price for the option underlying
#   option.prices : a data.frame indexed by ID of the current option prices
#   pricing.date : the pricing date for the report
#   position.table.filename : location of the position table   
#

analyze.portfolio <- function( spot.price, option.prices, pricing.date, position.table.filename = "string" )
{
  print.riskreport <- TRUE
  print.pnlprojection <- TRUE
  plot.pnlprojection <- TRUE
  
  # Get position table
  pos <- read.positiondata( position.table.filename, "Expiration", "%d-%b-%y")
  
  # Merge in current prices, and closeout prices
  last.price <- mapply( process.closeout, pos$Closeout, pos$Type, pos$Strike, option.prices  )
    
  option.prices.df <- data.frame( ID = names(option.prices), LastPrice = last.price)
  pos <- merge(pos, option.prices.df, by = "ID")
  
  # Generate discount factors
  df.curve <- bootstrap.df.curve(pricing.date)
  pos$DiscountFactor <- discount.factor( df.curve, pos$Expiration )
  
  # Calculate implied volatilities
  ivol <- mapply( implied.volatility, pos$LastPrice, pos$Type, pos$Strike, pos$Expiration, spot.price, pos$DiscountFactor, pricing.date  )
  pos$Volatility <- ivol
  
  # Recover prices
  pos$LastPriceCalc <- mapply( BSPrice, pos$Type, pos$Strike, pos$Expiration, spot.price, pos$Volatility, pos$DiscountFactor, pricing.date )
  pos$NPV <- pos$LastPrice * lot.size * pos$Quantity
  pos$LTD.PNL <- pos$NPV - pos$CostBasis * lot.size * pos$Quantity

  print( pos )
  
  # Calculate per-unit greeks
  if( print.riskreport )
  {
    pos.open <- filter( pos, Closeout == 0)
    
    greeks.mapply <- mapply( BSGreeks, pos.open$Type, pos.open$Strike, pos.open$Expiration, spot.price, pos.open$Volatility, pos.open$DiscountFactor, pricing.date )
    greeks <- data.frame( ID = pos.open$ID )
    greeks <- cbind( greeks, df.convert( t(greeks.mapply)))
    print( greeks )
    
    # Generate sensitivity risk reports for the portfolio
    risk.measure.spec <- data.frame(
      name = c( "delta", "delta 1%", "dv100", "vega 1%", "daily carry" ),
      greek = c( "delta", "delta", "rho", "vega", "tau" ),
      delta = c( 0.01, spot.price/100, 0.01, 0.01, 1/365 ),
      stringsAsFactors = FALSE
    )
    risk.report.df <- risk.report( pos.open, greeks, risk.measure.spec )
    totals <- apply( subset( risk.report.df, select=-ID ), MARGIN=2, sum)
    print( risk.report.df )
    print( totals )
  }
  
  if( print.pnlprojection )
  {
    # Generate a pnl projection matrix
    spots <- spot.price * c( 0.5, 0.75, 0.9, 0.95, 1, 1.05, 1.1, 1.25 )
    spots <- sort( c( spots, 10 ) )
    
    dates <- c( pricing.date + c(0,7), pricing.date %m+% months(c(1,3,6,12, 18), FALSE))
    
    pnl.report.df <- pnl.report( pos, spots, dates, df.curve )
    
    pnl.summary <- pnl.report.df %>% 
      group_by( spot, pricing.date ) %>%
      summarize( PNL=sum(PNL), LTD.PNL =sum(LTD.PNL), NPV=sum(NPV))
    
    print( "NPV" )
    print( dcast( pnl.summary, pricing.date ~ spot, value.var="NPV") )
    print( "PNL" )
    print( dcast( pnl.summary, pricing.date ~ spot, value.var="PNL") )
    print( "Life-to-date PNL" )
    print( dcast( pnl.summary, pricing.date ~ spot, value.var="LTD.PNL") )
    
    scale.pref <- scale_y_continuous( labels = comma ) 

    if( plot.pnlprojection )
    {
      
      g1<-ggplot( pnl.summary ) +geom_line( aes(x=pricing.date, y=PNL, color=as.character( spot ))) + scale.pref + scale_color_discrete( name = "spot")
      g2<-ggplot( pnl.summary )  +geom_line( aes(x=spot, y=PNL, color=as.character(pricing.date)))+ scale.pref+ scale_color_discrete( name = "date")
      print( g1 )
      print( g2 )
      
      g3<-ggplot( pnl.summary ) +geom_line( aes(x=pricing.date, y=LTD.PNL, color=as.character( spot )))+ scale.pref+ scale_color_discrete( name = "spot")
      g4<-ggplot( pnl.summary )  +geom_line( aes(x=spot, y=LTD.PNL, color=as.character(pricing.date)))+ scale.pref+ scale_color_discrete( name = "date" )
      print( g3)
      print( g4)
      
      #g3<-ggplot( pnl.summary ) +geom_line( aes(x=pricing.date, y=NPV, color=as.character( spot )))+ scale.pref
      #g4<-ggplot( pnl.summary )  +geom_line( aes(x=spot, y=NPV, color=as.character(pricing.date)))+ scale.pref
      
    }
  }
}

#analyze.portfolio( spot.price, option.prices, today, "string")

