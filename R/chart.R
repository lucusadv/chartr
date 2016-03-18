# Get formatted date ranges for querying in quantmod Delt
ten_yr <- lubridate::add_with_rollback(Sys.Date(), years(-10))
five_yr <- lubridate::add_with_rollback(Sys.Date(), years(-5))
three_yr <- lubridate::add_with_rollback(Sys.Date(), years(-3))
one_yr <- lubridate::add_with_rollback(Sys.Date(), years(-1))
six_mo <- lubridate::add_with_rollback(Sys.Date(), months(-6))
three_mo <- lubridate::add_with_rollback(Sys.Date(), months(-3))
one_mo <- lubridate::add_with_rollback(Sys.Date(), months(-1))
one_wk <- lubridate::add_with_rollback(Sys.Date(), weeks(-1))
one_day <- lubridate::add_with_rollback(Sys.Date(), days(-1))

#'
#'Gets price change over period
#'param START date format
#'param END date format
#'param DATAFRAME data frame over period
#'returns price_change: difference in Cl over period
#'
GetPriceChange <- function(END, START, DATAFRAME) {
  a <- DATAFRAME[DATAFRAME$date == START,1]
  b <- DATAFRAME[DATAFRAME$date == END,1]
  # if gold data at b returns numeric(0) i.e. at off hours, get data s.t. b = END-1, a = START-1
  if (identical(b, numeric(0))) {
    assign("a", DATAFRAME[DATAFRAME$date == (START-1),1])
    assign("b", DATAFRAME[DATAFRAME$date == (END-1),1])
    # if gold data at b still returns numeric(0) i.e. at off hours, get data s.t. b = END-2, a = START-2
    if (identical(b, numeric(0))) {
      assign("a", DATAFRAME[DATAFRAME$date == (START-2),1])
      assign("b", DATAFRAME[DATAFRAME$date == (END-2),1])
    }
  }
  price_change <- format(as.numeric(b-a), nsmall = 2)
  return (price_change)
}

# Get end of previous week (Eopw)
Eopw <- function(DAY) {
  dates <- seq((Sys.Date()-7), (Sys.Date()-1), by="days")
  return (dates[wday(dates, label = T)==DAY])
}
# Get end of previous month (Eopm)
Eopm <- function(DAY) {
  return (as.Date(format(DAY, "%Y-%m-01")) - 1)
}

# formats the date for Delt operation
FormatDelt <- function(DATE) {
  formatted_date <- paste(as.character(DATE), as.character(Sys.Date()), sep="::")
  return (formatted_date)
}

# gets max in date range (i.e. one_mo, three_mo, etc)
GetMax <- function (RANGE) {
  return (max(gld_data[gld_data$date >= (RANGE),1], na.rm=TRUE))
}

# gets min in date range
GetMin <- function (RANGE) {
  return (min(gld_data[gld_data$date >= (RANGE),1], na.rm=TRUE))
}

#'
#'@param SYMBOL
#'@return data frame
#'@export
#'
Summarize <- function(SYMBOL) {
  delt_data <- data.frame(c(Delt(xts(Cl(SYMBOL))[FormatDelt(ten_yr)])))     # Delt to retrieve % changes limited to one_mo
  delt_data$date = as.Date(rownames(delt_data))                       # Retreive date attribute in delt row

  h <- hash()

  h$day <- list(percent=percent(tail(delt_data[,1], 1)),
                price=GetPriceChange(Sys.Date(), one_day, gld_data))

  h$wtd <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(Eopw('Fri'))], na.rm = TRUE)),
                price=GetPriceChange(Sys.Date(), Eopw('Fri'), gld_data))

  h$week <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_wk)], na.rm = TRUE)),
                 price=GetPriceChange(Sys.Date(), one_wk, gld_data))

  h$mtd <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= Eopm(Sys.Date())], na.rm = TRUE)),
                price=GetPriceChange(Sys.Date(), Eopm(Sys.Date()), gld_data))

  h$month <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_mo)], na.rm = TRUE)),
                  price=GetPriceChange(Sys.Date(), one_mo, gld_data))

  h$qtr <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(three_mo)], na.rm = TRUE)),
                price=GetPriceChange(Sys.Date(), three_mo, gld_data))

  h$year <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_yr)], na.rm = TRUE)),
                 price=GetPriceChange(Sys.Date(), one_yr, gld_data))

  # Create summary data table
  summary_data <- data.frame(
    DAY=c(h[["day"]]$percent, h[["day"]]$price),
    WTD=c(h[["wtd"]]$percent, h[["wtd"]]$price),
    WEEK=c(h[["week"]]$percent, h[["week"]]$price),
    MTD=c(h[["mtd"]]$percent, h[["mtd"]]$price),
    MONTH=c(h[["month"]]$percent, h[["month"]]$price),
    QTR=c(h[["qtr"]]$percent, h[["qtr"]]$price),
    YEAR=c(h[["year"]]$percent, h[["year"]]$price)
  )
}

#'
#'
#'
SaveCharts <- function () {
  # Default theme for all tableGrobs
  k_theme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.4)),
    rowhead = list(fg_params=list(cex = 0.4))
  )
  theme_set(theme_bw(base_size = 5))
}
