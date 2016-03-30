#'@import quantmod
#'@import xts
#'@import ggplot2
#'@import grid
#'@import hash
#'@import gridExtra
#'@import zoo
#'@import scales
#'@import data.table
#'@import lubridate
#'@import plyr
#'@import reshape2

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

# Get end of previous week (Eopw)
Eopw <- function(DATE) {
  dates <- seq((Sys.Date()-7), (Sys.Date()-1), by="days")
  return (dates[wday(dates, label = T)==DATE])
}
# Get end of previous month (Eopm)
Eopm <- function(DATE) {
  return (as.Date(format(DATE, "%Y-%m-01")) - 1)
}

# formats the date for Delt operation
FormatDelt <- function(DATE) {
  formatted_date <- paste(as.character(DATE), as.character(Sys.Date()), sep="::")
  return (formatted_date)
}

# Gets max in data frame
GetMax <- function (DATAFRAME, DATE) {
  return (max(DATAFRAME[DATAFRAME$date >= (DATE),1], na.rm=TRUE))
}

# Gets min in date range
GetMin <- function (DATAFRAME, DATE) {
  return (min(DATAFRAME[DATAFRAME$date >= (DATE),1], na.rm=TRUE))
}

#'
#'Gets price change over period
#'@param DATAFRAME: data frame of prices
#'@param START: date format
#'@param END: date format
#'@return price_change: difference in prices over period
#'@export
#'
GetPriceChange <- function(DATAFRAME, END, START) {
  start_date <- DATAFRAME[DATAFRAME$date == START,1]
  end_date <- DATAFRAME[DATAFRAME$date == END,1]
  # if gold data at end_date returns numeric(0) i.e. at off hours, get data s.t. end_date = END-1, start_date = START-1
  if (identical(end_date, numeric(0))) {
    assign("start_date", DATAFRAME[DATAFRAME$date == (START-1),1])
    assign("end_date", DATAFRAME[DATAFRAME$date == (END-1),1])
    # if gold data at end_date still returns numeric(0) i.e. at off hours, get data s.t. end_date = END-2, start_date = START-2
    if (identical(end_date, numeric(0))) {
      assign("start_date", DATAFRAME[DATAFRAME$date == (START-2),1])
      assign("end_date", DATAFRAME[DATAFRAME$date == (END-2),1])
    }
  }
  price_change <- format(as.numeric(end_date - start_date), nsmall = 2)
  return (price_change)
}
