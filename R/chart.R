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
