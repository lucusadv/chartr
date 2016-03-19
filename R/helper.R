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

# gets max in date range (i.e. one_mo, three_mo, etc)
# gld_data is the object data.frame(Cl(GLD))
GetMax <- function (DATAFRAME, DATE) {
  return (max(DATAFRAME[DATAFRAME$date >= (DATE),1], na.rm=TRUE))
}

# gets min in date range
GetMin <- function (DATAFRAME, DATE) {
  return (min(DATAFRAME[DATAFRAME$date >= (DATE),1], na.rm=TRUE))
}

#'
#'Gets price change over period
#'param START date format
#'param END date format
#'param DATAFRAME data frame over period
#'returns price_change: difference in Cl over period
#'
GetPriceChange <- function(END, START, DATAFRAME) {
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
