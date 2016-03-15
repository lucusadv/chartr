#'Gets delta in price = (b-a) where START is a, END is b
#'takes from format lubridate::add_with_rollback, returns format to 3 decimal places
#'
#'@param newer formatted date
#'@param older formatted date
#'@return difference in price
#'@export
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
  diff <- format(as.numeric(b-a), nsmall = 2)
  return (diff)
}
