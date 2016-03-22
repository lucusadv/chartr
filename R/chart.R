label <- format(Sys.Date(), format = "%d-%b-%Y")
source_path <- paste(getwd(), "/R/helper.R", sep = "")
output_path <- paste(getwd(), "/demo/", label, ".pdf", sep = "")
source(file = source_path, local = TRUE)

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

#'Generates a summary matrix of percent and price changes
#'@param SYMBOL: ticker
#'@return summary_data: matrix of percent and price changes
#'@export
#'
Summary <- function(SYMBOL, max=ten_yr) {
  SYMBOL
  delt_data <- data.frame(c(Delt(xts(Cl(SYMBOL))[FormatDelt(ten_yr)])))
  # Retreive date attribute in delt row
  delt_data$date = as.Date(rownames(delt_data))
  # Delt to retrieve % changes limited to one_mo
  data_frame <- data.frame(Cl(SYMBOL))

  h <- hash()
  h$day <- list(percent=percent(tail(delt_data[,1], 1)),
                price=GetPriceChange(data_frame, Sys.Date(), one_day))
  h$wtd <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(Eopw('Fri'))], na.rm = TRUE)),
                price=GetPriceChange(data_frame, Sys.Date(), Eopw('Fri')))
  h$week <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_wk)], na.rm = TRUE)),
                 price=GetPriceChange(data_frame, Sys.Date(), one_wk))
  h$mtd <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= Eopm(Sys.Date())], na.rm = TRUE)),
                price=GetPriceChange(data_frame, Sys.Date(), Eopm(Sys.Date())))
  h$month <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_mo)], na.rm = TRUE)),
                  price=GetPriceChange(data_frame, Sys.Date(), one_mo))
  h$qtr <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(three_mo)], na.rm = TRUE)),
                price=GetPriceChange(data_frame, Sys.Date(), three_mo))
  h$year <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_yr)], na.rm = TRUE)),
                 price=GetPriceChange(data_frame, Sys.Date(), one_yr))
  summary_data <- data.frame(
    DAY=c(h[["day"]]$percent, h[["day"]]$price),
    WTD=c(h[["wtd"]]$percent, h[["wtd"]]$price),
    WEEK=c(h[["week"]]$percent, h[["week"]]$price),
    MTD=c(h[["mtd"]]$percent, h[["mtd"]]$price),
    MONTH=c(h[["month"]]$percent, h[["month"]]$price),
    QTR=c(h[["qtr"]]$percent, h[["qtr"]]$price),
    YEAR=c(h[["year"]]$percent, h[["year"]]$price)
  )
  return (summary_data)
}

#'Generates paginated chart books of performance over time
#'@param SYMBOLS: list of tickers
#'@return null: generates pdf
#'@export
#'
ChartBook <- function (SYMBOLS) {
  # Default theme for all tableGrobs
  k_theme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.4)),
    rowhead = list(fg_params=list(cex = 0.4))
  )
  theme_set(theme_bw(base_size = 5))

  for (SYMBOL in SYMBOLS) {
    print("ChartBook: ")
    print(SYMBOL)
    data_frame <- data.frame(Cl(SYMBOL))
    summary_data <- Summary(SYMBOL)
    summary_table <- tableGrob(summary_data,
                         theme = k_theme,
                         rows = c("Percent Change", "Absolute Change")
    )
    one_mo_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("1-Month") +
      scale_y_continuous(limits = c(GetMin(data_frame, one_mo), GetMax(data_frame, one_mo))) +
      scale_x_date(limits = c(one_mo, NA), date_labels = "%b %d", date_breaks = "1 week")
    three_mo_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("3-Month") +
      scale_y_continuous(limits = c(GetMin(data_frame, three_mo), GetMax(data_frame, three_mo))) +
      scale_x_date(limits = c(three_mo, NA), date_labels = "%b %d", date_breaks = "3 weeks")
    six_mo_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("6-Month") +
      scale_y_continuous(limits = c(GetMin(data_frame, six_mo), GetMax(data_frame, six_mo))) +
      scale_x_date(limits = c(six_mo, NA), date_labels = "%b", date_breaks = "1 month")
    one_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("12-Month") +
      scale_y_continuous(limits = c(GetMin(data_frame, one_yr), GetMax(data_frame, one_yr))) +
      scale_x_date(limits = c(one_yr, NA), date_labels = "%b", date_breaks = "2 months")
    three_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("3-Year") +
      scale_y_continuous(limits = c(GetMin(data_frame, three_yr), GetMax(data_frame, three_yr))) +
      scale_x_date(limits = c(three_yr, NA), date_labels = "%b %Y", date_breaks = "1 year")
    five_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("5-Year") +
      scale_y_continuous(limits = c(GetMin(data_frame, five_yr), GetMax(data_frame, five_yr))) +
      scale_x_date(limits = c(five_yr, NA), date_labels = "%Y", date_breaks = "1 year")
    ten_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("10-Year") +
      scale_y_continuous(limits = c(GetMin(data_frame, ten_yr), GetMax(data_frame, ten_yr))) +
      scale_x_date(limits = c(ten_yr, NA), date_labels = "%Y", date_breaks = "2 years")
    all_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("All Time") +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years")

    # Export to pdf, file="/path/to/chart/book/directory"
    pdf(file = output_path, onefile = FALSE, paper = "letter", width = 0, height = 0,
        title = format(SYMBOL))
    # suppress warnings of rows missing values
    # with summary: arrangeGrob(summary, one_mo_plot, heights=c(0.5,2))
    suppressWarnings(grid.arrange(arrangeGrob(summary_table, one_mo_plot, heights=c(0.5, 2)),
                                  arrangeGrob(three_mo_plot, heights=c(2)),
                                  arrangeGrob(six_mo_plot, heights=c(2)),
                                  arrangeGrob(one_yr_plot, heights=c(2)),
                                  arrangeGrob(three_yr_plot, heights=c(2)),
                                  arrangeGrob(five_yr_plot, heights=c(2)),
                                  arrangeGrob(ten_yr_plot, heights=c(2)),
                                  arrangeGrob(all_plot, heights=c(2)),
                                  nrow=4, ncol=2))
    dev.off()
  }
}
