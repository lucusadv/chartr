source("helper.R")

#'
#'@param SYMBOL
#'@return data frame
#'@export
#'
Summary <- function(SYMBOL, max=ten_yr) {
  delt_data <- data.frame(c(Delt(xts(Cl(SYMBOL))[FormatDelt(ten_yr)])))
  # Retreive date attribute in delt row
  delt_data$date = as.Date(rownames(delt_data))
  # Delt to retrieve % changes limited to one_mo
  data_frame <- data.frame(Cl(SYMBOL))
  h <- hash()
  h$day <- list(percent=percent(tail(delt_data[,1], 1)),
                price=GetPriceChange(Sys.Date(), one_day, data_frame))
  h$wtd <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(Eopw('Fri'))], na.rm = TRUE)),
                price=GetPriceChange(Sys.Date(), Eopw('Fri'), data_frame))
  h$week <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_wk)], na.rm = TRUE)),
                 price=GetPriceChange(Sys.Date(), one_wk, data_frame))
  h$mtd <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= Eopm(Sys.Date())], na.rm = TRUE)),
                price=GetPriceChange(Sys.Date(), Eopm(Sys.Date()), data_frame))
  h$month <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_mo)], na.rm = TRUE)),
                  price=GetPriceChange(Sys.Date(), one_mo, data_frame))
  h$qtr <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(three_mo)], na.rm = TRUE)),
                price=GetPriceChange(Sys.Date(), three_mo, data_frame))
  h$year <- list(percent=percent(mean(delt_data$Delt.1.arithmetic[delt_data$date >= as.character(one_yr)], na.rm = TRUE)),
                 price=GetPriceChange(Sys.Date(), one_yr, data_frame))
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

#'
#'@param SYMBOLS
#'
ChartBook <- function (SYMBOLS) {
  # Default theme for all tableGrobs
  k_theme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 0.4)),
    colhead = list(fg_params=list(cex = 0.4)),
    rowhead = list(fg_params=list(cex = 0.4))
  )
  theme_set(theme_bw(base_size = 5))

  for (SYMBOL in c(SYMBOLS)) {
    data_frame <- data.frame(Cl(SYMBOL))
    one_mo_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("1-Month") +
      scale_y_continuous(limits = c(GetMin(one_mo), GetMax(one_mo))) +
      scale_x_date(limits = c(one_mo, NA), date_labels = "%b %d", date_breaks = "1 week")
    three_mo_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("3-Month") +
      scale_y_continuous(limits = c(GetMin(three_mo), GetMax(three_mo))) +
      scale_x_date(limits = c(three_mo, NA), date_labels = "%b %d", date_breaks = "3 weeks")
    six_mo_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("6-Month") +
      scale_y_continuous(limits = c(GetMin(six_mo), GetMax(six_mo))) +
      scale_x_date(limits = c(six_mo, NA), date_labels = "%b", date_breaks = "1 month")
    one_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("12-Month") +
      scale_y_continuous(limits = c(GetMin(one_yr), GetMax(one_yr))) +
      scale_x_date(limits = c(one_yr, NA), date_labels = "%b", date_breaks = "2 months")
    three_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("3-Year") +
      scale_y_continuous(limits = c(GetMin(three_yr), GetMax(three_yr))) +
      scale_x_date(limits = c(three_yr, NA), date_labels = "%b %Y", date_breaks = "1 year")
    five_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("5-Year") +
      scale_y_continuous(limits = c(GetMin(five_yr), GetMax(five_yr))) +
      scale_x_date(limits = c(five_yr, NA), date_labels = "%Y", date_breaks = "1 year")
    ten_yr_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("10-Year") +
      scale_y_continuous(limits = c(GetMin(ten_yr), GetMax(ten_yr))) +
      scale_x_date(limits = c(ten_yr, NA), date_labels = "%Y", date_breaks = "2 years")
    all_plot <- ggplot() +
      geom_line(data=data_frame, aes(x=date, y=price)) + xlab("All Time") +
      scale_x_date(date_labels = "%Y", date_breaks = "5 years")

    # Export to pdf, file="/path/to/chart/book/directory"
    label <- format(Sys.Date(), format = "%d-%b-%Y")
    path <- paste("/home/bryan/R/ecocharts/chart\ book/", label, ".pdf", sep = "")
    pdf(file=path, onefile=FALSE, paper="legal", width=0, height=0,
        title="Gold Prices (FRED Series: GOLDPMGBD228NLBM)")
    # suppress warnings of rows missing values
    # with summary: arrangeGrob(summary, one_mo_plot, heights=c(0.5,2))
    suppressWarnings(grid.arrange(arrangeGrob(summary, one_mo_plot, heights=c(0.5, 2)),
                                  arrangeGrob(three_mo_plot, heights=c(2)),
                                  arrangeGrob(six_mo_plot, heights=c(2)),
                                  arrangeGrob(one_yr_plot, heights=c(2)),
                                  arrangeGrob(three_yr_plot, heights=c(2)),
                                  arrangeGrob(five_yr_plot, heights=c(2)),
                                  arrangeGrob(ten_yr_plot, heights=c(2)),
                                  arrangeGrob(all_plot, heights=c(2)),
                                  arrangeGrob(bonds_plot, heights=c(2)),
                                  arrangeGrob(volatility_plot, heights=c(2)),
                                  nrow=5, ncol=2))
    dev.off()
  }
}
