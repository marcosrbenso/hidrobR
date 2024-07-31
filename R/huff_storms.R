#' Filtering of rainfall events from sub-daily time series
#'
#' @param date A vector containing date of the rainfall time series. The date must be as POSIXct class.
#' @param depth A vector containing the precipitation depth time series in mm.
#' @param MIT
#' @param Dmin The minimum
#' @param Thres
#' @import dplyr
#' @examples
#' MIT = 6 #
#' Dmin = 13 # minimum event depth
#' Thres = 0.5

hourly_time_series

date <- hourly_time_series$Date
depth <- hourly_time_series$Rainfall.depth

rain_events <- function(date,depth,MIT=6,Dmin=13,Thres=0.5){
  data <- data.frame(date,depth)

  Time_Step <- difftime(date[2], date[1], units = "mins")

  rain <- ifelse(depth >= Thres, 1, 0)
  length <- rep(rle(rain)$lengths, rle(rain)$lengths)
  eventflag <- ifelse(rain == 1, 1, ifelse(rain == 0 & length < MIT, 1, 0))
  eventflag <- ifelse(length >= MIT & rain == 0, 0, eventflag)
  eventid = rep(seq(1,length(rle(eventflag)$lengths)), rle(eventflag)$lengths)

  summary <- data.frame(date,eventid,depth, rain, length, eventflag) %>%
    filter(eventflag == 1) %>%
    group_by(eventid) %>%
    summarise(
      precip = sum(depth),
      Start = first(date),
      End = last(date),
      duration = as.numeric(difftime(End,Start, units = "mins"))+Time_Step
    ) %>%
    filter(
      precip > Dmin
    ) %>%
    mutate(eventid = 1:n())

  events <- list()
  for(i in 1:nrow(summary)){
    events[[i]] <- data  %>% filter(date >= summary[i,]$Start & date <= summary[i,]$End)
  }

  return(list(summary,events))

}


