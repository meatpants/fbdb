

library(fitbitScraper)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

cookie <- login('samjpatten@gmail.com', 'finger73', rememberMe = FALSE)

sleep <- get_sleep_data(cookie, start_date = '2015-09-01', end_date = '2016-11-12')$df %>% 
          select(startDateTime, endDateTime, breaks)

breaks <- sleep$breaks[[1]]

for(i in 2:nrow(sleep))
{
  breaks <- breaks %>% bind_rows(sleep$breaks[[i]])
}

breaks <- breaks %>% select(startDateTime, duration)

breaks$startDateTime <- sapply(breaks$startDateTime, 
                          function(x) ISOdatetime(x[[1]], x[2], x[3], x[4], x[5], x[6])) 

breaks$startDateTime <- as.POSIXct(breaks$startDateTime, origin = "1970-01-01")

breaks$endDateTime <- breaks$startDateTime + minutes(breaks$duration)

# Build timeline

breaks <- breaks %>% 
          rename(Sleep = endDateTime, Wake = startDateTime) %>%
          gather("State", "Time", c(Sleep, Wake)) %>%
          select(State, Time)

sleep <-  sleep %>%
          rename(Sleep = startDateTime, Wake = endDateTime) %>%
          gather("State", "Time", c(Sleep, Wake)) %>%
          select(State, Time)

sleep$Time <- parse_date_time(sleep$Time, "%Y-%m-%d %H:%M:%S", tz="")

tz(timeline$Time) <- "UTC"

timeline <-   breaks %>% 
              bind_rows(sleep) %>%
              arrange(Time) %>%
              filter(!is.na(Time))

timeline$State <- factor(timeline$State)

# Allocate each record to a day

timeline$Sleep.Date <- (as_date(ifelse(hour(timeline$Time) < 17
                                      , date(timeline$Time) - days(1)
                                      , date(timeline$Time)))
                               ) + hours(17)
tz(timeline$Sleep.Date)

timeline$offset <- timeline$Time - timeline$Sleep.Date 

start.date <- timeline %>% 
              select(Time) %>% 
              slice(1)

end.date <-   timeline %>% 
              select(Time) %>% 
              slice(n())














