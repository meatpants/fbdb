
LIB.LOC = 'C:/Users/spatten/R_Packs'

library(fitbitScraper, lib.loc = LIB.LOC)
library(ggplot2, lib.loc = LIB.LOC)
library(tidyr, lib.loc = LIB.LOC)
library(dplyr, lib.loc = LIB.LOC)
library(lubridate, lib.loc = LIB.LOC)

cookie <- login('samjpatten@gmail.com', 'finger73', rememberMe = FALSE)

sleep <- get_sleep_data(cookie, start_date = '2015-09-01', end_date = '2016-07-09')$df %>% 
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

timeline <-   breaks %>% 
              bind_rows(sleep) %>%
              arrange(Time) %>%
              filter(!is.na(Time))


start.date <- timeline %>% 
              select(Time) %>% 
              slice(1)
start.date <- trunc(start.date[,1]$Time, "day")
start.date <- update(start.date, hour = 17)

end.date <-   timeline %>% 
              select(Time) %>% 
              slice(n())
end.date <-   trunc(end.date[,1]$Time, "day")
end.date <-   update(end.date, hour = 17)













