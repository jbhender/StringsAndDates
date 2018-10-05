## Dates and Times
##
## This script contains typed examples from Chapter 16 of
## *R for Data Science* by Hadley Wickham: 
## http://r4ds.had.co.nz/dates-and-times.html
##
## Updated: October 4, 2018

# libraries: ------------------------------------------------------------------
#install.pacakages('lubridate'); install.packages('nycflights14')
library(tidyverse); library(lubridate); library(nycflights13)

# Get some information on the current date/time: ------------------------------
Sys.time() # base R
now()
today()
now
today

# Inspect the class of these objects: -----------------------------------------
class(Sys.time())
class(now())
class(today())

# Read through this help page to understand more: ------------------------------
?POSIXct # sometimes referred to as dttm or "date-time"
# A POSIXct object represent the number of seconds since the beginning of 1970 

# Similarly dates are the number of *days* since the begninning of 1970
class(as.Date(now()))
?Date
# Built in object 
origin

# Reading dates and times from data: ------------------------------------------

# Combine 'y' (Year), 'm' (month), and 'd' (day) in an appropriate order
ymd('2018-10-5')
mdy('Oct 5 2018')
dmy('5Oct18')

# Create a departure time in the flights data: --------------------------------
flights

flights %>% select(year, month, day, hour, minute) %>%
  mutate(
    dep_string = sprintf('%i-%i-%i', year, month, day),
    dep_date = ymd( sprintf('%i-%i-%i', year, month, day) ) 
    ) 

# How I would typically create this in practice: ------------------------------
flights = flights %>% 
  mutate( dep_date = ymd( sprintf('%i-%i-%i', year, month, day) ) ) 

# See also, ?make_date

# Use one of the above with an underscore and hms (hours, minutes, seconds)
# in any order to create a date-time (POSIXct)

flights %>% select(year, month, day, hour, minute) %>%
  mutate( 
    deptime_str = sprintf('%i-%i-%i %02i:%02i:%02i', year, month, day, hour, minute, 0),
    departure = ymd_hms( deptime_str )
  )

# Store departure permnanently
flights = flights %>% 
  mutate( 
    departure = ymd_hms( 
        sprintf('%i-%i-%i %i:%i:%i', year, month, day, hour, minute, 0) 
        ) 
  ) 

# Define a function for dealing with odd time format by using modular or
# "clock" arithmetic
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}

flights = flights %>%
  mutate( 
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time) 
  ) 

flights %>% select( ends_with("delay"), ends_with("time") )

# Time zones: -----------------------------------------------------------------
Sys.timezone()

(x1 = ymd_hms("2015-06-01 12:00:00", tz = "America/New_York"))
(x2 = ymd_hms("2015-06-01 18:00:00", tz = "Europe/Copenhagen"))
(x3 = ymd_hms("2015-06-02 04:00:00", tz = "Pacific/Auckland"))
x1 - x2
x2 - x3
as.numeric(x1)

?tz #! Note the first line under details 
tz(x2) = 'America/New_York'
x1 - x2
x2 + duration(6 ,'months') # Note the difference in how the time-zone is printed 
tz(x1)

