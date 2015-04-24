library("fImport")
library("forecast")
require("magrittr")

# Get data from FRED
retail <- fredSeries("RSAFSNA", from = "1992-01-01")
plot(retail)

# Classical decomposition forecast
retail.stl <- stl(retail, s.window = "periodic")
plot(retail.stl)
(retail.forecast <- forecast(retail.stl, h = 12))
plot(retail.forecast)

# Exponential smoothing forecast
(retail.ets <- ets(retail))
(retail.forecast <- forecast(retail.ets, h = 12))
plot(retail.forecast)

###########################
# assigning months for retail sales
###########################

vec <- tail(retail.forecast$model$states)[6,] # this is the last rows
vec.s <- vec[3:length(vec)] # Seasonal Part of the vector

december <- vec.s[max(vec.s) == vec.s] %>%
  names %>%
  substr(start = 2, stop = 2) %>%
  as.numeric # this gives of the position of december in the vector

monthsss <- c(3:1, 12:2) # Pooki Method

mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sept", "Oct", "Nov", "Dec") %>% rev # vector used for assignement

(newmonths <- mon[monthsss]) # Pooki Final Result

##########################
# Dynamic Assignment of the variables
# ----- Not a the mod operator, it does work though
#######################

# This segment of code extracts the dates of the forecast
# This allows us to figure out what the last real data point
# We recieved
###

forecastDates <- retail.forecast %>% as.data.frame %>% rownames

#  Extracts the first forecated month.
###

lastMonth <- forecastDates[1] %>% substr(start = 1, stop = 3)

# This gives us the possition of the First month forecasted + 1
# because the mon vector has been reversed it gives us the last month
# of real data.
###

location <- grep(lastMonth, mon) + 1

# Reorganizeing the mon vetor to give us the month of the
# seasonal indicators
####

months <- c(mon[location:length(mon)], mon[1:(location - 1)])

# ----------- test print of our results
##########

cbind(months, vec.s)
