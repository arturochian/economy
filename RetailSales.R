library("fImport")
library("forecast")

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

monthsss <- c(3:1, 12:2)

mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sept", "Oct", "Nov", "Dec") # vector used for assignement

(newmonths <- mon[monthsss])

months <- character(length = 12) # intializes and empty vector length 12

for(i in 0:11){
  if(december + i <= 12)
  {
      months[december + i] <- mon[i+1]
  }
  else
  {
    months[december + i - 12] <- mon[i+1]
  }
}

rownames(retail.forecast)
