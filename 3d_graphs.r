# Take the start and end
# Use plot3D to draw the 3d-plot

## Required Libraries
library(plyr)
library(rgl)

# Recommended Libraries
#library(lubridate)

## Put constant vars here
y.units <- "month"

# Load the chat data in the form:
# Numeric identifier, Username, Time of Post
# Posts are in ascending order because each subsequent post was written after the previous one (except when it wasn't)
chats <- read.csv("data/DPRStuff.csv")

# Order the rows from earliest post to latest post.
chats3 <- chats[order(chats[ ,"post.time"]), ]

# In ascending order, the earliest post time will also be at the top of the list (chats3)
#first.post <- chats3[1, "post.time"]

## To avoid dealing with the first post occurring at 0 minutes, we will assume that the first forum post was made at the beginning of the
## week of DPR's first post. The first DPR post was made on Saturday, 18th of June. There set first post for 15th of June, 7 AM.
first.post <- as.POSIXct("2011-06-15 07:00:00")

## ToDo: Normalize the time of day better

# Ross was on the West Coast so set time as Pacific.
#difftime(as.POSIXlt(chats[ ,"post.time"][[500]], format="%Y-%m-%d"), as.POSIXlt(first.post, format="%Y-%m-%d"), units="mins")

# Calculate the length of time that SR had been running when a post was made.
## N.B.: Try and change to POSIXct to include timezone (http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html)
passed.time <- ddply(chats3, .(username), summarize, sr.run.time = difftime(as.POSIXlt(post.time), as.POSIXlt(first.post), units="weeks"))

# Record the number of weeks that have passed since the first post
## Take the ceiling to reflect that it is the nth week. E.g., the first seven days are in week 1 but dividing by 7 falls short.
post.weeks <- ceiling(passed.time$sr.run.time)

# Just do this for now as it should be 1 (or equiv to week 1)
## Can be removed now that we assume the first post happens a short time before DPR
#post.weeks[1] <- 1

## Nothing to see here, move along.
#post.hours <- as.numeric(passed.time$sr.run.time) %% rep(10, nrow(passed.time))
#post.hours2 <- (passed.time$sr.run.time - floor(passed.time$run.time)) /7

## 12 %% 4 = 0
## 13 %/% 4 = 3
## Create bins for time of day in 2 hour bins
## E.g., 12 AM - 2 AM or 6 PM - 8 PM
x.plot <- 1:12

## Allow for weeks or months to be the y axis
## Assume that one month is roughly equivalent to 4 weeks for now
## Also hard code 120 to not have to worry about finding the value of last week.
if(y.units=="month") {
  y.plot <- 1:(120/4)
} else {
  y.plot <- 1:120
}

## Initialize with zeros?
z.plot <- matrix(0, nrow=length(y.plot), ncol=length(x.plot))


## Now populate the z matrix with the frequencies
for(i in 1:length(post.weeks)) {
  
  ## i.w is the ... As the y-axis is in months the first four weeks all occur together, hence the %/%. +1 starts the week count at 1.
  i.w <- (as.numeric(post.weeks[i]) %/% 4) ##@ToDo: fix the trailing digit
  
  ## Only need the trailing digit when the modulo of the post weeks is not zero 
  if((as.numeric(post.weeks[i]) %% 4)!=0) {
    i.w <- i.w + 1
  }
  
  post.date <- format(as.POSIXct(chats3$post.time[i]), "%Y-%m-%d")
  post.ref.date <- paste(post.date, "23:59:59")
  time.until.eod <- as.numeric(difftime(as.POSIXct(post.ref.date), as.POSIXct(chats3$post.time[i]), units="mins"))

  ## ToDo fix this so that it is the x-axis
  x10 <- (time.until.eod %/% 60 ) / 2

#   if(is.na(z.plot[i.w, x10])) {
#     z.plot[i.w, x10] <- 1
#   } else {
  z.plot[i.w, x10] <- z.plot[i.w, x10] + 1
#  }
#  i.post.time <- format(as.POSIXlt(chats3$post.time[i.w]), "%H:%M:%S")
#  as.POSIXlt(format("01/01/1990 23:59:59"), "%H:%M:%S")
}

# N.B. Chats from Saturday, 2011-06-18 until Tuesday 2013-10-01
#total.dates <- nrow(chats)
#as.Date(chats[1,3], format="%Y-%m-%d") - as.Date(chats[total.dates,3], format="%Y-%m-%d")
#jj <- ceiling(difftime(as.Date(chats[1,"post.time"], format="%Y-%m-%d"), as.Date(chats[536,"post.time"], format="%Y-%m-%d"), units="weeks"))

## For x-axis
## https://stackoverflow.com/questions/13067012/date-format-for-plotting-x-axis-ticks-of-time-series-data

## Very nice time-series graph
## https://stackoverflow.com/questions/1896419/plotting-a-3d-surface-plot-with-contour-map-overlay-using-r

## Now graph the results
persp3d(x.plot, y.plot, z.plot, xlim=c(0,24), xlab = "Time of Day", ylab = "Month Number", zlab = "Post Frequency", col="skyblue")

