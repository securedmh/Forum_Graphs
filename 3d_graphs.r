## TODO(securedmh): Try and change to POSIXct to include timezone (http://www.noamross.net/blog/2014/2/10/using-times-and-dates-in-r---presentation-code.html)
## TODO(securedmh): Normalize the time of day better by finding out what timezone the forum was on.

## Required Libraries
library(plyr)
library(rgl)


## Constant Variables

## Total number of forum posts made by DPR
kTotalPosts <- 536

## To avoid dealing with the first post occurring at 0 minutes, we will assume that the first forum post was made at the beginning of the
## week of DPR's first post, which was made on Saturday, 18th of June. Therefor assume the first forum post was 15th of June, 9 AM.
## The forum was believed to be running on GMT so assume that as well and use POSIXlt for the zone.
kFirstPost <- "2011-06-15 09:00:00"
kLastPost <- "2013-10-02 00:00:00"


## Set units of the y-axis of graph
y.units <- "month"

# Set working directory to the project directory and store csv file in seperate 'data' dir
setwd("~/code/r_projects/forum_graphs")

# Load the chat data in the form: Numeric identifier, Username, Time of Post
# Posts are in ascending order because each subsequent post was written after the previous one (except when it wasn't)
chats.new.to.old <- read.csv("data/DPRStuff.csv")

# Order the rows from earliest post to latest post.
chats <- chats.new.to.old[order(chats.new.to.old[ ,"post.time"]), ]

# Calculate the length of time, in weeks, that SR had been running when a post was made.
# weeks.passed <- ddply(chats, .(username), summarize, sr.run.time = difftime(as.POSIXct(post.time, tz = "GMT"), as.POSIXct(kFirstPost, tz = "GMT"), "GMT", units="weeks"))

## Determine the week and month of operation for each post made
## E.g., the first seven days of operation were week 1 so divide by 7 and take the ceiling of the result to reflect that.
chats.extended <- ddply(chats, .(username, post.time), summarise, 
                          week.number = as.factor(ceiling(difftime(as.POSIXct(post.time, tz = "GMT"), as.POSIXct(kFirstPost, tz = "GMT"), "GMT", units="weeks"))),
                          month.number = as.factor(sapply(post.time, function(x){
                            length(seq.POSIXt(as.POSIXct(kFirstPost, tz = "GMT"), as.POSIXct(x, tz = "GMT"), "month"))
                          }))
)

## X-axis is time of day so create bins for every 2 hours
## E.g., 12 AM - 2 AM or 6 PM - 8 PM
x.plot <- seq(0,22,2)

## Y-axis is week or month of SR operating
if(y.units=="month") {
  max.month <- as.integer(as.character(chats.extended$month.number[kTotalPosts]))
  y.plot <- seq(1, max.month, 1) 
} else {
  y.plot <- as.integer(as.character(chats.extended$week.number[kTotalPosts]))
}

## Initialize z variable as for persp3d plot
## For DPR forum posts by month and time of day matrix should be 
z.plot <- matrix(0, nrow=length(y.plot), ncol=length(x.plot))

## Now populate the z matrix with the amount of posts each week/month for a day/time of day
for(i in 1:length(chats.extended$month.number)) {
  
  ## Find which month of operation
  operating.month <- as.integer(as.character(chats.extended$month.number[i]))
  
  ## R doesn't compare times of day well so create a time as close to midnight
  ## for the day of each post to work out what time the post is.
  post.date <- format(as.POSIXct(chats.extended$post.time[i], tz = "GMT", "%Y-%m-%d"))

#  post.ref.date <- paste(post.date, "23:59:59")
  post.ref.date <- paste(post.date, "00:00:00")
  time.until.eod <- as.numeric(difftime(as.POSIXct(chats.extended$post.time[i]), as.POSIXct(post.ref.date), units="mins"))
  x.point <- ((time.until.eod %/% 60) %/% 2) + 1

  z.plot[operating.month, x.point] <- z.plot[operating.month, x.point] + 1

}

## Colour based on frequency (z-axis)
# http://stackoverflow.com/questions/17258787/formating-of-persp3d-plot
# http://entrenchant.blogspot.com/2014/03/custom-tick-labels-in-r-perspective.html
nbcol = 100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol  = cut(z.plot, nbcol)

## Now graph the results using persp3d from package rgl
persp3d(x.plot, y.plot, z.plot, theta=50, phi=25, expand=0.75, col=color[zcol],
        ticktype="detailed", xlab = "Time of Day", ylab = "Month Number",
        zlab = "Post Frequency", box = FALSE, axes = FALSE)

## Label the axis
title3d(main = "Frequency of Posts By DPR on SR1 Forum")

## Use axes3d and axis3d to label the graph
x.axes.labels <- c("00:00", "02:00", "04:00", "06:00", "08:00", "10:00", "12:00", "14:00", "16:00", "18:00", "20:00", "22:00")

# repositions x axis and draws default z axis
axes3d(edges = c('x--'), labels = x.axes.labels, nticks = length(x.plot))

# Use custom labels
#axis3d(edge = 'y+-', at = seq(500,2000,by=500), 
#       labels = rownames(fd)[seq(500,2000,by=500)] )
#y.axes.labels.all <- c(
#  "Jun 2011", "Jul 2011", "Aug 2011", "Sep 2011", "Oct 2011", "Nov 2011", "Dec 2011",
#  "Jan 2012", "Feb 2012", "Mar 2012", "Apr 2012", "May 2012", "Jun 2012", "Jul 2012", "Aug 2012", "Sep 2012", "Oct 2012", "Nov 2011", "Dec 2011",
#  "Jan 2013", "Feb 2013", "Mar 2013", "Apr 2013", "May 2013", "Jun 2013", "Jul 2013", "Aug 2013", "Sep 2013", "Oct 2013"
#)

# Every second month of operation
#y.axes.labels <- c(
#  "Jun 2011", "Aug 2011", "Oct 2011", "Dec 2011",
#  "Feb 2012", "Apr 2012", "Jun 2012", "Aug 2012", "Oct 2012", "Dec 2011",
#  "Feb 2013", "Apr 2013", "Jun 2013", "Aug 2013", "Oct 2013"
#)

y.axes.labels <- format(seq.POSIXt(as.POSIXct(kFirstPost), as.POSIXct(kLastPost), by = "2 months"), format = "%Y-%m-%d")

axis3d(edge = 'y--', at = seq(1,28,2), labels = y.axes.labels)

## Unused Code
# Calculate the month of operation each post was made during
#post.month.number1 <- lapply(chats$post.time, function(x){
#print(typeof(x))
#  length(seq.POSIXt(as.POSIXct(kFirstPost, tz = "GMT"), as.POSIXct(x, tz = "GMT"), "month"))
#})

## Determine the month of operation each post was made during
#post.month.number <- ddply(chats, .(username), .fun = function(x){
#  lapply(x$post.time, function(xx){
#print(typeof(x))
#    length(seq.POSIXt(as.POSIXct(kFirstPost, tz = "GMT"), as.POSIXct(xx, tz = "GMT"), "month"))
#  })
#   print(typeof(x))
#   View(x)
#   print(typeof(x$post.time))
#   print(x$post.time)
#   print(as.POSIXct(x$post.time, tz = "GMT"))
#  print(length(seq.POSIXt(as.POSIXct(kFirstPost, tz = "GMT"), as.POSIXct(x$post.time, tz = "GMT"), "month")))
#},
#.inform = TRUE)
#post.month.number <- ddply(chats, .(username), summarise, sr.run.time = length(seq.POSIXt(as.POSIXct(kFirstPost, tz = "GMT"), as.POSIXct(post.time, tz = "GMT"), "month")))

## i.w is the ... As the y-axis is in months the first four weeks all occur together, hence the %/%. +1 starts the week count at 1.
## WHAT THE FUCK DID I DO HERE?
#i.w <- (as.numeric(post.week.number[i]) %/% 4) ##@ToDo: fix the trailing digit

## Only need the trailing digit when the modulo of the post weeks is not zero 
#if((as.numeric(post.week.number[i]) %% 4)!=0) {
#  i.w <- i.w + 1
#}

# N.B. Chats from Saturday, 2011-06-18 until Tuesday 2013-10-01
#total.dates <- nrow(chats)
#as.Date(chats[1,3], format="%Y-%m-%d") - as.Date(chats[total.dates,3], format="%Y-%m-%d")
#jj <- ceiling(difftime(as.Date(chats[1,"post.time"], format="%Y-%m-%d"), as.Date(chats[536,"post.time"], format="%Y-%m-%d"), units="weeks"))

## For x-axis
## https://stackoverflow.com/questions/13067012/date-format-for-plotting-x-axis-ticks-of-time-series-data

## Very nice time-series graph
## https://stackoverflow.com/questions/1896419/plotting-a-3d-surface-plot-with-contour-map-overlay-using-r
