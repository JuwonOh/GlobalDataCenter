
###############################
## Make a chronology graph
###############################
## http://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
library(ggplot2)
library(scales)
library(lubridate)
raw <- readLines("post2018july.txt")
text <- gsub(".*:", "", raw)
date <- gsub(":.*", "", raw)
marker <- unlist(lapply(date, nchar))
marker.identifier <- which(marker > 10)
n.marker <- length(marker.identifier)
marker.text <- text[marker.identifier]
marker.date <- date[marker.identifier]

###############################
## https://stackoverflow.com/questions/44265512/creating-a-timeline-in-r
###############################
df.date <- sub("-.*,", ",", marker.date)

df <- data.frame(events = marker.text,
                 groups = sample(1:5, 37, replace=TRUE), 
                 start_date = as.Date(df.date, format = "%B %d, %Y"),
                 end_date = as.Date(df.date, format = "%B %d, %Y"),
                 stringsAsFactors = FALSE
)

library(vistime)
vistime(df, events = "events",  groups="groups", 
        start = "start_date", end = "start_date")



###############################
## resources
###############################

df <- data.frame(
    DeviceName = c("Cypher Sirolimus DES", "Taxus Express 2", "Cypher Select Sirolimus DES",
                   "Cypher Select Plus Sirolimus DES", "Taxus Liberte", "Endeavor ABT578",
                   "Endeavor Sprint Zotarolimus DES", "Xience V", "Taxus Element Monrail ION",
                   "Xience Nano", "Promus Element Plus", "Xience Prime",
                   "Endeavor Resolute DES","Endeavor Resolute Integrity DES", "Promus Premier", "Xience Xpedition LL and SV"),
    DeviceManufacturer = c("Cordis Cashel","Boston Scientific","Cordis Cashel",
                           "Cordis Cashel","Boston Scientific","Medtronic Inc",
                           "Medtronic Inc", "Abbott Vascular", "Boston Scientific",
                           "Abbott Vascular","Boston Scientific", "Abbott Vascular",
                           "Medtronic Inc", "Medtronic Inc","Boston Scientific", "Abbott Vascular"),
    start_date = as.Date(c("2002-11-15", "2003-09-09", "2005-10-21", 
                           "2006-10-25","2008-02-05", "2008-02-27",
                           "2009-06-10", "2009-08-21", "2011-08-19",
                           "2011-10-24", "2012-01-30", "2012-04-10",
                           "2012-04-14", "2013-03-07", "2013-09-30", "2014-02-19")),
    end_date = as.Date(c("2002-11-15", "2003-09-09", "2005-10-21", 
                           "2006-10-25","2008-02-05", "2008-02-27",
                           "2009-06-10", "2009-08-21", "2011-08-19",
                           "2011-10-24", "2012-01-30", "2012-04-10",
                           "2012-04-14", "2013-03-07", "2013-09-30", "2014-02-19")),
    stringsAsFactors = FALSE
)

###############################
## different example
###############################
df <- data.frame(month = c(rep("July", 5), rep("August", 4), rep("September", 7),
                           rep("October", 5), rep("November", 5),
                           rep("December", 2), rep("January", 6), "February", 
                           rep("March", 2)), 
                 year = c(rep("2018", 28), rep("2019", 9)),
                 milestone = marker.text,
                 status = marker.text)
## month	year	milestone	status
df$date <- with(df, ymd(sprintf('%04d%02d%02d', year, month, 1)))
df <- df[with(df, order(date)), ]
head(df)

## Next we’ll convert the status to an ordinal categorical variable, in order of criticality ranging from “Complete” to “Critical”. We’ll also define some hexadecimal colour values to associate with these statuses.

status_levels <- c("Complete", "On Target", "At Risk", "Critical")
status_colors <- c("#0070C0", "#00B050", "#FFC000", "#C00000")

df$status <- factor(df$status, levels=status_levels, ordered=TRUE)

## In our timeline, we want to vary the height and direction of the lines, because otherwise the text for our milestones will clash.
## We need to assign the lines and the heights for milestones within the same month to be the same, so we only change the height and position values.
## We then order our data frame by date and status, so that the most critical status is plotted last and the colours displayed are for the most critical milestone status.
positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
directions <- c(1, -1)

line_pos <- data.frame(
    "date"=unique(df$date),
    "position"=rep(positions, length.out=length(unique(df$date))),
    "direction"=rep(directions, length.out=length(unique(df$date)))
)

df <- merge(x=df, y=line_pos, by="date", all = TRUE)
df <- df[with(df, order(date, status)), ]

head(df)

## If there are multiple milestones for a given month, we need to slightly alter their positions (slightly higher if above our timeline and slightly lower if below our timeline).
## We can do a cumulative count of individual dates to check if we have multiple milestones for a given month.


text_offset <- 0.05

df$month_count <- ave(df$date==df$date, df$date, FUN=cumsum)
df$text_position <- (df$month_count * text_offset * df$direction) + df$position
head(df)


## Because we want to display all months on our timelines, not just the months we have events for, we’ll create a data frame containing all of our months.
## We’ll start 2 months before the first milestone and end 2 months after the last milestone for a little bit of a buffer.

month_buffer <- 2

month_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

## We do the same for the years that we also want to display.
## We’ll only display years for which there is a December/January crossover, this is what our intersect line is doing.
## In [8]:
year_date_range <- seq(min(df$date) - months(month_buffer), max(df$date) + months(month_buffer), by='year')
year_date_range <- as.Date(
    intersect(
        ceiling_date(year_date_range, unit="year"),
        floor_date(year_date_range, unit="year")
    ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)
## Now that we’ve got our data in a state ready to be plotted, we can put together our plot.


#### PLOT ####

timeline_plot<-ggplot(df,aes(x=date,y=0, col=status, label=milestone))
timeline_plot<-timeline_plot+labs(col="Milestones")
timeline_plot<-timeline_plot+scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE)
timeline_plot<-timeline_plot+theme_classic()

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                color = "black", size=0.3)

# Plot vertical segment lines for milestones
timeline_plot<-timeline_plot+geom_segment(data=df[df$month_count == 1,], aes(y=position,yend=0,xend=date), color='black', size=0.2)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(aes(y=0), size=3)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 axis.ticks.y=element_blank(),
                 axis.text.x =element_blank(),
                 axis.ticks.x =element_blank(),
                 axis.line.x =element_blank(),
                 legend.position = "bottom"
                )

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, aes(x=month_date_range,y=-0.1,label=month_format),size=2.5,vjust=0.5, color='black', angle=90)
# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df, aes(x=year_date_range,y=-0.2,label=year_format, fontface="bold"),size=2.5, color='black')
# Show text for each milestone
timeline_plot<-timeline_plot+geom_text(aes(y=text_position,label=milestone),size=2.5)
print(timeline_plot)


###############################
## others: https://github.com/shosaco/vistime
###############################

