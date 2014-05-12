get_workout_data <- function() {
    
    require(RCurl)
    require(lubridate)
    googleURL <- "https://docs.google.com/spreadsheet/pub?key=0At_x4S00cjjPdEs2bE1wT2F2UVpDZlAyZ0Fua2gzX2c&output=csv"
    column_class <- c("character", "factor", "character", "character", "numeric")
    workout_data<-read.csv(textConnection(getURL(googleURL)), na.strings="-", 
                           colClasses=column_class)
    names(workout_data) <- tolower(names(workout_data))
    workout_data$datetime <- strptime(paste(workout_data$date,workout_data$start.time), format="%m/%d/%Y %H:%M:%s")
    workout_data$duration <- as.duration(hms(workout_data$duration))
    workout_data$week <- as.Date(cut(data$datetime,breaks="week",start.on.monday=0))
    columns <- c("sport", "duration", "distance", "datetime", "week")
    final_data <- workout_data[columns]
    final_data
}

generate_sport_barplot <- function() {
    png("sports.png")
    data <- get_workout_data()
    barplot(table(data$sport), col="blue", xlab="Sports", ylab="Counts")
    dev.off()
}

generate_month_barplot <- function() {
    data <- get_workout_data()
    temp_data <- split(data,month(data$datetime, label=TRUE))
    barplot(sapply(temp_data,function(x) sum_d(x[,"duration"])))
}

generate_barplot <- function(period="week") {
    require(ggplot2)
    data <- get_workout_data()
    if (period == "month"){
        ggplot(data=data[order(data$sport),], aes(x=month(datetime), y=duration, fill=sport)) + 
            geom_bar(stat="identity")
    }
    else {
        ggplot(data=data[order(data$sport),], aes(x=week, y=duration, fill=sport)) + 
            geom_bar(stat="identity")
        
    }
}