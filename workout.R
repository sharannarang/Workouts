get_workout_data <- function() {
    
    require(RCurl)
    require(lubridate)
    require(rjson)
    googleURL <- "https://docs.google.com/spreadsheet/pub?key=0At_x4S00cjjPdEs2bE1wT2F2UVpDZlAyZ0Fua2gzX2c&output=csv"
    column_class <- c("character", "factor", "character", "character", "numeric")
    ## Download data from google docs
    workout_data<-read.csv(textConnection(getURL(googleURL)), na.strings="-", 
                           colClasses=column_class)
    ## Fix the column names
    names(workout_data) <- tolower(names(workout_data))
    
    ## convert to datetime and duration
    workout_data$datetime <- strptime(paste(workout_data$date,workout_data$start.time), format="%m/%d/%Y %H:%M:%s")
    workout_data$duration <- as.duration(hms(workout_data$duration))
    
    ## Find week information
    workout_data$week <- as.Date(cut(workout_data$datetime,breaks="week",start.on.monday=0))
    
    ## Return only useful columns
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
    temp_data <- split(data,month(data$datetime, label=TRUE), drop=T)
    barplot(sapply(temp_data,function(x) sum_d(x[,"duration"])))
}

sum_d <- function(durations) {
    total <- 0
    for (d in durations) {
        total <- total + d
    }
    as.duration(total)
}

generate_barplot <- function(period="week") {
    require(ggplot2)
    require(lubridate)
    require(scales)
    data <- get_workout_data()
    total <- sum(data$duration)
    current_date <- ymd(Sys.Date())
    start_date <- ymd("2014/1/1")
    diff_date <- interval(start_date,current_date)
    if (period == "month"){
        ## %/% operetor is used when you don't want the answer as a fraction. 
        ## 6 %/% 5 = 1 6 / 5 = 1.2
        months <- diff_date %/% months(1)
        average <- total/months
        average <- average/(60*60)        
        ggplot(data=data[order(data$sport),], aes(x=month(datetime, label=TRUE), y=duration/(60*60), fill=sport)) + 
            geom_bar(stat="identity") + ylab("Duration (Hours)") + geom_hline(yintercept=average, aes(color="Average")) + xlab("Month")        
        
    }
    else {
        weeks <- diff_date/eweeks()
        average <- total/weeks
        average <- average/(60*60)
        ggplot(data=data[order(data$sport),], aes(x=week, y=duration/(60*60), fill=sport)) + 
            geom_bar(stat="identity") + ylab("Duration (Hours)") + geom_hline(yintercept=average, aes(color="Average"))
    }
}

generate_dPlot_month <- function(sport = "All", year=2014) {
    generate_rchart("dPlot", sport, year)
}

generate_nPlot_month <- function(sport = "All", year=2014) {
    generate_rchart("nPlot", sport, year)
}

generate_rchart <- function(plot="nPlot", sport = "All", year = 2014) {
    library(reshape2)
    library(rCharts)
    workout <- get_workout_data()
    if (sport != "All") {
        workout <- workout[workout$sport == sport, ]        
    }
    workout <- workout[year(workout$datetime)==year,]
    workout$month <- month(workout$datetime, label=T)
    workout.melt <- melt(workout, id.vars = c("month", "sport"), measure.vars = c("duration"))
    workout.cast <- dcast(workout.melt, month+sport~variable, sum)    
    workout.cast$duration <- workout.cast$duration/(60*60)
    if (plot == "nPlot") {
        n1 <- nPlot(duration~month, group = "sport", data=workout.cast, type='multiBarChart')
        n1    
    }
    else if (plot == "dPlot") {
        d1 <- dPlot(y = "month", x = "duration", data =workout.cast, groups= "sport", type="bar")
        d1$yAxis(type="addCategoryAxis")
        d1$xAxis(type="addMeasureAxis")
        d1$legend(
            x = 0,
            y = 0,
            width = 500,
            height = 75,
            horizontalAlign = "right"
        )
        d1
    }
}


