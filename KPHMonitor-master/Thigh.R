library(chron)
library(lubridate)
library(tidyverse)

#	AP Functions

#	Function to read in AP Events file

activ.pal.file.reader <- function(file.name.and.path)
{
    data <- read.csv(file.name.and.path, stringsAsFactors=FALSE)
    data <- data[,(1:6)]
    names(data) <- c("time","datacount","interval","activity","cumulativesteps","methrs")

    data$time <- as.character(data$time)
    data[,2] <- as.numeric(as.character(data[,2]))
    data[,3] <- as.numeric(as.character(data[,3]))
    data[,4] <- as.numeric(as.character(data[,4]))
    data[,5] <- as.numeric(as.character(data[,5]))*2 #event files have half the actual number of steps for some reason
    data[,6] <- as.numeric(as.character(data[,6]))
    return(data)
}

##test
#temp$time <- as.numeric(temp$time)
#temp$time <- chron(temp$time,origin=c(month=12,day=30,year=1899), format=c("m-d-y","h:m:s"))
#temp$time <- as.POSIXct(temp$time)
#temp$time <- strptime(temp$time,format="%Y-%m-%d %H:%M:%S")

#	Function to convert Events file read in by 'activ.pal.file.reader' to a second-by-second file
#data<-temp

second.by.second <- function(data)
{
    sec.by.sec.data <- data.frame(time=NA, date=NA, ap.posture=NA, mets=NA, met.hours=NA, steps=NA)
    sec.by.sec.data <- sec.by.sec.data[-1,]

    n <- dim(data)[1]
    time.of.each.event <- as.vector(difftime(strptime(data$time[seq_len(n - 1) + 1],format="%Y-%m-%d %H:%M:%S"),strptime(data$time[seq_len(n - 1)],format="%Y-%m-%d %H:%M:%S"), units="secs"))
    start.time <- strptime(data$time[1],format="%Y-%m-%d %H:%M:%S")

    time.of.each.event <- c(time.of.each.event, round(data[n,"interval"],0))
    te <- length(time.of.each.event)
    time.of.each.event[is.na(time.of.each.event)==T] <- 0
    time.of.each.event[time.of.each.event<0] <- 0

    events <- rep((1:te),time.of.each.event)
    length(time.of.each.event)
    acts <- rep(data$activity,time.of.each.event)
    n <- length(acts)
    # The met hours per second in the interval.
    met.hours <- data$methrs/data$interval
    met.hours <- rep(met.hours,time.of.each.event)
    datacount <- rep(data$datacount,time.of.each.event)
    # To compute mets per second in the interval, multiply methours by 3600 sec/hour and divide by number of seconds.
    mets <- data$methrs * 3600/data$interval
    mets <- rep(mets,time.of.each.event)
    steps <- rep(data$cumulativesteps,time.of.each.event)
    # Make 15-sec epoch variable and METs
    times <- start.time+(0:(n-1))
    fifteen.sec.times <- start.time + (15*rep(0:(floor(n/15)),each=15,length=n))
    fifteen.sec.mets <- tapply(mets, fifteen.sec.times, mean)
    fifteen.sec.mets <- rep(fifteen.sec.mets, each=15, length=n)

    date <- substring(format(times),1,10)

    sec.by.sec.data <- merge(sec.by.sec.data, data.frame(time=times, datacount=datacount, date=date, ap.posture=acts, mets=mets, fifteen.sec.mets=fifteen.sec.mets, met.hours=met.hours, steps=steps, num.events=events, stringsAsFactors=FALSE), all=TRUE)

    return(sec.by.sec.data)
}

#	Function to eliminate off time
# This function reads in a on/off log (.csv file) and eliminates time off
# The on/off log must be in correct format

on.off <- function(data)
{
    on.off.log <- read.csv(paste(out.directory,"DO_LOG_final.csv", sep=""))
    on.off.log$id <- as.character(on.off.log$id)
    on.off.log <- on.off.log[on.off.log$id==s& on.off.log$visit==v,]

    on.off.record$date2 <-paste(on.off.record$start_month,"/",on.off.record$start_day,"/",on.off.record$start_year,sep="")
    on.off.record$date.time.on <- as.POSIXlt(strptime(paste(on.off.record$date2, on.off.record$start_time), "%m/%d/%Y %H:%M:%S"))
    on.off.record$date.time.off <- as.POSIXlt(strptime(paste(on.off.record$date2, on.off.record$stop_time), "%m/%d/%Y %H:%M:%S"))
    on.off.log$hours.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%m/%d/%y %H:%M:%S"),strptime(on.off.log$date.time.on,format="%m/%d/%y %H:%M:%S"), units="hours"))

    #	if on/off times recorded - loop through and label time monitor is not worn
    if(dim(on.off.log)[1]>0)
    {
        data$off <- 1
        for (t in (1:dim(on.off.log)[1]))
        {
            on <- strptime(on.off.log$date.time.on[t],"%m/%d/%y %H:%M:%S")
            off <- strptime(on.off.log$date.time.off[t],"%m/%d/%y %H:%M:%S")
            n <- dim(data)[1]
            inds <- (1:n)[((data$time>=on)&(data$time<=off))]
            if (length(inds)>0)
                data$off[inds] <- 0
        }
        if(dim(on.off.log)[1]==0)
            data$off <- "No.On.Off.Log"
    }	#end bed loop
    return(data)
}

####	start functions to estimate PA and SB variables

#	Function to calculate the number of sitting/lying bouts at least 'n' minutes in duration.  User specifies n

AP.compute.num.bouts.n.min.at.sed <- function(posture, n)
{
    acts <- posture == 0
    lengths.of.continuous.bouts.of.sed <- apply(as.matrix(strsplit(paste(acts, collapse=""), split="FALSE", fixed=TRUE)[[1]]), 1, function(x) {nchar(x)/4})
    return(sum(lengths.of.continuous.bouts.of.sed > n*60))
}

#	Function to calculate the minutes spent in sitting/lying bouts at least 'n' minutes in duration.  User specifies n

AP.compute.min.bouts.n.min.at.sed <- function(posture, n)
{
    acts <- posture == 0
    lengths.of.continuous.bouts.of.sed <- apply(as.matrix(strsplit(paste(acts, collapse=""), split="FALSE", fixed=TRUE)[[1]]), 1, function(x) {nchar(x)/4})
    lengths <- lengths.of.continuous.bouts.of.sed > n*60

    return(sum(lengths.of.continuous.bouts.of.sed[lengths])/60)
}

# tapply(temp.5$ap.posture,temp.5$day.for.wearer, AP.compute.min.bouts.n.min.at.sed,n=60)/60

#	Function to calculate the minutes spent in sitting/lying

sed.time.AP <- function(posture)
{
    sed.hours <- sum((posture==0),na.rm=T)/3600
    if (sed.hours==0)
        sed.hours <- NA
    return(as.numeric(sed.hours))
}

#	Function to calculate the minutes spent in standing

stand.time.AP <- function(posture)
{
    stand.hours <- sum((posture=="1"),na.rm=T)/3600
    if (stand.hours==0)
        stand.hours <- NA
    return(stand.hours)
}

#	Function to calculate the minutes spent in stepping

step.time.AP <- function(posture)
{
    step.hours <- sum((posture=="2"),na.rm=T)/3600
    if (step.hours==0)
        step.hours <- NA
    return(step.hours)
}

#	Function to calculates the number of breaks in sitting

breaks.AP <- function(posture)
{
    y <- posture
    n <- length(y)

    mmm <- length(y)
    one <- y[-mmm]
    two <- y[-1]

    # transitions from sed to not
    trans.up <-
        (one=="0")&(two!="0")
    num.up.AP <- sum(trans.up,na.rm=T)
    if (num.up.AP==0)
        num.up.AP <- NA
    return(num.up.AP=num.up.AP)
}

#	Function to calculate the hours spent in light intensity (1.5-2.99 METs)

lit.time.AP <- function(mets)
{
    lit.hours <- sum((mets>=1.5)&(mets<3))/3600
    if (lit.hours==0)
        lit.hours <- NA
    return(lit.hours)
}

#	Function to calculate the hours spent in MVPA (>=3 METs)

mvpa.time.AP <- function(mets)
{
    mvpa.hours <- sum(mets>=3)/3600
    if (mvpa.hours==0)
        mvpa.hours <- NA
    return(mvpa.hours)
}

#	Function to calculate the time spent in guideline minutes (bouts of MVPA (>=3 METs) that last at least 10 min. allows for 20% of time below 3 METs i.e. if bout is 10 min long, 2 min can be <3 METs)

tot.time.in.guideline.bouts.20.percent.below <- function(est.mets)
{
    total.active.time.in.bouts <- 0

    # indices where transitions take place
    mets.length <- length(est.mets)
    one <- est.mets[-mets.length]
    two <- est.mets[-1]

    trans <- c( FALSE, ((one<3)&(two>=3)) | ((one>=3)&(two<3)) )
    trans.inds <- c(1, seq_along(est.mets)[trans], (mets.length+1))

    # how long are the periods of activity and inactivity
    durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]

    # identify if interval is activity or inactivity (they alternate)
    active.interval <- rep(FALSE,length=length(durations))

    if (est.mets[1]<3)
        active.interval <- rep(c(FALSE, TRUE),length=length(durations))
    if (est.mets[1]>=3)
        active.interval <- rep(c(TRUE, FALSE),length=length(durations))


    # Create some empty vectors which will be used to keep track of the
    # start and end points of the bouts in the durations vector.
    bout.starts <- c()
    bout.ends <- c()

    # Create some variables which will be used in constructing the bouts.
    active.inds <- seq_along(durations)[active.interval]
    if(length(active.inds) > 0) {
        possible.bout.active.inds <- 1
        possible.bout.active.inds.length <- 1
        bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
        end.possible.bout <- FALSE

        while(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
            # Determine if adding the next inactive interval to the current possible bout
            # would cause the last 10 minutes of the bout to be more than 20% inactive

            bout.inactivity.with.next.interval <- c(bout.inactivity, rep(1, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 1]))
            secs.to.extract <- min(length(bout.inactivity.with.next.interval), 10*60)
            last.10.min.of.possible.bout <- bout.inactivity.with.next.interval[seq_len(secs.to.extract) + (length(bout.inactivity.with.next.interval) - secs.to.extract)]

            if( sum(last.10.min.of.possible.bout) <= 2*60 ) {
                # If adding the next inactive interval means the last 10 min has <= 2 min of inactivity,
                # add it and the next active interval to the possible-bout.
                bout.inactivity <- c(bout.inactivity.with.next.interval, rep(0, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 2]))
                possible.bout.active.inds <- c(possible.bout.active.inds, possible.bout.active.inds[possible.bout.active.inds.length] + 1)
                possible.bout.active.inds.length <- possible.bout.active.inds.length + 1
                if(possible.bout.active.inds[possible.bout.active.inds.length] == length(active.inds)) {
                    # If we just added the last active interval to the possible bout, end the possible bout.
                    end.possible.bout <- TRUE
                }
            } else {
                # If adding the next inactive interval means the last 10 min has > 2 min of inactivity,
                # stop building this bout.
                end.possible.bout <- TRUE
            }

            if(end.possible.bout) {
                # If the possible bout is long enough, add it to the list of bouts.
                # Reset to start building the next possible bout.

                possible.bout.length <- sum(durations[seq(from=active.inds[possible.bout.active.inds[1]], to=active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
                if(possible.bout.length >= 10*60) {
                    # If the total duration of the possible bout is >= 10 min, it is a bout.

                    # The start position of the bout is recorded as an index in the input est.mets vector.
                    # This is calculated as the sum of the durations of all intervals before the start of this bout, plus 1
                    bout.starts <- c(bout.starts, sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1)

                    # The end position of the bout is recorded as an index in the input est.mets vector.
                    # This is calculated as the sum of the durations of all intervals up through the end this bout
                    bout.ends <- c(bout.ends, sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]))

                    # add the active time in this bout to the total active time in all bouts
                    total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
                } else {
                    # If it is < 10 min, see if we can add on some inactive time to the beginning and/or end
                    # to reach the 10 min threshold and make it into a bout.  Otherwise, discard it.

                    # Calculate the number of seconds needed to fill in to reach the 10 minute threshold.
                    seconds.missing <- 10*60 - possible.bout.length
                    if(seconds.missing + sum(bout.inactivity) <= 2*60) {
                        # If the number of seconds we need to fill in to reach 10 minutes
                        # would not push us over the 10 minute threshold, add in time
                        # from the beginning and then from the end to reach 10 minutes.

                        # Get the current start time and end time of the bout, calculated as above.
                        bout.start <- sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1
                        bout.end <- sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])

                        # Find the number of seconds we can add on to the beginning.
                        # We do not want to overlap with the previous bout.
                        if(length(bout.ends) > 0) {
                            last.bout.end <- bout.ends[length(bout.ends)]
                        } else {
                            last.bout.end <- 0
                        }
                        seconds.to.add <- min(bout.start - last.bout.end + 1, seconds.missing)
                        bout.start <- bout.start - seconds.to.add
                        seconds.missing <- seconds.missing - seconds.to.add

                        # If necessary, add the rest of the time on to the end.
                        # (We know there are "enough" extra seconds at the end because we exceeded the 2 minute threshold
                        # when we tried to add on the whole inactive interval.)
                        if(seconds.missing > 0) {
                            bout.end <- bout.end + seconds.missing
                        }

                        # Add the new bout to the list of bouts.
                        bout.starts <- c(bout.starts, bout.start)
                        bout.ends <- c(bout.ends, bout.end)

                        # add the active time in this bout to the total active time in all bouts
                        total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
                    }
                }

                # Set up to start building the next possible bout.
                possible.bout.active.inds <- possible.bout.active.inds[possible.bout.active.inds.length] + 1
                possible.bout.active.inds.length <- 1

                end.possible.bout <- FALSE

                if(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
                    bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
                }
            }
        }
    }


    return(sum((bout.ends + 1) - bout.starts)/60)

    #	return( c(length(bout.starts), sum((bout.ends + 1) - bout.starts)/60, total.active.time.in.bouts/60) )
}

#	Function to calculate the number of bouts in guideline minutes (bouts of MVPA (>=3 METs) that last at least 10 min. allows for 20% of time below 3 METs i.e. if bout is 10 min long, 2 min can be <3 METs)

num.guideline.bouts.20.percent.below <- function(est.mets)
{
    total.active.time.in.bouts <- 0

    # indices where transitions take place
    mets.length <- length(est.mets)
    one <- est.mets[-mets.length]
    two <- est.mets[-1]

    trans <- c( FALSE, ((one<3)&(two>=3)) | ((one>=3)&(two<3)) )
    trans.inds <- c(1, seq_along(est.mets)[trans], (mets.length+1))

    # how long are the periods of activity and inactivity
    durations <- trans.inds[-1]-trans.inds[-length(trans.inds)]

    # identify if interval is activity or inactivity (they alternate)
    active.interval <- rep(FALSE,length=length(durations))

    if (est.mets[1]<3)
        active.interval <- rep(c(FALSE, TRUE),length=length(durations))
    if (est.mets[1]>=3)
        active.interval <- rep(c(TRUE, FALSE),length=length(durations))


    # Create some empty vectors which will be used to keep track of the
    # start and end points of the bouts in the durations vector.
    bout.starts <- c()
    bout.ends <- c()

    # Create some variables which will be used in constructing the bouts.
    active.inds <- seq_along(durations)[active.interval]
    if(length(active.inds) > 0) {
        possible.bout.active.inds <- 1
        possible.bout.active.inds.length <- 1
        bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
        end.possible.bout <- FALSE

        while(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
            # Determine if adding the next inactive interval to the current possible bout
            # would cause the last 10 minutes of the bout to be more than 20% inactive

            bout.inactivity.with.next.interval <- c(bout.inactivity, rep(1, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 1]))
            secs.to.extract <- min(length(bout.inactivity.with.next.interval), 10*60)
            last.10.min.of.possible.bout <- bout.inactivity.with.next.interval[seq_len(secs.to.extract) + (length(bout.inactivity.with.next.interval) - secs.to.extract)]

            if( sum(last.10.min.of.possible.bout) <= 2*60 ) {
                # If adding the next inactive interval means the last 10 min has <= 2 min of inactivity,
                # add it and the next active interval to the possible-bout.
                bout.inactivity <- c(bout.inactivity.with.next.interval, rep(0, durations[active.inds[possible.bout.active.inds[possible.bout.active.inds.length]] + 2]))
                possible.bout.active.inds <- c(possible.bout.active.inds, possible.bout.active.inds[possible.bout.active.inds.length] + 1)
                possible.bout.active.inds.length <- possible.bout.active.inds.length + 1
                if(possible.bout.active.inds[possible.bout.active.inds.length] == length(active.inds)) {
                    # If we just added the last active interval to the possible bout, end the possible bout.
                    end.possible.bout <- TRUE
                }
            } else {
                # If adding the next inactive interval means the last 10 min has > 2 min of inactivity,
                # stop building this bout.
                end.possible.bout <- TRUE
            }

            if(end.possible.bout) {
                # If the possible bout is long enough, add it to the list of bouts.
                # Reset to start building the next possible bout.

                possible.bout.length <- sum(durations[seq(from=active.inds[possible.bout.active.inds[1]], to=active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])
                if(possible.bout.length >= 10*60) {
                    # If the total duration of the possible bout is >= 10 min, it is a bout.

                    # The start position of the bout is recorded as an index in the input est.mets vector.
                    # This is calculated as the sum of the durations of all intervals before the start of this bout, plus 1
                    bout.starts <- c(bout.starts, sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1)

                    # The end position of the bout is recorded as an index in the input est.mets vector.
                    # This is calculated as the sum of the durations of all intervals up through the end this bout
                    bout.ends <- c(bout.ends, sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])]))

                    # add the active time in this bout to the total active time in all bouts
                    total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
                } else {
                    # If it is < 10 min, see if we can add on some inactive time to the beginning and/or end
                    # to reach the 10 min threshold and make it into a bout.  Otherwise, discard it.

                    # Calculate the number of seconds needed to fill in to reach the 10 minute threshold.
                    seconds.missing <- 10*60 - possible.bout.length
                    if(seconds.missing + sum(bout.inactivity) <= 2*60) {
                        # If the number of seconds we need to fill in to reach 10 minutes
                        # would not push us over the 10 minute threshold, add in time
                        # from the beginning and then from the end to reach 10 minutes.

                        # Get the current start time and end time of the bout, calculated as above.
                        bout.start <- sum(durations[seq_len(active.inds[possible.bout.active.inds[1]] - 1)]) + 1
                        bout.end <- sum(durations[seq_len(active.inds[possible.bout.active.inds[possible.bout.active.inds.length]])])

                        # Find the number of seconds we can add on to the beginning.
                        # We do not want to overlap with the previous bout.
                        if(length(bout.ends) > 0) {
                            last.bout.end <- bout.ends[length(bout.ends)]
                        } else {
                            last.bout.end <- 0
                        }
                        seconds.to.add <- min(bout.start - last.bout.end + 1, seconds.missing)
                        bout.start <- bout.start - seconds.to.add
                        seconds.missing <- seconds.missing - seconds.to.add

                        # If necessary, add the rest of the time on to the end.
                        # (We know there are "enough" extra seconds at the end because we exceeded the 2 minute threshold
                        # when we tried to add on the whole inactive interval.)
                        if(seconds.missing > 0) {
                            bout.end <- bout.end + seconds.missing
                        }

                        # Add the new bout to the list of bouts.
                        bout.starts <- c(bout.starts, bout.start)
                        bout.ends <- c(bout.ends, bout.end)

                        # add the active time in this bout to the total active time in all bouts
                        total.active.time.in.bouts <- total.active.time.in.bouts + length(bout.inactivity) - sum(bout.inactivity)
                    }
                }

                # Set up to start building the next possible bout.
                possible.bout.active.inds <- possible.bout.active.inds[possible.bout.active.inds.length] + 1
                possible.bout.active.inds.length <- 1

                end.possible.bout <- FALSE

                if(possible.bout.active.inds[possible.bout.active.inds.length] < length(active.inds)) {
                    bout.inactivity <- rep(0, durations[active.inds[possible.bout.active.inds]])
                }
            }
        }
    }


    return(length(bout.starts))

    #	return( c(length(bout.starts), sum((bout.ends + 1) - bout.starts)/60, total.active.time.in.bouts/60) )
}

#	Function identifies subjects and visits and loops through appropriate files to produce PA and SB estimates per day for each subject
# directory - path to folder with events files


subjects <- function(out.directory)
{
    on.off.log <-read.csv(paste(out.directory,"DO_LOG_final_ap.csv",sep=""))
    subs <- unique(on.off.log$id)
    return(subs)
}

visits <- function(out.directory)
{
    on.off.log <-read.csv(paste(out.directory,"DO_LOG_final_ap.csv",sep=""))
    visit <- unique(as.character(on.off.log$visit))
    return(visit)
}

#need to find the SECOND (2nd) sunday in March given a year
getMarchdaylight <- function(year)
{
    notSunday <- TRUE
    date <- 7 #second sunday can occur @ the earliest on 3/8
    month <- 3
    while (notSunday)
    {
        date <- date + 1
        day <- as.Date(paste(year,'/',month, '/', date, sep = ''))
        if (weekdays(day) == 'Sunday')
        {
            notSunday <- FALSE
        }
    }
    return (day)
}

#finds the first sunday in November given a year
getNovdaylight <- function(year)
{
    notSunday <- TRUE
    date <- 0
    month <- 11
    while (notSunday)
    {
        date <- date + 1
        day <- as.Date(paste(year,'/',month, '/', date, sep = ''))
        if (weekdays(day) == 'Sunday')
        {
            notSunday <- FALSE
        }
    }
    return (day)
}

#	Function to convert julian time.  Older AP Events files use julian time
#	time is a vector of julian time

# daylights savings happens sec sunday in march, and first sun in nov. summer is +7 (25200), winter is +8(28800) *Note these numbers only work for PST
# takes in boolean, changeTZ, that decides to add the time difference or not.
julian <- function(temp, changeTZ)
{
    temp$time <- as.numeric(temp$time)
    temp$time <- chron(temp$time,origin=c(month=12,day=30,year=1899), format=c("m-d-y","h:m:s"))
    if (changeTZ)
    {
        dates <- dates(temp$time)
        if (length(unique(year(dates))) == 1)
        {
            march <- getMarchdaylight(unique(year(dates)))
            november <- getNovdaylight(unique(year(dates)))
            if ((as.Date(min(dates)) >= march) && (november >= as.Date(max(dates))))
            {
                temp$time <- as.POSIXct(temp$time) + 25200
                temp$time <- strptime(temp$time,format="%Y-%m-%d %H:%M:%S")
            } else if ((as.Date(max(dates)) <= march) || (november <= as.Date(min(dates))))
            {
                temp$time <- as.POSIXct(temp$time) + 28800
                temp$time <- strptime(temp$time,format="%Y-%m-%d %H:%M:%S")
            } else
            {
                times <- vector()
                j <- 1
                repeat
                {
                    march <- getMarchdaylight(year(dates[j]))
                    november <- getNovdaylight(year(dates[j]))
                    if (as.Date(dates[j]) >= march &&  november >= as.Date(dates[j]))
                    {
                        times <- append(times, 25200)
                    } else
                    {
                        times <- append(times, 28800)
                    }
                    j <- j + 1
                    if (j > length(dates))
                    {
                        break
                    }
                }
                temp$time <- as.POSIXct(temp$time) + times
                temp$time <- strptime(temp$time,format="%Y-%m-%d %H:%M:%S")
            }
        } else
        {
            times <- vector()
            j <- 1
            repeat
            {
                march <- getMarchdaylight(year(dates[j]))
                november <- getNovdaylight(year(dates[j]))
                if (as.Date(dates[j]) >= march &&  november >= as.Date(dates[j]))
                {
                    times <- append(times, 25200)
                } else
                {
                    times <- append(times, 28800)
                }
                j <- j + 1
                if (j > length(dates))
                {
                    break
                }
            }
            temp$time <- as.POSIXct(temp$time) + times
            temp$time <- strptime(temp$time,format="%Y-%m-%d %H:%M:%S")
        }
    } else
    {
        temp$time <- as.POSIXct(temp$time)
        temp$time <- strptime(temp$time,format="%Y-%m-%d %H:%M:%S")
    }
}

### function to perform predictions using the thigh data files. combines with groundtruth.
# ap.directory is the location of the thigh raw data files
# out.directory is the location where you want the final file to reside
# crit.directory is where the groundtruth.csv is located (and named this) and DO_log_final.csv is located (and named this)
# changeTZ is a boolean to tell the program to adjust the time from UCT to PST. Preset to True.
# final file is called MergedThighResults_post_ds.csv
thighmodel <- function(ap.directory, out.directory, crit.directory, changeTZ = TRUE)
{
    i<-1
    filelist <- list.files(ap.directory)

    for (i in 1:length(filelist))
    {
        file.name.and.path <- paste(ap.directory,filelist[i],sep='')
        temp <- activ.pal.file.reader(file.name.and.path)
        s <-   substr(filelist[i],4,7)
        id <- s
        num <- substring(id, 3)

        # head(temp)
        # dim(temp)

        t <- dim(temp)[1]

        temp <- temp[!(temp[,"time"] == "1899-12-30"),]
        n <- dim(temp)[1]

        if (is.character(temp$time)==TRUE&t==n)
        {
            temp$time <- julian(temp, changeTZ)
        }

        temp <- second.by.second(temp)
        #data<-second.by.second(temp)

        #	loop to remove label on/off time

        on.off.log <- read.csv(paste0(crit.directory,"DO_log_final.csv"))
        on.off.log$id <- as.character(on.off.log$id)
        on.off.log <- on.off.log[on.off.log$id==s,]

        if (nrow(on.off.log) > 0)
        {
            on.off.log$date2 <-paste(on.off.log$start_month,"/",on.off.log$start_day,"/",on.off.log$start_year,sep="")
            on.off.log$date.time.on <- as.POSIXlt(strptime(paste(on.off.log$date2, on.off.log$start_time), "%m/%d/%Y %H:%M:%S"))
            on.off.log$date.time.off <- as.POSIXlt(strptime(paste(on.off.log$date2, on.off.log$stop_time), "%m/%d/%Y %H:%M:%S"))
            on.off.log$mins.on <- as.vector(difftime(strptime(on.off.log$date.time.off,format="%Y-%m-%s %H:%M:%S"),strptime(on.off.log$date.time.on,format="%Y-%m-%s %H:%M:%S"), units="mins"))

            n.junk <- dim(on.off.log)[1]


            #this isn't really working well.../at all.
            for (j in 1:n.junk)
            {
                #           time after on                           &         time before off
                temp$session[(temp$time>=on.off.log$date.time.on[j])&(temp$time<=on.off.log$date.time.off[j])] <-  j
            }
            # summary(temp$session)

            data<- na.omit(temp)


            if (nrow(data) > 0)
            {
                data$counter <- 1
                data$ID <- id

            # head(data)
            # tail(data)

            # make .csv file with PA and SB variables per daytai

            # results.table <- data.frame(id=s,session=unique(data$session),
            #
            #
            #                             wear.hrs = tapply(data$counter,data$session,sum)/3600,
            #
            #                             sed.mins = tapply(data$ap.posture,data$session,sed.time.AP)*60,
            #                             stand.mins = tapply(data$ap.posture,data$session,stand.time.AP)*60,
            #                             step.mins = tapply(data$ap.posture,data$session,step.time.AP)*60,
            #
            #                             lit.mins = tapply(data$mets,data$session,lit.time.AP)*60,
            #                             mvpa.mins = tapply(data$mets,data$session,mvpa.time.AP)*60,
            #
            #                             breaks = tapply(data$ap.posture,data$session,breaks.AP),
            #                             break.rate = tapply(data$ap.posture,data$session,breaks.AP)/(tapply(data$ap.posture,data$session,sed.time.AP)),
            #
            #                             guideline.minutes = tapply(data$mets,data$session,tot.time.in.guideline.bouts.20.percent.below),
            #                             num.guideline.bouts = tapply(data$mets,data$session,num.guideline.bouts.20.percent.below),
            #
            #                             min.in.sed.30 = tapply(data$ap.posture,data$session,AP.compute.min.bouts.n.min.at.sed,n=30),
            #                             min.in.sed.60 = tapply(data$ap.posture,data$session,AP.compute.min.bouts.n.min.at.sed,n=60),
            #
            #                             num.bouts.in.sed.30 = tapply(data$ap.posture,data$session,AP.compute.num.bouts.n.min.at.sed,n=30),
            #                             num.bouts.in.sed.60 = tapply(data$ap.posture,data$session,AP.compute.num.bouts.n.min.at.sed,n=60))

                data <- data %>%
                  mutate(
                    ap.posture.cat = case_when(
                      ap.posture == 0.0 ~ "sedentary",
                      ap.posture == 1.0 ~ "standing",
                      ap.posture == 2.0 ~ "stepping",
                      ap.posture == 2.1 ~ "cycling",
                      ap.posture == 3.1 ~ "primary lying",
                      ap.posture == 3.2 ~ "secondary lying",
                      ap.posture == 4.0 ~ "non-wear",
                      ap.posture == 5.0 ~ "travelling",

                      TRUE ~ NA_character_
                    ),
                    ap_intensity = case_when(
                      ap.posture %in% c(0.0, 3.1, 3.2, 5.0) ~ "sedentary",
                      ap.posture %in% c(1.0, 2.0, 2.1) & mets < 3 ~ "light",
                      ap.posture %in% c(1.0, 2.0, 2.1) & mets >= 3 & mets < 6 ~ "moderate",
                      ap.posture %in% c(1.0, 2.0, 2.1) & mets >= 6 ~ "vigorous",
                      ap.posture %in% c(4.0) & mets >= 6 ~ "nonwear",

                      TRUE ~ NA_character_
                    )
                  )


                if (i==1){
                    # write.csv(data, file=paste(out.directory,"ap_do_output.csv", sep=""), row.names=F, append=F)
                    results <- data
                } else if (i>1) {
                    # write.csv(data, file=paste(out.directory,"ap_do_output.csv", sep=""), row.names=F, append=T, col.names=F)
                    results <- results %>%
                        bind_rows(data)
                }
            }

            print(i)
        }
    }

    # save.image("./thigh_data/StackedThighResults.RData")
    #load("thigh_data/StackedThighResults.RData")


    #### Merge Results with Ground Truth ####

    # subresults <- results %>%
    #     select(day, time, METs.rf, METs.lm, MET.lev.rf, MET.lev.tr, sed.rf, sed.tr, loc.rf, loc.tr, combo.rf, combo.tr, id)

    results2 <- results %>%
        rename(
            datetime = time,
            id = ID
        ) %>%
        mutate(
            hour = paste(hour(datetime), minute(datetime), second(datetime), sep = ":"),
            time = as.character(hms(hour)),
            day = mdy(paste(month(datetime), day(datetime), year(datetime), sep = "/"))
        ) %>%
        select(day, time, ap.posture, mets, session, counter, id, ap.posture.cat, ap_intensity)

    # Read in ground truth
    setwd(crit.directory)
    groundt <- read_csv("groundtruth.csv")

    # Read in DO log
    do_log <- read_csv("DO_log_final.csv")
    do_log <- do_log %>%
        select(id, obs, type) %>%
        rename(DO_session = obs) %>%
        mutate(DO_session = substr(DO_session, 1, 3)) %>%
        distinct()


    subgroundt <- groundt %>%
        select(`Unnamed: 0`, observation, date, coding, primary_behavior, primary_posture, primary_upperbody,
               primary_intensity, secondary_behavior, secondary_posture, secondary_upperbody, secondary_intensity,
               num_postures, transition, actual_time, time, posture_coding, type, start.time, broad_activity, detailed_activity,
               walking_running_bouts, updated_activity) %>%
        mutate(
            time = as.character(hms(actual_time)),
            day = mdy(date),
            id = substr(observation, 1, 4),
            DO_session = substr(observation, 5, 7))

    merged <- results2 %>%
        inner_join(subgroundt, by = c("id", "day", "time")) %>%
        select(-date) %>%
        inner_join(do_log, by = c("id", "DO_session")) %>%
        rename(DO_log_type = type.y)

    write_csv(merged, paste0(out.directory, "MergedThighResults_post_ds.csv"))
}