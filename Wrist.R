library(tree)
library(randomForest)
library(stringi)
library(data.table)
library(tidyverse)
library(lubridate)
library(GGIR)
library(TLBC)

read.act <- function(file.name.and.path)
{
   f <- file(file.name.and.path)
   head.data <- readLines(f,n=10)

   start.time <- head.data[3]
   start.time <- (strsplit(start.time,split=" ")[[1]][3])
   start.date <- head.data[4]
   start.date <- (strsplit(start.date,split=" ")[[1]][3])
   start.time <- as.POSIXlt(strptime(paste(start.date,start.time),"%m/%d/%Y %H:%M:%S"))


   data <- read.csv(file.name.and.path, skip = 10, header = T)
   n <- dim(data)[1] # number of rows in data
   full.data <-
     data.frame(time = start.time + (0:(n-1))/80,
                data)

   names(full.data) = c('time', 'V1', 'V2', 'V3')
   return(full.data)
}

pow.625 <- function(vm)
{
  mods <- Mod(fft(vm))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  inds <- (1:n)[(freq>0.6)&(freq<2.5)]
  pow625 <- sum(mods[inds])/sum(mods) #% of mods in inds
  mods[is.na(mods)] <- 0
  if (sd(vm)==0)
    pow625 <- 0
  return(pow625)
}

dom.freq <- function(vm)
{
  if(length(vm)==1)
    return(NA)
  mods <- Mod(fft(vm))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  dom.ind <- which.max(mods) #index of max
  d.f <- as.vector(freq[which.max(mods)]) #get freq at index of max of mods
  return(d.f)
}

frac.pow.dom.freq <- function(vm)
{
  mods <- Mod(fft(vm))
  mods <- mods[-1]
  n <- length(mods)
  n <- floor(n/2)
  freq <- 80*(1:n)/(2*n)
  mods <- mods[1:n]
  rat <- max(mods)/sum(mods) #% of sum that is in max of mods
  mods[is.na(mods)] <- 0
  if (sd(vm)==0)
    rat <- 0
  return(rat)

}

daysums <- function(ag.directory, out.directory, models.directory)
{
  load(paste(models.directory, 'mods.RData', sep = ''))

  win.width <- 15 # width of "windows" to analyze in seconds
  filelist <- list.files(path=ag.directory)


  #n.sub <- length(filelist)
  iii<-1

  for (iii in 1:length(filelist))
  {

    sub <- substr(filelist[iii],4,7)
    filename <- paste(ag.directory,filelist[iii],sep='')

    #sub.info <- subset(start.stop,start.stop$id==sub)

    print(Sys.time())
    print(iii)

    #start <- start.stop$on.times[1]
    #start <- strptime(start,format= "%m/%d/%Y %H:%M:%S")

    #stop <- start.stop$off.times[2]
    #stop <- strptime(stop,format= "%m/%d/%Y %H:%M:%S")

    #sub.info$on.times<-	strptime(sub.info$on.times,format="%m/%d/%Y %H:%M:%S")
    #sub.info$off.times<-	strptime(sub.info$off.times, format="%m/%d/%Y %H:%M:%S")

    ag.data <- read.act(filename)
    n <- dim(ag.data)[1]

    head(ag.data)
    tail(ag.data)

    mins <- ceiling(n/(80*win.width))
    ag.data$min <- rep(1:mins,each=win.width*80)[1:n]
    ag.data$vm <- sqrt(ag.data$V1^2+ag.data$V2^2+ag.data$V3^2)
    ag.data$v.ang <- 90*(asin(ag.data$V1/ag.data$vm)/(pi/2))
    ag.data.sum <- data.frame(mean.vm=tapply(ag.data$vm,ag.data$min,mean,na.rm=T),
                              sd.vm=tapply(ag.data$vm,ag.data$min,sd,na.rm=T),
                              mean.ang=tapply(ag.data$v.ang,ag.data$min,mean,na.rm=T),
                              sd.ang=tapply(ag.data$v.ang,ag.data$min,sd,na.rm=T),
                              p625=tapply(ag.data$vm,ag.data$min,pow.625),
                              dfreq=tapply(ag.data$vm,ag.data$min,dom.freq),
                              ratio.df=tapply(ag.data$vm,ag.data$min,frac.pow.dom.freq))

    ag.data.sum$start.time <- as.POSIXct(tapply(ag.data$time,ag.data$min,min,na.rm=T),origin="1970-01-01 00:00.00 UTC")


    head(ag.data.sum)
    # applies models from JAP paper
    # ag.data.sum has estimaes over time
    ag.data.sum$METs.rf <- predict(rf.met.model,newdata=ag.data.sum)
    ag.data.sum$METs.lm <- predict(lm.met.model,newdata=ag.data.sum)
    ag.data.sum$METs.rf[ag.data.sum$sd.vm==0] <- 1
    ag.data.sum$METs.lm[ag.data.sum$sd.vm==0] <- 1

    ag.data.sum$MET.lev.rf <- predict(rf.met.level.model,newdata=ag.data.sum)
    ag.data.sum$MET.lev.tr <- predict(tr.met.level.model,newdata=ag.data.sum,type="class")
    ag.data.sum$sed.rf <- predict(rf.sed.model,newdata=ag.data.sum)
    ag.data.sum$sed.tr <- predict(tr.sed.model,newdata=ag.data.sum,type="class")
    ag.data.sum$loc.rf <- predict(rf.loc.model,newdata=ag.data.sum)
    ag.data.sum$loc.tr <- predict(tr.loc.model,newdata=ag.data.sum,type="class")
    ag.data.sum$combo.rf <- predict(rf.combo.model,newdata=ag.data.sum)
    ag.data.sum$combo.tr <- predict(tr.combo.model,newdata=ag.data.sum,type="class")


    head(ag.data.sum)

    # daily summaries
    ag.data.sum$day <- as.POSIXct(trunc(ag.data.sum$start.time,units="day"))
    ag.data.sum$hour <- as.POSIXct(trunc(ag.data.sum$start.time,units="hour"))

    #ag.data.sum$id <- sub

    ag.data.sum <- ag.data.sum %>%
      mutate(
        day = paste(month(start.time), day(start.time), year(start.time), sep = "/"),
        hour = paste(hour(start.time), minute(start.time), second(start.time), sep = ":"),
        id = sub
      )

    ag.data.sum.sec <- ag.data.sum %>%
      slice(rep(1:n(), each = win.width)) %>%
      mutate(
        time = as.character(hms(hour) + 0:(win.width-1)),
        day = mdy(day)
      )

    if (iii == 1) {
      results <- ag.data.sum.sec
    } else {
      results <- rbind(results, ag.data.sum.sec)
    }

  }
  return(results)
}

### function to perform predictions using the wrist data files. combines with groundtruth.
# ap.directory is the location of the wrist raw data files
# out.directory is the location where you want the final file to reside
# crit.directory is where the groundtruth.csv is located (and named this), mods.RData (and named this) and DO_log_final.csv is located (and named this).
# final file is called MergedResults_wrist_sedsphere_20190710.csv
wristmodel <- function(ag.directory, out.directory, crit.directory)
{
  load(paste0(crit.directory, "mods.RData"))
  datahz <- 80

  win.width <- 15 # width of "windows" to analyze in seconds
  filelist <- list.files(ag.directory)
  n.sub <- length(filelist)
  i<- 1
  for (i in 1:length(filelist))
  {
    sub <- substr(filelist[i],4,7)
    filename <- paste(ag.directory,filelist[i],sep='')

    print(Sys.time())
    print(i)

    ag.data <- read.act(filename)

    n <- nrow(ag.data)

    mins <- ceiling(n/(datahz*win.width))
    ag.data <- ag.data %>%
      mutate(
        min = rep(1:mins,each=win.width*datahz)[1:n],
        vm = sqrt(V1^2 + V2^2 + V3^2),
        v.ang = 90*(asin(V1/vm)/(pi/2)),
        VMcorrG = abs(sqrt(V1^2 + V2^2 + V3^2)-1)
      )


    ag_data_raw_wrist.sum <- data.frame(mean.x=tapply(ag.data$V1,ag.data$min,mean,na.rm=T),
                                        mean.y=tapply(ag.data$V2,ag.data$min,mean,na.rm=T),
                                        mean.z=tapply(ag.data$V3,ag.data$min,mean,na.rm=T),
                                        mean.vm = tapply(ag.data$vm,ag.data$min,mean,na.rm=T),
                                        sd.vm = tapply(ag.data$vm,ag.data$min,sd,na.rm=T),
                                        p625 =  tapply(ag.data$vm,ag.data$min,pow.625),
                                        dfreq =  tapply(ag.data$vm,ag.data$min,dom.freq),
                                        ratio.df =  tapply(ag.data$vm,ag.data$min,frac.pow.dom.freq),
                                        sum.VMcorrG = tapply(ag.data$VMcorrG,ag.data$min,sum,na.rm=T),
                                        sum.vm = tapply(ag.data$VM, ag.data$min, sum, na.rm = T),
                                        start.time = as.POSIXct(tapply(ag.data$time,ag.data$min,min,na.rm=T),origin="1970-01-01 00:00.00 UTC")
                                     )

    VMcorrG_mod_15s = 489

    ag_data_raw_wrist.sum$v.ang <- ifelse(ag_data_raw_wrist.sum$mean.y > 1, asin(1)*180/pi,
                                          ifelse(ag_data_raw_wrist.sum$mean.y < -1, asin(-1)*180/pi,
                                                 asin(pmin(pmax(ag_data_raw_wrist.sum$mean.y,-1.0),1.0))*180/pi))

    # 0 = Sedentary, 1 = Standing, 2 = Activity
    ag_data_raw_wrist.sum$sedsphere = ifelse(ag_data_raw_wrist.sum$sum.VMcorrG > VMcorrG_mod_15s,2,
                                             ifelse(ag_data_raw_wrist.sum$v.ang < -15,1,0))

    sedsphere = rep(ag_data_raw_wrist.sum$sedsphere, each = win.width)

    ag_data_raw_wrist.sum$v.ang2 <- acos(ag_data_raw_wrist.sum$mean.y/ag_data_raw_wrist.sum$mean.vm)*180/pi

    # 0 = Sedentary, 1 = Standing, 2 = Activity
    ag_data_raw_wrist.sum$sedsphere2 = ifelse(ag_data_raw_wrist.sum$sum.VMcorrG > VMcorrG_mod_15s,2,
                                              ifelse(ag_data_raw_wrist.sum$v.ang2 < 105,1,0))

    sedsphere2 = rep(ag_data_raw_wrist.sum$sedsphere2, each = win.width)


    head(ag_data_raw_wrist.sum)

    #get this into merged, met.rd, mets
    ag.data.sum <- ag_data_raw_wrist.sum %>%
      mutate(
        day = paste(month(start.time), day(start.time), year(start.time), sep = "/"),
        hour = paste(hour(start.time), minute(start.time), second(start.time), sep = ":"),
        id = sub
      )

    # Expand min rows to seconds rows -- min is epoch, epochs are win.width seconds long
    ag.data.sum.sec <- ag.data.sum %>%
      slice(rep(1:n(), each = win.width)) %>%
      mutate(
          time = as.character(hms(hour) + 0:(win.width-1)),
          day = mdy(day)
      )

    # Append Results
    if (i == 1) {
      results <- ag.data.sum.sec
    } else {
      results <- results %>%
        bind_rows(ag.data.sum.sec)
    }
  }

  day.sum <- daysums(ag.directory, out.directory, crit.directory)
  day.sum$day <- as.Date(day.sum$day)

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
      select(observation, date, coding, primary_behavior, primary_posture, primary_upperbody,
             primary_intensity, secondary_behavior, secondary_posture, secondary_upperbody, secondary_intensity,
             num_postures, transition, actual_time, time, posture_coding, type, start.time, broad_activity, detailed_activity,
             walking_running_bouts, updated_activity) %>%
      mutate(
          time = as.character(hms(actual_time)),
          day = mdy(date),
          id = substr(observation, 1, 4),
          DO_session = substr(observation, 5, 7))

  merged <- results %>%
      select(day, time, start.time, id, hour, sedsphere, sedsphere2) %>%
      inner_join(subgroundt, by = c("id", "day", "time")) %>%
      select(-date) %>%
      inner_join(do_log, by = c("id", "DO_session")) %>%
      rename(DO_log_type = type.y)

  merged <- merge(merged, day.sum)

  write_csv(merged, paste0(out.directory, "MergedResults_wrist_sedsphere_20190710.csv"))
}