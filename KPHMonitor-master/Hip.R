library(PhysicalActivity)
library(nnet)
library(accelerometry)
library(tidyverse)
library(Sojourn)
library(Sojourn.Data)

# Classifies intensity given data and a set of cutpoints
get.intensity <- function(data, cutpoints=c(1.5, 3, 6))
{
  ifelse(data < cutpoints[1],
         "sed",
         ifelse(data < cutpoints[2],
                "light",
                ifelse(data < cutpoints[3],
                       "mod",
                       "vig")))
}

# calculates the coefficient of variation of a given vector
#takes in vector
#what is the 'coef of a variation'
coef.of.var <- function(x) {
  if(mean(x, na.rm=T) == 0) 0
  else 100 * ( sd(x, na.rm=T) / mean(x, na.rm=T) )
}

# This returns the MET estimated by Crouter et al. (2006)
crout.2006 <- function(counts) {
  n <- length(counts)

  # calculate coefficients of variation
  data <- data.frame(
    counts=counts,
    minute=rep(1:ceiling(n/60), each=60, length=n),
    ten.sec=rep(1:ceiling(n/10), each=10, length=n)
  ) %>%
    # append minute-level and 10s-level counts
    group_by(minute) %>% mutate(cpm=sum(counts)) %>% ungroup() %>%
    group_by(ten.sec) %>% mutate(cpts=sum(counts)) %>% ungroup() %>%
    # append CV of the 1s-level and 10s-level counts, per minute
    group_by(minute) %>% mutate(cvs=coef.of.var(cpts)) %>% ungroup()

  # calculate METs and type (locomotive, lifestyle, or sitting)
  return(
    data %>% mutate(
      METs=ifelse(
        cpm <= 50,
        1,
        ifelse((cvs > 0) & (cvs <= 10),
               2.379833 * exp(0.00013529 * cpm),
               2.330519 + 0.001646*cpm - 1.2017e-7*cpm^2 + 3.3779e-12*cpm^3)
      ),
      type=ifelse(
        cpm <= 50,
        "sit",
        ifelse((cvs > 0) & (cvs <= 10),
               "loc",
               "life")
      )
    )
  )
}

# This returns the MET estimated by Crouter et al. (2010)
crout.2010 <- function(counts) {
  n <- length(counts)
  data <- data.frame(
    counts=counts,
    ten.sec=rep(1:ceiling(n/10), each=10, length=n)
  )

  # get the counts per 10s
  data.ts <- data %>%
    group_by(ten.sec) %>%
    summarize(cpts=sum(counts)) %>%
    # calculate leads and lags (for next part)
    mutate(
      lead5=lead(cpts, 5),
      lead4=lead(cpts, 4),
      lead3=lead(cpts, 3),
      lead2=lead(cpts, 2),
      lead1=lead(cpts, 1),
      lag0=lag(cpts, 0),
      lag1=lag(cpts, 1),
      lag2=lag(cpts, 2),
      lag3=lag(cpts, 3),
      lag4=lag(cpts, 4),
      lag5=lag(cpts, 5)
    )

  # calculate minimum CV of the 10-second counts over 60-second windows
  data.ts <- data.ts %>%
    mutate(
      win1=apply(select(data.ts, lead5:lag0), 1, coef.of.var),
      win2=apply(select(data.ts, lead4:lag1), 1, coef.of.var),
      win3=apply(select(data.ts, lead3:lag2), 1, coef.of.var),
      win4=apply(select(data.ts, lead2:lag3), 1, coef.of.var),
      win5=apply(select(data.ts, lead1:lag4), 1, coef.of.var),
      win6=apply(select(data.ts, lag0:lag5), 1, coef.of.var)
    ) %>%
    rowwise() %>%
    mutate(cvs=min(c(win1, win2, win3, win4, win5, win6)))

  # join back to the original data
  data <- inner_join(data, data.ts)

  # calculate METs and type (locomotive, lifestyle, or sitting)
  return(
    data %>% mutate(
      METs=ifelse(
        cpts <= 8,
        1,
        ifelse(cvs <= 10,
               2.294275 * exp(0.00084679 * cpts),
               0.749395 + (0.716431 * log(cpts)) - (0.179874 * log(cpts)^2) + (0.033173 * log(cpts)^3))
      ),
      type=ifelse(
        cpts <= 8,
        "sit",
        ifelse((cvs > 0) & (cvs <= 10),
               "loc",
               "life")
      )
    )
  )
}

#read in csv files. predict with models. write to csv
# ag.dir: the directory path for the 'in' (raw data) hip csv files
# out.dir: the directory path for the 'out' (processed data) hip csv files
# final file will be named MergedHipResults.csv
hipmodel <- function(ag.dir, out.dir)
{
  first <- TRUE
  filelist <- list.files(ag.dir)
  for (file in filelist)
  {
    # get path to this file and ID
    path.to.file <- paste0(ag.dir, file)
    id <- substr(file, 4, 7)

    # read in second-level data
    data <- read_csv(path.to.file, skip=10) %>%
      select(
        time=timestamp,
        counts=axis1, axis2, axis3,
        steps, vm=vectormagnitude, lux
      ) %>%
      mutate(id=id)

    ## SECOND-LEVEL METHODS
    data <- data %>% mutate(
      soj.1x=get.intensity(soj_1x_original(counts)$METs),
      soj.3x=get.intensity(soj_3x_original(counts, axis2, axis3, vm)$METs),
      crout06=get.intensity(crout.2006(counts)$METs),
      crout10=get.intensity(crout.2010(counts)$METs)
    )

    ## MINUTE-LEVEL METHODS
    data$minute <- as.character(
      as.POSIXct(data$time,
                 format='%Y-%m-%d %H:%M:%S', tz='America/Los_Angeles', usetz=T),
      format='%Y-%m-%d %H:%M'
    )
    results.minute <- data %>%
      group_by(minute) %>%
      summarize(
        counts=sum(counts),
        vm=sum(vm)
      ) %>%
      mutate(
        freedson=get.intensity(counts, cutpoints=c(100, 1952, 5725)),
        sasaki=get.intensity(vm, cutpoints=c(200, 2691, 6167)),
        nhanes=get.intensity(counts, cutpoints=c(100, 2020, 5999))
      ) %>% select(-counts, -vm)

    # join the second-level results to the minute-level results
    if (first)
    {
      results <- data %>% inner_join(results.minute, by="minute") %>% select(-minute, -(counts:lux))
      first <- FALSE

    } else
    {
      results2 <- data %>% inner_join(results.minute, by="minute") %>% select(-minute, -(counts:lux))
      results <- rbind(results, results2)
    }
  }
  write_csv(results, paste0(out.dir, 'MergedHipResults.csv'))
}