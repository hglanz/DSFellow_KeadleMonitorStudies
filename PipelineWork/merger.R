library(tidyverse)
library(mefa)
library(sqldf)

### Function to merge all 3 out files into 1 files with groundtruth
# out(thigh, wrist, hip) show R where the outputs of those files come from
# groundtruth is the full location of the groundtruth (directory and filename)
# out is the where you want the reslts to be. insert directory and filename
# final file will be named ResultsMerged1sec.csv
merger <- function(outthigh.directory, outwrist.directory, outhip.directory, out.directory)
{
  wrist <- read_csv(paste(outwrist.directory, 'MergedResults_wrist_sedsphere_20190710.csv', sep = ''))
  thigh <- read_csv(paste(outthigh.directory, 'MergedThighResults_post_ds.csv', sep = ''))
  hip <- hip2hip <- read_csv(paste(outhip.directory, 'MergedHipResults.csv', sep = ''))

  #merging
  wrist$start.time <- wrist$start.time.y
  wrist <- select(wrist, -'start.time.y')
  thighwrist <- merge(thigh, wrist, by = c('day','id','time','observation','coding','primary_behavior','primary_posture','primary_upperbody','primary_intensity','secondary_behavior','secondary_posture','secondary_upperbody','secondary_intensity','num_postures','transition','actual_time','posture_coding','type.x','broad_activity','detailed_activity','updated_activity','DO_session','DO_log_type','start.time'))
  hip$day <- as.Date.POSIXct(hip$time)
  hip$actual_time <- format(hip$time, format = '%H:%M:%S')
  hip <- select(hip, -'time')
  thighwrist$actual_time <- as.character(thighwrist$actual_time)
  hip <- hip[hip$actual_time %in% unique(thighwrist$actual_time),]
  merged <- merge(thighwrist, hip)

  cols <- c("coding", "primary_intensity", "secondary_intensity", "ap_intensity", "ap.posture.cat", "soj.1x", "soj.3x", "crout06", "crout10", "freedson","sasaki", "nhanes", "MET.lev.rf", "MET.lev.tr", "sed.rf", "sed.tr")
  for (col in cols)
  {
    merged[,col][merged[,col]=='non-sedentary'] <- 'non-sed'
    merged[,col][merged[,col]=='sedentary'] <- 'sed'
    merged[,col][merged[,col]=='moderate'] <- 'mod'
    merged[,col][merged[,col]=='vigorous'] <- 'vig'
  }

  write.csv(merged, file = paste(out.directory, 'ResultsMerged1sec.csv', sep = ''))
}

### This function creates a new file that creates a csv file with a count for each number of level in each combination of id, predictor, and session
# in.directory is where ResultsMerged1sec.csv is located
# out.directory is where you want the summary file to end up
# final file will be called PredictorSummary.csv
summary <- function(in.directory, out.directory)
{
  merged <- read.csv(paste(in.directory, 'ResultsMerged1sec.csv', sep = ''))
  predictors <- c("coding", "primary_intensity", "secondary_intensity", "ap_intensity", "soj1x", "soj3x", "crout06", "crout10", "freedson","sasaki", "nhanes", "METlevrf", "METlevtr", "sedrf", "sedtr")
  merged$soj1x <- merged$soj.1x
  merged$soj3x <- merged$soj.3x
  merged$METlevrf <- merged$MET.lev.rf
  merged$METlevtr <- merged$MET.lev.tr
  merged$sedrf <- merged$sed.rf
  merged$sedtr <- merged$sed.tr
  first <- TRUE

  for (pred in predictors)
  {
    for (level in unique(merged[,pred]))
    {
      sqlstatement <- paste("SELECT id, session, COUNT(", pred,") AS '", pred,":",level,"' FROM merged WHERE ", pred," = '", level,"' GROUP BY id, session", sep ='')
      temp <- sqldf(sqlstatement)
      if (first)
      {
        summary <- temp
        first <- FALSE
      } else
      {
        summary <- merge(summary, temp, all = TRUE)
      }
    }
  }
  summary[is.na(summary)] <- 0
  write.csv(summary, file = paste(out.directory, 'PredictorSummary.csv', sep = ''))
}

wristmodel("C:/Users/jackl/Desktop/KPHMonitor/data/wrist/", "C:/Users/jackl/Desktop/KPHMonitor/processed/wrist/","C:/Users/jackl/Desktop/KPHMonitor/processed/")
merger("C:/Users/jackl/Desktop/KPHMonitor/processed/thigh/", "C:/Users/jackl/Desktop/KPHMonitor/processed/wrist/","C:/Users/jackl/Desktop/KPHMonitor/processed/hip/","C:/Users/jackl/Desktop/KPHMonitor/processed/")
summary("C:/Users/jackl/Desktop/KPHMonitor/processed/","C:/Users/jackl/Desktop/KPHMonitor/processed/")
