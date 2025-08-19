# R script to process BART task data

# initial setup ----
rm(list=ls())
library(dplyr)


filePath <- "/work/2025 Bangor - MF decision making"
for (diskLbl in paste(letters, ":", sep="")) {
  mainFolder <- paste(diskLbl, filePath, sep="")
  if (file.exists(mainFolder)) {break}
}
if (!file.exists(mainFolder)) {
  warning('I cannot find the main folder')
} else {
  print(paste("working in unit", diskLbl))
  setwd(mainFolder)
  print("setting the folder path")
}

# get list of files (one file = one participant)
folderPath = paste(mainFolder, "data_raw",sep="/")
fileList = list.files(path = folderPath,
                      pattern = ".csv")
isBART = grepl('bart', fileList, fixed = TRUE)
fileList = fileList[isBART]


# open each dataset, extract metrics, store data ----

# recording-level dataset (long format)
recordingLvl_long = data.frame()  # initialize dataframe
rowCounter_long = 0 # initialize row counter
for (fileIdx in 1:length(fileList)) {
  rowCounter_long = rowCounter_long + 1
  
  # import data
  filePath = paste(folderPath,fileList[fileIdx],sep="/")
  recordingInfo = read.csv(filePath, header = TRUE, sep = ",",
                           dec = ".", fill = TRUE)
  
  # remove rows at the bottom if they show NA
  rows2keep = !is.na(recordingInfo$trials.thisTrialN)
  recordingInfo = recordingInfo[rows2keep,]
  
  # number of balloons presented in this session
  temp = recordingInfo$trials.thisN;
  nBalloons = temp[length(temp)]+1; # because Python counts from 0
  
  # number of balloons popped and not popped
  temp = as.logical(recordingInfo$popped)
  nBalloons_popped    = sum(temp)
  nBalloons_nonpopped = sum(temp==FALSE)
  if (nBalloons_popped+nBalloons_nonpopped!=nBalloons) {
    warning('number of popped and unpopped balloons does not match the total number of balloons')
  }
  
  # number of pumps
  temp = recordingInfo$nPumps_done
  nPumps = sum(temp) # total number of pumps in this session
  nPumps_popped    = sum(recordingInfo$nPumps_done[as.logical(recordingInfo$popped)]) # number of pumps for popped balloons
  nPumps_nonpopped = sum(recordingInfo$nPumps_done[!as.logical(recordingInfo$popped)]) # number of pumps for unpopped balloons
  if (nPumps_popped+nPumps_nonpopped!=nPumps) {
    warning('number of pumps in popped and unpopped balloons does not match the number of pumps of all balloons')
  }
  
  # average number of pumps
  MeanPumps           = nPumps/nBalloons
  MeanPumps_popped    = nPumps_popped/nBalloons_popped
  MeanPumps_nonpopped = nPumps_nonpopped/nBalloons_nonpopped
  
  # time in between consecutive pumps ("l" was the key pressed to pump the balloon)
  lKey_times = NA
  for (trialIdx in 1:dim(recordingInfo)[1]) {
    # within each trial, find the responses with 'l' key 
    temp = recordingInfo$key_resp.keys[trialIdx]
    keypresses = regmatches(temp,gregexpr('[al]', temp))[[1]]
    keypresses_lKey = grepl('l',keypresses)
    
    # times associated with each keypress
    temp = recordingInfo$key_resp.rt[trialIdx]
    keypressesRT = as.numeric( strsplit(gsub("\\]","",gsub("\\[","",temp)),", ")[[1]] )
    
    lKey_times[trialIdx] = median(diff(keypressesRT[keypresses_lKey]))
  }
  lKey_MdnTimes           = median(lKey_times, na.rm=TRUE)
  lKey_MdnTimes_popped    = median(lKey_times[as.logical(recordingInfo$popped)], na.rm=TRUE)
  lKey_MdnTimes_nonpopped = median(lKey_times[!as.logical(recordingInfo$popped)], na.rm=TRUE)
  
  # BART score
  BARTscore = recordingInfo$earnings[dim(recordingInfo)[1]];
  
  # store in a long format dataset (one row per file, even when multiple files are recorded per participant)
  recordingLvl_long[rowCounter_long,"recording"]       = fileList[fileIdx]
  recordingLvl_long[rowCounter_long,"nBalloons"]           = nBalloons
  recordingLvl_long[rowCounter_long,"nBalloons_popped"]    = nBalloons_popped
  recordingLvl_long[rowCounter_long,"nBalloons_nonpopped"] = nBalloons_nonpopped
  recordingLvl_long[rowCounter_long,"nPumps"]           = nPumps
  recordingLvl_long[rowCounter_long,"nPumps_popped"]    = nPumps_popped
  recordingLvl_long[rowCounter_long,"nPumps_nonpopped"] = nPumps_nonpopped
  recordingLvl_long[rowCounter_long,"MeanPumps"]           = MeanPumps
  recordingLvl_long[rowCounter_long,"MeanPumps_popped"]    = MeanPumps_popped
  recordingLvl_long[rowCounter_long,"MeanPumps_nonpopped"] = MeanPumps_nonpopped
  recordingLvl_long[rowCounter_long,"lKey_MdnTimes"]           = lKey_MdnTimes
  recordingLvl_long[rowCounter_long,"lKey_MdnTimes_popped"]    = lKey_MdnTimes_popped
  recordingLvl_long[rowCounter_long,"lKey_MdnTimes_nonpopped"] = lKey_MdnTimes_nonpopped
  recordingLvl_long[rowCounter_long,"BARTscore"] = BARTscore
}

# store 
filePath = paste(mainFolder,"data_processing",sep="/")
fileName = paste("BART_long",".csv",sep="")
write.csv(recordingLvl_long,paste(filePath,fileName,sep="/"), row.names = FALSE)



# trial-level dataset (long format)
trialLvl_long = data.frame()  # initialize dataframe
rowCounter_long = 0 # initialize row counter
for (fileIdx in 1:length(fileList)) {
  
  # import data
  filePath = paste(folderPath,fileList[fileIdx],sep="/")
  recordingInfo = read.csv(filePath, header = TRUE, sep = ",",
                           dec = ".", fill = TRUE)
  
  # remove rows at the bottom if they show NA
  rows2keep = !is.na(recordingInfo$trials.thisTrialN)
  recordingInfo = recordingInfo[rows2keep,]
  
  for (trialIdx in 1:dim(recordingInfo)[1]) {
    # update row counter
    rowCounter_long = rowCounter_long + 1  
    
    # popped or nonpopped
    popped = recordingInfo[trialIdx,"popped"]
    
    # number of pumps
    nPumps = recordingInfo[trialIdx,"nPumps_done"]
    
    # time in between consecutive pumps
    # within each trial, find the responses with 'l' key 
    temp = recordingInfo$key_resp.keys[trialIdx]
    keypresses = regmatches(temp,gregexpr('[al]', temp))[[1]]
    keypresses_lKey = grepl('l',keypresses)
    # times associated with each keypress
    temp = recordingInfo$key_resp.rt[trialIdx]
    keypressesRT = as.numeric( strsplit(gsub("\\]","",gsub("\\[","",temp)),", ")[[1]] )
    lKey_times = median(diff(keypressesRT[keypresses_lKey]))
    
    # store in a long format dataset (one row per trial)
    trialLvl_long[rowCounter_long,"recording"]    = fileList[fileIdx]
    trialLvl_long[rowCounter_long,"popped"]       = popped
    trialLvl_long[rowCounter_long,"nPumps"]       = nPumps
    trialLvl_long[rowCounter_long,"lKey_times"]       = lKey_times
  }
}

# store 
filePath = paste(mainFolder,"data_processing",sep="/")
fileName = paste("BART_trialLvl_long_raw",".csv",sep="")
write.csv(trialLvl_long,paste(filePath,fileName,sep="/"), row.names = FALSE)

