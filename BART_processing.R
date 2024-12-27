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
  nBalloonsPopped    = sum(temp)
  nBalloonsNotPopped = sum(temp==FALSE)
  
  # number of pumps
  temp = recordingInfo$nPumps_done
  nPumpsTot = sum(temp) # total number of pumps in this session
  nPumpsAdj = sum(recordingInfo$nPumps_done[!as.logical(recordingInfo$popped)]) # adjusted to consider only the pumps done for balloons that did not pop
  
  # time in between consecutive pumps
  keypressesRT_lKey_Tot = NA
  for (trialIdx in 1:dim(recordingInfo)[1]) {
    temp = recordingInfo$key_resp.keys[trialIdx]
    keypresses = regmatches(temp,gregexpr('[al]', temp))[[1]]
    keypresses_lKey = grepl('l',keypresses)
    
    temp = recordingInfo$key_resp.rt[trialIdx]
    keypressesRT = as.numeric( strsplit(gsub("\\]","",gsub("\\[","",temp)),", ")[[1]] )
    
    keypressesRT_lKey_Tot[trialIdx] = median(diff(keypressesRT[keypresses_lKey]))
  }
  keypressesRT_lKey_Adj = keypressesRT_lKey_Tot[!as.logical(recordingInfo$popped)] # adjusted to consider only the pumps done for balloons that did not pop  

  # BART score
  BARTscore = recordingInfo$earnings[dim(recordingInfo)[1]];
    
  # store in a long format dataset (one row per file, even when multiple files are recorded per participant)
  recordingLvl_long[rowCounter_long,"recording"]       = fileList[fileIdx]
  recordingLvl_long[rowCounter_long,"nBalloons"]       = nBalloons
  recordingLvl_long[rowCounter_long,"nBalloonsPopped"] = nBalloonsPopped
  recordingLvl_long[rowCounter_long,"nBalloonsNotPopped"] = nBalloonsNotPopped
  recordingLvl_long[rowCounter_long,"nPumpsTot"] = nPumpsTot
  recordingLvl_long[rowCounter_long,"nPumpsAdj"] = nPumpsAdj
  recordingLvl_long[rowCounter_long,"keypressesRT_lKey_Tot"] = median(keypressesRT_lKey_Tot)
  recordingLvl_long[rowCounter_long,"keypressesRT_lKey_Adj"] = median(keypressesRT_lKey_Adj)
  recordingLvl_long[rowCounter_long,"BARTscore"] = BARTscore
}





# store 
filePath = paste(mainFolder,"data_processing",sep="/")
fileName = paste("BART_long",".csv",sep="")
write.csv(recordingLvl_long,paste(filePath,fileName,sep="/"), row.names = FALSE)
