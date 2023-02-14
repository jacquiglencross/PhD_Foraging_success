

# AXY pipeline
rm(list = ls())
pacman::p_load(tidyverse, dplyr, lubridate)

metadata <- read.csv("E:/Chapter 4 - foraging success/Metadata_MASTER.csv") %>%
  filter(!is.na(AXY_filename_new))   #read in metadata and filter so only interested in deployments with axy data

axydeploys <- unique(metadata$deployID)  # create a list of deploy IDs which had an axy

input_dir <- ("E:/Chapter 4 - foraging success/test_data/")
output_dir <- ("E:/Chapter 4 - foraging success/processed_data/")


tdrALL <- readRDS(here(output_dir, ("TDR_final.RDS")))

templist = list()  # create templist

i<- axydeploys[4]
#for (i in axydeploys) {    # start loop
  
  
## need to sort out time - currently reading in %M:%OS and ignoring hours
  axy <- read.csv(paste0(input_dir,i,"_axytdr.csv")) %>%   # read in axy data
    #mutate(Time1 = strptime(DateTime, format("%d/%m/%Y %H:%M:%OS")))# %>%
    mutate(DateTime = paste(Date,Time, sep=" ")) %>% #Make DateTime column
    mutate(DateTime = dmy_hms(DateTime)) %>%
    #mutate(Time1 = strptime(DateTime, format = "%d/%m/%Y %H:%M:%OS"))# %>%
    dplyr::select(DateTime, X, Y, Z, Date, Time) %>%
    mutate(deployID = i) #and remove file suffix
  
  tdr <- tdrALL %>% 
    filter(deployID == i) %>%
    group_by(DiveID) %>%
    mutate(bottom = ifelse(Divephase == "B", 1, 0),
           bottom_length = sum(bottom)) %>%
    mutate(Shape = ifelse(bottom_length > 4, "U", "V")) # add dive shape column based on bottom time

  
  # identify start time of first foraging dive
  firstdive <- tdr %>% 
    ungroup() %>%
    filter(Shape == "U") %>%
    filter(tripID == 1) %>%
    summarise(DateTime = first(DateTime),
              diveID = first(DiveID))
  
  # identify end time of last foraging dive
  lastdive <- tdr %>% 
    ungroup() %>%
    filter(Shape == "U") %>%
    filter(tripID == 1) %>%
    summarise(DateTime = last(DateTime),
              diveID = last(DiveID))
  
  #subset axy data so it starts 10 minutes before the first forage dive starts
  # and ends 10 minutes after the last dive ends
  axy_subset <- axy %>%
    filter(DateTime > firstdive$DateTime - minutes(10)) %>%
    filter(DateTime < lastdive$DateTime + minutes(10)) %>%
    mutate(totala = sqrt((X^2)+(Y^2)+(Z^2)))  #add a column for overall dynamic body acceleration

  
  
#}




## STEP 1: read in axy file
# - make sure all of the column names are the same
# - add deployID column


## STEP 2: read in tdr file (already processed and gone through the pipeline)
# - subset for same deployID
# - subset for trip 1 (avoid pseudoreplication) and for U dives ( bottom time > 4 s)
# - identify datetime for first dive - 10 minutes and last divev + 10 minutes


## STEP 3: subset the axy
#  remove points before datetime(tdr first trip - 10)
# remove points before datetime(tdr last trip + 10)


## STEP 4: merge subsetted axy with subsetted tdr
# subset for foraging dives +/- 5 seconds from datetime(tdr, when diveID = x)
# calculate obda

# rm(old df (NOT TEMPLIST))

#end loop


## STEP 5: bring all the datasets into one
# templist > rbindlist (Jacqui's polygon plot code)
# read in metadata
# columns: deployID, diveID, DateTime, OBDA, Sex


