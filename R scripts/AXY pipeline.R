

# AXY pipeline
rm(list = ls())
pacman::p_load(tidyverse, dplyr, lubridate, here, ggExtra)

metadata <- read.csv("D:/Chapter 4 - foraging success/Metadata_MASTER.csv") %>%
  filter(!is.na(AXY_filename_new))   #read in metadata and filter so only interested in deployments with axy data

deploys <- unique(metadata$deployID)  # create a list of deploy IDs which had an axy

input_dir <- ("D:/raw_penguin/Robben/AXY_Raw/2018R_AXY_Raw/")
csv_dir <- ("D:/raw_penguin/Robben/AXY_Subset/2018R_AXY_subset/")
output_dir <- ("D:/Chapter 4 - foraging success/processed_data/")

AXYTDRfiles <- fs::dir_ls(input_dir, glob = "*_axytdr*.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name
AXYTDRdeploys <- as.data.frame(AXYTDRfiles) %>%
  mutate(filename = (basename(AXYTDRfiles))) %>% #strip out the filename by removing path prefix
  mutate(deployID = (str_replace(filename,"_axytdr", replacement=""))) %>% #extract birdID by removing axytdr from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

#for the purpose of the test <- subset so we have a list of deployIDs that match the ones in the test folder
axydeploys <-  as.data.frame(deploys) %>% filter(deploys %in% AXYTDRdeploys$deployID)
axydeploys <- axydeploys$deploys

rm(AXYTDRdeploys, AXYTDRfiles, metadata, deploys)
templist = list()  # create templist

i<- axydeploys[1]
for (i in axydeploys) {    # start loop

  
## need to sort out time for 2017 data - currently reading in %M:%OS and ignoring hours
  axy <- read.csv(paste0(input_dir,i,"_axytdr.csv")) %>%   # read in axy data
    mutate(DateTimeOS = paste(Date,Time, sep=" ")) %>% #Make DateTime column
    mutate(DateTime = dmy_hms(DateTimeOS)) %>% 
    dplyr::select(DateTime, X, Y, Z, Date, Time, DateTimeOS) %>%
    mutate(deployID = i) #and remove file suffix

  tdr <-  readRDS(here(output_dir, ("TDR_final.RDS"))) %>% 
    filter(deployID == i) %>%
    group_by(DiveID) %>%
    mutate(bottom = ifelse(Divephase == "B", 1, 0),
           bottom_length = sum(bottom)) %>%
    mutate(Shape = ifelse(bottom_length > 4, "U", "V"))  # add dive shape column based on bottom time


  
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
    select(-DateTime, - Date, -Time)
  
  write.csv(axy_subset, paste(csv_dir, i, "axy_subset.csv"))
  rm(axy, firstdive, lastdive)
  gc()
  invisible(gc())
  
  tdrsmall <- tdr %>%
    dplyr::select(-c(Temp, Pressureformat, TDR_tag, bottom, bottom_length)) 
  rm(tdr)
  
  future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
  
  axytdr <- axy_subset %>%
    left_join(., tdrsmall) %>%
    fill(DiveID, .direction = "down") %>%
    fill(Shape, .direction = "down") %>%
    fill(Divephase, .direction = "down")

  
  diveIDs <-  axytdr %>%
    filter(DiveID > 0) %>%
    filter(!is.na(DiveID)) %>%
    filter(Shape == "U")
    
  diveIDlist <- unique(diveIDs$DiveID)
  
  templist1 = list()
  #j <- diveIDlist[1]
  
  
  for (j in diveIDlist) { 
  
  axytdr_j <- axytdr %>% filter(DiveID == j)  #subset df to look at diveID j
  
  divestart <- axytdr_j %>%    
    summarise(DateTime = first(DateTime),    #when does dive j start
              diveID = first(DiveID))
 
  
   diveend <- axytdr_j %>% 
    summarise(DateTime = last(DateTime),    #when does dive j end
              diveID = last(DiveID))
  
   axytdr_dive <- axytdr %>%            # subset df to only look at diveID j with buffer
    filter(DateTime > divestart$DateTime - seconds(5)) %>%   # 5 second buffer before
    filter(DateTime < diveend$DateTime + seconds(5)) %>%   # 5 second buffer after
    mutate(DiveID_new = j) %>%
    mutate(totala = sqrt((X^2)+(Y^2)+(Z^2)))  #add a column for overall dynamic body acceleration
  
   templist1[[j]] <- axytdr_dive  
   
   rm(axytdr_j, divestart, diveend, axytdr_dive)
  
  }

  templist[[i]] <- data.table::rbindlist(templist1) 

finalaxytdr<- data.table::rbindlist(templist)

rm(axy, tdr, firstdive, lastdive, axy_subset, axytdr, diveIDs, diveIDlist)
}

saveRDS(finalaxytdr, file=(file.path(output_dir,(paste0("AXYTDR18",".RDS"))))) #Save as an RDS object (load this later with 'readRDS' function)

axyALL <- readRDS(here(output_dir, ("AXYTDR18.RDS")))

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



## Some exploratory plots

test <- finalaxytdr %>%
  fill(Divephase, .direction = "down") 

testtdr <- test %>%
  filter(!is.na(Calibrated_depth)) %>%
  mutate(dDepth = lag(Calibrated_depth) - Calibrated_depth) %>%  #calculate the change in depth - 
  #NOTE: this should be change in depth (m) per s, 
  #but as our TDRs record once a second I've skipped a step but will add in at some point ####
mutate(dDepth = as.numeric(dDepth)) %>%
  filter(!is.na(dDepth)) %>%  #remove NA values so next line works but probably need to keep them in ####
mutate(Wigglepos = ifelse(max(dDepth) > 0.3, TRUE, FALSE),  # wiggle defined in Sala et al 2012 is change in depth of more than 0.3m in a second
       Wiggleneg = ifelse(min(dDepth) < -0.3, TRUE, FALSE),  # have included positive and negative values but probably should only include one or the other ####
       Wiggle = ifelse(Wigglepos == TRUE | Wiggleneg == TRUE, TRUE, FALSE)) %>%
  filter(Wiggle == TRUE)

test1 <- test %>%
  filter(deployID == "01_2018R") %>%
  filter(DiveID == 111)

summary(finalaxytdr)

ggplot(data = testtdr) +
  geom_histogram(aes(x = dDepth)) + geom_vline(aes(xintercept = 0.3), col = "blue") +
  geom_vline(aes(xintercept = -0.3), col = "blue") + 
  facet_wrap(~ Divephase, ncol = 2) + theme_classic() #+ scale_y_log10()


ggplot(data = test) +
  geom_histogram(aes(x = totala)) + geom_vline(aes(xintercept = 2), col = "blue") +
  facet_wrap(~ Divephase, ncol = 2) + theme_classic() + scale_y_log10()

axyplot <- ggplot(data = test1) +
  geom_line(aes(x = DateTime, y = log(totala))) +
  #facet_wrap(~ Divephase, ncol = 1) + 
  theme_classic()
tdrplot <- ggplot(data = test1) +
  geom_point(aes(x = DateTime, y = -Calibrated_depth)) +
  #facet_wrap(~ Divephase, ncol = 1) + 
  theme_classic()

gridExtra::grid.arrange(tdrplot, axyplot, ncol = 1)

