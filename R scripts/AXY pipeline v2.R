# AXY pipeline

#start with an empty environment
rm(list = ls())
gc()
invisible(gc())
#load packages
pacman::p_load(tidyverse, dplyr, lubridate, here, ggExtra, zoo, beepr)

#load data
metadata <- read.csv("D:/Chapter 4 - foraging success/Metadata_MASTER.csv") %>%
  filter(!is.na(AXY_filename_new))   #read in metadata and filter so only interested in deployments with axy data

deploys <- unique(metadata$deployID)  # create a list of deploy IDs which had an axy

input_dir <- ("D:/raw_penguin/Robben/AXY_Raw/") #where to find raw axy data
output_dir <- ("D:/Chapter 4 - foraging success/processed_data/") 
csv_dir <- ("D:/Chapter 4 - foraging success/wiggles_ODBA_files/") #where to save csv files

#list all files that end in axytdr in input_dir
AXYTDRfiles <- fs::dir_ls(input_dir, glob = "*_axytdr*.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name
#extract deployID from filenames
AXYTDRdeploys <- as.data.frame(AXYTDRfiles) %>%
  mutate(filename = (basename(AXYTDRfiles))) %>% #strip out the filename by removing path prefix
  mutate(deployID = (str_replace(filename,"_axytdr", replacement=""))) %>% #extract birdID by removing axytdr from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

#for the purpose of the test <- subset so we have a list of deployIDs that match the ones in the test folder
axydeploys <-  as.data.frame(deploys) %>% filter(deploys %in% AXYTDRdeploys$deployID)
axydeploys <- axydeploys$deploys

#read in all TDR data
tdrALL <- readRDS(here(output_dir, ("TDR_final.RDS")))

i = axydeploys[1]
for (i in axydeploys) {    # start loop
  
  filename = AXYTDRdeploys$AXYTDRfiles[AXYTDRdeploys$deployID == i] #subset so we are only reading in one file at a time
  
  
  ## need to sort out time for 2017 data - currently reading in %M:%OS and ignoring hours
  axy <- read.csv(paste0(filename)) %>%   # read in axy data
    mutate(DateTimeOS = paste(Date,Time, sep=" ")) %>% #Make DateTime column
    mutate(DateTime = dmy_hms(DateTimeOS)) %>% 
    dplyr::select(DateTime, X, Y, Z, Date, Time, DateTimeOS) %>%
    mutate(deployID = i) #and remove file suffix
  
  tdr <- tdrALL %>% 
    filter(deployID == i) %>%
    group_by(DiveID) %>%
    mutate(bottom = ifelse(Divephase == "B", 1, 0),
           bottom_length = sum(bottom)) %>%
    mutate(Shape = ifelse(bottom_length > 4, "U", "V"))  # add dive shape column based on bottom time
  
  # Wiggles step 1: Subsetting for bottom phase only where we're looking for "wiggles"
  TDR_bottom <- tdr %>%
    filter(Divephase == "B") # subset for bottom phase only 
  
  # Wiggles step 2: Working out wiggles according to the definition in Campell, 2016.
  Wiggles_final <- TDR_bottom %>%
    group_by(deployID, DiveID) %>%  # group by DiveID
    mutate(lag = lag(Calibrated_depth,1)-Calibrated_depth) %>% # commutes the change in depth from the next row 
    mutate(rollm = rollmean(lag, k = 3, fill = NA, align = 'right')) %>% # commutes the rolling average for the previous 3 seconds  
    mutate(diff =  lag- rollm) %>% # commutes the differences between rolling mean and lag
    
    # defines wiggles where the difference between rolling mean and lag is greater than or equal to 0.3
    mutate( Wiggle = case_when(diff >= 0.3 ~ 1,
                               diff <= -0.3 ~ 1,
                               diff < 0.3 & diff > -0.3 ~ 0 )) %>%
    dplyr::select(-c(lag,rollm,diff)) # remove unwanted columns
  
  Wiggles_perdive <- Wiggles_final %>%
    group_by(DiveID) %>%
    filter(!is.na(Wiggle)) %>%
    summarise(no_wiggles = sum(Wiggle))
  
  # identify start time of first foraging dive
  firstdive <- tdr %>% 
    ungroup() %>%
    filter(Shape == "U") %>%
    filter(tripID == 1) %>%
    summarise(DateTime = first(DateTime),
              diveID = first(DiveID))
  
  # identify end time of last foraging dive
  lastdive <-tdr %>% 
    ungroup() %>%
    filter(Shape == "U") %>%
    filter(tripID == 1) %>%
    summarise(DateTime = last(DateTime),
              diveID = last(DiveID))
  
  #subset axy data so it starts 1 minute before the first forage dive starts
  # and ends 1 minute after the last dive ends
  axy_subset <- axy %>%
    filter(DateTime > firstdive$DateTime - minutes(1)) %>%
    filter(DateTime < lastdive$DateTime + minutes(1)) 
  
  tdrshort <- tdr %>%        #create a reduced version of the tdr data with some of the valuable columns we want to keep
    dplyr::select(DateTime, DiveID, Shape, Divephase)
  
  axytdr <- axy_subset %>%
    left_join(tdrshort) %>%
    merge(., Wiggles_perdive, all.y = T) %>%
    fill(DiveID, .direction = "down") %>% 
    fill(Shape, .direction = "down") %>%
    fill(Divephase, .direction = "down")
  
  #create a list of all forage diveIDs in the file
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
      mutate_at(c('X', 'Y', 'Z'), as.numeric) %>%
      mutate(totala = sqrt((X^2)+(Y^2)+(Z^2)))  #add a column for overall dynamic body acceleration
    

    templist1[[j]] <- axytdr_dive  
    
    rm(axytdr_j, divestart, diveend, axytdr_dive)
    
  }
  # spit out a csv file for each individual  
axytdr1 <- data.table::rbindlist(templist1) 
 
write.csv(axytdr1, paste0(csv_dir, i, "_subset.csv")) 
 # templist[[i]] <- data.table::rbindlist(templist1) 
  
 # finalaxytdr<- data.table::rbindlist(templist)
  

  
  rm(axy, tdr, firstdive, lastdive, axy_subset, axytdr, diveIDs, diveIDlist)
}

gc()
invisible(gc())

# read in all subset files to create one file

setwd(csv_dir)
AXYTDRfiles <- fs::dir_ls(csv_dir, glob = "*_subset*.csv", type="file", recurse = FALSE) # list location all files with "tdr" in the name
AXYTDRdeploys <- AXYTDRfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c"))  %>%  #read in TDR files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_subset", replacement=""))) %>% #extract birdID by removing subset from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix  
  

modivesums <- AXYTDRdeploys %>%
  mutate(totala = as.numeric(totala),
         Wiggle = as.numeric(no_wiggles),
         Wiggle_bottom = ifelse(is.na(Wiggle), 0, Wiggle)) %>%
  filter(Divephase == "B") %>%
  group_by(deployID, DiveID) %>%
  summarise(meanODBA = mean(totala),
            Shape = first(Shape),
            no_wiggles = as.factor(max(Wiggle_bottom))) %>%
  mutate(wiggleYesNo = ifelse(no_wiggles != 0, "Wiggle", "NoWiggle"))
write.csv(modivesums, paste0(output_dir, "divesummaries.csv"))

metadatashort <- metadata %>%
  select(deployID, Year, Sex_0.5)
divessums <- modivesums %>%
  left_join(., metadatashort)

beep(5)

ggplot(data = modivesums) +
  geom_histogram(aes(x = meanODBA)) + #geom_vline(aes(xintercept = 0.3), col = "blue") +
  #geom_vline(aes(xintercept = -0.3), col = "blue") + 
  facet_wrap(~ wiggleYesNo, ncol = 1) + theme_classic() 
ggplot(data = modivesums) +
  geom_point(aes(x = meanODBA, y = no_wiggles)) + #geom_vline(aes(xintercept = 0.3), col = "blue") +
  theme_classic() 

library(lme4)

subset_divessums <- divessums %>% filter(!is.na(Sex_0.5)) %>% filter(Year < 2021)
model1 <- lmerTest::lmer(meanODBA ~ Sex_0.5 + as.factor(Year) + (1|deployID), data = subset_divessums)
summary(model1)
#I have temporarily removed files that are empty so it runs - check why these files are empty
# either because no wiggles or something has gone wrong
#eg <- read.csv("D:/Chapter 4 - foraging success/wiggles_ODBA_files/02_2017R_subset.csv")



## Some exploratory plots

test <- axytdr1 %>%
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

summary(axytdr1)

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
