# **The African penguin movement analysis pipeline**

# ON FIRST USE, RUN ENTIRE SCRIPT TO CREATE SUB-FILES
# On R Studio this can be run as a background job (tabs next to console)

# Prepare environment (RUN THIS SECTION EVERY TIME) ====================================================================================================================================================================
#install.packages("pacman") #install pacman if not already
# Install and/or load required packages using pacman
pacman::p_load(tidyverse, dplyr, lubridate, stringr, purrr, #tidyr
               data.table, imputeTS, #datawrangling
               foreach, doParallel, furrr, progressr, #parallel
               sf, crawl, diveMove, forecast, zoo, #movement analysis
               ggplot2, #datavis
               here, beepr) #other useful packages 


# Declare input/output directories (because we're using the 'here' function these shouldn't need changing)
input_dir <- here("E:/raw_penguin") #top location of raw files)
output_dir <- here("E:/Chapter 4 - foraging success/processed_data") #location to save processed files)

# List paths of all TDR files and read in metadata file
TDRfiles <- fs::dir_ls(input_dir, glob = "*_tdr*.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name
metadata <- read_csv(here("E:/Chapter 4 - foraging success/metadata_MASTER.csv")) # read the metadata_MASTER file (should be put in raw_data folder)

# If whole script has already been run (required at first use), then start from whichever .RDS file is required:
#1. TDR_clean.RDS - TDR data combined from multiple csv files and cleaned
#2. TDR_processed.RDS - TDR data that has be filtered and calibrated with diveMove
#3. TDR_final.RDS - TDR data with trips detected
#4. TDR_summary.RDS - dive summaries of TDR_final by deployID

# STEP 1: Read in and clean TDR data ===================================================================================================================================================================================

## Read in TDR files with deployID extracted from filename -------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRdata <- TDRfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>%  #read in TDR files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_tdr", replacement=""))) %>% #extract birdID by removing tdr from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

## Create dataframe for deployIDs with 'Depth' only  -------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRdepth <- TDRdata %>%
  filter(!is.na(Depth)) %>% #remove rows where Depth *is not* already converted (i.e. Depth is not NA)
  mutate(decimals = nchar(str_split(as.character(Depth), pattern = "\\.", n=2, simplify=TRUE)[,2])) %>% #count number of decimal places (as characters)
  group_by(deployID) %>% #group by deployID ready for mean decimal calculations
  mutate(Pressureformat = case_when( #create Pressureformat label using mean number of decimal places for each deployID
    max(decimals) == 1 ~ "mbar", #.1 gets labelled as "mbar"
    max(decimals) == 2 ~ "bar", #.01 as "bar"
    max(decimals) == 3 ~ "dbar")) %>% #.001 as "dbar"
  dplyr::select(-c(decimals)) %>% #remove decimal column (not needed anymore)
  mutate(Pressureformat = case_when( #manually edit Pressureformat for some IDs (decimal places missing in raw files)
    deployID == "07_2014D" ~ "bar",
    deployID == "11_2014R" ~ "bar",
    TRUE ~ as.character(Pressureformat) )) %>%
  mutate(across(c("Temp", "Depth"), as.numeric)) %>% #change Temperature and Depth to numeric
  group_by(deployID) %>% #group again in case data was ungrouped in previous step
  mutate(Depth = case_when( #convert dbar/mbar to bar (Depth in m) using correct conversions and calibrations
    Pressureformat == "bar" ~ Depth,
    Pressureformat == "dbar" ~ Depth*10,
    Pressureformat == "mbar" ~ (Depth-min(Depth))/100 )) #calibrate Depth for Depth=mbar files (check not done already)

## Create dataframe for deployIDs with 'Pressure' only -----------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRpressure <- TDRdata %>%
  filter(is.na(Depth)) %>% #remove rows where Depth *is* already converted (i.e. Depth is NA)
  mutate(decimals = nchar(str_split(as.character(Pressure), pattern = "\\.", n=2, simplify=TRUE)[,2])) %>% #count number of decimal places (as characters)
  group_by(deployID) %>% #group by deployID ready for mean decimal calculations
  mutate(Pressureformat = case_when( #create Pressureformat label using mean number of decimal places for each deployID
    max(decimals) == 1 ~ "mbar", #.1 gets labelled as "mbar"
    max(decimals) == 2 ~ "bar", #.01 as "bar"
    max(decimals) == 3 ~ "dbar")) %>% #.001 as "dbar"
  dplyr::select(-c(decimals)) %>% #remove decimal column (not needed anymore)
  mutate(across(c("Temp", "Pressure", "Depth"), as.numeric)) %>% #change Temperature and Pressure to numeric
  group_by(deployID) %>% #group again in case data was ungrouped in previous step
  mutate(Depth = case_when( #convert dbar/mbar to bar (Depth in m) using correct conversions and calibrations
    Pressureformat == "bar" ~ Pressure,
    Pressureformat == "dbar" ~ Pressure*10,
    Pressureformat == "mbar" ~ (Pressure-min(Pressure))/100 )) #calibrate mbar TDR using lowest Pressure reading (per group)

## Merge TDRdepth and TDRpressure --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRall <- as_tibble(rbindlist(list(TDRdepth, TDRpressure))) #merge the two TDR datasets into one tibble (rbindlist is fastest)
rm(TDRpressure,TDRdepth, TDRdata, TDRfiles) #drop the big datasets created, and keep only TDRall

# Extract TDRtag type from metadata ready to add to TDRclean
TDR_tags <- metadata %>% dplyr::select(deployID, TDR_tag)

# Do some final cleaning, tidying and removal of unnecessary columns
TDRclean <- TDRall %>%
  filter(Depth < 150)  %>% #filter out extreme Depth values > 150 (e.g. weird end of many files)
  mutate(DateTime = paste(Date,Time, sep=" ")) %>% #Make DateTime column
  mutate(DateTime = dmy_hms(DateTime)) %>% #Format DateTime column
  dplyr::select(DateTime, Temp, Depth, deployID, Pressureformat) %>% #Keep DateTime, Temp, Depth, DeployID, Pressureformat
  left_join(., TDR_tags, by="deployID") %>% #join TDR_tag info to full dataset
  mutate(Depth = case_when(deployID == "08_2010R" ~ Depth-15, #manually adjust Depth for one very weird file
                           TRUE ~ as.numeric(Depth))) %>% #do nothing to all other rows
  group_by(deployID) %>%
  filter(!duplicated(DateTime))
rm(TDRall) #drop TDRall dataframe

# Check there are no NAs or duplicates in TDR data (duplicated might take a while time to check)
if(sum(colSums(is.na(TDRclean))) >1) {message(paste("AP pipeline:There are",(sum(colSums(is.na(TDRclean)))),"NA values in TDR data "))
} else {message("AP pipeline: No NA's in TDR data")}
if(sum(anyDuplicated(dplyr::select(TDRclean[grep("2022",TDRclean$deployID),],deployID, DateTime)))>1) {message(paste("AP pipeline: duplicates detected in 2022 TDR data"))
} else {message("AP pipeline: no duplicates in 2022 TDR data!")}

## Save TDRclean--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRclean, file = here(output_dir,"TDR_cleaned.RDS"), compress="xz") #save as RDS file (loaded with 'readRDS')
#saveRDS(TDRclean, file = here(output_dir,"TDR_cleaned_zip.RDS"), compress="xz") #save as RDS with xz compression (~.25 original size but slower save/load)
beep(5) #actual fanfare noises for finishing this step!

# Tidy up after saving
rm(TDR_tags,TDRclean)


# STEP 2: Processing TDR data with diveMove ============================================================================================================================================================================

## Specify files to remove from further analyses (no dives or bad data) ------------------------------------------------------------------------------------------------------------------------------------------------
remove_files <- c("02_2017R", #no dives
                  "24_2017R", #no dives
                  "05_2018R") #bad logger (100 rows of datetimes at 09:00:48)
message("AP pipeline: Removing all data for ",length(remove_files)," TDR files with no dives/bad data (",toString(remove_files),")")

## Read in TDRclean, remove bad files and split into dataframe per deployID   ------------------------------------------------------------------------------------------------------------------------------------------
TDRdives <- readRDS(here(output_dir, "TDR_cleaned.RDS")) %>% #read in TDRclean RDS file
  filter(!(deployID %in% remove_files)) %>% #filter to remove files deployID
  group_by(deployID) %>% group_split(.) #group and split by deployID ready for diveMove processing

## DIVEMOVE VARIABLES FOR TDR CALIBRATION (recursive filtering for zero offset correction — per Luque & Fried 2011) ----------------------------------------------------------------------------------------------------
depth.bounds <- c(-5, 5) #this is the range of where we expect the surface to be found
probs <- c(0.5, 0.3) #running quantiles for each of the two steps used (probs[1] is 1st smoothing step, probs[2] is 2nd filtering step)
K <- c(2,360) #window size for each step in seconds (e.g K[1] is window size of 1st step, K[2] is window size of 2nd step)
dive.threshold <- 5 #Depth below which anything should be considered a dive

## Run recursive filtering for zero offset correction (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015850))   -----------------------------------------------------------------------------------
future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
TDRcalibrated <- TDRdives %>% #Create and calibrate each TDR object using recursive filtering
  future_map(~ diveMove::createTDR(time=.x$DateTime, depth=.x$Depth, dtime=1, file=.x$deployID[1]),.progress=TRUE) %>% #create diveMove object with deployID as source file name
  future_map(~ diveMove::calibrateDepth(.x, dive.thr=dive.threshold, zoc.method="filter",k=K, probs=probs, depth.bounds = depth.bounds, na.rm=TRUE),.progress=TRUE)

## Extract key data columns from each of the TDRcalibrated s4 diveMove objects and cbind  ------------------------------------------------------------------------------------------------------------------------------
deployIDs <- TDRcalibrated %>% map(~ as_tibble(.x@tdr@file)) #extracts the deployID for each dataframe
depth <- TDRcalibrated %>% map(~ as_tibble(.x@tdr@depth)) #extracts the calibrated depth
diveID <- TDRcalibrated %>% map(~ as_tibble(.x@dive.activity[["dive.id"]])) #extract the diveID number
divephase <- TDRcalibrated %>% map(~ as_tibble(.x@dive.phases)) #extracts the dive phases for each dive
TDRextracted <- Reduce(function(x,y) Map(cbind, x, y),list(deployIDs,depth,diveID,divephase)) #cbind each of the lists together with an anonymous function
rm(deployIDs,depth,diveID,divephase) #remove all the elements extracted
invisible(gc()) #quietly call garbage collection to free up ram

## Combine to create the processed TDR dataframe -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRprocessed <- Reduce(function(x,y) Map(cbind, x, y),list(TDRdives,TDRextracted)) %>%
  rbindlist(.) %>% as_tibble(., .name_repair = make.unique) %>% #bind lists together and repair names
  dplyr::select(-value) %>%
  rename(Calibrated_depth = value.1, DiveID = value.2, Divephase = value.3) %>%
  dplyr::select(DateTime, Temp, Depth, Calibrated_depth, DiveID, Divephase, deployID, Pressureformat, TDR_tag)

## Save TDRprocessed ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRprocessed, file= here(output_dir,("TDR_processed.RDS"))) #Save as an RDS object (load this later with 'readRDS' function)
beep(5) #more fanfare noises

## Tidy up after saving
rm(TDRcalibrated,TDRdives,TDRextracted,TDRprocessed, remove_files, K, depth.bounds,probs,dive.threshold)


#STEP 3: Despiking and Trip detection ==================================================================================================================================================================================
# First: remove spikes from a select number of files
# Second: reprocess the entire processed dataset with diveMove again and extract just the trips to stitch back onto the original 

# List of deployID's that show the weird spikes/aka multiple zero level problem
despike_list <- c("01_2008D", "03_2008D", "04_2008D", "05_2008D")

# Despiking process (basically a TDR speed filter)
TDRdespike <- readRDS(here(output_dir,("TDR_processed.RDS"))) %>% #read in TDRprocessed file
  group_by(deployID) %>% #group by deployID
  filter(deployID %in% despike_list) %>% #filter to only the deployIDs in despike_list
  mutate(lead = lead(Calibrated_depth,1)-Calibrated_depth) %>% #compute change in depth from preceding row
  mutate(lag = lag(Calibrated_depth,1)-Calibrated_depth) %>% #compute change in depth to next row
  mutate(change = abs(lead) + abs(lag)) %>% #absolute change in depth +/- 1 row
  mutate(Calibrated_depth = case_when( #use absolute change to detect spikes and revert those to NA's
    change > 9 ~ NA_real_, #paste NA when absolute change is more that 9 (e.g. like 5m down, then 5m up in two seconds)
    change <= 9 ~ Calibrated_depth, #leave as calibrated depth when absolute change is less than 9
    change == NA ~ Calibrated_depth, #Also leave examples where change is NA (at beginning and end of dataframe)
    TRUE ~ as.numeric(Calibrated_depth))) %>% #Everything else that I've forgotten about leave as calibrated depth
  mutate(Calibrated_depth = na_interpolation(Calibrated_depth,option="linear")) %>% #impute linear interpolation for despiked points (now NA's)
  dplyr::select(-c(lead,lag,change))

# rbind the despiked dataset back onto the original dataset
TDRnospike <- readRDS(here(output_dir,("TDR_processed.RDS"))) %>% #read in the same TDRprocessed file
  filter(!deployID %in% despike_list) #filter out the opposite set of deployments (i.e. those without spike problems)
TDRprocessed <- rbind(TDRdespike,TDRnospike) #bind the two datasets together

#Tidy up after saving
rm(TDRnospike,TDRdespike,despike_list)
invisible(gc()) #quietly call garbage collection to free up ram

## DIVEMOVE VARIABLES FOR TRIP PHASE DETECTION -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# currently using: 4, 3hrs, 3mins
depth_thr <- 4 #threshold below which TDR values 'could' be classified as dry (hacky - only using for diveMove trip phase detection)
dry_thr <- (3600*3) #dry error threshold in seconds (nseconds*hours). Dry phases shorter than this threshold will be considered as wet (see ?calibrateDepth)
wet_thr <- (60*3)  #wet threshold in seconds (nseconds*minutes). At-sea phases shorter than this threshold will be considered as trivial wet (see ?calibrateDepth)
trip_thr <- 60*15 #minimum threshold for trip length in seconds (i.e. remove trips shorter than 15 minutes)

# Get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
future::plan(multisession, workers = 7) 

## Using despiked dataset, use .detPhase to detect trip phases ---------------------------------------------------------------------------------------------------------------------------------------------------------
TDRalltrips <- TDRprocessed %>%
  mutate(wet_cond = case_when( #create a new column called wet_cond (for diveMove::calibrateDepth to detect trips)
    Calibrated_depth < depth_thr ~ FALSE, #when calibrated depth less than depth threshold, classify "wet_cond" as dry (FALSE)
    Calibrated_depth >= depth_thr ~ TRUE)) %>% #when calibrated depth more than depth threshold, classify "wet_cond" as wet (TRUE) 
  group_by(deployID) %>% group_split(.) %>% #group by deployID and split ready for diveMove processing
  future_map(~ diveMove:::.detPhase(time=.x$DateTime, depth=.x$Calibrated_depth, interval=1, wet.cond = .x$wet_cond, dry.thr=dry_thr, wet.thr=wet_thr),.progress=TRUE)

# Extracts only the tripID per row for each dataframe (this is the only thing we care about for trip detection)
tripID <- map(TDRalltrips, "phase.id") %>% #extract phase id from elements created by detphase
  map(~ as_tibble(.x)) #convert to tibble ready for recombining with full dataset
rm(TDRalltrips) #tidy up after extracting tripID

# Stitch tripID for each deployment back onto despiked TDRprocessed object
TDRprocessed <- TDRprocessed %>%
  group_by(deployID) %>% group_split(.) #group by deployID and split
TDRtrips <- Reduce(function(x,y) Map(cbind, x, y),list(TDRprocessed,tripID)) %>%
  rbindlist(.) %>% as_tibble(.) %>% #rbind the list of dataframes back together as one tibble
  rename(tripID = value) #rename the trip column (currently called 'value') to tripID

# Tidy up after stitching
rm(TDRprocessed,tripID) #tidy up after combining TDRprocessed and tripID
invisible(gc()) #quietly call garbage collection to free up ram

# Process data using tripID to remove all trip phases where SD is less than 1 or shorter than threshold, and rerank starting at 1
TDRfinal <- TDRtrips %>% 
  group_by(deployID,tripID) %>% #group by deployID and tripID
  summarise(sd=sd(Calibrated_depth), length=length(tripID)) %>% #for each trip of deploymentID, create a summary of standard deviation and length
  left_join(TDRtrips,.) %>%
  mutate(trip_num = case_when( #when SD depth for group is higher than 1, change group trip number
    length < trip_thr | sd < 1 ~ 0, #when trip length is below threshold -OR- SD is less than 1, change tripID to zero (does not count as a trip)
    sd >= 1 ~ tripID )) %>% #if sd exceeds 1, keep tripID (counts as a trip)
  mutate(trip_num = na_if(trip_num, 0)) %>% #change all the zeros to NA's (to help avoid problems with plotting later)
  ungroup() %>% group_by(deployID) %>% #remove previous grouping and focus only on deployID
  mutate(trip_num = dense_rank(trip_num)) %>% #create new rank using remaining trip_num (e.g. 1,1,4,4,6,6 becomes 1,1,2,2,3,3)
  dplyr::select(-c(sd,length,tripID)) %>% #drop sd, length, and tripID 
  rename(tripID = trip_num)

## Save TDRfinal -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRfinal, file=here(output_dir,"TDR_final.RDS")) #Save as an RDS object (load with 'readRDS')
beep(8) #final fanfare noises!
















# Tidy up after saving
rm(TDRfinal,TDRtrips, depth_thr, dry_thr, wet_thr, trip_thr)
invisible(gc()) #quietly call garbage collection to free up ram

# STEP 4: Extraction of dive summaries for each deployID (1st trip only) ===============================================================================================================================================

## Read in Metadata_MASTER and filter columns (all individuals with biometrics have a sex classified)
metadata_sex <- read_csv(file = here("E:/Chapter 2 HMM/Metadata_MASTER.csv")) %>%
  dplyr::select(deployID, Sex_0.5, Year, Closure, Colony) %>% rename(Sex = Sex_0.5)

## Read in cleaned and processed TDR file, and create list of deployIDs  -----------------------------------------------------------------------------------------------------------------------------------------------
TDRpost <- readRDS(here(output_dir, "TDR_final.RDS")) %>% #read in the TDRfinal file
  replace_na(list(tripID = 0)) %>% #replace the na's in trip number with zeroes
  mutate(tripID = as_factor(tripID)) %>% #mutate the tripID column into a factor for plotting
  left_join(., metadata_sex, by="deployID") %>% #add metadata (Sex) to TDRdataset
  ungroup()

## Filter data for first trips and summarise using diveMove::diveStats   -----------------------------------------------------------------------------------------------------------------------------------------------
future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)

## Reprocess the data for first trip with diveMove (now using no offset or any corrections) to detect individual dives   -----------------------------------------------------------------------------------------------
TDRreprocess <- TDRpost %>%
  filter(tripID == 1) %>% #remove individuals without sex and subset to dives from first trip
  ungroup() %>% group_by(deployID) %>% group_split(.) %>% #group and split by deployID ready for diveMove processing
  future_map(~ diveMove::createTDR(time=.x$DateTime, depth=.x$Calibrated_depth, dtime=1, file=.x$deployID[1]),.progress=TRUE) %>% #create diveMove object with deployID as source file name
  future_map(~ diveMove::calibrateDepth(.x, dive.thr=5, zoc.method="offset",offset=0, na.rm=TRUE),.progress=TRUE)

## Loop to that creates a diveMove summary datafile for all deployID (hate loops but couldn't find a better way to do it)  ---------------------------------------------------------------------------------------------
templist = list() #initialise list for loop

for (i in 1:length(TDRreprocess)) {
  temp <- diveMove::diveStats(TDRreprocess[[i]])
  temp$deployID <- (TDRreprocess[[i]]@tdr@file)
  temp$diveID <- seq.int(nrow(temp)) 
  templist[[i]] <- temp
}
TDRsummary <- data.table::rbindlist(templist) %>% #rbind output of the loop into a dataframe
  left_join(., metadata_sex, by="deployID") #add sex information

## Save dive stat summary file   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRsummary, file=here(output_dir,"TDR_summary.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after stitching
rm(TDRpost, TDRreprocess, temp, i ,templist, TDRsummary) #tidy up after combining TDRprocessed and tripID
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 5: Read in raw GPS data =========================================================================================================================================================================================

# List paths of all GPS files
GPSfiles <- fs::dir_ls(input_dir, glob = "*gps*.csv", type="file", recurse = TRUE) # list location of all files with "gps" in the name

## Read in GPS files and extract deployID from filename ----------------------------------------------------------------------------------------------------------------------------------------------------------------
GPSdata <- GPSfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>% #read in GPS files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_gps", replacement="")),.before=Date) %>% #extract birdID by removing gps from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

## Create datetime column and clean up dataframe -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
GPSclean <- GPSdata %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>% #Create DateTime column
  drop_na(.) %>% #remove rows with NA's (only present in newly-created DateTime column)
  mutate(across(c(Lat,Lon),as.numeric)) %>% #turn lat/lon into numeric
  dplyr::select(-c(filename)) #remove filename column
message("AP pipeline: Ignore 15 failed to parse warning above (some bad dates starting with 00 e.g. '00-01-17')")

## Filter out based on time jumps (> 1 second & < 2 days) --------------------------------------------------------------------------------------------------------------------------------------------------------------
GPStimefilter <- GPSclean %>%
  group_by(deployID) %>%  #group by deployID
  arrange(DateTime) %>% #arrange by DateTime (defaults to descending)
  mutate(diff_secs = as.duration(DateTime - lag(DateTime)), #calculate difference in time to previous row (output as duration)
         diff_long = ifelse(diff_secs > days(2),T,F), #convert diff_secs into a logical threshold for >2 days
         diff_short = ifelse(diff_secs < seconds(1),T,F)) %>% #convert diff_secs into a logical threshold for <1 sec
  replace_na(list(diff_long = FALSE, diff_short=FALSE)) %>% #convert NA's in threshold col to FALSE
  filter(cumsum(diff_long)<1) %>% #remove all rows of data after a time jump greater that 2 days
  filter(diff_short == FALSE) %>% #remove rows where time jump is less than 1 second
  dplyr::select(-c(diff_long, diff_short))

message("AP pipeline: ",nrow(GPSclean)-nrow(GPStimefilter)," GPS rows removed with our time jump filter (", 
        round((nrow(GPSclean)-nrow(GPStimefilter))/nrow(GPSclean)*100,digits=2),"% of data)")

## Filter out based on speed travelled from previous point & to next point (>12.4 km/h or 3.444 m/s) -------------------------------------------------------------------------------------------------------------------
GPSspeedfilter <- GPStimefilter %>%
  left_join(., (metadata %>% dplyr::select(deployID, Nest_Lat, Nest_Lon)), by = "deployID") %>% #add nest lat/lon to data
  ungroup() %>%
  mutate(geometry_GPS = st_transform(st_as_sf(., coords=c("Lon","Lat"), crs=4326), crs = 4326)$geometry, # assign GPS geometry and transform to lon lat for dist calcs
         geometry_CP = st_transform(st_as_sf(., coords=c("Nest_Lon","Nest_Lat"), crs=4326), crs = 4326)$geometry) %>% #assign nest geometry and transform to lon lat for dist calcs
  group_by(deployID) %>%
  mutate(diff_sec_prev = as.duration(DateTime - lag(DateTime)),
         diff_sec_next = as.duration(lead(DateTime) - DateTime),
         distance_prev = st_distance(geometry_GPS, lag(geometry_GPS), by_element = T), #distance travelled from previous point
         distance_next = st_distance(geometry_GPS, lead(geometry_GPS), by_element = T), #distance traveled to next point
         speed_msec_prev = as.numeric(distance_prev)/as.numeric(diff_sec_prev), #calculate speed moving from previous point (distance/time)
         speed_msec_next = as.numeric(distance_next)/as.numeric(diff_sec_next), #calculate speed moving to next point (distance/time)
         nestdist = st_distance(geometry_GPS, geometry_CP, by_element = T),.after=diff_secs) %>% #calculate distance between nest and current GPS location
  filter(speed_msec_prev < 12.4*(5/18) & speed_msec_next < 12.4*(5/18)) %>%  #speed filter - remove any point where the speed is over 12.4 km/h (Wilson 1986) 5/18 converts it into m/s
  ungroup() %>% #ungroup before final cleanup
  dplyr::select(-c(geometry_GPS, geometry_CP,diff_sec_prev,diff_sec_next,distance_prev,distance_next,speed_msec_next)) %>% #remove geometries and extra columns
  rename(speed_msec = speed_msec_prev) #rename the speed prev column

message("AP pipeline: ",nrow(GPStimefilter)-nrow(GPSspeedfilter)," GPS rows removed with our speed filter (", 
        round((nrow(GPStimefilter)-nrow(GPSspeedfilter))/nrow(GPStimefilter)*100,digits=2),"% of data)")

## Save GPS data -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(GPSspeedfilter, file=here(output_dir,"GPS_filter.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfiles,GPSdata,GPSclean,GPStimefilter,GPSspeedfilter)
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 6: Trip detection from GPS  =====================================================================================================================================================================================

## Use GPS data to classify trips in and out of island buffer ----------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in a map files of islands (including a buffer to account for inaccuracy in gps devices
islands500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_buffer(., dist = 500) %>% # 500m buffer in case of noisy GPS data
  st_transform(4326) #transform back to latlon
#st_crs(islands500, parameters = TRUE)$units_gdal #find units used for any named sf object
#sf_use_s2(TRUE) #if you get spammed with warnings from st_intersects below, you need to switch s2 back on

#Define trips as contiguous rows spent outside the island buffer (using rleid and denserank)
GPStrips <- readRDS(here(output_dir,"GPS_filter.RDS")) %>% #read in the GPSfilter file from step 5
  st_as_sf(coords = c('Lon', 'Lat'), crs=4326, remove=FALSE) %>% #add geometry column for analyses with sf
  mutate(intersection = as.integer(st_intersects(geometry, islands500)), #is the point intersection with an island (number)
         island = if_else(is.na(intersection), 'NA', islands500$name[intersection])) %>% #add the name of the island
  mutate(island = str_remove_all(island, pattern = c(" Island"))) %>% #remove "island" suffix from island name
  group_by(deployID) %>% arrange(DateTime) %>% 
  mutate(TripOrColony = ifelse(is.na(intersection), "trip", "colony"), #define as trip/colony based on presence/absence of NA
         trips_all = as.numeric(rleid(TripOrColony)), #number trips based on changes in TripOrColony (e.g.colony,trip,trip,trip,colony -becomes- 1,2,2,2,3)
         trip_npoints = rep(rle(trips_all)$lengths, rle(trips_all)$lengths), #length of trip (in rows)
         trips_only = case_when( #create case_when to define if a numbered trip should count as a trip
           TripOrColony == "colony" ~ NA_real_, #in colony, so does not count as a trip
           TripOrColony == "trip" ~ trips_all, #outside colony, so leave as numbered trip
           trip_npoints <= 1 ~ NA_real_), #ignore trips lasting one point (probably bad GPS)
         tripID = dense_rank(trips_only)) %>% #create new rank w/ trip_num (e.g. 1,1,3,3,5,5 becomes 1,1,2,2,3,3)
  dplyr::select(-c(intersection,TripOrColony,trips_all,trips_only,trip_npoints)) #remove extra columns

## Classify complete/incomplete trips ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.5km buffer area (home range) to account for porpoising 
islands1500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_buffer(., dist = 1500) %>% # 500m buffer in case of noisy GPS data
  st_transform(4326) #transform back to latlon

# Find if points intersect with islands1500 and summarise
GPStripsummary <- GPStrips %>% #use GPStrips object with numbered trip IDs
  mutate(homeintersection = as.integer(st_intersects(geometry, islands1500)), #is the point intersection with an island1500 (add island number)
         homeisland = if_else(is.na(homeintersection), 'NA', islands1500$name[homeintersection])) %>% #if intersection, add island name
  st_drop_geometry() %>%
  group_by(deployID, tripID) %>% arrange(DateTime) %>% 
  summarise(first = first(homeintersection), #summarise first row of each homeintersection (i.e. island number if intersecting)         
            last = last(homeintersection)) %>% #summarise last row of each homeintersection (i.e. island number if intersecting)
  replace_na(list(first = 0, last = 0))  %>% #replace NA's in first/last column with zeroes (easier to deal with for detecting tripcomplete)
  mutate(tripcomplete = case_when( #if trip starts and finishes inside same island area then it's a complete trip
    first==0|last==0 ~ FALSE, #if island number is zero, trip is not complete
    !is.na(tripID) & first==last ~ TRUE, #if trip is not NA and first island same as last, trip is complete
    !is.na(tripID) & first!=last ~ FALSE, #if trip is not NA and first island different to last, trip is not complete
    is.na(tripID) ~ NA)) %>% #if tripID is zero, NA
  dplyr::select(deployID, tripID, tripcomplete)

# Add GPStripsummary data to original GPStrips
GPSfinal <- GPStrips %>%
  left_join(., GPStripsummary, by = c("deployID", "tripID")) %>% #add complete trip data (by deployID and tripID)
  relocate(tripID, tripcomplete, .after = island) %>% #move trip columns to right
  relocate(Nest_Lon, Nest_Lat, .after = Lat) %>% #move nest columns left
  dplyr::select(-c(geometry)) #drop the geometry column

## Save GPStripcomplete ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(GPSfinal, file=here(output_dir,"GPS_final.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPStrips,GPStripsummary,GPSfinal,islands500,islands1500)
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 7: Merge TDR/GPS data ===========================================================================================================================================================================================

#If you're running out of vector memory, we can increase the amount of disk memory used:
#library(usethis) #package with some handy functions
#usethis::edit_r_environ() #using this function to edit the .Renviron file
#"R_MAX_VSIZE=32Gb" <<< add this '(without quotations) to the .Renviron file that appears, and save

## Load in final versions of both datasets -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
future::plan(multisession, workers = 4) #Reduce number of workers back to one to reduce resource hogging (no benefit of extra CPUs for joining data *I think*)

# Read TDR data
TDRfinal <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID) #rename trip ID column

# Read GPS data — interpolate to 1 sec for merging
GPSfinal <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int) #Keeping track of real GPS points

## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
message("AP pipeline: ","TDR data has ",length(unique(TDRfinal$deployID))," individuals, GPS has ",length(unique(GPSfinal$deployID)),
        "\n  ",sum(unique(TDRfinal$deployID) %in% unique(GPSfinal$deployID))," individuals in TDR dataset have GPS data ready to merge",
        "\n  ",sum(!unique(TDRfinal$deployID) %in% unique(GPSfinal$deployID))," individuals in TDR have no GPS",
        "\n  ",sum(!unique(GPSfinal$deployID) %in% unique(TDRfinal$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_merged <- 
  left_join(x = TDRfinal, y = GPSfinal, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_merged, file=here(output_dir,"TDRGPS_merged.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinal,TDRfinal,TDRGPS_merged)
invisible(gc()) #quietly call garbage collection to free up ram

#Specify subsampling (find a way to keep mostly 'real' GPS rows?)
df <- readRDS(file=here(output_dir,"TDRGPS_merged.RDS"))


# STEPS STILL TO CODE ==================================================================================================================================================================================================

# Do we want a GPS-centric dataset? (at the moment it is TDR-centric — some GPS rows might be lost if devices run longer than TDR)


# DATA VERIFICATION / OTHER CODE SNIPPETS ==============================================================================================================================================================================

##Check variables names are correct (mutate ones that are close and flag any remaining)
##Check format for date of all files (timezone??)

#Finding all NA's in the data
#GPSclean %>% filter(if_any(everything(), is.na))

#Check if a dataset has duplicated rows (for if you get the dreaded 'can't be duplicate values' error from diveMove)
#setDT(TDRdives, key = c("deployID", "DateTime")) #TDRdives in the name of the object
#TDRdives[, N := .N, by = key(TDRdives)]          # count rows per group
#TDRdives[N > 1]

##Check deployment ID's are all correct (and give warning if not)
#if(sum(is.na(metadata$deployID)|metadata$deployID== '') >1) {
#  warning(paste("There are",(sum(is.na(metadata$deployID)|metadata$deployID== '')),
#                "rows with invalid deployment ID's:",
#                str_flatten((which(is.na(metadata$deployID)|metadata$deployID== '')),collapse=", ")))
#} else {print("All deployments good")}

## Check sampling interval
#getmode <- function(v) {
#uniqv <- unique(v)
#uniqv[which.max(tabulate(match(v, uniqv)))]}

##Check sampling interval (tidyr)
#test <- GPSdata %>% subset(deployID=="01_2010R") %>%
#mutate(Diff = parse_time(Time) - lag(parse_time(Time)))
#getmode(test2$Diff)





#### ALL DATA ####
# Declare input/output directories (because we're using the 'here' function these shouldn't need changing)
input_dir <- here("E:/Chapter 3 Fishery closures/raw_penguin") #top location of raw files)
output_dir <- here("E:/Chapter 2 HMM/processed_data/ALL/") #location to save processed files)

# List paths of all TDR files and read in metadata file
TDRfiles <- fs::dir_ls(input_dir, glob = "*tdr*.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name
metadata <- read_csv(here("E:/Chapter 3 Fishery closures/metadata_MASTER.csv")) # read the metadata_MASTER file (should be put in raw_data folder)

# If whole script has already been run (required at first use), then start from whichever .RDS file is required:
#1. TDR_clean.RDS - TDR data combined from multiple csv files and cleaned
#2. TDR_processed.RDS - TDR data that has be filtered and calibrated with diveMove
#3. TDR_final.RDS - TDR data with trips detected
#4. TDR_summary.RDS - dive summaries of TDR_final by deployID

# STEP 1: Read in and clean TDR data ===================================================================================================================================================================================

## Read in TDR files with deployID extracted from filename -------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRdata <- TDRfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>%  #read in TDR files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_tdr", replacement=""))) %>% #extract birdID by removing tdr from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

## Create dataframe for deployIDs with 'Depth' only  -------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRdepth <- TDRdata %>%
  filter(!is.na(Depth)) %>% #remove rows where Depth *is not* already converted (i.e. Depth is not NA)
  mutate(decimals = nchar(str_split(as.character(Depth), pattern = "\\.", n=2, simplify=TRUE)[,2])) %>% #count number of decimal places (as characters)
  group_by(deployID) %>% #group by deployID ready for mean decimal calculations
  mutate(Pressureformat = case_when( #create Pressureformat label using mean number of decimal places for each deployID
    max(decimals) == 1 ~ "mbar", #.1 gets labelled as "mbar"
    max(decimals) == 2 ~ "bar", #.01 as "bar"
    max(decimals) == 3 ~ "dbar")) %>% #.001 as "dbar"
  dplyr::select(-c(decimals)) %>% #remove decimal column (not needed anymore)
  mutate(Pressureformat = case_when( #manually edit Pressureformat for some IDs (decimal places missing in raw files)
    deployID == "07_2014D" ~ "bar",
    deployID == "11_2014R" ~ "bar",
    TRUE ~ as.character(Pressureformat) )) %>%
  mutate(across(c("Temp", "Depth"), as.numeric)) %>% #change Temperature and Depth to numeric
  group_by(deployID) %>% #group again in case data was ungrouped in previous step
  mutate(Depth = case_when( #convert dbar/mbar to bar (Depth in m) using correct conversions and calibrations
    Pressureformat == "bar" ~ Depth,
    Pressureformat == "dbar" ~ Depth*10,
    Pressureformat == "mbar" ~ (Depth-min(Depth))/100 )) #calibrate Depth for Depth=mbar files (check not done already)

## Create dataframe for deployIDs with 'Pressure' only -----------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRpressure <- TDRdata %>%
  filter(is.na(Depth)) %>% #remove rows where Depth *is* already converted (i.e. Depth is NA)
  mutate(decimals = nchar(str_split(as.character(Pressure), pattern = "\\.", n=2, simplify=TRUE)[,2])) %>% #count number of decimal places (as characters)
  group_by(deployID) %>% #group by deployID ready for mean decimal calculations
  mutate(Pressureformat = case_when( #create Pressureformat label using mean number of decimal places for each deployID
    max(decimals) == 1 ~ "mbar", #.1 gets labelled as "mbar"
    max(decimals) == 2 ~ "bar", #.01 as "bar"
    max(decimals) == 3 ~ "dbar")) %>% #.001 as "dbar"
  dplyr::select(-c(decimals)) %>% #remove decimal column (not needed anymore)
  mutate(across(c("Temp", "Pressure", "Depth"), as.numeric)) %>% #change Temperature and Pressure to numeric
  group_by(deployID) %>% #group again in case data was ungrouped in previous step
  mutate(Depth = case_when( #convert dbar/mbar to bar (Depth in m) using correct conversions and calibrations
    Pressureformat == "bar" ~ Pressure,
    Pressureformat == "dbar" ~ Pressure*10,
    Pressureformat == "mbar" ~ (Pressure-min(Pressure))/100 )) #calibrate mbar TDR using lowest Pressure reading (per group)

## Merge TDRdepth and TDRpressure --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRall <- as_tibble(rbindlist(list(TDRdepth, TDRpressure))) #merge the two TDR datasets into one tibble (rbindlist is fastest)
rm(TDRpressure,TDRdepth, TDRdata, TDRfiles) #drop the big datasets created, and keep only TDRall

# Extract TDRtag type from metadata ready to add to TDRclean
TDR_tags <- metadata %>% dplyr::select(deployID, TDR_tag)

# Do some final cleaning, tidying and removal of unnecessary columns
TDRclean <- TDRall %>%
  filter(Depth < 150)  %>% #filter out extreme Depth values > 150 (e.g. weird end of many files)
  mutate(DateTime = paste(Date,Time, sep=" ")) %>% #Make DateTime column
  mutate(DateTime = dmy_hms(DateTime)) %>% #Format DateTime column
  dplyr::select(DateTime, Temp, Depth, deployID, Pressureformat) %>% #Keep DateTime, Temp, Depth, DeployID, Pressureformat
  left_join(., TDR_tags, by="deployID") %>% #join TDR_tag info to full dataset
  mutate(Depth = case_when(deployID == "08_2010R" ~ Depth-15, #manually adjust Depth for one very weird file
                           TRUE ~ as.numeric(Depth))) #do nothing to all other rows
rm(TDRall) #drop TDRall dataframe

# Check there are no NAs or duplicates in TDR data (duplicated might take a while time to check)
if(sum(colSums(is.na(TDRclean))) >1) {message(paste("AP pipeline:There are",(sum(colSums(is.na(TDRclean)))),"NA values in TDR data "))
} else {message("AP pipeline: No NA's in TDR data")}
if(sum(anyDuplicated(dplyr::select(TDRclean[grep("2022",TDRclean$deployID),],deployID, DateTime)))>1) {message(paste("AP pipeline: duplicates detected in 2022 TDR data"))
} else {message("AP pipeline: no duplicates in 2022 TDR data!")}

## Save TDRclean--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRclean, file = here(output_dir,"TDR_cleaned.RDS"), compress="xz") #save as RDS file (loaded with 'readRDS')
#saveRDS(TDRclean, file = here(output_dir,"TDR_cleaned_zip.RDS"), compress="xz") #save as RDS with xz compression (~.25 original size but slower save/load)
beep(5) #actual fanfare noises for finishing this step!

# Tidy up after saving
rm(TDR_tags,TDRclean)


# STEP 2: Processing TDR data with diveMove ============================================================================================================================================================================

## Specify files to remove from further analyses (no dives or bad data) ------------------------------------------------------------------------------------------------------------------------------------------------
remove_files <- c("02_2017R", #no dives
                  "24_2017R", #no dives
                  "05_2018R") #bad logger (100 rows of datetimes at 09:00:48)
message("AP pipeline: Removing all data for ",length(remove_files)," TDR files with no dives/bad data (",toString(remove_files),")")

## Read in TDRclean, remove bad files and split into dataframe per deployID   ------------------------------------------------------------------------------------------------------------------------------------------
TDRdives <- readRDS(here(output_dir, "TDR_cleaned.RDS")) %>% #read in TDRclean RDS file
  filter(!(deployID %in% remove_files)) %>% #filter to remove files deployID
  group_by(deployID) %>% group_split(.) #group and split by deployID ready for diveMove processing

## DIVEMOVE VARIABLES FOR TDR CALIBRATION (recursive filtering for zero offset correction — per Luque & Fried 2011) ----------------------------------------------------------------------------------------------------
depth.bounds <- c(-5, 5) #this is the range of where we expect the surface to be found
probs <- c(0.5, 0.3) #running quantiles for each of the two steps used (probs[1] is 1st smoothing step, probs[2] is 2nd filtering step)
K <- c(2,360) #window size for each step in seconds (e.g K[1] is window size of 1st step, K[2] is window size of 2nd step)
dive.threshold <- 5 #Depth below which anything should be considered a dive

## Run recursive filtering for zero offset correction (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015850))   -----------------------------------------------------------------------------------
future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
TDRcalibrated <- TDRdives %>% #Create and calibrate each TDR object using recursive filtering
  future_map(~ diveMove::createTDR(time=.x$DateTime, depth=.x$Depth, dtime=1, file=.x$deployID[1]),.progress=TRUE) %>% #create diveMove object with deployID as source file name
  future_map(~ diveMove::calibrateDepth(.x, dive.thr=dive.threshold, zoc.method="filter",k=K, probs=probs, depth.bounds = depth.bounds, na.rm=TRUE),.progress=TRUE)

## Extract key data columns from each of the TDRcalibrated s4 diveMove objects and cbind  ------------------------------------------------------------------------------------------------------------------------------
deployIDs <- TDRcalibrated %>% map(~ as_tibble(.x@tdr@file)) #extracts the deployID for each dataframe
depth <- TDRcalibrated %>% map(~ as_tibble(.x@tdr@depth)) #extracts the calibrated depth
diveID <- TDRcalibrated %>% map(~ as_tibble(.x@dive.activity[["dive.id"]])) #extract the diveID number
divephase <- TDRcalibrated %>% map(~ as_tibble(.x@dive.phases)) #extracts the dive phases for each dive
TDRextracted <- Reduce(function(x,y) Map(cbind, x, y),list(deployIDs,depth,diveID,divephase)) #cbind each of the lists together with an anonymous function
rm(deployIDs,depth,diveID,divephase) #remove all the elements extracted
invisible(gc()) #quietly call garbage collection to free up ram

## Combine to create the processed TDR dataframe -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRprocessed <- Reduce(function(x,y) Map(cbind, x, y),list(TDRdives,TDRextracted)) %>%
  rbindlist(.) %>% as_tibble(., .name_repair = make.unique) %>% #bind lists together and repair names
  dplyr::select(-value) %>%
  rename(Calibrated_depth = value.1, DiveID = value.2, Divephase = value.3) %>%
  dplyr::select(DateTime, Temp, Depth, Calibrated_depth, DiveID, Divephase, deployID, Pressureformat, TDR_tag)

## Save TDRprocessed ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRprocessed, file= here(output_dir,("TDR_processed.RDS"))) #Save as an RDS object (load this later with 'readRDS' function)
beep(5) #more fanfare noises

## Tidy up after saving
rm(TDRcalibrated,TDRdives,TDRextracted,TDRprocessed, remove_files, K, depth.bounds,probs,dive.threshold)


#STEP 3: Despiking and Trip detection ==================================================================================================================================================================================
# First: remove spikes from a select number of files
# Second: reprocess the entire processed dataset with diveMove again and extract just the trips to stitch back onto the original 

# List of deployID's that show the weird spikes/aka multiple zero level problem
despike_list <- c("01_2008D", "03_2008D", "04_2008D", "05_2008D")

# Despiking process (basically a TDR speed filter)
TDRdespike <- readRDS(here(output_dir,("TDR_processed.RDS"))) %>% #read in TDRprocessed file
  group_by(deployID) %>% #group by deployID
  filter(deployID %in% despike_list) %>% #filter to only the deployIDs in despike_list
  mutate(lead = lead(Calibrated_depth,1)-Calibrated_depth) %>% #compute change in depth from preceding row
  mutate(lag = lag(Calibrated_depth,1)-Calibrated_depth) %>% #compute change in depth to next row
  mutate(change = abs(lead) + abs(lag)) %>% #absolute change in depth +/- 1 row
  mutate(Calibrated_depth = case_when( #use absolute change to detect spikes and revert those to NA's
    change > 9 ~ NA_real_, #paste NA when absolute change is more that 9 (e.g. like 5m down, then 5m up in two seconds)
    change <= 9 ~ Calibrated_depth, #leave as calibrated depth when absolute change is less than 9
    change == NA ~ Calibrated_depth, #Also leave examples where change is NA (at beginning and end of dataframe)
    TRUE ~ as.numeric(Calibrated_depth))) %>% #Everything else that I've forgotten about leave as calibrated depth
  mutate(Calibrated_depth = na_interpolation(Calibrated_depth,option="linear")) %>% #impute linear interpolation for despiked points (now NA's)
  dplyr::select(-c(lead,lag,change))

# rbind the despiked dataset back onto the original dataset
TDRnospike <- readRDS(here(output_dir,("TDR_processed.RDS"))) %>% #read in the same TDRprocessed file
  filter(!deployID %in% despike_list) #filter out the opposite set of deployments (i.e. those without spike problems)
TDRprocessed <- rbind(TDRdespike,TDRnospike) #bind the two datasets together

#Tidy up after saving
rm(TDRnospike,TDRdespike,despike_list)
invisible(gc()) #quietly call garbage collection to free up ram

## DIVEMOVE VARIABLES FOR TRIP PHASE DETECTION -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# currently using: 4, 3hrs, 3mins
depth_thr <- 4 #threshold below which TDR values 'could' be classified as dry (hacky - only using for diveMove trip phase detection)
dry_thr <- (3600*3) #dry error threshold in seconds (nseconds*hours). Dry phases shorter than this threshold will be considered as wet (see ?calibrateDepth)
wet_thr <- (60*3)  #wet threshold in seconds (nseconds*minutes). At-sea phases shorter than this threshold will be considered as trivial wet (see ?calibrateDepth)
trip_thr <- 60*15 #minimum threshold for trip length in seconds (i.e. remove trips shorter than 15 minutes)

# Get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
future::plan(multisession, workers = 7) 

## Using despiked dataset, use .detPhase to detect trip phases ---------------------------------------------------------------------------------------------------------------------------------------------------------
TDRalltrips <- TDRprocessed %>%
  mutate(wet_cond = case_when( #create a new column called wet_cond (for diveMove::calibrateDepth to detect trips)
    Calibrated_depth < depth_thr ~ FALSE, #when calibrated depth less than depth threshold, classify "wet_cond" as dry (FALSE)
    Calibrated_depth >= depth_thr ~ TRUE)) %>% #when calibrated depth more than depth threshold, classify "wet_cond" as wet (TRUE) 
  group_by(deployID) %>% group_split(.) %>% #group by deployID and split ready for diveMove processing
  future_map(~ diveMove:::.detPhase(time=.x$DateTime, depth=.x$Calibrated_depth, interval=1, wet.cond = .x$wet_cond, dry.thr=dry_thr, wet.thr=wet_thr),.progress=TRUE)

# Extracts only the tripID per row for each dataframe (this is the only thing we care about for trip detection)
tripID <- map(TDRalltrips, "phase.id") %>% #extract phase id from elements created by detphase
  map(~ as_tibble(.x)) #convert to tibble ready for recombining with full dataset
rm(TDRalltrips) #tidy up after extracting tripID

# Stitch tripID for each deployment back onto despiked TDRprocessed object
TDRprocessed <- TDRprocessed %>%
  group_by(deployID) %>% group_split(.) #group by deployID and split
TDRtrips <- Reduce(function(x,y) Map(cbind, x, y),list(TDRprocessed,tripID)) %>%
  rbindlist(.) %>% as_tibble(.) %>% #rbind the list of dataframes back together as one tibble
  rename(tripID = value) #rename the trip column (currently called 'value') to tripID

# Tidy up after stitching
rm(TDRprocessed,tripID) #tidy up after combining TDRprocessed and tripID
invisible(gc()) #quietly call garbage collection to free up ram

# Process data using tripID to remove all trip phases where SD is less than 1 or shorter than threshold, and rerank starting at 1
TDRfinal <- TDRtrips %>% 
  group_by(deployID,tripID) %>% #group by deployID and tripID
  summarise(sd=sd(Calibrated_depth), length=length(tripID)) %>% #for each trip of deploymentID, create a summary of standard deviation and length
  left_join(TDRtrips,.) 
rm(TDRtrips)
TDRfinal <- TDRfinal %>% 
  mutate(trip_num = case_when( #when SD depth for group is higher than 1, change group trip number
    length < trip_thr | sd < 1 ~ 0, #when trip length is below threshold -OR- SD is less than 1, change tripID to zero (does not count as a trip)
    sd >= 1 ~ tripID )) %>% #if sd exceeds 1, keep tripID (counts as a trip)
  mutate(trip_num = na_if(trip_num, 0)) %>% #change all the zeros to NA's (to help avoid problems with plotting later)
  ungroup() %>% group_by(deployID) %>% #remove previous grouping and focus only on deployID
  mutate(trip_num = dense_rank(trip_num)) %>% #create new rank using remaining trip_num (e.g. 1,1,4,4,6,6 becomes 1,1,2,2,3,3)
  dplyr::select(-c(sd,length,tripID)) %>% #drop sd, length, and tripID 
  rename(tripID = trip_num)

## Save TDRfinal -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRfinal, file=here(output_dir,"TDR_final.RDS")) #Save as an RDS object (load with 'readRDS')
beep(8) #final fanfare noises!

# Tidy up after saving
rm(TDRfinal,TDRtrips, depth_thr, dry_thr, wet_thr, trip_thr)
invisible(gc()) #quietly call garbage collection to free up ram

# STEP 4: Extraction of dive summaries for each deployID (1st trip only) ===============================================================================================================================================

## Read in Metadata_MASTER and filter columns (all individuals with biometrics have a sex classified)
metadata_sex <- read_csv(file = here("E:/Chapter 2 HMM/Metadata_MASTER.csv")) %>%
  dplyr::select(deployID, Sex_0.5, Year, Closure, Colony) %>% rename(Sex = Sex_0.5)

## Read in cleaned and processed TDR file, and create list of deployIDs  -----------------------------------------------------------------------------------------------------------------------------------------------
TDRpost <- readRDS(here(output_dir, "TDR_final.RDS")) %>% #read in the TDRfinal file
  replace_na(list(tripID = 0)) %>% #replace the na's in trip number with zeroes
  mutate(tripID = as_factor(tripID)) %>% #mutate the tripID column into a factor for plotting
  left_join(., metadata_sex, by="deployID") %>% #add metadata (Sex) to TDRdataset
  ungroup()

## Filter data for first trips and summarise using diveMove::diveStats   -----------------------------------------------------------------------------------------------------------------------------------------------
future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)

## Reprocess the data for first trip with diveMove (now using no offset or any corrections) to detect individual dives   -----------------------------------------------------------------------------------------------
TDRreprocess <- TDRpost %>%
  filter(tripID == 1) %>% #remove individuals without sex and subset to dives from first trip
  ungroup() %>% group_by(deployID) %>% group_split(.) %>% #group and split by deployID ready for diveMove processing
  future_map(~ diveMove::createTDR(time=.x$DateTime, depth=.x$Calibrated_depth, dtime=1, file=.x$deployID[1]),.progress=TRUE) %>% #create diveMove object with deployID as source file name
  future_map(~ diveMove::calibrateDepth(.x, dive.thr=5, zoc.method="offset",offset=0, na.rm=TRUE),.progress=TRUE)

## Loop to that creates a diveMove summary datafile for all deployID (hate loops but couldn't find a better way to do it)  ---------------------------------------------------------------------------------------------
templist = list() #initialise list for loop

for (i in 1:length(TDRreprocess)) {
  temp <- diveMove::diveStats(TDRreprocess[[i]])
  temp$deployID <- (TDRreprocess[[i]]@tdr@file)
  temp$diveID <- seq.int(nrow(temp)) 
  templist[[i]] <- temp
}
TDRsummary <- data.table::rbindlist(templist) %>% #rbind output of the loop into a dataframe
  left_join(., metadata_sex, by="deployID") #add sex information

## Save dive stat summary file   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRsummary, file=here(output_dir,"TDR_summary.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after stitching
rm(TDRpost, TDRreprocess, temp, i ,templist, TDRsummary) #tidy up after combining TDRprocessed and tripID
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 5: Read in raw GPS data =========================================================================================================================================================================================

# List paths of all GPS files
GPSfiles <- fs::dir_ls(input_dir, glob = "*gps*.csv", type="file", recurse = TRUE) # list location of all files with "gps" in the name

## Read in GPS files and extract deployID from filename ----------------------------------------------------------------------------------------------------------------------------------------------------------------
GPSdata <- GPSfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>% #read in GPS files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_gps", replacement="")),.before=Date) %>% #extract birdID by removing gps from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

## Create datetime column and clean up dataframe -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
GPSclean <- GPSdata %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>% #Create DateTime column
  drop_na(.) %>% #remove rows with NA's (only present in newly-created DateTime column)
  mutate(across(c(Lat,Lon),as.numeric)) %>% #turn lat/lon into numeric
  dplyr::select(-c(filename)) #remove filename column
message("AP pipeline: Ignore 15 failed to parse warning above (some bad dates starting with 00 e.g. '00-01-17')")

## Filter out based on time jumps (> 1 second & < 2 days) --------------------------------------------------------------------------------------------------------------------------------------------------------------
GPStimefilter <- GPSclean %>%
  group_by(deployID) %>%  #group by deployID
  arrange(DateTime) %>% #arrange by DateTime (defaults to descending)
  mutate(diff_secs = as.duration(DateTime - lag(DateTime)), #calculate difference in time to previous row (output as duration)
         diff_long = ifelse(diff_secs > days(2),T,F), #convert diff_secs into a logical threshold for >2 days
         diff_short = ifelse(diff_secs < seconds(1),T,F)) %>% #convert diff_secs into a logical threshold for <1 sec
  replace_na(list(diff_long = FALSE, diff_short=FALSE)) %>% #convert NA's in threshold col to FALSE
  filter(cumsum(diff_long)<1) %>% #remove all rows of data after a time jump greater that 2 days
  filter(diff_short == FALSE) %>% #remove rows where time jump is less than 1 second
  dplyr::select(-c(diff_long, diff_short))

message("AP pipeline: ",nrow(GPSclean)-nrow(GPStimefilter)," GPS rows removed with our time jump filter (", 
        round((nrow(GPSclean)-nrow(GPStimefilter))/nrow(GPSclean)*100,digits=2),"% of data)")

## Filter out based on speed travelled from previous point & to next point (>12.4 km/h or 3.444 m/s) -------------------------------------------------------------------------------------------------------------------
GPSspeedfilter <- GPStimefilter %>%
  left_join(., (metadata %>% dplyr::select(deployID, Nest_Lat, Nest_Lon)), by = "deployID") %>% #add nest lat/lon to data
  ungroup() %>%
  mutate(geometry_GPS = st_transform(st_as_sf(., coords=c("Lon","Lat"), crs=4326), crs = 4326)$geometry, # assign GPS geometry and transform to lon lat for dist calcs
         geometry_CP = st_transform(st_as_sf(., coords=c("Nest_Lon","Nest_Lat"), crs=4326), crs = 4326)$geometry) %>% #assign nest geometry and transform to lon lat for dist calcs
  group_by(deployID) %>%
  mutate(diff_sec_prev = as.duration(DateTime - lag(DateTime)),
         diff_sec_next = as.duration(lead(DateTime) - DateTime),
         distance_prev = st_distance(geometry_GPS, lag(geometry_GPS), by_element = T), #distance travelled from previous point
         distance_next = st_distance(geometry_GPS, lead(geometry_GPS), by_element = T), #distance traveled to next point
         speed_msec_prev = as.numeric(distance_prev)/as.numeric(diff_sec_prev), #calculate speed moving from previous point (distance/time)
         speed_msec_next = as.numeric(distance_next)/as.numeric(diff_sec_next), #calculate speed moving to next point (distance/time)
         nestdist = st_distance(geometry_GPS, geometry_CP, by_element = T),.after=diff_secs) %>% #calculate distance between nest and current GPS location
  filter(speed_msec_prev < 12.4*(5/18) & speed_msec_next < 12.4*(5/18)) %>%  #speed filter - remove any point where the speed is over 12.4 km/h (Wilson 1986) 5/18 converts it into m/s
  ungroup() %>% #ungroup before final cleanup
  dplyr::select(-c(geometry_GPS, geometry_CP,diff_sec_prev,diff_sec_next,distance_prev,distance_next,speed_msec_next)) %>% #remove geometries and extra columns
  rename(speed_msec = speed_msec_prev) #rename the speed prev column

message("AP pipeline: ",nrow(GPStimefilter)-nrow(GPSspeedfilter)," GPS rows removed with our speed filter (", 
        round((nrow(GPStimefilter)-nrow(GPSspeedfilter))/nrow(GPStimefilter)*100,digits=2),"% of data)")

## Save GPS data -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(GPSspeedfilter, file=here(output_dir,"GPS_filter.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfiles,GPSdata,GPSclean,GPStimefilter,GPSspeedfilter)
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 6: Trip detection from GPS  =====================================================================================================================================================================================

## Use GPS data to classify trips in and out of island buffer ----------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in a map files of islands (including a buffer to account for inaccuracy in gps devices
islands500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_buffer(., dist = 500) %>% # 500m buffer in case of noisy GPS data
  st_transform(4326) #transform back to latlon
#st_crs(islands500, parameters = TRUE)$units_gdal #find units used for any named sf object
#sf_use_s2(TRUE) #if you get spammed with warnings from st_intersects below, you need to switch s2 back on

#Define trips as contiguous rows spent outside the island buffer (using rleid and denserank)
GPStrips <- readRDS(here(output_dir,"GPS_filter.RDS")) %>% #read in the GPSfilter file from step 5
  st_as_sf(coords = c('Lon', 'Lat'), crs=4326, remove=FALSE) %>% #add geometry column for analyses with sf
  mutate(intersection = as.integer(st_intersects(geometry, islands500)), #is the point intersection with an island (number)
         island = if_else(is.na(intersection), 'NA', islands500$name[intersection])) %>% #add the name of the island
  mutate(island = str_remove_all(island, pattern = c(" Island"))) %>% #remove "island" suffix from island name
  group_by(deployID) %>% arrange(DateTime) %>% 
  mutate(TripOrColony = ifelse(is.na(intersection), "trip", "colony"), #define as trip/colony based on presence/absence of NA
         trips_all = as.numeric(rleid(TripOrColony)), #number trips based on changes in TripOrColony (e.g.colony,trip,trip,trip,colony -becomes- 1,2,2,2,3)
         trip_npoints = rep(rle(trips_all)$lengths, rle(trips_all)$lengths), #length of trip (in rows)
         trips_only = case_when( #create case_when to define if a numbered trip should count as a trip
           TripOrColony == "colony" ~ NA_real_, #in colony, so does not count as a trip
           TripOrColony == "trip" ~ trips_all, #outside colony, so leave as numbered trip
           trip_npoints <= 1 ~ NA_real_), #ignore trips lasting one point (probably bad GPS)
         tripID = dense_rank(trips_only)) %>% #create new rank w/ trip_num (e.g. 1,1,3,3,5,5 becomes 1,1,2,2,3,3)
  dplyr::select(-c(intersection,TripOrColony,trips_all,trips_only,trip_npoints)) #remove extra columns

## Classify complete/incomplete trips ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.5km buffer area (home range) to account for porpoising 
islands1500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_buffer(., dist = 1500) %>% # 500m buffer in case of noisy GPS data
  st_transform(4326) #transform back to latlon

# Find if points intersect with islands1500 and summarise
GPStripsummary <- GPStrips %>% #use GPStrips object with numbered trip IDs
  mutate(homeintersection = as.integer(st_intersects(geometry, islands1500)), #is the point intersection with an island1500 (add island number)
         homeisland = if_else(is.na(homeintersection), 'NA', islands1500$name[homeintersection])) %>% #if intersection, add island name
  st_drop_geometry() %>%
  group_by(deployID, tripID) %>% arrange(DateTime) %>% 
  summarise(first = first(homeintersection), #summarise first row of each homeintersection (i.e. island number if intersecting)         
            last = last(homeintersection)) %>% #summarise last row of each homeintersection (i.e. island number if intersecting)
  replace_na(list(first = 0, last = 0))  %>% #replace NA's in first/last column with zeroes (easier to deal with for detecting tripcomplete)
  mutate(tripcomplete = case_when( #if trip starts and finishes inside same island area then it's a complete trip
    first==0|last==0 ~ FALSE, #if island number is zero, trip is not complete
    !is.na(tripID) & first==last ~ TRUE, #if trip is not NA and first island same as last, trip is complete
    !is.na(tripID) & first!=last ~ FALSE, #if trip is not NA and first island different to last, trip is not complete
    is.na(tripID) ~ NA)) %>% #if tripID is zero, NA
  dplyr::select(deployID, tripID, tripcomplete)

# Add GPStripsummary data to original GPStrips
GPSfinal <- GPStrips %>%
  left_join(., GPStripsummary, by = c("deployID", "tripID")) %>% #add complete trip data (by deployID and tripID)
  relocate(tripID, tripcomplete, .after = island) %>% #move trip columns to right
  relocate(Nest_Lon, Nest_Lat, .after = Lat) %>% #move nest columns left
  dplyr::select(-c(geometry)) #drop the geometry column

## Save GPStripcomplete ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(GPSfinal, file=here(output_dir,"GPS_final.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPStrips,GPStripsummary,GPSfinal,islands500,islands1500)
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 7: Merge TDR/GPS data ===========================================================================================================================================================================================

#If you're running out of vector memory, we can increase the amount of disk memory used:
#library(usethis) #package with some handy functions
#usethis::edit_r_environ() #using this function to edit the .Renviron file
#"R_MAX_VSIZE=32Gb" <<< add this '(without quotations) to the .Renviron file that appears, and save
output_dir <- here("E:/Chapter 2 HMM/processed_data/ALL/") #location to save processed files)

## Load in final versions of both datasets -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
future::plan(multisession, workers = 6) #Reduce number of workers back to one to reduce resource hogging (no benefit of extra CPUs for joining data *I think*)

# Read TDR data
TDRfinalp1 <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year < 2014)  %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID)  #rename trip ID column


# Read GPS data — interpolate to 1 sec for merging
GPSfinalp1 <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year < 2014)  %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int)  #Keeping track of real GPS points


## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
message("AP pipeline: ","TDR data has ",length(unique(TDRfinalp1$deployID))," individuals, GPS has ",length(unique(GPSfinalp1$deployID)),
        "\n  ",sum(unique(TDRfinalp1$deployID) %in% unique(GPSfinalp1$deployID))," individuals in TDR dataset have GPS data ready to merge",
        "\n  ",sum(!unique(TDRfinalp1$deployID) %in% unique(GPSfinalp1$deployID))," individuals in TDR have no GPS",
        "\n  ",sum(!unique(GPSfinalp1$deployID) %in% unique(TDRfinalp1$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_mergedp1 <- 
  left_join(x = TDRfinalp1, y = GPSfinalp1, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_mergedp1, file=here(output_dir,"TDRGPS_merged2008_2013.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinalp1,TDRfinalp1,TDRGPS_mergedp1)
invisible(gc()) #quietly call garbage collection to free up ram


# Read TDR data
TDRfinalp2 <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2013) %>%
  filter(Year < 2015) %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID) #rename trip ID column
gc()

# Read GPS data — interpolate to 1 sec for merging
GPSfinalp2 <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2013) %>%
  filter(Year < 2015) %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int)  #Keeping track of real GPS points
gc()
## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
message("AP pipeline: ","TDR data has ",length(unique(TDRfinalp2$deployID))," individuals, GPS has ",length(unique(GPSfinalp2$deployID)),
        "\n  ",sum(unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR dataset have GPS data ready to merge",
        "\n  ",sum(!unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR have no GPS",
        "\n  ",sum(!unique(GPSfinalp2$deployID) %in% unique(TDRfinalp2$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_mergedp2 <- 
  left_join(x = TDRfinalp2, y = GPSfinalp2, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_mergedp2, file=here(output_dir,"TDRGPS_merged2014.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinalp2,TDRfinalp2,TDRGPS_mergedp2)
invisible(gc()) #quietly call garbage collection to free up ram

# Read TDR data
TDRfinalp3 <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2014) %>%
  filter(Year < 2016) %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID) #rename trip ID column
gc()

# Read GPS data — interpolate to 1 sec for merging
GPSfinalp3 <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2014) %>%
  filter(Year < 2016) %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int)  #Keeping track of real GPS points
gc()
## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
#message("AP pipeline: ","TDR data has ",length(unique(TDRfinalp2$deployID))," individuals, GPS has ",length(unique(GPSfinalp2$deployID)),
#        "\n  ",sum(unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR dataset have GPS data ready to merge",
#        "\n  ",sum(!unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR have no GPS",
#        "\n  ",sum(!unique(GPSfinalp2$deployID) %in% unique(TDRfinalp2$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_mergedp3 <- 
  left_join(x = TDRfinalp3, y = GPSfinalp3, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_mergedp3, file=here(output_dir,"TDRGPS_merged2015.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinalp3,TDRfinalp3,TDRGPS_mergedp3)
invisible(gc()) #quietly call garbage collection to free up ram
#

# Read TDR data
TDRfinalp4 <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2015) %>%
  filter(Year < 2018) %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID) #rename trip ID column
gc()

# Read GPS data — interpolate to 1 sec for merging
GPSfinalp4 <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2015) %>%
  filter(Year < 2018) %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int)  #Keeping track of real GPS points
gc()
## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
#message("AP pipeline: ","TDR data has ",length(unique(TDRfinalp2$deployID))," individuals, GPS has ",length(unique(GPSfinalp2$deployID)),
#        "\n  ",sum(unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR dataset have GPS data ready to merge",
#        "\n  ",sum(!unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR have no GPS",
#        "\n  ",sum(!unique(GPSfinalp2$deployID) %in% unique(TDRfinalp2$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_mergedp4 <- 
  left_join(x = TDRfinalp4, y = GPSfinalp4, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_mergedp4, file=here(output_dir,"TDRGPS_merged2016-2017.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinalp4,TDRfinalp4,TDRGPS_mergedp4)
invisible(gc()) #quietly call garbage collection to free up ram
#


# Read TDR data
TDRfinalp5 <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2017) %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID) #rename trip ID column
gc()

# Read GPS data — interpolate to 1 sec for merging
GPSfinalp5 <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  mutate(Year = format(DateTime, format = "%Y")) %>%
  filter(Year > 2017) %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int)  #Keeping track of real GPS points
gc()
## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
#message("AP pipeline: ","TDR data has ",length(unique(TDRfinalp2$deployID))," individuals, GPS has ",length(unique(GPSfinalp2$deployID)),
#        "\n  ",sum(unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR dataset have GPS data ready to merge",
#        "\n  ",sum(!unique(TDRfinalp2$deployID) %in% unique(GPSfinalp2$deployID))," individuals in TDR have no GPS",
#        "\n  ",sum(!unique(GPSfinalp2$deployID) %in% unique(TDRfinalp2$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_mergedp5 <- 
  left_join(x = TDRfinalp5, y = GPSfinalp5, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_mergedp5, file=here(output_dir,"TDRGPS_merged2018-2019.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinalp5,TDRfinalp5,TDRGPS_mergedp5)
invisible(gc()) #quietly call garbage collection to free up ram
#


#############2022 data ######################


# Declare input/output directories (because we're using the 'here' function these shouldn't need changing)
input_dir <- here("E:/Deployments Robben 2022/processed_tdr/test") #top location of raw files)
output_dir <- here("E:/Deployments Robben 2022/processed_data/") #location to save processed files)

# List paths of all TDR files and read in metadata file
TDRfiles <- fs::dir_ls(input_dir, glob = "*tdr*.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name
metadata <- read_csv(here("E:/Deployments Robben 2022/R codes/Metadata.csv")) # read the metadata_MASTER file (should be put in raw_data folder)

# If whole script has already been run (required at first use), then start from whichever .RDS file is required:
#1. TDR_clean.RDS - TDR data combined from multiple csv files and cleaned
#2. TDR_processed.RDS - TDR data that has be filtered and calibrated with diveMove
#3. TDR_final.RDS - TDR data with trips detected
#4. TDR_summary.RDS - dive summaries of TDR_final by deployID

# STEP 1: Read in and clean TDR data ===================================================================================================================================================================================

## Read in TDR files with deployID extracted from filename -------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRdata <- TDRfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>%  #read in TDR files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_tdr", replacement=""))) %>% #extract birdID by removing tdr from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

## Create dataframe for deployIDs with 'Depth' only  -------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRdepth <- TDRdata %>%
  filter(!is.na(Depth)) %>% #remove rows where Depth *is not* already converted (i.e. Depth is not NA)
  mutate(decimals = nchar(str_split(as.character(Depth), pattern = "\\.", n=2, simplify=TRUE)[,2])) %>% #count number of decimal places (as characters)
  group_by(deployID) %>% #group by deployID ready for mean decimal calculations
  mutate(Pressureformat = case_when( #create Pressureformat label using mean number of decimal places for each deployID
    max(decimals) == 1 ~ "mbar", #.1 gets labelled as "mbar"
    max(decimals) == 2 ~ "bar", #.01 as "bar"
    max(decimals) == 3 ~ "dbar")) %>% #.001 as "dbar"
  dplyr::select(-c(decimals)) %>% #remove decimal column (not needed anymore)
  mutate(Pressureformat = case_when( #manually edit Pressureformat for some IDs (decimal places missing in raw files)
    deployID == "07_2014D" ~ "bar",
    deployID == "11_2014R" ~ "bar",
    TRUE ~ as.character(Pressureformat) )) %>%
  mutate(across(c("Temp", "Depth"), as.numeric)) %>% #change Temperature and Depth to numeric
  group_by(deployID) %>% #group again in case data was ungrouped in previous step
  mutate(Depth = case_when( #convert dbar/mbar to bar (Depth in m) using correct conversions and calibrations
    Pressureformat == "bar" ~ Depth,
    Pressureformat == "dbar" ~ Depth*10,
    Pressureformat == "mbar" ~ (Depth-min(Depth))/100 )) #calibrate Depth for Depth=mbar files (check not done already)

## Create dataframe for deployIDs with 'Pressure' only -----------------------------------------------------------------------------------------------------------------------------------------------------------------
#TDRpressure <- TDRdata %>%
#  filter(is.na(Depth)) %>% #remove rows where Depth *is* already converted (i.e. Depth is NA)
#  mutate(decimals = nchar(str_split(as.character(Pressure), pattern = "\\.", n=2, simplify=TRUE)[,2])) %>% #count number of decimal places (as characters)
#  group_by(deployID) %>% #group by deployID ready for mean decimal calculations
#  mutate(Pressureformat = case_when( #create Pressureformat label using mean number of decimal places for each deployID
#    max(decimals) == 1 ~ "mbar", #.1 gets labelled as "mbar"
#    max(decimals) == 2 ~ "bar", #.01 as "bar"
#    max(decimals) == 3 ~ "dbar")) %>% #.001 as "dbar"
#  dplyr::select(-c(decimals)) %>% #remove decimal column (not needed anymore)
#  mutate(across(c("Temp", "Pressure", "Depth"), as.numeric)) %>% #change Temperature and Pressure to numeric
#  group_by(deployID) %>% #group again in case data was ungrouped in previous step
#  mutate(Depth = case_when( #convert dbar/mbar to bar (Depth in m) using correct conversions and calibrations
#    Pressureformat == "bar" ~ Pressure,
#    Pressureformat == "dbar" ~ Pressure*10,
#    Pressureformat == "mbar" ~ (Pressure-min(Pressure))/100 )) #calibrate mbar TDR using lowest Pressure reading (per group)

## Merge TDRdepth and TDRpressure --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRall <- as_tibble(rbindlist(list(TDRdepth))) #merge the two TDR datasets into one tibble (rbindlist is fastest)
rm(TDRdepth, TDRdata, TDRfiles) #drop the big datasets created, and keep only TDRall

# Extract TDRtag type from metadata ready to add to TDRclean
TDR_tags <- metadata %>% dplyr::select(deployID, TDR_tag)

# Do some final cleaning, tidying and removal of unnecessary columns
TDRclean <- TDRall %>%
  filter(Depth < 150)  %>% #filter out extreme Depth values > 150 (e.g. weird end of many files)
  mutate(DateTime = paste(Date,Time, sep=" ")) %>% #Make DateTime column
  mutate(DateTime = dmy_hms(DateTime)) %>% #Format DateTime column
  dplyr::select(DateTime, Temp, Depth, deployID, Pressureformat) %>% #Keep DateTime, Temp, Depth, DeployID, Pressureformat
  left_join(., TDR_tags, by="deployID") %>% #join TDR_tag info to full dataset
  mutate(Depth = case_when(deployID == "08_2010R" ~ Depth-15, #manually adjust Depth for one very weird file
                           TRUE ~ as.numeric(Depth))) #do nothing to all other rows
rm(TDRall) #drop TDRall dataframe

# Check there are no NAs or duplicates in TDR data (duplicated might take a while time to check)
if(sum(colSums(is.na(TDRclean))) >1) {message(paste("AP pipeline:There are",(sum(colSums(is.na(TDRclean)))),"NA values in TDR data "))
} else {message("AP pipeline: No NA's in TDR data")}
if(sum(anyDuplicated(dplyr::select(TDRclean[grep("2022",TDRclean$deployID),],deployID, DateTime)))>1) {message(paste("AP pipeline: duplicates detected in 2022 TDR data"))
} else {message("AP pipeline: no duplicates in 2022 TDR data!")}

## Save TDRclean--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRclean, file = here(output_dir,"TDR_cleaned.RDS"), compress="xz") #save as RDS file (loaded with 'readRDS')
#saveRDS(TDRclean, file = here(output_dir,"TDR_cleaned_zip.RDS"), compress="xz") #save as RDS with xz compression (~.25 original size but slower save/load)
#beep(5) #actual fanfare noises for finishing this step!

# Tidy up after saving
rm(TDR_tags,TDRclean)


# STEP 2: Processing TDR data with diveMove ============================================================================================================================================================================

## Specify files to remove from further analyses (no dives or bad data) ------------------------------------------------------------------------------------------------------------------------------------------------
remove_files <- c("02_2017R", #no dives
                  "24_2017R", #no dives
                  "05_2018R") #bad logger (100 rows of datetimes at 09:00:48)
message("AP pipeline: Removing all data for ",length(remove_files)," TDR files with no dives/bad data (",toString(remove_files),")")

## Read in TDRclean, remove bad files and split into dataframe per deployID   ------------------------------------------------------------------------------------------------------------------------------------------
TDRdives <- readRDS(here(output_dir, "TDR_cleaned.RDS")) %>% #read in TDRclean RDS file
  filter(!(deployID %in% remove_files)) %>% #filter to remove files deployID
  group_by(deployID) %>% group_split(.) #group and split by deployID ready for diveMove processing

## DIVEMOVE VARIABLES FOR TDR CALIBRATION (recursive filtering for zero offset correction — per Luque & Fried 2011) ----------------------------------------------------------------------------------------------------
depth.bounds <- c(-5, 5) #this is the range of where we expect the surface to be found
probs <- c(0.5, 0.3) #running quantiles for each of the two steps used (probs[1] is 1st smoothing step, probs[2] is 2nd filtering step)
K <- c(2,360) #window size for each step in seconds (e.g K[1] is window size of 1st step, K[2] is window size of 2nd step)
dive.threshold <- 5 #Depth below which anything should be considered a dive

## Run recursive filtering for zero offset correction (https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0015850))   -----------------------------------------------------------------------------------
future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
TDRcalibrated <- TDRdives %>% #Create and calibrate each TDR object using recursive filtering
  future_map(~ diveMove::createTDR(time=.x$DateTime, depth=.x$Depth, dtime=1, file=.x$deployID[1]),.progress=TRUE) %>% #create diveMove object with deployID as source file name
  future_map(~ diveMove::calibrateDepth(.x, dive.thr=dive.threshold, zoc.method="filter",k=K, probs=probs, depth.bounds = depth.bounds, na.rm=TRUE),.progress=TRUE)

## Extract key data columns from each of the TDRcalibrated s4 diveMove objects and cbind  ------------------------------------------------------------------------------------------------------------------------------
deployIDs <- TDRcalibrated %>% map(~ as_tibble(.x@tdr@file)) #extracts the deployID for each dataframe
depth <- TDRcalibrated %>% map(~ as_tibble(.x@tdr@depth)) #extracts the calibrated depth
diveID <- TDRcalibrated %>% map(~ as_tibble(.x@dive.activity[["dive.id"]])) #extract the diveID number
divephase <- TDRcalibrated %>% map(~ as_tibble(.x@dive.phases)) #extracts the dive phases for each dive
TDRextracted <- Reduce(function(x,y) Map(cbind, x, y),list(deployIDs,depth,diveID,divephase)) #cbind each of the lists together with an anonymous function
rm(deployIDs,depth,diveID,divephase) #remove all the elements extracted
invisible(gc()) #quietly call garbage collection to free up ram

## Combine to create the processed TDR dataframe -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
TDRprocessed <- Reduce(function(x,y) Map(cbind, x, y),list(TDRdives,TDRextracted)) %>%
  rbindlist(.) %>% as_tibble(., .name_repair = make.unique) %>% #bind lists together and repair names
  dplyr::select(-value) %>%
  rename(Calibrated_depth = value.1, DiveID = value.2, Divephase = value.3) %>%
  dplyr::select(DateTime, Temp, Depth, Calibrated_depth, DiveID, Divephase, deployID, Pressureformat, TDR_tag)

## Save TDRprocessed ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRprocessed, file= here(output_dir,("TDR_processed.RDS"))) #Save as an RDS object (load this later with 'readRDS' function)
beep(5) #more fanfare noises

## Tidy up after saving
rm(TDRcalibrated,TDRdives,TDRextracted,TDRprocessed, remove_files, K, depth.bounds,probs,dive.threshold)


#STEP 3: Despiking and Trip detection ==================================================================================================================================================================================
# First: remove spikes from a select number of files
# Second: reprocess the entire processed dataset with diveMove again and extract just the trips to stitch back onto the original 

# List of deployID's that show the weird spikes/aka multiple zero level problem
despike_list <- c("01_2008D", "03_2008D", "04_2008D", "05_2008D")

# Despiking process (basically a TDR speed filter)
TDRdespike <- readRDS(here(output_dir,("TDR_processed.RDS"))) %>% #read in TDRprocessed file
  group_by(deployID) %>% #group by deployID
  filter(deployID %in% despike_list) %>% #filter to only the deployIDs in despike_list
  mutate(lead = lead(Calibrated_depth,1)-Calibrated_depth) %>% #compute change in depth from preceding row
  mutate(lag = lag(Calibrated_depth,1)-Calibrated_depth) %>% #compute change in depth to next row
  mutate(change = abs(lead) + abs(lag)) %>% #absolute change in depth +/- 1 row
  mutate(Calibrated_depth = case_when( #use absolute change to detect spikes and revert those to NA's
    change > 9 ~ NA_real_, #paste NA when absolute change is more that 9 (e.g. like 5m down, then 5m up in two seconds)
    change <= 9 ~ Calibrated_depth, #leave as calibrated depth when absolute change is less than 9
    change == NA ~ Calibrated_depth, #Also leave examples where change is NA (at beginning and end of dataframe)
    TRUE ~ as.numeric(Calibrated_depth))) %>% #Everything else that I've forgotten about leave as calibrated depth
  mutate(Calibrated_depth = na_interpolation(Calibrated_depth,option="linear")) %>% #impute linear interpolation for despiked points (now NA's)
  dplyr::select(-c(lead,lag,change))

# rbind the despiked dataset back onto the original dataset
TDRnospike <- readRDS(here(output_dir,("TDR_processed.RDS"))) %>% #read in the same TDRprocessed file
  filter(!deployID %in% despike_list) #filter out the opposite set of deployments (i.e. those without spike problems)
TDRprocessed <- rbind(TDRdespike,TDRnospike) #bind the two datasets together

#Tidy up after saving
rm(TDRnospike,TDRdespike,despike_list)
invisible(gc()) #quietly call garbage collection to free up ram

## DIVEMOVE VARIABLES FOR TRIP PHASE DETECTION -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# currently using: 4, 3hrs, 3mins
depth_thr <- 4 #threshold below which TDR values 'could' be classified as dry (hacky - only using for diveMove trip phase detection)
dry_thr <- (3600*3) #dry error threshold in seconds (nseconds*hours). Dry phases shorter than this threshold will be considered as wet (see ?calibrateDepth)
wet_thr <- (60*3)  #wet threshold in seconds (nseconds*minutes). At-sea phases shorter than this threshold will be considered as trivial wet (see ?calibrateDepth)
trip_thr <- 60*15 #minimum threshold for trip length in seconds (i.e. remove trips shorter than 15 minutes)

# Get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)
future::plan(multisession, workers = 7) 

## Using despiked dataset, use .detPhase to detect trip phases ---------------------------------------------------------------------------------------------------------------------------------------------------------
TDRalltrips <- TDRprocessed %>%
  mutate(wet_cond = case_when( #create a new column called wet_cond (for diveMove::calibrateDepth to detect trips)
    Calibrated_depth < depth_thr ~ FALSE, #when calibrated depth less than depth threshold, classify "wet_cond" as dry (FALSE)
    Calibrated_depth >= depth_thr ~ TRUE)) %>% #when calibrated depth more than depth threshold, classify "wet_cond" as wet (TRUE) 
  group_by(deployID) %>% group_split(.) %>% #group by deployID and split ready for diveMove processing
  future_map(~ diveMove:::.detPhase(time=.x$DateTime, depth=.x$Calibrated_depth, interval=1, wet.cond = .x$wet_cond, dry.thr=dry_thr, wet.thr=wet_thr),.progress=TRUE)

# Extracts only the tripID per row for each dataframe (this is the only thing we care about for trip detection)
tripID <- map(TDRalltrips, "phase.id") %>% #extract phase id from elements created by detphase
  map(~ as_tibble(.x)) #convert to tibble ready for recombining with full dataset
rm(TDRalltrips) #tidy up after extracting tripID

# Stitch tripID for each deployment back onto despiked TDRprocessed object
TDRprocessed <- TDRprocessed %>%
  group_by(deployID) %>% group_split(.) #group by deployID and split
TDRtrips <- Reduce(function(x,y) Map(cbind, x, y),list(TDRprocessed,tripID)) %>%
  rbindlist(.) %>% as_tibble(.) %>% #rbind the list of dataframes back together as one tibble
  rename(tripID = value) #rename the trip column (currently called 'value') to tripID

# Tidy up after stitching
rm(TDRprocessed,tripID) #tidy up after combining TDRprocessed and tripID
invisible(gc()) #quietly call garbage collection to free up ram

# Process data using tripID to remove all trip phases where SD is less than 1 or shorter than threshold, and rerank starting at 1
TDRfinal <- TDRtrips %>% 
  group_by(deployID,tripID) %>% #group by deployID and tripID
  summarise(sd=sd(Calibrated_depth), length=length(tripID)) %>% #for each trip of deploymentID, create a summary of standard deviation and length
  left_join(TDRtrips,.) %>%
  mutate(trip_num = case_when( #when SD depth for group is higher than 1, change group trip number
    length < trip_thr | sd < 1 ~ 0, #when trip length is below threshold -OR- SD is less than 1, change tripID to zero (does not count as a trip)
    sd >= 1 ~ tripID )) %>% #if sd exceeds 1, keep tripID (counts as a trip)
  mutate(trip_num = na_if(trip_num, 0)) %>% #change all the zeros to NA's (to help avoid problems with plotting later)
  ungroup() %>% group_by(deployID) %>% #remove previous grouping and focus only on deployID
  mutate(trip_num = dense_rank(trip_num)) %>% #create new rank using remaining trip_num (e.g. 1,1,4,4,6,6 becomes 1,1,2,2,3,3)
  dplyr::select(-c(sd,length,tripID)) %>% #drop sd, length, and tripID 
  rename(tripID = trip_num)

## Save TDRfinal -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRfinal, file=here(output_dir,"TDR_final.RDS")) #Save as an RDS object (load with 'readRDS')
beep(8) #final fanfare noises!

# Tidy up after saving
rm(TDRfinal,TDRtrips, depth_thr, dry_thr, wet_thr, trip_thr)
invisible(gc()) #quietly call garbage collection to free up ram

# STEP 4: Extraction of dive summaries for each deployID (1st trip only) ===============================================================================================================================================

## Read in Metadata_MASTER and filter columns (all individuals with biometrics have a sex classified)
metadata_sex <- read_csv(file = here("E:/Deployments Robben 2022/R codes/Metadata.csv")) %>%
  dplyr::select(deployID, Sex_0.5, Year, Closure, Colony) %>% rename(Sex = Sex_0.5)

## Read in cleaned and processed TDR file, and create list of deployIDs  -----------------------------------------------------------------------------------------------------------------------------------------------
TDRpost <- readRDS(here(output_dir, "TDR_final.RDS")) %>% #read in the TDRfinal file
  replace_na(list(tripID = 0)) %>% #replace the na's in trip number with zeroes
  mutate(tripID = as_factor(tripID)) %>% #mutate the tripID column into a factor for plotting
  left_join(., metadata_sex, by="deployID") %>% #add metadata (Sex) to TDRdataset
  ungroup()

## Filter data for first trips and summarise using diveMove::diveStats   -----------------------------------------------------------------------------------------------------------------------------------------------
future::plan(multisession, workers = 7) #get ready to run in multiple sessions (this might slow down your computer so close background apps beforehand)

## Reprocess the data for first trip with diveMove (now using no offset or any corrections) to detect individual dives   -----------------------------------------------------------------------------------------------
TDRreprocess <- TDRpost %>%
  filter(tripID == 1) %>% #remove individuals without sex and subset to dives from first trip
  ungroup() %>% group_by(deployID) %>% group_split(.) %>% #group and split by deployID ready for diveMove processing
  future_map(~ diveMove::createTDR(time=.x$DateTime, depth=.x$Calibrated_depth, dtime=1, file=.x$deployID[1]),.progress=TRUE) %>% #create diveMove object with deployID as source file name
  future_map(~ diveMove::calibrateDepth(.x, dive.thr=5, zoc.method="offset",offset=0, na.rm=TRUE),.progress=TRUE)

## Loop to that creates a diveMove summary datafile for all deployID (hate loops but couldn't find a better way to do it)  ---------------------------------------------------------------------------------------------
templist = list() #initialise list for loop

for (i in 1:length(TDRreprocess)) {
  temp <- diveMove::diveStats(TDRreprocess[[i]])
  temp$deployID <- (TDRreprocess[[i]]@tdr@file)
  temp$diveID <- seq.int(nrow(temp)) 
  templist[[i]] <- temp
}
TDRsummary <- data.table::rbindlist(templist) %>% #rbind output of the loop into a dataframe
  left_join(., metadata_sex, by="deployID") #add sex information

## Save dive stat summary file   ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRsummary, file=here(output_dir,"TDR_summary.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after stitching
rm(TDRpost, TDRreprocess, temp, i ,templist, TDRsummary) #tidy up after combining TDRprocessed and tripID
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 5: Read in raw GPS data =========================================================================================================================================================================================

# List paths of all GPS files
input_dir <- here("E:/Deployments Robben 2022/") #top location of raw files)

GPSfiles <- fs::dir_ls(input_dir, glob = "*gps*.csv", type="file", recurse = TRUE) # list location of all files with "gps" in the name

## Read in GPS files and extract deployID from filename ----------------------------------------------------------------------------------------------------------------------------------------------------------------
GPSdata <- GPSfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>% #read in GPS files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_gps", replacement="")),.before=Date) %>% #extract birdID by removing gps from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID))) #and remove file suffix

## Create datetime column and clean up dataframe -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
GPSclean <- GPSdata %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>% #Create DateTime column
  drop_na(.) %>% #remove rows with NA's (only present in newly-created DateTime column)
  mutate(across(c(Lat,Lon),as.numeric)) %>% #turn lat/lon into numeric
  dplyr::select(-c(filename)) #remove filename column
message("AP pipeline: Ignore 15 failed to parse warning above (some bad dates starting with 00 e.g. '00-01-17')")

## Filter out based on time jumps (> 1 second & < 2 days) --------------------------------------------------------------------------------------------------------------------------------------------------------------
GPStimefilter <- GPSclean %>%
  group_by(deployID) %>%  #group by deployID
  arrange(DateTime) %>% #arrange by DateTime (defaults to descending)
  mutate(diff_secs = as.duration(DateTime - lag(DateTime)), #calculate difference in time to previous row (output as duration)
         diff_long = ifelse(diff_secs > days(2),T,F), #convert diff_secs into a logical threshold for >2 days
         diff_short = ifelse(diff_secs < seconds(1),T,F)) %>% #convert diff_secs into a logical threshold for <1 sec
  replace_na(list(diff_long = FALSE, diff_short=FALSE)) %>% #convert NA's in threshold col to FALSE
  filter(cumsum(diff_long)<1) %>% #remove all rows of data after a time jump greater that 2 days
  filter(diff_short == FALSE) %>% #remove rows where time jump is less than 1 second
  dplyr::select(-c(diff_long, diff_short))

message("AP pipeline: ",nrow(GPSclean)-nrow(GPStimefilter)," GPS rows removed with our time jump filter (", 
        round((nrow(GPSclean)-nrow(GPStimefilter))/nrow(GPSclean)*100,digits=2),"% of data)")

## Filter out based on speed travelled from previous point & to next point (>12.4 km/h or 3.444 m/s) -------------------------------------------------------------------------------------------------------------------
GPSspeedfilter <- GPStimefilter %>%
  left_join(., (metadata %>% dplyr::select(deployID, Nest_Lat, Nest_Lon)), by = "deployID") %>% #add nest lat/lon to data
  ungroup() %>%
  mutate(geometry_GPS = st_transform(st_as_sf(., coords=c("Lon","Lat"), crs=4326), crs = 4326)$geometry, # assign GPS geometry and transform to lon lat for dist calcs
         geometry_CP = st_transform(st_as_sf(., coords=c("Nest_Lon","Nest_Lat"), crs=4326), crs = 4326)$geometry) %>% #assign nest geometry and transform to lon lat for dist calcs
  group_by(deployID) %>%
  mutate(diff_sec_prev = as.duration(DateTime - lag(DateTime)),
         diff_sec_next = as.duration(lead(DateTime) - DateTime),
         distance_prev = st_distance(geometry_GPS, lag(geometry_GPS), by_element = T), #distance travelled from previous point
         distance_next = st_distance(geometry_GPS, lead(geometry_GPS), by_element = T), #distance traveled to next point
         speed_msec_prev = as.numeric(distance_prev)/as.numeric(diff_sec_prev), #calculate speed moving from previous point (distance/time)
         speed_msec_next = as.numeric(distance_next)/as.numeric(diff_sec_next), #calculate speed moving to next point (distance/time)
         nestdist = st_distance(geometry_GPS, geometry_CP, by_element = T),.after=diff_secs) %>% #calculate distance between nest and current GPS location
  filter(speed_msec_prev < 12.4*(5/18) & speed_msec_next < 12.4*(5/18)) %>%  #speed filter - remove any point where the speed is over 12.4 km/h (Wilson 1986) 5/18 converts it into m/s
  ungroup() %>% #ungroup before final cleanup
  dplyr::select(-c(geometry_GPS, geometry_CP,diff_sec_prev,diff_sec_next,distance_prev,distance_next,speed_msec_next)) %>% #remove geometries and extra columns
  rename(speed_msec = speed_msec_prev) #rename the speed prev column

message("AP pipeline: ",nrow(GPStimefilter)-nrow(GPSspeedfilter)," GPS rows removed with our speed filter (", 
        round((nrow(GPStimefilter)-nrow(GPSspeedfilter))/nrow(GPStimefilter)*100,digits=2),"% of data)")

## Save GPS data -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(GPSspeedfilter, file=here(output_dir,"GPS_filter.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfiles,GPSdata,GPSclean,GPStimefilter,GPSspeedfilter)
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 6: Trip detection from GPS  =====================================================================================================================================================================================

## Use GPS data to classify trips in and out of island buffer ----------------------------------------------------------------------------------------------------------------------------------------------------------

# Read in a map files of islands (including a buffer to account for inaccuracy in gps devices
islands500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_buffer(., dist = 500) %>% # 500m buffer in case of noisy GPS data
  st_transform(4326) #transform back to latlon
#st_crs(islands500, parameters = TRUE)$units_gdal #find units used for any named sf object
#sf_use_s2(TRUE) #if you get spammed with warnings from st_intersects below, you need to switch s2 back on

#Define trips as contiguous rows spent outside the island buffer (using rleid and denserank)
GPStrips <- readRDS(here(output_dir,"GPS_filter.RDS")) %>% #read in the GPSfilter file from step 5
  st_as_sf(coords = c('Lon', 'Lat'), crs=4326, remove=FALSE) %>% #add geometry column for analyses with sf
  mutate(intersection = as.integer(st_intersects(geometry, islands500)), #is the point intersection with an island (number)
         island = if_else(is.na(intersection), 'NA', islands500$name[intersection])) %>% #add the name of the island
  mutate(island = str_remove_all(island, pattern = c(" Island"))) %>% #remove "island" suffix from island name
  group_by(deployID) %>% arrange(DateTime) %>% 
  mutate(TripOrColony = ifelse(is.na(intersection), "trip", "colony"), #define as trip/colony based on presence/absence of NA
         trips_all = as.numeric(rleid(TripOrColony)), #number trips based on changes in TripOrColony (e.g.colony,trip,trip,trip,colony -becomes- 1,2,2,2,3)
         trip_npoints = rep(rle(trips_all)$lengths, rle(trips_all)$lengths), #length of trip (in rows)
         trips_only = case_when( #create case_when to define if a numbered trip should count as a trip
           TripOrColony == "colony" ~ NA_real_, #in colony, so does not count as a trip
           TripOrColony == "trip" ~ trips_all, #outside colony, so leave as numbered trip
           trip_npoints <= 1 ~ NA_real_), #ignore trips lasting one point (probably bad GPS)
         tripID = dense_rank(trips_only)) %>% #create new rank w/ trip_num (e.g. 1,1,3,3,5,5 becomes 1,1,2,2,3,3)
  dplyr::select(-c(intersection,TripOrColony,trips_all,trips_only,trip_npoints)) #remove extra columns

## Classify complete/incomplete trips ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.5km buffer area (home range) to account for porpoising 
islands1500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_buffer(., dist = 1500) %>% # 500m buffer in case of noisy GPS data
  st_transform(4326) #transform back to latlon

# Find if points intersect with islands1500 and summarise
GPStripsummary <- GPStrips %>% #use GPStrips object with numbered trip IDs
  mutate(homeintersection = as.integer(st_intersects(geometry, islands1500)), #is the point intersection with an island1500 (add island number)
         homeisland = if_else(is.na(homeintersection), 'NA', islands1500$name[homeintersection])) %>% #if intersection, add island name
  st_drop_geometry() %>%
  group_by(deployID, tripID) %>% arrange(DateTime) %>% 
  summarise(first = first(homeintersection), #summarise first row of each homeintersection (i.e. island number if intersecting)         
            last = last(homeintersection)) %>% #summarise last row of each homeintersection (i.e. island number if intersecting)
  replace_na(list(first = 0, last = 0))  %>% #replace NA's in first/last column with zeroes (easier to deal with for detecting tripcomplete)
  mutate(tripcomplete = case_when( #if trip starts and finishes inside same island area then it's a complete trip
    first==0|last==0 ~ FALSE, #if island number is zero, trip is not complete
    !is.na(tripID) & first==last ~ TRUE, #if trip is not NA and first island same as last, trip is complete
    !is.na(tripID) & first!=last ~ FALSE, #if trip is not NA and first island different to last, trip is not complete
    is.na(tripID) ~ NA)) %>% #if tripID is zero, NA
  dplyr::select(deployID, tripID, tripcomplete)

# Add GPStripsummary data to original GPStrips
GPSfinal <- GPStrips %>%
  left_join(., GPStripsummary, by = c("deployID", "tripID")) %>% #add complete trip data (by deployID and tripID)
  relocate(tripID, tripcomplete, .after = island) %>% #move trip columns to right
  relocate(Nest_Lon, Nest_Lat, .after = Lat) %>% #move nest columns left
  dplyr::select(-c(geometry)) #drop the geometry column

## Save GPStripcomplete ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(GPSfinal, file=here(output_dir,"GPS_final.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPStrips,GPStripsummary,GPSfinal,islands500,islands1500)
invisible(gc()) #quietly call garbage collection to free up ram


# STEP 7: Merge TDR/GPS data ===========================================================================================================================================================================================

#If you're running out of vector memory, we can increase the amount of disk memory used:
#library(usethis) #package with some handy functions
#usethis::edit_r_environ() #using this function to edit the .Renviron file
#"R_MAX_VSIZE=32Gb" <<< add this '(without quotations) to the .Renviron file that appears, and save

## Load in final versions of both datasets -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
future::plan(multisession, workers = 4) #Reduce number of workers back to one to reduce resource hogging (no benefit of extra CPUs for joining data *I think*)

# Read TDR data
TDRfinal <- readRDS(file=here(output_dir,"TDR_final.RDS")) %>%
  select(-c(Pressureformat, TDR_tag)) %>% #remove some unnecessary columns
  rename(TDRtripID = tripID) #rename trip ID column

# Read GPS data — interpolate to 1 sec for merging
GPSfinal <- readRDS(file=here(output_dir,"GPS_final.RDS")) %>%
  group_by(deployID) %>% arrange(DateTime) %>%
  select(-c(Date,Time)) %>% #remove extra columns
  rename(GPStripID = tripID) %>% #rename trip ID column
  tidyr::complete(DateTime=seq.POSIXt(min(DateTime), max(DateTime), by="sec")) %>% #add extra seconds between every GPS value
  mutate(Lon_int = zoo::na.approx(object = Lon, x=DateTime), #interpolate Lon (object=values, x=timeseries)
         Lat_int = zoo::na.approx(object = Lat, x=DateTime), #interpolate Lat (object=values, x=timeseries)
         GPS_real = ifelse(!is.na(Lon), TRUE, FALSE), .after=Lat_int) #Keeping track of real GPS points

## Diagnostic message summarising data available to merge  -------------------------------------------------------------------------------------------------------------------------------------------------------------
message("AP pipeline: ","TDR data has ",length(unique(TDRfinal$deployID))," individuals, GPS has ",length(unique(GPSfinal$deployID)),
        "\n  ",sum(unique(TDRfinal$deployID) %in% unique(GPSfinal$deployID))," individuals in TDR dataset have GPS data ready to merge",
        "\n  ",sum(!unique(TDRfinal$deployID) %in% unique(GPSfinal$deployID))," individuals in TDR have no GPS",
        "\n  ",sum(!unique(GPSfinal$deployID) %in% unique(TDRfinal$deployID))," individuals in GPS have no TDR")
invisible(gc()) #quietly call garbage collection to free up ram

## Merge TDR with TDR (#Add GPS as extra column in TDR data)  ----------------------------------------------------------------------------------------------------------------------------------------------------------
TDRGPS_merged <- 
  left_join(x = TDRfinal, y = GPSfinal, by = c("deployID", "DateTime")) %>% #annotate TDR data with GPS points (where available)
  relocate(deployID, .before=DateTime) #move deployID to front

## Save TDRGPS_merged --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(TDRGPS_merged, file=here(output_dir,"TDRGPS_merged.RDS")) #Save summary as an RDS object (load with 'readRDS')

# Tidy up after saving
rm(GPSfinal,TDRfinal,TDRGPS_merged)
invisible(gc()) #quietly call garbage collection to free up ram

#Specify subsampling (find a way to keep mostly 'real' GPS rows?)
df <- readRDS(file=here(output_dir,"TDRGPS_merged.RDS"))


