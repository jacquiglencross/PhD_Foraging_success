# camera pipeline

#start with an empty environment
rm(list = ls())
gc()
invisible(gc())
#load packages
pacman::p_load(tidyverse, dplyr, lubridate, here, ggExtra, zoo, beepr)

#load data
metadata <- read.csv("F:/Chapter 4 - foraging success/BORIS/RobbenCamera_metadata_final.csv") %>%
  filter(!is.na(Filename)) %>%  #read in metadata and filter so only interested in deployments with axy data
  mutate(Observation.id = gsub(".AVI", "", Filename))

input_dir <- ("F:/Chapter 4 - foraging success/BORIS/Watched videos/") #where to find raw axy data
output_dir <- ("F:/Chapter 4 - foraging success/processed_data/") 

#list all files that end in axytdr in input_dir
camerafiles <- fs::dir_ls(input_dir, glob = "*_labelled*.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name


#i = camerafiles[1]
templist = list()
for (i in camerafiles){
  cameradf <- read.csv(i, head = T) %>%
    rename(., "deployID" = "Subject") %>%
    dplyr::select(-c("Media.file.name","Image.index.start","Image.index.stop","Image.file.path.start","Image.file.path.stop" ,"Comment.start" ,"Comment.stop")) %>%
    left_join(., metadata)
  
  templist[[i]] <- cameradf
  
}

camera_labelled <- data.table::rbindlist(templist, fill = T) 
