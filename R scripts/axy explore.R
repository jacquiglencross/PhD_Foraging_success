#load packages
pacman::p_load(tidyverse, dplyr, lubridate, stringr, purrr, #tidyr
               data.table, imputeTS, #datawrangling
               foreach, doParallel, furrr, progressr, #parallel
               sf, crawl, diveMove, forecast, zoo, #movement analysis
               ggplot2, #datavis
               here, beepr, gridExtra, gganimate, magick) #other useful packages 

rm(list = ls())
gc()
invisible(gc())


#filtering by tdr data first forage dive - 10 min
# load all tdr data
tdrALL <- readRDS("E:/Deployments Robben 2022/processed_data/TDR_final.RDS")
deployIDs <- unique(tdrALL$deployID)
#deployIDs <- deployIDs[2:26]

j <- deployIDs[1]
for (j in deployIDs) {
  #load individual axy file
  axy <- read.csv(paste0("E:/Deployments Robben 2022/",j,"/",j,"_axytdr.csv"))
  
  # filter to the deployID of the axy
  tdr <- tdrALL %>% filter(deployID == j) %>%
    group_by(DiveID) %>%
    mutate(bottom = ifelse(Divephase == "B", 1, 0),
           bottom_length = sum(bottom)) %>%
    mutate(Shape = ifelse(bottom_length > 4, "U", "V")) # add dive shape column based on bottom time
  
  #clean up
  #rm(tdrALL)
  gc()
  invisible(gc())
  
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
    mutate(DateTime = paste(Date,Time, sep=" ")) %>% #Make DateTime column
    mutate(DateTime = dmy_hms(DateTime)) %>%
    filter(DateTime > firstdive$DateTime - minutes(10)) %>%
    filter(DateTime < lastdive$DateTime + minutes(10)) %>%
    mutate(totala = sqrt((X^2)+(Y^2)+(Z^2))) %>%  #add a column for overall dynamic body acceleration
    select(-Date, -Time)
  
  #write out subsetted axy file as csv
  write.csv(axy_subset, paste0("E:/Chapter 4 - foraging success/axy_subset_data/", j, "_axytdr_subset.csv"))
  
  rm(axy, firstdive, lastdive)
  
  gc()
  invisible(gc())
  
  
  #create list of diveIDs
  diveids <- tdr %>% select(DateTime, DiveID, tripID, Shape) 
  
  #create DiveID, tripID and dive shape columns and filter for forage dives only 
  axydives <- axy_subset %>%
    left_join(., diveids) %>%
    fill(DiveID, .direction = "down") %>% #.direction = "down" fills NA values with the last non-NA value
    fill(tripID, .direction = "down") %>%
    fill(Shape, .direction = "down") %>%
    filter(Shape == "U")
  
  
  
  tdrdives <- tdr %>%
    filter(Shape == "U") %>% # forage dives only
    filter(Divephase == "B") %>%  #only interested in the bottom segment of the dive
    group_by(DiveID) %>% #group by diveID
    mutate(dDepth = lag(Depth) - Depth) %>%  #calculate the change in depth - 
                                      #NOTE: this should be change in depth (m) per s, 
                                             #but as our TDRs record once a second I've skipped a step but will add in at some point ####
    mutate(dDepth = as.numeric(dDepth)) %>%
    filter(!is.na(dDepth)) %>%  #remove NA values so next line works but probably need to keep them in ####
    mutate(Wigglepos = ifelse(max(dDepth) > 0.3, TRUE, FALSE),  # wiggle defined in Sala et al 2012 is change in depth of more than 0.3m in a second
           Wiggleneg = ifelse(min(dDepth) < -0.3, TRUE, FALSE),  # have included positive and negative values but probably should only include one or the other ####
           Wiggle = ifelse(Wigglepos == TRUE | Wiggleneg == TRUE, TRUE, FALSE)) %>%
    filter(Wiggle == TRUE)
    

  foragedives <- as.data.frame(unique(ifelse(tdrdives$DiveID %in% axydives$DiveID, tdrdives$DiveID, NA))) 
  foragedives <- foragedives[!is.na(foragedives)]
  #i <- foragedives[1]
  

  for (i in foragedives) {
    
    
    axy1dive <- axydives %>%
      filter(DiveID == i)
    
    axyplot <- ggplot(data = axy1dive, aes(x = DateTime, y = totala)) +
      geom_line()
    
    
    tdr1dive <- tdrdives %>%
      filter(DiveID == i)
    
    tdrplot <- ggplot(data = tdr1dive, aes(x = DateTime, y = dDepth)) +
    geom_line() + xlim(min(axy1dive$DateTime), max(axy1dive$DateTime))
    
    plots <- grid.arrange(tdrplot, axyplot, ncol = 1)
    deploy <- tdr1dive$deployID[1]
    ggsave(plots, file=paste0("E:/Chapter 4 - foraging success/wiggleOBDA plots/",deploy,"_",i,".png"), width = 8, height = 5)
  }

  rm(deploy, plots, tdrplot, tdr1dive, axyplot, axy1dive, foragedives, tdrdives, axydives, axy_subset, tdr)
}




# filtering by location ####
setwd("D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Raw")

axy <- read.csv("04_2017R_axytdr.csv")
head(axy)
gps <- read.csv("02_2022R_gps.csv")

# Read in a map files of islands (including a buffer to account for inaccuracy in gps devices
islands500 <- readRDS(here("~/PhD/AP_WC_Foraging/raw_data/map_files/RobbDass_highres_sf.RDS")) %>% #read in the sf object for Dassen & Robben
  st_transform(3857) %>% #3857=Spherical Mercator projection (m)
  st_transform(4326) #transform back to latlon


#Define trips as contiguous rows spent outside the island buffer (using rleid and denserank)
GPStrip <- gps %>% #read in the GPSfilter file from step 5
  st_as_sf(coords = c('Lon', 'Lat'), crs=4326, remove=FALSE) %>% #add geometry column for analyses with sf
  mutate(intersection = as.integer(st_intersects(geometry, islands500)), #is the point intersection with an island (number)
         island = if_else(is.na(intersection), 'NA', islands500$name[intersection])) %>%
  filter(is.na(intersection)) %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
  summarise(start = min(DateTime),
            end = max(DateTime))

names(axy)
axy1 <- axy %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
  filter(DateTime > GPStrip$start) %>%
  filter(DateTime < GPStrip$end) %>%
  mutate(aX = X,
         aY = Y,
         aZ = Z)


ggplot(axy1, aes(DateTime)) +
  geom_line(aes(y =aX), color = "green") + 
  geom_line(aes(y =aY), color = "blue") +
  geom_line(aes(y =aZ), color = "red")
  
axy2 <- axy %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
  filter(DateTime > GPStrip$start) %>%
  filter(DateTime < GPStrip$end) %>%
  mutate(totala = sqrt((X^2)+(Y^2)+(Z^2)))

axy3 <- axy2[1:1000,]
ggplot(axy3, aes(DateTime)) +
  geom_line(aes(y =totala), color = "blue") 
axy4 <- axy2[1001:2000,]
ggplot(axy4, aes(DateTime)) +
  geom_line(aes(y =totala), color = "blue") 

tdr<- read.csv("02_2022R_tdr.csv") %>%
  mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
  filter(DateTime > GPStrip$start) %>%
  filter(DateTime < GPStrip$end) 


tdrsub <- tdr[16537:16637,]
tdrplot <- ggplot(tdrsub, aes(DateTime)) +
  geom_line(aes(y =-Depth), color = "blue") 

axysub <- axy2 %>%
  filter(DateTime > min(tdrsub$DateTime)) %>%
  filter(DateTime < max(tdrsub$DateTime))

axyplot <- ggplot(axysub, aes(DateTime)) +
  geom_line(aes(y =totala), color = "blue") 
grid.arrange(tdrplot, axyplot, ncol = 1)

tdrsub0 <- tdr[16582:16612,]
tdrplot0 <- ggplot(tdrsub0, aes(DateTime)) +
  geom_line(aes(y =-Depth), color = "blue") 

axysub0 <- axy2 %>%
  filter(DateTime > min(tdrsub0$DateTime)) %>%
  filter(DateTime < max(tdrsub0$DateTime))

axyplot0 <- ggplot(axysub0, aes(DateTime)) +
  geom_line(aes(y =totala), color = "blue") 
grid.arrange(tdrplot0, axyplot0, ncol = 1)




tdrsub1 <- tdr[16637:16737,]
tdrplot1 <- ggplot(tdrsub1, aes(DateTime)) +
  geom_line(aes(y =-Depth), color = "blue") 

axysub1 <- axy2 %>%
  filter(DateTime > min(tdrsub1$DateTime)) %>%
  filter(DateTime < max(tdrsub1$DateTime))

axyplot1 <- ggplot(axysub1, aes(DateTime)) +
  geom_line(aes(y =totala), color = "blue") 
grid.arrange(tdrplot1, axyplot1, ncol = 1)



tdrsub2 <- tdr[18237:21837,]
axysub2 <- axy2 %>%
  filter(DateTime > min(tdrsub2$DateTime)) %>%
  filter(DateTime < max(tdrsub2$DateTime))

ggplot() +
  geom_line(tdrsub2, mapping=aes(x = DateTime, y =sqrt(Depth)), color = "blue") +
  geom_line(axysub2, mapping=aes(x = DateTime, y = totala), color = "green") #+ 
  scale_y_continuous(name = "Depth",
    sec.axis = sec_axis(~.*1000, name="Total acceleration"))


### birds with camera data
  setwd("E:/Deployments Robben 2022/06_2022R/")
  
  axy <- read.csv("06_2022R_axytdr.csv") %>%
    mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date")
  tdr <- axy %>% filter(!is.na(Depth)) %>% filter(Depth > 1)
  
  head(axy)
  ggplot(tdr, aes(x=DateTime, y = Depth)) +
    geom_line() 
  
  dives <- tdr %>% filter(Depth > 1) %>%
    summarise(start = min(DateTime),
           end = max(DateTime))
names(axy)

  axy1 <- axy %>%
    mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
    filter(DateTime > dives$start) %>%
    filter(DateTime < dives$end) %>%
    mutate(totala = sqrt((accX^2)+(accY^2)+(accZ^2)))
  
  ggplot() +
    geom_line(tdr, mapping=aes(x = DateTime, y =Depth), color = "blue") +
    geom_line(axy1, mapping=aes(x = DateTime, y = totala), color = "green") + 
  scale_y_continuous(name = "Depth",
                     sec.axis = sec_axis(~.*1000, name="Total acceleration"))
  
  
  tdrsub <- tdr[1123:3220,]
  tdrplot <- ggplot(tdrsub, aes(DateTime)) +
    geom_line(aes(y =-Depth), color = "blue") 
  
  axysub <- axy1 %>%
    filter(DateTime > min(tdrsub$DateTime)) %>%
    filter(DateTime < max(tdrsub$DateTime))
  
  axyplot <- ggplot(axysub, aes(DateTime)) +
    geom_line(aes(y =totala), color = "blue") 
  grid.arrange(tdrplot, axyplot, ncol = 1)
  
  tdrsub0 <- tdrsub[1840:2105,] %>% filter(!is.na(DateTime))
  tdrplot0 <- ggplot(tdrsub0, aes(DateTime)) +
    geom_line(aes(y =-Depth), color = "blue") 
  
  axysub0 <- axy1 %>%
    filter(DateTime > min(tdrsub0$DateTime)) %>%
    filter(DateTime < max(tdrsub0$DateTime))
  
  axyplot0 <- ggplot(axysub0, aes(DateTime)) +
    geom_line(aes(y =totala), color = "blue") 
  grid.arrange(tdrplot0, axyplot0, ncol = 1)
  
  
  tdrsub0 <- tdr[3061:3121,] #06:56:30 -> 06:57:30 
  tdrplot0 <- ggplot(tdrsub0, aes(DateTime)) +
    geom_line(aes(y =-Depth), color = "blue") 
  
  axysub0 <- axy1 %>%
    filter(DateTime > min(tdrsub0$DateTime)) %>%
    filter(DateTime < max(tdrsub0$DateTime))
  
  axyplot0 <- ggplot(axysub0, aes(DateTime)) +
    geom_line(aes(y =totala), color = "blue") 
  grid.arrange(tdrplot0, axyplot0, ncol = 1)
  
  
  
# example 2
  ### birds with camera data
  setwd("E:/Deployments Robben 2022/09_2022R/")
  
  axy <- read.csv("09_2022R_axytdr.csv") %>%
    mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date")
  tdr <- axy %>% filter(!is.na(Depth)) %>% filter(Depth > 1)
  
  head(axy)
  ggplot(tdr, aes(x=DateTime, y = Depth)) +
    geom_line() 
  
  dives <- tdr %>% filter(Depth > 1) %>%
    summarise(start = min(DateTime),
              end = max(DateTime))
  names(axy)
  
  axy1 <- axy %>%
    mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
    filter(DateTime > dives$start) %>%
    filter(DateTime < dives$end) %>%
    mutate(totala = sqrt((accX^2)+(accY^2)+(accZ^2)))
  
  
  
setwd("E:/Chapter 4 - foraging success/prey capture")  
  tdrsub <- tdr[5915:5961,]
  tdrplot <- ggplot(tdrsub, aes(DateTime)) +  transition_reveal(DateTime) +
    geom_line(aes(y =-Depth), color = "blue") 
  tdrgif <- animate(tdrplot, renderer = gifski_renderer())
  anim_save("tdr_animation.gif", tdrplot, renderer = gifski_renderer())
  axysub <- axy1 %>%
    filter(DateTime > min(tdrsub$DateTime)) %>%
    filter(DateTime < max(tdrsub$DateTime))
  
  axyplot <- ggplot(axysub, aes(DateTime)) + transition_reveal(DateTime) +
    geom_line(aes(y =totala), color = "blue") 
  axygif <-animate(axyplot, renderer = gifski_renderer())
  anim_save("axy_animation.gif", axyplot, renderer = gifski_renderer())
  
  
axymax <-axysub$DateTime[axysub$totala > 3.4]
axymin <-axysub$DateTime[axysub$totala < 0.2]
  
  ggplot() +
    geom_line(tdrsub, mapping=aes(x = DateTime, y =-Depth), color = "blue") +
    geom_line(axysub, mapping=aes(x = DateTime, y = totala), color = "green") + 
    geom_vline(xintercept = axymin, lty = "dashed") +
    geom_vline(xintercept = axymax) +    
    scale_y_continuous(name = "Depth",
                       sec.axis = sec_axis(~., name="Total acceleration"))
  

  tdr_mgif <- image_read(tdrgif)
  axy_mgif <- image_read(axygif)
  
  new_gif <- image_append(c(tdr_mgif[1], axy_mgif[1]), stack = TRUE)
  for(i in 2:100){
    combined <- image_append(c(tdr_mgif[i], axy_mgif[i]), stack = TRUE)
    new_gif <- c(new_gif, combined)
  }
new_gif 
anim_save("tdraxy_animation.gif", new_gif, renderer = gifski_renderer())
  
