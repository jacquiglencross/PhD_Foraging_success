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

tdr <- readRDS("E:/Deployments Robben 2022/processed_data/TDR_final.RDS")

axy <- read.csv("E:/Deployments Robben 2022/02_2022R/02_2022R_axytdr.csv")
tdr02 <- tdr %>% filter(deployID == axy$deployID[1]) %>%
  group_by(DiveID) %>%
  mutate(bottom = ifelse(Divephase == "B", 1, 0),
         bottom_length = sum(bottom)) %>%
  mutate(Shape = ifelse(bottom_length > 4, "U", "V"))
rm(tdr)
gc()
invisible(gc())

firstdive <- tdr02 %>% #filter(tripID == 1) %>%
  group_by(DiveID) %>%
  filter(bottom_length > 3) %>%
  ungroup() %>%
  #filter(DiveID == DiveID[1]) %>%
  summarise(DateTime = first(DateTime))
lastdive <- tdr02 %>% #filter(tripID == 1) %>%
  group_by(DiveID) %>%
  filter(bottom_length > 3) %>%
  ungroup() %>%
  filter(tripID == 1) %>%
  #filter(DiveID == DiveID[1]) %>%
  summarise(DateTime = last(DateTime),
            diveID = last(DiveID))

axy02 <- axy %>%
  mutate(DateTime = paste(Date,Time, sep=" ")) %>% #Make DateTime column
  mutate(DateTime = dmy_hms(DateTime)) %>%
  filter(DateTime > firstdive$DateTime - minutes(10)) %>%
  filter(DateTime < lastdive$DateTime + minutes(10)) %>%
  mutate(totala = sqrt((X^2)+(Y^2)+(Z^2))) %>%
  select(-Date, -Time)

rm(axy)


rm(axy02b)

gc()
invisible(gc())

plot(axy02b$DateTime, axy02b$totala)
head(axy$Time)

#format(DateTime_int, "%d/%m/%Y %H:%M:%OS2")


diveids <- tdr02 %>% select(DateTime, DiveID, Shape) 

axydives <- axy02 %>%
  left_join(., diveids) %>%
  fill(DiveID, .direction = "down")



















# filtering by location 
setwd("E:/Deployments Robben 2022/02_2022R/")

axy <- read.csv("02_2022R_axytdr.csv")

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
  
