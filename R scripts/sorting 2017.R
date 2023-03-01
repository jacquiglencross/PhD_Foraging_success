pacman::p_load(tidyverse, dplyr, lubridate, stringr, purrr, #tidyr
               data.table, imputeTS, #datawrangling
               foreach, doParallel, furrr, progressr, #parallel
               sf, crawl, diveMove, forecast, zoo, #movement analysis
               ggplot2, #datavis
               here, beepr) #other useful packages 


setwd("D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Raw")

#df <- read.csv("C:/Users/jg287/Downloads/AP_RI_D1_EW079-17_08062017_RI01_S1.csv")

# sort time column ####
decisecs <- seq(0.00, 0.08, by = 0.02)
count <- seq(1, 5, by = 1)
decimalnums <- cbind(decisecs, count, colnames(c("decisecs", "counts")))


i = "01_2017R"
axy <- read.csv(paste0("01_2017R_axytdr.csv")) %>%   # read in axy data
  #mutate(DateTimeOS = paste(Date,Time, sep=" ")) %>% #Make DateTime column
  #mutate(DateTime = dmy_hms(DateTimeOS)) %>% 
  dplyr::select(X, Y, Z, Date, Time, TagID) %>%#, DateTime,  DateTimeOS)
  mutate(deployID = i) %>% #and remove file suffix
  mutate(Date = dmy(Date)) %>%
  mutate(Time1 = paste(00, Time, sep=":")) %>%
  mutate(Time1 = lubridate::hms(Time1, roll =FALSE))

summary(axy$Date)
## Using TDR won't work because multiple minutes:seconds
#tdr <- read.csv(paste0("D:/raw_penguin/Robben/TDR_Raw/2017R_TDR_Raw/01_2017R_tdr.csv")) %>%
#  mutate(Date = dmy(Date)) %>%
#  mutate(deployID = i,
#         hour = as.character(format(strptime(Time, "%H:%M:%S"),"%H")),
#         minsec = format(strptime(Time, "%H:%M:%S"),"%M:%S")) %>%
#  rename(., "Timetdr" = "Time") %>%
#  select(-Pressure, -Temp)
#axy2 <- axy1 %>%
  #merge(x=.,y=tdr,by=c("minsec", "Date"),all.x=TRUE)
#  left_join(., tdr)

metadata <- read.csv("D:/Chapter 4 - foraging success/Metadata_MASTER.csv")%>%
  select(deployID,  AXY_starttime) %>%  
  filter(deployID == i) %>%
  mutate(AXY_starttime1 = lubridate::hms(AXY_starttime, roll =FALSE))# %>%
 # mutate(AXYhour = format(strptime(AXY_starttime, "%H:%M:%S"),"%H"))


#hours <- metadata  %>% 
#  mutate(axyhour = format(strptime(AXY_starttime, "%H:%M:%S"),"%H"))

options(digits.secs = 3) 

starttime <- ("14:31:11.040")
#addtime <- ("00:00:00.020")
  
axy1 <- axy %>%
  mutate(rownum = 1:nrow(.)) %>%
  mutate(NewTime = ifelse(rownum == 1 , starttime, NA),
         NewTime = hms(NewTime)) 
axy2 <- axy1[1:100,]
axy2$NewTime = as.vector(axy2$NewTime)
tmediff <- as.vector(seconds(0.02))
testing <- axy2 %>%
   mutate(NewTime1 = accumulate(NewTime, ~ .x + tmediff)) #accumulate is the function from purrr


str(axy2$NewTime)


axy2$NewTime1[1] = axy2$NewTime[1]
for(idx in 2:length(axy2$NewTime)) {
  axy2$NewTime1[idx] = axy2$NewTime1[idx - 1] + seconds(0.02)
}
axy2[ , "NewTime" := seconds(0.02) + shift(B, 1L, type="lag")]

templist1 = list()
#i <- 2


for (i in 2:nrow(axy1)) { 
  lagTime <- axy1 %>% 
    select(rownum, NewTime) %>%
    filter(rownum == i-1)
  
  new<- axy1 %>% 
    filter(rownum == 1) %>%
    mutate(NewTime1 = lagTime$NewTime + seconds(0.02))
  

  templist1[[i]] <- new  
  print("done")
}
axy3 <- data.table::rbindlist(templist1) 




write.csv(axy1, file = "D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Rawnew/01_2017R_axytdr.csv")



#split columns ####
axy <- read.csv("02_2017R_axytdr.csv", sep = "",  head = T) %>% #skip = 1,, col.names = columns
  select(-level)
head(axy)

write.csv(axy, file = "D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Rawnew/02_2017R_axytdr.csv")
rm(axy)
