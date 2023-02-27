setwd("D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Raw")

df <- read.csv("C:/Users/jg287/Downloads/AP_RI_D1_EW079-17_08062017_RI01_S1.csv")

# sort time column ####
decisecs <- seq(0.00, 0.08, by = 0.02)
count <- seq(1, 5, by = 1)
decimalnums <- cbind(decisecs, count, colnames(c("decisecs", "counts")))


i = "01_2017R"
axy <- read.csv(paste0("01_2017R_axytdr.csv")) %>%   # read in axy data
  #mutate(DateTimeOS = paste(Date,Time, sep=" ")) %>% #Make DateTime column
  #mutate(DateTime = dmy_hms(DateTimeOS)) %>% 
  dplyr::select(X, Y, Z, Date, Time, TagID) %>%#, DateTime,  DateTimeOS)
  mutate(deployID = i) #and remove file suffix

setwd("D:/raw_penguin/Robben/TDR_Raw/2017R_TDR_Raw")
tdr <- read.csv(paste0("01_2017R_tdr.csv")) %>%
  mutate(Date = as.POSIXct(Date, format ="%d/%m/%Y")) %>%
  mutate(deployID = i,
         hour = format(strptime(Time, "%H:%M:%S"),"%H"),
         minsec = format(strptime(Time, "%H:%M:%S"),"%M:%S")) %>%
  select(-Pressure, -Temp, -Time)


#metadata <- read.csv("D:/Chapter 4 - foraging success/Metadata_MASTER.csv")%>%
#  select(deployID,  AXY_starttime) %>%
#  filter(deployID == i)


#hours <- metadata  %>% 
#  mutate(axyhour = format(strptime(AXY_starttime, "%H:%M:%S"),"%H"))

options(digits.secs = 3) 

#newhour <- ("2017-06-08 00:00:00.0")
  
  
axy1 <- axy %>%
  #mutate(StartTimeOS = metadata$AXY_starttime) %>%
  #mutate(StartDateTime = dmy_hms(paste(Date[1], StartTimeOS))) %>%
  mutate(Date = as.POSIXct(Date, format ="%d/%m/%Y")) %>% 
  mutate(Time1 = paste(00, Time, sep=":")) %>%  
  mutate(minsec = format(strptime(Time1, "%H:%M:%S"),"%M:%S")) %>%
  left_join(., tdr)



  mutate(DateTime = as.POSIXct(paste(Date,Time1))) %>%
  mutate(newhour = ifelse(DateTime == newhour, TRUE, FALSE)) %>%
  mutate(hourdiff = rle(newhour)$lengths %>% {rep(seq(length(.)), .)})         #calculate time elapsed between start and end

axy2 <- axy1 %>%
  mutate(hour = as.numeric(hours$axyhour) + as.numeric(hourdiff) - 1) %>%
  mutate(NewTime = paste(hour, Time, sep=":")) %>%
  select(Date, NewTime, X, Y, Z, TagID) %>%
  dplyr::rename(., "Time" = "NewTime") 

axy3 <- axy2 %>%
  mutate(DateTime = as.POSIXct(paste(Date,Time))) %>%
  group_by(DateTime) %>%
  mutate(counts = row_number())


axy2 <- axy1 %>%
  #mutate(OS = as.numeric(format(strptime(TimeOS, "%H:%M:%OS"),"%OS"))) %>%
  #mutate(decimals = OS %% 1) %>%
  mutate(DateTimeOS = paste(Date,TimeOS, sep=" ")) %>%
  

summary(axy2$counts)


write.csv(axy1, file = "D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Rawnew/01_2017R_axytdr.csv")



#split columns ####
axy <- read.csv("02_2017R_axytdr.csv", sep = "",  head = T) %>% #skip = 1,, col.names = columns
  select(-level)
head(axy)

write.csv(axy, file = "D:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Rawnew/02_2017R_axytdr.csv")
rm(axy)
