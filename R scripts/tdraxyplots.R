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

setwd("E:/Deployments Robben 2022/")

tdrs <- readRDS("E:/Deployments Robben 2022/processed_data/TDR_final.RDS") 
deployids <- unique(tdrs$deployID)
dives <- tdrs %>%
  select(DateTime, DiveID, Divephase, deployID)
#i = deployids[1]
for (i in deployids) {
  divespenguin <- dives %>%
  filter(deployID == i)
  
  axy <- read.csv(paste0(i, "/",i,"_axytdr.csv"), header = T)
  
  axy1 <- axy %>%
    mutate(DateTime = dmy_hms(paste(.$Date,.$Time)),.before="Date") %>%
    mutate(totala = sqrt((X^2)+(Y^2)+(Z^2))) %>%
    select(-Date, -Time) %>%
    left_join(., divespenguin)
  
  divephases <- axy1 %>%
    select(Divephase, DiveID, DateTime) 
  
  divephasenew <-  na.locf(divephases,fromLast=TRUE)
  
  axy2 <- axy1 %>%
    select(-Divephase, -DiveID) %>%
    left_join(., divephasenew) %>%
    filter(DiveID != 0)
  
  diveno <- unique(axy2$DiveID)
  for (j in diveno) {
    axy3 <- axy2 %>%
      filter(DiveID == j)
    tdrplot <- ggplot(axy3, aes(DateTime)) +
      geom_line(aes(y =-Depth), color = "blue") 
    
    
    axyplot <- ggplot(axy3, aes(DateTime)) +
      geom_line(aes(y =totala), color = "blue") 
    plots <- grid.arrange(tdrplot, axyplot, ncol = 1)
    setwd("E:/Chapter 4 - foraging success/TDRAXY plots/")
    ggsave(plots, file=paste0(i,"_", j, ".png"), width = 8, height = 5)
    
    
  }
  
  
rm(divespenguin, axy, divephases, divephasenew, axy1, axy2, diveno, axy3, axyplot, tdrplot, plots)
gc()
invisible(gc())
}


