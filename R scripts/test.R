input_dir <- ("E:/raw_penguin/Robben/AXY_Raw/2017R_AXY_Raw")

AXYfiles <- fs::dir_ls(input_dir, glob = "*axytdr.csv", type="file", recurse = TRUE) # list location all files with "tdr" in the name


axydata <- AXYfiles %>%
  set_names(nm = (basename(.))) %>% #strip out the filename by removing path prefix
  map_df(read_csv, .id="filename",col_types = cols(.default = "c")) %>%  #read in TDR files and add filename column (all columns as characters)
  mutate(deployID = (str_replace(filename,"_axytdr", replacement=""))) %>% #extract birdID by removing _axytdr from filename
  mutate(deployID = (tools::file_path_sans_ext(deployID)))

axy<- read.csv("02_2017R_axytdr.csv")

head(axy)


setwd("E:/raw_penguin/Robben/AXY_Raw/2018R_AXY_Raw")

axy1<- read.csv("01_2018R_axytdr.csv", header = T)

head(axy1)


setwd("E:/raw_penguin/Robben/AXY_Raw/2019R_AXY_Raw")

axy2<- read.csv("01_2019R_axytdr.csv", header = T)

head(axy2)


setwd("E:/raw_penguin/Robben/AXY_Raw/2022R_AXY_Raw")

axy3<- read.csv("02_2022R_axytdr.csv", header = T)

head(axy3)
