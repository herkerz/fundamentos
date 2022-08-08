library(dplyr)
library(tidyr)

df <- read.table("C:\\Users\\hgker\\Desktop\\master_ds\\fundamentos_r\\data\\marambio_2007.dat")

df %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))