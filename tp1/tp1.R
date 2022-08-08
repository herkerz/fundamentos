library(ggplot2)
library(GGally)
library(tidyr)

df <- read.csv("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp1/europe.csv")


region <- function(country) {
  
  east_europe <- c("Bulgaria","Czech Republic","Hungary","Poland","Slovakia","Ukraine")
  north_europe <- c("Denmark","Estonia","Finland","Iceland","Ireland","Latvia","Lithuania","Norway","Sweden","United Kingdom")
  south_europe <- c("Croatia","Greece","Italy","Portugal","Slovenia","Spain")
  ##west_europe <- c("Austria","Belgium","France","Germany","Luxembourg","Netherlands","Switzerland")
  
  if (country %in% east_europe) return("Eastern Europe") 
  if (country %in% north_europe) return("Northen Europe") 
  if (country %in% south_europe) return("Southern Europe") 
  else return("Western Europe")
}

df_region <- df %>% 
  rowwise() %>% 
  mutate(Region = factor(region(Country)))

df_region %>% select(Country, Region) %>% count(Region)

glimpse(df)
summary(df)


cov_matrix = cov(select_if(df, is.numeric))

library(reshape)
meltData <- melt(df %>% select(-Area))
boxplot(data=meltData, value~variable)

# df_t <- data.frame(t(df))
# names(df_t) <- df_t[1,]
# df_t <- df_t[-1,]

# ggpairs(df_region,columns = 2:8, mapping=aes(color=Region))

# ggplot(df,aes(Military,GDP, color = Region, size=Unemployment)) + 
#   geom_point() + 
#   scale_x_log10()

