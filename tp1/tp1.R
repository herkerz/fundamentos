library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(visdat)
library(reshape)

df <- read.csv("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp1/europe.csv")

df <- df %>% 
        mutate(Area = Area / 100000,
               GDP = GDP / 1000)

vis_miss(df) + labs(caption="Fig 1. Missing Data.")
vis_dat(df) + labs(caption="Fig 1. Missing Data y tipo de datos en el dataset.")


ggplot(melt(df),
       aes(x=1, y=value)) +
  
    geom_boxplot(fill="grey",
                 outlier.color="dark red") +
    facet_wrap(~variable, scales="free") + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())


ggpairs(df,
        columns = 2:8,
        lower=list(continuous=wrap("points",colour="navy")),
        diag=list(continuous=wrap(ggally_box_no_facet, 
                                  colour = "dark red", 
                                  fill="grey",
                                  outlier.color="dark red")
                  )
        ) +
  labs(caption="Fig 2. Exploratory data analysis")
  

# 
# region <- function(country) {
#   
#   east_europe <- c("Bulgaria","Czech Republic","Hungary","Poland","Slovakia","Ukraine")
#   north_europe <- c("Denmark","Estonia","Finland","Iceland","Ireland","Latvia","Lithuania","Norway","Sweden","United Kingdom")
#   south_europe <- c("Croatia","Greece","Italy","Portugal","Slovenia","Spain")
#   ##west_europe <- c("Austria","Belgium","Germany","Luxembourg","Netherlands","Switzerland")
#   
#   if (country %in% east_europe) return("Eastern Europe") 
#   if (country %in% north_europe) return("Northen Europe") 
#   if (country %in% south_europe) return("Southern Europe") 
#   else return("Western Europe")
# }
# 
# df_region <- df %>% 
#   rowwise() %>% 
#   mutate(Region = factor(region(Country)))
# 
# df_region %>% select(Country, Region) %>% count(Region)




# df_t <- data.frame(t(df))
# names(df_t) <- df_t[1,]
# df_t <- df_t[-1,]



# ggplot(df,aes(Military,GDP, color = Region, size=Unemployment)) +
#   geom_point() +
#   scale_x_log10()



# ###### graph with corr color
# 
# my_fn <- function(data, mapping, method="p", use="pairwise", ...){
# 
#   # grab data
#   x <- eval_data_col(data, mapping$x)
#   y <- eval_data_col(data, mapping$y)
# 
#   # calculate correlation
#   corr <- cor(x, y, method=method, use=use)
# 
#   # calculate colour based on correlation value
#   # Here I have set a correlation of minus one to blue,
#   # zero to white, and one to red
#   # Change this to suit: possibly extend to add as an argument of `my_fn`
#   colFn <- colorRampPalette(c("navy", "white", "red"), interpolate ='spline')
#   fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
# 
#   ggally_cor(data = data, mapping = mapping, ...) +
#     theme_void() +
#     theme(panel.background = element_rect(fill=fill))
# }
# 
# ggpairs(df_region,columns = 2:8,
#         upper = list(continuous = my_fn))
# # lower = list(continuous = "smooth"))
# 
