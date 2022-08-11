library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(visdat)
library(reshape)
library(gridExtra)

is_outlier <- function(data){
  iqr = IQR(data)
  quantiles = quantile(data, probs=c(.25,.75))
  return (data < quantiles[1] - (1.5 * iqr) |
                       data > quantiles[2] + (1.5 * iqr))
}

df <- read.csv("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp1/europe.csv")

df <- df %>% 
      mutate(Area = Area / 100000,
               GDP = GDP / 1000) 

# Mahalanobis Dist

df_with_dist <- df %>% 
    select_if(is.numeric) %>% 
    mutate(Dist_Mahalanobis = mahalanobis(.,colMeans(.),cov(.))) %>% 
    left_join(df) %>% 
    select("Country","Dist_Mahalanobis","Area","GDP",
           "Inflation","Life.expect","Military","Pop.growth","Unemployment") 


# # Mahalanobis Dist outliers table
# 
# df_with_dist %>% 
#   arrange(Dist_Mahalanobis) %>% 
#   filter(is_outlier(Dist_Mahalanobis)) %>% 
#   tableGrob(.) %>% 
#   grid.arrange(.)
# 
# 
# # Boxplot Maha dist
# 
# ggplot(df_with_dist,aes(x=1,y=Dist_Mahalanobis)) +
# 
#   geom_boxplot() +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   labs(title="boxplot",
#       caption="Figgfg")

scatter_1 <- ggplot(df %>% filter(!Country %in% c("Spain","Luxembourg")),aes(x=Unemployment,y=GDP)
                    ) +
              geom_point(colour="navy"
                         ) +
              geom_point(data=df %>% filter(Country == "Spain"), 
                         colour="darkorange",
                         size=3
                         ) +
              geom_point(data=df %>% filter(Country == "Luxembourg"), 
                         colour="magenta",
                         size=3
                         ) 

scatter_2 <- ggplot(df %>% filter(!Country %in% c("Ukraine","Luxembourg")),aes(x=Inflation,y=GDP)
                        ) +
              geom_point(colour="navy"
                         ) +
              geom_point(data=df %>% filter(Country == "Ukraine"), 
                         colour="red",
                         size=3,show.legend = TRUE
                         ) +
              geom_point(data=df %>% filter(Country == "Luxembourg"), 
                         colour="magenta",
                         size=3
                         ) 

boxplot_out <- ggplot(df_with_dist,aes(x=Dist_Mahalanobis,y=1)) +
  
  geom_boxplot() +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title="boxplot",
       caption="Figgfg") 

grid.arrange(
             boxplot_out,
             arrangeGrob(scatter_1,
                         scatter_2,ncol=2),

             top="Mahalanobis Distance Outliers")



# vis_miss(df) + 
#   labs(caption="Fig 1. Missing Data.")
# 
# vis_dat(df) + 
#   labs(caption="Fig 1. Missing Data y tipo de datos en el dataset.")
# 
# 
# ggplot(melt(df),
#        aes(x=1, y=value)) +
#   
#     geom_boxplot(fill="grey",
#                  outlier.color="dark red") +
#     facet_wrap(~variable, scales="free") + 
#     theme(axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# 
# ggpairs(df,
#         columns = 2:8,
#         lower=list(continuous=wrap("points",colour="navy")),
#         diag=list(continuous=wrap(ggally_box_no_facet, 
#                                   colour = "dark red", 
#                                   fill="grey",
#                                   outlier.color="dark red")
#                   )
#         ) +
#   labs(caption="Fig 2. Exploratory data analysis")
#   
# 
# ggcorr(df, label = T) +
#   labs(title= "Matriz de correlacion",
#       caption="Fig 3. Matriz de correlacion.")




