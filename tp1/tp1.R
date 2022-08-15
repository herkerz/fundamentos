library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(visdat)
library(reshape)
library(gridExtra)
library(FactoMineR)
library(factoextra)

is_outlier <- function(data){
  iqr = IQR(data)
  quantiles = quantile(data, probs=c(.25,.75))
  return (data < quantiles[1] - (1.5 * iqr) |
                       data > quantiles[2] + (1.5 * iqr))
}

df <- read.csv("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp1/europe.csv")

df <- df %>% 
      mutate(Area = Area / 1000 ,
               GDP = GDP / 1000 ) 

df_numeric <- df %>%  select_if(is.numeric)

# Mahalanobis Dist

df_with_dist <- df %>% 
    select_if(is.numeric) %>% 
    mutate(Dist_Mahalanobis = mahalanobis(.,colMeans(.),cov(.))) %>% 
    left_join(df) %>% 
    select("Country","Dist_Mahalanobis","Area","GDP",
           "Inflation","Life.expect","Military","Pop.growth","Unemployment") 


# # Mahalanobis Dist outliers table
# # 
# df_with_dist %>%
#   arrange(Dist_Mahalanobis) %>%
#   filter(is_outlier(Dist_Mahalanobis)) %>%
#   tableGrob(.) %>%
#   grid.arrange(.)
# 
# 
# # Boxplot Maha dist

ggplot(df_with_dist,aes(x=1,y=Dist_Mahalanobis)) +

  geom_boxplot() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="boxplot",
      caption="Figgfg")


# 
# scatter_1 <- ggplot(df %>% filter(!Country %in% c("Spain","Luxembourg")),aes(x=Unemployment,y=GDP)
#                     ) +
#               geom_point(colour="navy"
#                          ) +
#               geom_point(data=df %>% filter(Country == "Spain"),
#                          colour="darkorange",
#                          size=3
#                          ) +
#               geom_point(data=df %>% filter(Country == "Luxembourg"),
#                          colour="magenta",
#                          size=3
#                          )
# 
# scatter_2 <- ggplot(df %>% filter(!Country %in% c("Ukraine","Luxembourg")),aes(x=Inflation,y=GDP)
#                         ) +
#               geom_point(colour="navy"
#                          ) +
#               geom_point(data=df %>% filter(Country == "Ukraine"),
#                          colour="red",
#                          size=3,show.legend = TRUE
#                          ) +
#               geom_point(data=df %>% filter(Country == "Luxembourg"),
#                          colour="magenta",
#                          size=3
#                          )
# 
# boxplot_out <- ggplot(df_with_dist,aes(x=1,y=Dist_Mahalanobis)) +
# 
#   geom_boxplot() +
#   theme(axis.text.x =element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.x=element_blank()) +
#   labs(title="boxplot",
#        caption="Figgfg")
# 
# grid.arrange(
#              boxplot_out,scatter_1,
#                          scatter_2,ncol=3,
# 
#              top="Mahalanobis Distance Outliers")
# 
# 

# vis_miss(df) + 
#   labs(caption="Fig 1. Missing Data.")
# 
# vis_dat(df) + 
#   labs(caption="Fig 1. Missing Data y tipo de datos en el dataset.")
# 

# ggplot(melt(df %>% select(-Pop.growth)),
#        aes(x=1, y=value)) +
# 
#     geom_boxplot(fill="grey",
#                  outlier.color="dark red") +
#     facet_wrap(~variable, scales="free") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           axis.title.y=element_blank()) +
#     labs(title='Distribucion de las variables',
#          caption='Fig 3. Boxplot de las variables')
# 
# ggplot(df, aes(x=Unemployment)) + geom_histogram(bins=12)
# qqnorm(df$Area)
# qqline(df$Area)


ggpairs(df,
        columns = 2:8,
        lower=list(continuous=wrap("points",colour="navy")),
        diag=list(continuous=wrap(ggally_box_no_facet,
                                  colour = "dark red",
                                  fill="grey",
                                  outlier.color="dark red")
                  )
        ) +
  labs(title = "Pairplot del dataset",
    caption="Fig 2. Análisis exploratorio de datos")

# 
# ggcorr(df, label = T) +
#   labs(title= "Matriz de correlacion",
#       caption="Fig 3. Matriz de correlacion.")


pca_cov <- prcomp(df_numeric, scale=FALSE)
pca_cor <- prcomp(df_numeric, scale=TRUE)

fviz_pca_biplot(pca_cov, repel = TRUE, col.ind = "darkgrey",col.var="navy")
fviz_pca_biplot(pca_cor, repel = TRUE, col.ind = "darkgrey",col.var="navy")



varianza_explicada_cov <- fviz_eig(pca_cov, 
         addlabels = TRUE, 
         ggtheme = theme_gray(), 
         main="Matriz de Covarianza",
         xlab="Principal Component",
         ylab="Porcentaje de varianza explicada",
         ylim=c(0,100)) 
varianza_explicada_cor <- fviz_eig(pca_cor, 
         addlabels = TRUE, 
         ggtheme = theme_gray(), 
         main="Matriz de Correlacion",
         xlab="Principal Component",
         ylab="Porcentaje de varianza explicada",
         ylim=c(0,100)) 
grid.arrange(varianza_explicada_cov, varianza_explicada_cor,
             top="Porcentaje de Varianza explicada por cada componente",
             bottom="Fig X.")




eigenvalue_cov <- fviz_eig(pca_cov, 
                                   addlabels = TRUE, 
                                   ggtheme = theme_gray(), 
                                   main="Matriz de Covarianza",
                                   xlab="Principal Component",
                                   ylab="Autovalor asociado",
                                  choice="eigenvalue",
                                  ylim=c(0,240)) 
eigenvalue_cor <- fviz_eig(pca_cor, 
                                   addlabels = TRUE, 
                                   ggtheme = theme_gray(), 
                                   main="Matriz de Correlacion",
                                   xlab="Principal Component",
                                   ylab="Autovalor asociado",
                                   choice="eigenvalue",
                                  ylim=c(0,4)) 


grid.arrange(eigenvalue_cov, eigenvalue_cor,
             top="Autovalor asociado a cada componente",
             bottom="Fig X.")


tabla_1 <- tableGrob(round(pca_cov$rotation[,1:2],5)) %>% grid.arrange(.,top="Matriz Covarianzas")
tabla_2 <-tableGrob(round(pca_cor$rotation[,1:2],5)) %>% grid.arrange(.,top="Matriz Correlaciones")
grid.arrange(tabla_1,tabla_2, ncol=2, top="Autovectores principales")


fviz_pca_biplot(pca_cov) +
            labs(title= "PCA Biplot utilizando Matriz de Covarianzas")


fviz_pca_biplot(pca_cor,label="all",habillage =df$Country) +
  labs(title= "PCA Biplot utilizando Matriz de Correlaciones")

biplot_cor <- fviz_pca_biplot(pca_cor)+ labs(title="Biplot PCA Matriz de Covarianzas")

