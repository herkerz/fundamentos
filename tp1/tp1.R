library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(visdat)
library(reshape)
library(gridExtra)
library(factoextra)



df <- read.csv("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp1/europe.csv")


## Graficos visualizacion de datos faltantes y tipos de datos FIG 1

vis_dat(df) +
  labs(title= "Datos faltantes y tipo de datos",
    caption="Fig 1. Missing Data y tipo de datos en el dataset.")

# Tratamiento y filtrado del dataset

df <- df %>% 
      mutate(Area = Area / 1000 ,
               GDP = GDP / 1000 ) 

df_numeric <- df %>%  select_if(is.numeric)
row.names(df_numeric) = df$Country

# FIG 2 : Pairplot 

ggpairs(df,
        columns = 2:8,
        lower=list(continuous=wrap("points",colour="navy")),
        diag=list(continuous=wrap(ggally_box_no_facet,
                                  colour = "dark red",
                                  fill="grey",
                                  outlier.color="dark red"))
        ) +
  labs(title = "Pairplot del dataset",
       caption="Fig 2. Análisis exploratorio de datos")



## FIG 3: Boxplot para analisis de distribucion y outliers 
ggplot(melt(df),
       aes(x=1, y=value)) +
  
  geom_boxplot(fill="grey",
               outlier.color="dark red") +
  facet_wrap(~variable, scales="free",ncol=4) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title='Distribucion de las variables',
       caption='Fig 3. Boxplot de las variables')





# Calculo de Mahalanobis Dist

df_with_dist <- df %>% 
    select_if(is.numeric) %>% 
    mutate(Dist_Mahalanobis = mahalanobis(.,colMeans(.),cov(.))) %>% 
    left_join(df) %>% 
    select("Country","Dist_Mahalanobis","Area","GDP",
           "Inflation","Life.expect","Military","Pop.growth","Unemployment") 


# FIG 4: Boxplot Maha dist

ggplot(df_with_dist,aes(x=1,y=Dist_Mahalanobis)) +
  
  geom_boxplot(fill="grey",
               outlier.color="dark red") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
        ) +
  labs(title="Boxplot Distancia de Mahalanobis",
       caption="Fig 4. Boxplot Mahalanobis") +
  ylab("Distancia de Mahalanobis Calculada")


# TABLA 1: con valores de atipicos de distancia Maha
## Funcion para filtrar outliers en base a criterio 1.5*IQR

is_outlier <- function(data){
  iqr = IQR(data)
  quantiles = quantile(data, probs=c(.25,.75))
  return (data < quantiles[1] - (1.5 * iqr) |
            data > quantiles[2] + (1.5 * iqr))
}

## Tabla 

df_with_dist %>%
  arrange(Dist_Mahalanobis) %>%
  filter(is_outlier(Dist_Mahalanobis)) %>%
  tableGrob(.) %>%
  grid.arrange(.,
               top="Observaciones outliers Distancia Mahalanobis",
               bottom="Tabla 1. Observaciones Outliers")





### Box plot para analisis de distribucion y outliers

# Funcion de calculo de elipse mahalanobis de paquete ggtern
# importar el paquete entero producia error en alguna funcion de ggplot

mahalanobis_distance <- function(x,x.mean,x.cov,whichlines=c(0.95),m=360){
                    mdX       = matrix(NA,nrow=m,ncol=length(whichlines))
                    mdY       = matrix(NA,nrow=m,ncol=length(whichlines))
                    cov.svd   = svd(x.cov, nv = 0)
                    r         = cov.svd[["u"]] %*% diag(sqrt(cov.svd[["d"]]))
                    alphamd   = sqrt(qchisq(whichlines,2))
                    for (j in 1:length(whichlines)){
                      e1md    = cos(c(0:(m-1))/m * 2 * pi) * alphamd[j]
                      e2md    = sin(c(0:(m-1))/m * 2 * pi) * alphamd[j]
                      emd     = cbind(e1md, e2md)
                      ttmd    = t(r %*% t(emd)) + rep(1, m) %o% x.mean
                      mdX[,j] = ttmd[,1]
                      mdY[,j] = ttmd[,2]
                    }
  list(mdX = mdX,
       mdY = mdY)
}


maha_elipse_1 = mahalanobis_distance(df %>% select(Inflation,GDP),
                        colMeans(df %>% select(Inflation, GDP)),
                        cov(df %>% select(Inflation, GDP)),
                        whichlines=c(0.95))

maha_1 <- ggplot(df, aes(Inflation,GDP)) +
  geom_point(col="navy") +
  geom_point(data.frame(maha_elipse_1), 
             mapping=aes(mdX,mdY),
             shape="-",
             col="darkred") +
  labs(title="Inflation vs GDP outliers ")



maha_elipse_2 = mahalanobis_distance(df %>% select(Unemployment,GDP),
                                     colMeans(df %>% select(Unemployment, GDP)),
                                     cov(df %>% select(Unemployment, GDP)),
                                     whichlines=c(0.95))

maha_2 <- ggplot(df, aes(Unemployment,GDP)) +
  geom_point(col="navy") +
  geom_point(data.frame(maha_elipse_2), 
             mapping=aes(mdX,mdY),
             shape="-",
             col="darkred") +
  labs(title="Unemployment vs GDP outliers")


grid.arrange(maha_1, maha_2, ncol=2,
             bottom="Fig 5.Comparación outliers Mahalabonis vs distribución")



# Fig 6. Matriz de Correlacion

ggcorr(df_numeric, label = T,
       hjust=0.75) +
    labs(title= "Matriz de correlacion",
        caption="Fig 6. Matriz de correlacion.")






pca_cov <- prcomp(df %>% select_if(is.numeric), scale=FALSE) #esta diferencia es para quitarle los rownames de los graficos
pca_cor <- prcomp(df_numeric, scale=TRUE) 



# fig 7 Autovalores asociados

eigenvalue_cov <- fviz_eig(pca_cov,
                                   addlabels = TRUE,
                                   ggtheme = theme_gray(),
                                   main="Matriz de Covarianza",
                                   xlab="Componente Principal",
                                   ylab="Autovalor asociado",
                                  choice="eigenvalue",
                                  ylim=c(0,30000))
eigenvalue_cor <- fviz_eig(pca_cor,
                                   addlabels = TRUE,
                                   ggtheme = theme_gray(),
                                   main="Matriz de Correlacion",
                                   xlab="Componente Principal",
                                   ylab="Autovalor asociado",
                                   choice="eigenvalue",
                                  ylim=c(0,4))


grid.arrange(eigenvalue_cov, eigenvalue_cor,
             top="Autovalor asociado a cada componente",
             bottom="Fig 7. Autovalores")



# Fig 8. Porcentaje de varianza explicado
varianza_explicada_cov <- fviz_eig(pca_cov,
                                   addlabels = TRUE,
                                   ggtheme = theme_gray(),
                                   main="Matriz de Covarianza",
                                   xlab="Componente Principal",
                                   ylab="Porcentaje de varianza explicada",
                                   ylim=c(0,105))
varianza_explicada_cor <- fviz_eig(pca_cor,
                                   addlabels = TRUE,
                                   ggtheme = theme_gray(),
                                   main="Matriz de Correlacion",
                                   xlab="Componente Principal",
                                   ylab="Porcentaje de varianza explicada",
                                   ylim=c(0,100))
grid.arrange(varianza_explicada_cov, varianza_explicada_cor,
             top="Porcentaje de Varianza explicada por cada componente",
             bottom="Fig 8. Varianza explicada")



# Tabla 2. Componentes principales
tabla_1 <- tableGrob(round(pca_cov$rotation[,1:2],5)) %>% 
            grid.arrange(.,top="Matriz Covarianzas")

tabla_2 <-tableGrob(round(pca_cor$rotation[,1:2],5)) %>% 
            grid.arrange(.,top="Matriz Correlaciones")

grid.arrange(tabla_1,tabla_2, ncol=2, 
             top="Primeras componentes Principales", 
             bottom=" Tabla 2. Componentes Principales")



# Fig 9: Biplot covarianza 


fviz_pca_biplot(pca_cov ) +
            labs(title= "PCA Biplot utilizando Matriz de Covarianzas",
                 caption = "Fig 9. Biplot Covarianza")

# Fig 10: Biplot correlacion

fviz_pca_biplot(pca_cor, repel = T , col.ind= "darkgrey", col.var="navy") + 
        labs(title="Biplot PCA Matriz de Covarianzas",
             caption = "Fig 10. Biplot Correlacion")
