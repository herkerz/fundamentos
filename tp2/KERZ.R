library(ggplot2)
library(GGally)
library(tidyr)
library(dplyr)
library(visdat)
library(reshape)
library(funModeling) 
library(grid)
library(gridExtra)
library(ggfortify)
library(broom)
library(olsrr)
library(pls)
library(Metrics)
library(lmtest)

df <- read.table("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp2/data_multiple_regression_exercice.csv")

## FIG 1 MISSING DATA
vis_dat(df) +
  labs(title= "Datos faltantes y tipo de datos",
       caption="Fig 1. Missing Data y tipo de datos en el dataset.")
 


### FIG 2 DISTRIBUCION DE VARIABLES - BOXPLOT

melt(df) %>%

  ggplot(.,
       aes(x=1, y=value)) +

  geom_boxplot(fill="grey",
               outlier.color="dark red") +
  facet_wrap(~variable, scales="free",ncol=12) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title='Distribucion de las variables',
       caption='Fig 2. Boxplot de las variables')


### FIG 3 BOXPLOT MAHALANOBIS

 
df %>% mutate(maha = mahalanobis(.,colMeans(.),cov(.))) %>%

  ggplot(.,aes(x=1,y=maha)) +
    geom_boxplot(fill="grey",
                 outlier.color="dark red") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()
          ) +

    labs(title="Distancia de Mahalanobis",
         caption="Fig 3. Boxplot Mahalanobis") +

    ylab("Distancia de Mahalanobis Calculada")



### FILTRADO DATASET Y SPLIT TRAIN-TEST


is_outlier <- function(data){
  iqr = IQR(data)
  quantiles = quantile(data, probs=c(.25,.75))
  return (data < quantiles[1] - (1.5 * iqr) |
            data > quantiles[2] + (1.5 * iqr))
}


df_filtrado <- df %>% 
  mutate(maha = mahalanobis(.,colMeans(.),cov(.))) %>%
  filter(!is_outlier(maha)) 


idx_sample = get_sample(data = df_filtrado, percentage_tr_rows=0.8, seed = 222)

df_train =df[idx_sample,]
df_test = df[-idx_sample,]


########## MODELO LINEAL

modelo_lineal = lm(weight ~ . , data= df_train)


## FIG 4 COEFFICIENT PLOT MODELO LINEAL

coef_plot1<- tidy(modelo_lineal, conf.int = TRUE) %>%
  mutate(p_value_lower_0.05 = p.value < 0.05)

ggplot(coef_plot1[-1,], 
       aes(estimate, 
           term, 
           xmin = conf.low, 
           xmax = conf.high, 
           height = 0,
           color=p_value_lower_0.05)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 4) +
  geom_errorbarh() +
  labs(title="Coefficient Plot",
       caption="Fig 4. Coefficiet Plot") +
  xlab("Beta estimado") +
  ylab("")


## FIG 5 PLOTEOS VALIDACION MODELO LINEAL

fig_5 <-autoplot(modelo_lineal,which=1:3)
grid.arrange(grobs = fig_5@plots, 
             top=textGrob("Validacion del Modelo Lineal", 
                          gp=gpar(fontsize=18)), 
             bottom = "Fig 5",ncol=3)


## TEST DE HETEROCEDASTICIDAD Y SUMMARY
bptest(modelo_lineal)
ols_coll_diag(modelo_lineal)
summary(modelo_lineal)


  
###### MODELO LINEAL CON VARIABLE EXPLICADA EN LOG
  
modelo_lineal_log = lm(log(weight) ~ . , data= df_train)

## FIG 6 PLOTEOS DE VALIDACION MODELO LINEAL CON VARIABLE EN LOG

fig_6 <-autoplot(modelo_lineal_log,which=1:3)
grid.arrange(grobs = fig_6@plots,
             top=textGrob("Validacion del Modelo con Log", 
                          gp=gpar(fontsize=18)), 
             bottom = "Fig 6",ncol=3)

  

## TEST DE HETEROCEDASTICIDAD Y SUMMARY
summary(modelo_lineal_log)
bptest(modelo_lineal_log)
ols_coll_diag(modelo_lineal_log)

## FIG 7 COEFFICIENT PLOT VARIABLE EN LOG

coef_plot2<- tidy(modelo_lineal_log, conf.int = TRUE) %>%
  mutate(p_value_lower_0.05 = p.value < 0.05)

ggplot(coef_plot2[-1,], 
       aes(estimate, 
           term, 
           xmin = conf.low, 
           xmax = conf.high, 
           height = 0,
           color=p_value_lower_0.05)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 4) +
  geom_errorbarh() + 
  labs(title="Coefficient Plot de LOG",
       caption="Fig 7. Coefficiet Plot") +
  xlab("Beta estimado") +
  ylab("")

## FIG 8 CORRELATION PLOT

library(tidyverse)

cor_data <- cor(df_filtrado %>% select(-maha)) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  pivot_longer(-rowname)

cor_data %>% 
  ggplot(aes(x=rowname, y=name, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue",mid="white",high="darkgreen",midpoint=0,limit=c(-1,1)) + 
  labs(title="Matriz de correlacion",
       caption="Fig 8. Matriz de correlacion") +
  ylab("") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

######## MODELO VARIABLE EXPLICADA EN LOG CON MIX SELECTION


modelo_lineal_mixed = ols_step_both_p(modelo_lineal_log,
                                      pent=0.05,
                                      prem=0.15)



## FIG 9 COEFFICIENT PLOT MIXED SELECTION

coef_plot3<- tidy(modelo_lineal_mixed$model, conf.int = TRUE) %>%
  mutate(p_value_lower_0.05 = p.value < 0.05)

ggplot(coef_plot3[-1,], 
       aes(estimate, 
           term, 
           xmin = conf.low, 
           xmax = conf.high, 
           height = 0,
           color=p_value_lower_0.05)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 4) +
  geom_errorbarh() + 
  labs(title="Coefficient Plot de Mixed",
       caption="Fig 9. Coefficiet Plot") +
  xlab("Beta estimado") +
  ylab("")


## FIG 10 VALIDACION MODELO MIX SELECTION
fig_10 <-autoplot(modelo_lineal_mixed$model,
                  which=1:3)
grid.arrange(grobs = fig_10@plots,
             top=textGrob("Validacion del Modelo con Mixed Selection", 
                          gp=gpar(fontsize=18)), 
             bottom = "Fig 10",ncol=3)


## SUMMARY Y TEST HETEROCEDASTICIDAD
summary(modelo_lineal_mixed$model)

bptest(modelo_lineal_mixed$model)





########### MODELO PCR
modelo_pca = pcr(log(weight) ~ ., 
                 data= df_train,scale=T)

## FIG 11 VALIDACION PCR
validation_data <- tibble(residuos = modelo_pca$residuals[,,8],
                          residuos.std = (residuos - mean(residuos)) / sd(residuos),
                          fitted_values = modelo_pca$fitted.values[,,8])


res.std_plot <- ggplot(validation_data, 
                       aes(fitted_values,residuos.std)) + 
  geom_point() + 
  xlab("Fitted Values") +
  ylab("Standarized residuals") +
  labs(title="Scale - Location")

qq_plot <- ggplot(validation_data, 
                  aes(sample=residuos.std)) + 
  geom_qq() +
  geom_qq_line() +
  xlab("Theorical Quantiles") +
  ylab("Standarized residuals") +
  labs(title="Normal Q-Q")

grid.arrange(res.std_plot,qq_plot,ncol=2,
             top = "Validacion del Modelo PCR",
             bottom="Fig 11")


## FIG 12 SCREE PLOT PCR
data.frame(var_explicada = modelo_pca$Xvar / modelo_pca$Xtotvar) %>% 
    ggplot(.,aes(1:length(var_explicada),var_explicada)) + 
    geom_line() + 
    geom_point(shape="O") + 
    xlab("Componente Principal") + 
    ylab("Porcentaje de Varianza Explicada") +
    labs(title = "Varianza explicada por Componente Principal",
         caption ="Fig 12. Screeplot")




## TABLA 1
rmse_log_test <-rmse(log(df_test$weight),
                     predict(modelo_lineal_log,df_test))
rmse_log_train <- rmse(log(df_train$weight),
                       predict(modelo_lineal_log,df_train))

rmse_mix_test <-rmse(log(df_test$weight),
                     predict(modelo_lineal_mixed$model,df_test))
rmse_mix_train<-rmse(log(df_train$weight),
                     predict(modelo_lineal_mixed$model,df_train))

rmse_pca_test <-rmse(log(df_test$weight),
                     predict(modelo_pca,df_test,ncomp=7))
rmse_pca_train <- rmse(log(df_train$weight),
                       predict(modelo_pca,df_train,ncomp=7))

table.rmse <- data.frame(Total_Var = c(rmse_log_train,rmse_log_test),
                     Mixed_Sel = c(rmse_mix_train,rmse_mix_test),
                     PCR = c(rmse_pca_train,rmse_pca_test))

rownames(table.rmse) <- c("Train","Test")

grid.arrange(tableGrob(round(table.rmse,4)),
             top="Comparacion de RMSE",
             bottom = "Tabla 1")

### FIG 13 ABLINE Y PREDICCIONES TODOS LOS MODELOS

ab_pca <- data.frame(predictions = predict(modelo_pca,df_test,ncomp=8),
                      real_values = log(df_test$weight)) %>% 
  ggplot(.,aes(real_values,.[,1])) + 
  geom_point() + 
  geom_abline(intercept = 0,slope= 1, color="blue",size=1,alpha=0.5) +
  xlab("Valores Testeo") +
  ylab("Predicciones") +
  labs(title="PCR")

ab_mix <- data.frame(predictions = predict(modelo_lineal_mixed$model,df_test),
                      real_values = log(df_test$weight)) %>% 
  ggplot(.,aes(real_values,predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0,slope= 1, color="green",size=1,alpha=0.5)  +
  xlab("Valores Testeo") +
  ylab("Predicciones") +
  labs(title="Mixed Selection")

ab_log <- data.frame(predictions = predict(modelo_lineal_mixed$model,df_test),
                      real_values = log(df_test$weight)) %>% 
  ggplot(.,aes(real_values,predictions)) + 
  geom_point() + 
  geom_abline(intercept = 0,slope= 1, color="red",size=1,alpha=0.5)  +
  xlab("Valores Testeo") +
  ylab("Predicciones") +
  labs(title="Total Variables")

grid.arrange(ab_log,ab_mix,ab_pca,ncol=3,
             top="Predicciones contra Valores Testeo",
             bottom = "Fig 13")
