library(stats)
library(visdat)
library(yardstick)
library(ggplot2)
library(reshape)
library(tidyr)
library(dplyr)
library(funModeling) 
library(stats)
library(forecast)
library(grid)
library(gridExtra)



load("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp3/acath.sav")


## Casteando las variables
acath$sex = factor(acath$sex)
acath$sigdz = as.numeric(acath$sigdz)
acath$age = as.integer(acath$age)
acath$cad.dur = as.numeric(acath$cad.dur)
acath$choleste = as.numeric(acath$choleste)


## FIG 1 MISSING VALUES
vis_dat(acath %>% select(-tvdlm)) +
  labs(title= "Datos faltantes y tipo de datos",
       caption="Fig 1. Missing Data y tipo de datos en el dataset.")



### FIG 2 BOXPLOT DE DISTRIBUCIONES

ggplot(melt(acath %>% select(-sigdz, -tvdlm)),
       aes(x=1, y=value)) +
  
  geom_boxplot(fill="grey",
               outlier.color="dark red") +
  facet_wrap(~variable, scales="free",ncol=3) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title='Distribucion de las variables',
       caption='Fig 2. Boxplot de las variables')

#### NA replace

mean_choleste_fem = colMeans(acath %>% filter(sex == 0 & !is.na(choleste)) %>% select(choleste))
mean_choleste_mal = colMeans(acath %>% filter(sex == 1 & !is.na(choleste)) %>% select(choleste))


acath$choleste[acath$sex == 0 & is.na(acath$choleste)] <- mean_choleste_fem[1]
acath$choleste[acath$sex == 1 & is.na(acath$choleste)] <- mean_choleste_mal[1]

## Split TRAIN TEST

df = acath

idx_sample = get_sample(data = df, 
                        percentage_tr_rows=0.8, 
                        seed = 111)

df_train =df[idx_sample,]
df_test = df[-idx_sample,]


######
################# MODELO LOGISTIC SIMPLE

modelo_logistic_simple = glm(sigdz ~ choleste, 
                             family=binomial,
                             data = df_train)

explanatory_data <- tibble(
  choleste = c(199)
)

prediction_data <- explanatory_data %>% 
  mutate(
    probability = predict(modelo_logistic_simple, 
                          explanatory_data, 
                          type="response"),
    most_likely_response = round(probability),
    odds = probability / ( 1 - probability )
  )



## FIG 3 PREDICCION MODELO SIMPLE
ggplot(df_train, aes( choleste,sigdz)) + 
  geom_point() +
  geom_smooth(se=F, 
              method="glm", 
              method.args=list(family=binomial)
              ) +
  
  geom_point(data = prediction_data,
             aes(choleste,
                 probability
                 ),
             color="red",size=3
             ) +
  
  labs(title="Regresion Logistica univariada",
       caption= "Fig 3. ")


## CONFUSION MATRIX

actual_response <- df_test$sigdz
predicted_response <- round( predict(modelo_logistic_simple,
                                     df_test,
                                     type = "response")
                             )

outcomes <- table(predicted_response,
                  actual_response 
                  )

confusion <- conf_mat(outcomes)

autoplot(confusion,type="heatmap") + 
  labs(title="Confusion Matrix Modelo Simple",
       caption = "Fig 4.")

## TABLA 1 METRICAS 
grid.arrange(tableGrob(summary(confusion,
                               event_level = "second") %>% 
                         slice(1,11,12,13) %>% 
                         mutate(Metric = .metric,
                                Estimate = round(.estimate,4)) %>% 
                         select(Metric, Estimate)),
             top="Metricas Modelo Simple",
             bottom = "Tabla 1")




########
################# MODELO LOGISTIC CON VARIABLES NO CATEGORICAS

modelo_logistic_no_cat <- glm(sigdz ~ choleste + age + cad.dur, 
                              family=binomial,
                              data = df_train)

actual_response <- df_test$sigdz
predicted_response <- round( predict(modelo_logistic_no_cat,
                                     df_test,
                                     type = "response")
                             )
## MATRIZ CONFUSION
outcomes <- table(predicted_response, 
                  actual_response )

confusion <- conf_mat(outcomes)

autoplot(confusion,type="heatmap") + 
  labs(title="Confusion Matrix Modelo No categoricas",
       caption = "Fig 5.")

## METRICAS
grid.arrange(tableGrob(summary(confusion,
                               event_level = "second") %>% 
                         slice(1,11,12,13) %>% 
                         mutate(Metric = .metric,
                                Estimate = round(.estimate,4)) %>% 
                         select(Metric, Estimate)),
             top="Metricas Modelo No categoricas",
             bottom = "Tabla 2")




#####
########### MODELO LOGISTIC CON VARIABLES CATEGORICAS


modelo_logistic_cat <- glm(sigdz ~ sex + choleste + age + cad.dur, 
                           family=binomial,
                           data = df_train)

summary(modelo_logistic_cat)

actual_response <- df_test$sigdz 
predicted_response <- round( predict(modelo_logistic_cat,
                                     df_test,
                                     type = "response")
                             )
## MATRIZ CONFUSION

outcomes <- table(predicted_response, actual_response )

confusion <- conf_mat(outcomes)

autoplot(confusion,type="heatmap") + 
  labs(title="Confusion Matrix Modelo Categoricas",
       caption = "Fig 6.")

## METRICAS
grid.arrange(tableGrob(summary(confusion,
                                event_level = "second") %>% 
                         slice(1,11,12,13) %>% 
                         mutate(Metric = .metric,
                                Estimate = round(.estimate,4)) %>% 
                         select(Metric, Estimate)),
             top="Metricas Modelo Categoricas",
             bottom = "Tabla 3")



######################################## ANOVA


caseros = c(11,14,7,15,11,13,11,16,10,15,
            18,12,9,9,10,10,15,10,14,10,
            10,12,14,12,15,7,13,6,10,15,
            20,10,13,10,6,14,8,10,8,11)

santos_lugares = c(13,10,12,7,5,10,10,16,9,7,
                   7,2,6,9,9,8,8,10,3,6,
                   5,2,9,3,4,5,10,8,5,9,
                   10,8,13,10,0,2,1,1,0,4)

podesta = c(6,7,3,5,9,6,1,6,0,2,
            5,6,11,6,7,0,5,7,5,4,
            7,4,2,8,9,6,1,4,7,7,
            8,9,7,5,1,6,9,4,7,6)

localidad <- factor(c(rep("CAS",40),rep("SL",40),rep("POD",40)))
frecuencia_asistencia <- c(caseros,santos_lugares,podesta)


### analisis de datos graficos y summary
datos <- data.frame(localidad,
                    frecuencia_asistencia)
ggplot(datos,
       aes(localidad,
           frecuencia_asistencia)
       ) +
  
  geom_boxplot() +
  
  labs(title="Distribucion de las variables",
       caption="Fig 1. Boxplot variables") +
  
  ylab("cantidad_asistencia") +
  
  xlab("Localidad")

tapply( frecuencia_asistencia,
        localidad, 
        summary)


## ANOVA TEST
anova_test <- aov(frecuencia_asistencia~localidad)

## VALIDACION MODELO
shapiro.test(anova_test$residuals)

library(ggfortify)
fig_2 <-autoplot(anova_test,which=1:3, ncol = 3)
grid.arrange(grobs = fig_2@plots,
             top=textGrob("Validacion del Modelo ANOVA", 
                          gp=gpar(fontsize=18)), 
             bottom = "Fig 2",ncol=3)


## TEST TUKEY
tukey<-TukeyHSD(anova_test)

plot(tukey, col="brown",sub="Fig 3") 
title("TUKEY HSD",line=3.2)


######################################## TIME SERIES

data(nottem)
nottem_ts = ts(nottem, 
               start=c(1920,1),
               freq=12)
start(nottem_ts)
end(nottem_ts)

## PLOT SERIE DE TIEMPO
plot(nottem_ts,
     col="darkgrey",
     main="Serie de tiempo Nottem",
     sub="Fig 1",
     lwd=2) 

nottem_decomp <- decompose(nottem_ts)

## PLOT TENDENCIA
plot(nottem_decomp$trend,
     main="Grafico de tendencia",
     sub="Fig 2.",
     ylab="Tendencia",
     xlab="Tiempo",
     col = "darkgray",
     lwd=2)

## BOXPLOT POR CICLO
boxplot(nottem_ts~cycle(nottem_ts),
        main="Boxplot por ciclo",
        sub="Fig 3.",
        ylab="Temperatura media",
        xlab="Mes")


nottem_train<- ts(nottem_ts,
                  frequency = 12, 
                  start = c(1920,1), 
                  end = c(1938,12))

### MODELO ARIMA
modelo_arima <- auto.arima(nottem_train, 
                           stationary = FALSE, 
                           seasonal = TRUE)

summary(modelo_arima)

forecast_temp<- forecast::forecast(modelo_arima, 
                         h =12)


table <- tibble(Mes = c("Jan","Feb","Mar",
                        "Apr","May","Jun",
                        "Jul","Aug","Sep",
                        "Oct","Nov","Dec"),
                Actual = round(tail(nottem_ts,12),1),
                Forecasted = round(forecast_temp$mean,1),
                Error = Actual - Forecasted)

## TABLA ACTUAL VS FORECAST
grid.arrange(tableGrob(table),
             top="Actual vs Forecasted",
             bottom = "Tabla 1")

## PLOT FORECAST
plot(forecast_temp,
     main="Forecast ARIMA",
     sub="Fig 4.",
     lwd=2,
     col="darkgrey",
     ylab="Temperatura media",
     xlab="Tiempo")
