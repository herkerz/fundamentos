library(stats)
library(visdat)
library(yardstick)
### dijo que la variable tvdlm no se usa ????

load("C:/Users/hgker/Desktop/master_ds/fundamentos_r/tp3/acath.sav")

acath$sex = factor(acath$sex)
acath$sigdz = factor(acath$sigdz)
acath$age = as.integer(acath$age)
acath$cad.dur = as.numeric(acath$cad.dur)
acath$choleste = as.numeric(acath$choleste)
## FIG 1 MISSING VALUES
vis_dat(acath) +
  labs(title= "Datos faltantes y tipo de datos",
       caption="Fig 1. Missing Data y tipo de datos en el dataset.")


## 
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



###### MODELO LOGISTIC SIMPLE

modelo_logistic_simple = glm(sigdz ~ choleste, family=binomial,data = acath)

explanatory_data <- tibble(
  choleste = c(199)
)

prediction_data <- explanatory_data %>% 
  mutate(
    probability = predict(modelo_logistic_simple, explanatory_data, type="response"),
    most_likely_response = round(probability),
    odds = probability / ( 1 - probability )
  )



## FIG 2 PREDICCION MODELO SIMPLE
ggplot(acath, aes(choleste, sigdz)) + 
  geom_point() +
  geom_smooth(se=F, method="glm", method.args=list(family=binomial)) +
  geom_point(data = prediction_data,
             aes(choleste,probability),
             color="red") +
  labs(title="Regresion Logistica univariada",
       caption= "Fig 2. ")


## CONFUSION 
actual_response <- acath %>% filter(!is.na(choleste)) %>% select(sigdz)
predicted_response <- round(fitted(modelo_logistic_simple))

outcomes <- table(predicted_response, actual_response[,1] )
confusion <- conf_mat(outcomes)

summary(confusion,event_level = "second")
######## MODELO LOGISTIC CON VARIABLES NO CATEGORICAS

modelo_logistic_no_cat <- glm(sigdz ~ choleste + age + cad.dur, family=binomial,data = acath)





######## MODELO LOGISTIC CON VARIABLES CATEGORICAS

modelo_logistic_cat <- glm(sigdz ~ sex + choleste + age + cad.dur, family=binomial,data = acath)
