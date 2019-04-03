rm(list = ls())
library(dplyr)
library(simPH)
library(riskRegression)
library(pROC)
library(ggplot2)
library(plyr)
library(tidyverse)
require(INLA)
require(lme4)
require(GGally)
library(INLAutils)
require(mgcv)
library(readxl)
library(lubridate)
theme_set(theme_bw())  

#===========================================================
## Directorio de trabajo automatico, tomado de 
# Get current directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
source("Multiplot.R")
#===========================================================
# If prob > 0.5 then 1, else 
precision = function(probs_mod, respuesta, threshold=0.5){
  results <- ifelse(probs_mod > threshold, 1, 0)
  misClasificError <- mean(respuesta != results)
  acc <- 1-misClasificError
  # prec = round(acc*100 ,3)
  print(paste("precision = ", round(acc*100,3), "%"))
}
#===========================================================

#### rEVISAR 947 -- SOFICAM ... va tener problemas.

# data <- xlsx::read.xlsx(file = "Final_Data_for_Analysis_Indicadores_v4.xlsx",
#                         sheetIndex = 1,
#                         startRow = 1)
# data$TASA_A_IMP = as.numeric(as.character(data$TASA_A_IMP))
# data$TASA_DE_EQUILIBRIO = as.numeric(as.character(data$TASA_DE_EQUILIBRIO))
# data$RENTABILIDAD_CARTERA = as.numeric(as.character(data$RENTABILIDAD_CARTERA))

# saveRDS(data, file = "data_cleaned_final_V4.rds")
df = readRDS(file = "data_cleaned_final_V4.rds")


times = read_excel("INICIO_OPERACIONES_IFNB - CLEAN.xlsx",
                   sheet = 2)

times = times %>% select("CLAVE_IF", "DATE_INICIO")
colnames(times)[1] <- "CLAVE"


df = df %>% left_join(times, by = "CLAVE")
df = df %>% mutate(CURRENT_DATE = ymd(paste0(anio,"-", mes,"-15")))
df = df %>% mutate(INT = interval(DATE_INICIO,CURRENT_DATE))
df = df %>% mutate(TiempoOper = INT %/% months(1))
df$TiempoOper <- df$TiempoOper/365  # in years.
df = df %>% select(-INT)


df = data.table(df)


# data = df %>% filter( mes %in% c(3,6,9,12) )

data = data[-which(abs(data$ROE)>1000),]
data = df


f = STATUS2 ~1 + ROA + ROE + ICVN + ICV + EFI_OPER + LIQUIDEZ_ACTIVOS + APALANC +
  TASA_A_IMP + TASA_P_IMP + TASA_DE_EQUILIBRIO + RAZON_GADMON_CCT + RENTABILIDAD_CARTERA +
  RAZON_MARGEN_FIN + RAZON_MARGEN_FIN_AJU + RAZON_MARGEN_OP + RAZON_MARGEN_NETO + 
  RAZON_CCFIRA_CCT + ICAP + TiempoOper + I(TiempoOper^2) + 
  f(CLAVE, model="ar1")

modelo = inla(f, data=data, 
              family="binomial", 
              Ntrials=1,
              control.predictor=list(compute=TRUE),
              control.family = list(link = "probit"),
              control.compute = list(dic = TRUE, waic = TRUE))

autoplot(modelo) #, which = c(1, 5), CI = TRUE)
modelo$waic$waic  #The lower AIC score signals a better model.

random_effects = modelo$summary.random$CLAVE
random_effects = as.data.frame(random_effects %>% select(ID, mean, `0.025quant`, `0.975quant`) )
colnames(random_effects)[1] = "CLAVE"

random_effects1 = random_effects[which(random_effects$mean > 0.5),]


(g3 =   ggplot(random_effects1, aes(fct_reorder(as.factor(CLAVE) , mean), mean )) + 
  geom_point(stat="identity", aes(color = mean), size=5) + 
  geom_errorbar(width=.25, aes(ymin=`0.025quant`, ymax=`0.975quant`))+
  geom_hline(yintercept = 0, col="red")+
  labs(title="Parametros") +
  ylab("Efecto")+ xlab("") +
  theme(axis.text.x = element_text(size=10 , angle=90, vjust=0.6),
        axis.text.y = element_text(size=8 )) + coord_flip()
)

EFECTOS = as.data.frame(modelo$summary.fixed)
EFECTOS = EFECTOS %>% rownames_to_column()
colnames(EFECTOS) = c("VARIABLE", "EFECTO", "STD_ERROR", "CI",  "0.5_q", "CS", "MODE", "KLD")
probabilidades_tr = modelo$summary.fitted.values$mean
mean_prob = mean(probabilidades_tr)
EFECTOS = EFECTOS %>% mutate(marg_prob = mean_prob*EFECTO*100)


auc(response = data$STATUS2, predictor = probabilidades_tr)
precision(probabilidades_tr, data$STATUS2)


EFECTOS1 = EFECTOS[-which(abs(EFECTOS$EFECTO) > 0.7),]
(
g1 =   ggplot(EFECTOS1, aes(fct_reorder(VARIABLE , EFECTO), EFECTO )) + 
    geom_point(stat="identity", aes(color = EFECTO), size=5) + 
    geom_errorbar(width=.25, aes(ymin=CI, ymax=CS))+
    geom_hline(yintercept = 0, col="red")+
    labs(title="Parametros") +
    ylab("Efecto")+ xlab("") +
    theme(axis.text.x = element_text(size=10 , angle=90, vjust=0.6),
          axis.text.y = element_text(size=8 )) + coord_flip()
)

EFECTOS2 = EFECTOS[-which(abs(EFECTOS$EFECTO) <= 0.7),]

(
g2 =   ggplot(EFECTOS2, aes(fct_reorder(VARIABLE , EFECTO), EFECTO )) + 
    geom_point(stat="identity", aes(color = EFECTO), size=5) + 
    geom_errorbar(width=.25, aes(ymin=CI, ymax=CS))+
    geom_hline(yintercept = 0, col="red")+
    labs(title="Par?metros") +
    ylab("Efecto")+ xlab("")+
    theme(axis.text.x = element_text(size=10 , angle=90, vjust=0.6),
          axis.text.y = element_text(size=8 )) + coord_flip()
)

pdf("efectos_betas.pdf", h=10, w=10)
multiplot(g1, g2)
dev.off()

#=====================================================
# Guardar datos.
# data$Survival_prob = 1-probabilidades_tr
# write.csv(data, file = "Estimated_Surv_Prob.csv", row.names = FALSE)
                    
                    
#=====================================================
## Validacion en testing
id_tst = which(!is.na(match(df$mes, c(1,2,4,5,7,8,10,11) )))
data_tst = df[id_tst,]

X_tst = as.matrix(cbind(1, data_tst[,c(10, 9, 11, 12, 13)]))
betas = EFECTOS$EFECTO   #coef(modelo)

reffect = data_tst %>% select("CLAVE") %>% left_join(random_effects, by = "CLAVE") %>% select(mean) %>% as.vector()

eta_i = t(betas %*% t(X_tst)) + reffect
prob_def = laply(eta_i, pnorm)
data_tst$prob_def = prob_def

auc(response = data_tst$STATUS2, predictor = prob_def)
precision(prob_def, data_tst$STATUS2)


#=====================================================
## Usamos modelo entrenado en full dataset
betas = EFECTOS$EFECTO
X = model.matrix(STATUS2 ~ 1 + ROA + ROE + ICVN + ICV + EFI_OPER + LIQUIDEZ_ACTIVOS + 
                   APALANC + TASA_A_IMP + TASA_P_IMP + TASA_DE_EQUILIBRIO + 
                   RAZON_GADMON_CCT + RENTABILIDAD_CARTERA + RAZON_MARGEN_FIN + 
                   RAZON_MARGEN_FIN_AJU + RAZON_MARGEN_OP + RAZON_MARGEN_NETO + 
                   RAZON_CCFIRA_CCT + ICAP + TiempoOper + I(TiempoOper^2), data = df)# as.matrix(cbind(1, df[,c(10, 11, 12, 13)]))
reffect = df %>% select("CLAVE") %>% left_join(random_effects, by = "CLAVE") 
reffect = reffect$mean
eta_i = as.vector( X %*% betas + reffect)
prob_def = pnorm(eta_i)
df$prob_def = prob_def
df$Survival_Pr = 1-prob_def
df$Score = eta_i

auc(response = df$STATUS2, predictor = prob_def)
precision(prob_def, df$STATUS2)

#=====================================================
### Vemos 947 -  SOFICAM y los muertos.
key = c(838, 848, 853, 860, 939, 942, 959, 960, 965)

plotting_prob_def <- function(key){
  data_to_plot = df %>% filter(CLAVE==key) 
  nombre_if = as.character(data_to_plot$NOMBRE[1])
  ggplot(data_to_plot, aes(x=Final, y=prob_def*100))+geom_point() + 
    geom_smooth(method = "gam", formula = y ~ s(x, k=5), size = 1, se = FALSE) +
    #ylim(0, 100)+
    labs(title = nombre_if,
         subtitle = "Evolucion de la Probabilidad de Default",
         x = "Meses",
         y = "Probabilidad de Default")
  ggsave(filename = paste0(nombre_if, ".pdf"), 
         w = 6, h = 4)  
}

ldply(key, plotting_prob_def)          

## Todos.
keys_2018_dec = df %>% filter(anio == "2018") %>% filter(mes=="12") %>% select(CLAVE) %>% unique()
ldply(keys_2018_dec$CLAVE, plotting_prob_def)          

## Ranking IFNBs a diciembre 2018, de acuerdo a probabilidad de default.
top_dec = df %>% filter(anio == "2018") %>% filter(mes=="12") %>% select(CLAVE, NOMBRE, prob_def)

(
  grafico_ranking <-
    ggplot(top_dec, aes(x = fct_reorder(factor(NOMBRE), prob_def, .desc = FALSE), y = prob_def*100))+
    geom_bar(stat = "identity", position = "identity", alpha = 0.7, 
             color = "#3182bd", fill = "#3182bd", width = 0.4) +
    geom_hline(yintercept = mean(top_dec$prob_def)*100, col = "red", linetype = 2) +
    coord_flip() + 
    geom_text(aes(label = round(prob_def*100,1), y = round(prob_def*100,1) + 0.7))+
    labs(title = "Ranking IFNB",
         subtitle = "Probabilidad de Default a diciembre-2018",
         y = "Probabilidad de Default (%)",
         x = "")
)
ggsave(filename = "Ranking_IFNB.pdf", 
       w = 12, h = 8) 



#=====================================================
# claves_ifnbs_tst =c(838, 848, 853, 860, 939, 942, 959, 960, 965) # unique(data$CLAVE)
claves_ifnbs_tst =c(838, 848, 853, 860, 939, 942, 959, 960, 965) # unique(data$CLAVE)


cross_validation = function(clave){
  id_testing = which(data$CLAVE == clave)
  id_training = which(data$CLAVE != clave)
  
  X_tr = regressors[id_training,]
  X_tst = regressors[id_testing, c("ICV", "EFI_OPER", "RAZON_GADMON_CCT", "TASA_DE_EQUILIBRIO")]
  
  y_tr = data$STATUS2[id_training]
  y_tst = data$STATUS2[id_testing]
  
  names = colnames(X_tr)
  f = formula(paste("y_tr ~ 1 + ", paste(names, collapse = "+")))
  
  dd = as.data.frame(cbind(y_tr, X_tr))
  dd$CLAVE = data$CLAVE[id_training]
  
  
  # modelo = inla(f, data=dd, family="binomial", Ntrials=1,
  #               control.predictor=list(compute=TRUE),
  #               control.family = list(link = "probit"))
  
  modelo = inla(y_tr ~ 1 + ICV  + EFI_OPER + RAZON_GADMON_CCT +  TASA_DE_EQUILIBRIO + f(CLAVE, model="iid"), 
                data=dd, family="binomial", Ntrials=1,
                control.predictor=list(compute=TRUE),
                control.family = list(link = "probit"))
  
  EFECTOS = as.data.frame(modelo$summary.fixed)
  EFECTOS = EFECTOS %>% rownames_to_column()
  colnames(EFECTOS) = c("VARIABLE", "EFECTO", "STD_ERROR", "CI",  "0.5_q", "CS", "MODE", "KLD")
  probabilidades_tr = modelo$summary.fitted.values$mean
  mean_prob = mean(probabilidades_tr)
  EFECTOS = EFECTOS %>% mutate(marg_prob = mean_prob*EFECTO*100)
  
  xlsx::write.xlsx(EFECTOS, file = paste0("Efectos_without_", clave, ".xlsx"), sheetName="Efectos", 
                   col.names=TRUE, row.names=FALSE, append=TRUE)
  
  auc_tr = auc(response = y_tr, predictor = probabilidades_tr)[[1]]
  precision_tr = precision(probabilidades_tr, y_tr)
  
  XX_tst = cbind(1, X_tst)
  betas = EFECTOS$EFECTO   #coef(modelo)
  reffect = random_effects[random_effects$ID == clave,2]
  eta_i = t(betas %*% t(XX_tst)) + reffect
  prob_def = pnorm(eta_i)
  prob_survival = 1-prob_def
  odd_falla = prob_def/(1-prob_def); odd_falla
  odd_surv = (1-prob_def)/prob_def; odd_surv
  data_surv = data.frame(survival_p = prob_survival, odd_falla = odd_falla, odd_surv = odd_surv)

  xlsx::write.xlsx(data_surv, file = paste0("Efectos_without_", clave, ".xlsx"), sheetName="Survival",
                 col.names=TRUE, row.names=FALSE, append=TRUE)

  # auc_tst = auc(response = as.vector(y_tst), predictor = as.vector(prob_def))[[1]]
  precision_tst = precision(prob_def, y_tst, threshold=0.5)
  
  return(resultado = c(auc_tr, precision_tr,  precision_tst))
}

resultado_cv = ldply(claves_ifnbs_tst, cross_validation)                    
xlsx::write.xlsx(resultado_cv, "resultado_cv.xlsx", row.names = FALSE)                             
            
