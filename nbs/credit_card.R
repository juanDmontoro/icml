# Ilustración matching: impago liquidación tarjeta de credito

library(tidyverse)
impago <- read.csv(file="https://juandmontoro.github.io/icml/data/credit_card.csv")
names(impago) <- tolower(names(impago))

# Descripcion dataset:
#    Límite tarjeta (New Taiwan dollar: individual consumer credit and his/her family, supplementary, credit)
#    Género (1 = male; 2 = female).
#    Education (1 = graduate school; 2 = university; 3 = high school; 4 = others).
#    Estado civil (1 = married; 2 = single; 3 = others).
#    Edad (Age, year).
#    Historial crediticio: pay0 (Sept 2005) - pay6 (Abril 2005). 
#        Valores:  -1,-2 = pagado; 1 = retraso 1 mes; 2 = 2 meses; . . .; 8 = 8 meses; 9 = 9 meses o más
#    Importe liquidación: bill_amt1 = Sept2005 a bill_amt6=  Abril 2005)
#    pay_amt1-pay_amt6 (importe pagos realizados liquidaciones anteriores, sept a abril)
#    (Importes monetarios en dólares NT)


# PREGUNTA: ¿Como afecta el impago en Abril de 2005 (tratamiento) al impago 
#            en Sept 2005 (respuesta)? 

impago <- impago %>% 
  mutate(impagoSept=pay_0>0,
         impagoAbril=pay_6>0,
         importeAbril=bill_amt6)

# descartamos variables que no utilizaremos
impago <- impago %>% select(-starts_with("pay_"),-starts_with("bill_amt"),
                            -default.payment.next.month,-id)
names(impago)

# Diferencia de medias
impago %>% 
  group_by(impagoAbril) %>% 
  summarise(proporcion_impago_sept=mean(impagoSept))

# 0. Solapamiento ---------------------------------------------------------

# Previo al matching 
# Análisis de potenciales variable numéricas que pueden influir en el estatus 
# de tratamiento (impagoAbril)

impago %>% ggplot(aes(x = importeAbril,fill = impagoAbril,)) + 
  geom_density(alpha=0.5)

impago %>% ggplot(aes(x = limit_bal, fill = impagoAbril)) + 
  geom_density(alpha=0.5)

impago %>% ggplot(aes(x = age,fill = impagoAbril)) + 
  geom_density(alpha=0.5)

impago %>% ggplot(aes(x = education, fill = impagoAbril)) + 
  geom_bar(alpha=0.5 )

impago %>% filter(education>3) %>% 
  ggplot(aes(x = education,fill = impagoAbril)) + 
  geom_bar(alpha=0.5)


# 1. Matching ---------------------------------------------------------

# Hacemos matching con una variable
library(MatchIt)
# Ver vignette online
# https://kosukeimai.github.io/MatchIt/articles/MatchIt.html

# 1.1 Emparejamiento exacto -----------------------------------------------

# Versión más simple del matching: cada unidad tratada se empareja con TODAS
# las unidades de control con idénticos valores en las covariables.
# Se descartan unidades no emparejadas

m.exact <- matchit(impagoAbril~importeAbril,
                   method="exact",
                   data=impago)

# Inspeccionamos el objeto resultante 
m.exact

# Aplicamos el método summary()
summary(m.exact)
# The effective sample size (ESS) is a measure of the size of a hypothetical unweighted 
# sample with roughly the same precision as a weighted sample.
# It is computed as n/(1+var(w)):

# Obtención de la muestra emparejada con método match.data()
m.exact.data <- match.data(m.exact)
glimpse(m.exact.data)
m.exact.data$subclass

# La variable subclass es el id de los emparejamientos (628 en total)
# Ejemplos de emparejamientos 
m.exact.data[m.exact.data$subclass==133,]
m.exact.data[m.exact.data$subclass==33,]
m.exact.data[m.exact.data$subclass==6,] # Saldo abril = $390 NT

# 1.2 Nearest Neighbor ----------------------------------------------------

# Nearest Neighbor usa una media de distancia (en este caso Mahalanobis)
# para emparejar tratados con controles
# Nota: el matching es "greedy" en el sentido que para cada unidad tratada 
# se elige el control más próximo sin tener en cuenta los matches que pueden ocurrir 
# posteriormente: no hay un criterio de optimización global de los matches
# Una alternativa es optimal matching que minimiza el valor absoluto de la 
# distancia media entre las parejas formadas.
# Otras alternativas a NN: full, quick (ver vignettes de Matchit)

m.nn.1 <- matchit(impagoAbril~importeAbril,
                  method="nearest",
                  distance="mahalanobis",
                  data=impago) 

m.nn.1
summary(m.nn.1)

# Creación de matched sample 
m.nn.1.data <- match.data(m.nn.1)
glimpse(m.nn.1.data) 

# La variable subclass contiene id de emparejamientos
length(unique(m.nn.1.data$subclass)) # 3079 emparejamientos (de 3079 unidades tratadas)
# Veamos el emparejamiento #2537
m.nn.1.data[m.nn.1.data$subclass==2537,]
# id 5904 (tratada) empareja con 7688 (no tratada)


# Argumentos adicionales para el matching
#  replacement=T/F
#  distance="glm"/mahalanobis"/"robust_mahalanobis"/"euclidean"/"scaled_euclidean"
#  caliper= named vector numérico con cada valor referido a la var a la que aplicamos el calibre
#  ratio=1. Para k:1 matching. También se puede usar un ratio k variable (no más de k parejas por unidad tratada)  

# EJEMPLO 1: matching 2:1 fijando ratio=2 (dos controles por unidad tratada)
m.nn.2 <- matchit(impagoAbril~importeAbril,
                  method="nearest",
                  distance="mahalanobis",
                  replace=FALSE, 
                  ratio=2,
                  data=impago) 

summary(m.nn.2) # se han usado 6158 unidades de control
m.nn.2.data <- match.data(m.nn.2)
glimpse(m.nn.2.data)
m.nn.2.data[m.nn.2.data$subclass==2537,]

# Existe la posibilidad de elegir un número variable de matches 
# (en función de la calidad de estos) por unidad tratada
# ver help ?method_nearest para opción ratio variable

# EJEMPLO 2: radio para la variable importeAbril
m.nn.3 <- matchit(impagoAbril~importeAbril,
                   method="nearest",
                   distance="mahalanobis",
                   caliper=c(importeAbril=0.05),   # vector numérico con nombres de variables
                   std.caliper = TRUE,             # opción por defecto: radio en unidades de sd de la(s) variable(s)
                   data=impago)
summary(m.nn.3) # 2 unidades tratadas no tienen control dentro del radio propuesto
m.nn.3.data <- match.data(m.nn.3)
m.nn.3.data[m.nn.3.data$subclass==1,]

# EJEMPLO 3: matching con reemplazamiento
m.nn.4 <- matchit(impagoAbril~importeAbril,
                  method="nearest",
                  distance="mahalanobis",
                  replace=TRUE, # observaciones no tratadas pueden emparejarse con más de una tratada
                  data=impago) 

summary(m.nn.4)
m.nn.4.data <- match.data(m.nn.4)
glimpse(m.nn.4.data) 

# Interpretación de los pesos cuando replace=TRUE: una unidad de control se empareja 
# con una unidad tratada que ya tiene dos unidades de control emparejadas a ella; esa misma 
# unidad de control se emparejada con una unidad tratada que tiene otra unidad 
# de control emparejada a ella. La unidad de control en cuestión
# recibiría un peso de 1/3 + 1/2 = 5/6.

# NO OBSTANTE, con replace=TRUE es preferible generar DF emparejado llamando a la función get_matches (en lugar de match_data)
# Esta función genera una fila por cada unidad (de control) que ha sido utilizada en un match
# Así recuperamos la variable de agrupación subclass (útil para la estimación de errores estándar)
m.nn.4.data <- get_matches(m.nn.4)
glimpse(m.nn.4.data) 
# Veamos las primeras 30 unidades de control
m.nn.4.data[m.nn.4.data$impagoAbril==FALSE,"id"] %>% as.numeric() %>% sort %>% head(30)
# Vemos que id=156 ha sido utilizada en múltiples matches!

# Nota: replacement solo posible en métodos NN y genetic



# 1.2 Coarsened exact matching ----------------------------------------------

m.cem.1 <- matchit(impagoAbril~importeAbril,
                    method="cem",
                    data=impago)

summary(m.cem.1)
m.cem.1.data <- match.data(m.cem.1)
glimpse(m.cem.1.data)
m.cem.1.data$subclass 

# El algoritmo ha creado 7 estratos para la variable importeAbril
# Se puede configurar el número de puntos de corte del proceso de discretización 
# de las variables numéricas con cutpoints. P.e. seleccionando como puntos 
# de corte los deciles de la distribución de la variable numérica importeAbril

m.cem.2 <- matchit(impagoAbril~importeAbril,
                 method="cem",
                 cutpoints= list(importeAbril = quantile(impago$importeAbril, 
                                                      probs = seq(0.1,1,.1))),
                 data=impago)
summary(m.cem.2)
m.cem.2.data <- match.data(m.cem.2)
glimpse(m.cem.2.data)
m.cem.2.data$subclass 

# Otros métodos en vignette del paquete 

# 2. Equilibrio -------------------------------------------------------------

# El equilibrio de covariables se evalua a partir de medidas estadísticas, incluyendo
# diferencias de medias estandarizadas, razones de varianza,  valores p de pruebas t o KS.
# El equilibrio puede ser reportado por medio de tablas de equilibrio o gráficas
# que muestran las medidas de equilibrio antes y después del matching.
# La existencia (o no) de equilibrio en variables de confusión tras el matching 
# permite juzgar la validez de la afirmación causal, basándose en los métodos utilizados 
# y las covariables elegidas.

# Ejemplo: evaluación del equilibrio resultante del emparejamiento exacto 
# Tabla resumen
summary(m.exact,improvement = TRUE)
# Plot de las distribuciones de las variables
plot(m.exact,type="density")

# Alternativamente, podemos recurrir al paquete cobalt 
# Tabulación del balanceo con bal.tab
# Visualización del equilibrio con bal.plot y love.plot
library(cobalt)

# Tabular
bal.tab(m.exact,stats = c("m","var","ks"))

#  Podemos incluir umbrales máximos a partir de los cuales 
#  consideramos las medias/varianzas/distribuciones empíricas no balanceadas
#  La tabla proporciona más información
bal.tab(m.exact,stats = c("m","var","ks"),thresholds=c(m=0.1,ks=0.05,var=2))

# Para las diferencias de medias estandarizadas, se han propuesto umbrales igual
# a .1 y .25. Stuart et al. (2013) encuentran que el primero (.1) es más efectivo
# para evaluar desequilibrios que conducirían a una estimación sesgada del efecto. 
# En general, las diferencias de medias estandarizadas deberían estar lo más cerca 
# posible de 0, pero el límite superior de .1 es un heurístico adecuado 
# para la selección de modelos. 
# Por otro lado, ratios de varianza cercanos a 1 indican un buen equilibrio, 
# si bien ratios mayores (2) pueden ser aceptables (Rubin, 2001).

# Versión gráfica del emparejamientoGráficamente
bal.plot(m.exact,var.name = "importeAbril",which="both",mirror = TRUE)
love.plot(m.exact,stats = c("m","var","ks"),thresholds=c(m=0.1,ks=0.05,var=2))


# Evaluar el equilibrio de las covariables del resto de muestras pareadas

# EQUILIBRIO del CEM1
bal.tab(m.cem.1,stats = c("m","var","ks"),thresholds=c(m=0.1,ks=0.05,var=2))
bal.plot(m.cem.1,var.name = "importeAbril",which="both",mirror = TRUE)
love.plot(m.cem.1,stats = c("m","var","ks"),thresholds=c(m=0.1,ks=0.05,var=2))



# 3. Matching con multiples variables de confusión -----------------------------------

# Exact matching
m.exact.m <- matchit(impagoAbril~.-impagoSept,
                  method="exact",
                  data=impago) 
m.exact.m
summary(m.exact.m, improvement = TRUE)

bal.tab(m.exact.m,abs=TRUE,stats = c("m","var","ks"),thresholds=c(m=0.1,ks=0.05,var=2))
love.plot(m.exact.m,abs=TRUE,stats = c("m","var","ks"),thresholds=c(m=0.1,ks=0.05,var=2),var.order = "unadjusted")
bal.plot(m.exact.m,var.name = "age",which = "both")
bal.plot(m.exact.m,var.name = "importeAbril",which = "both")
bal.plot(m.exact.m,var.name = "limit_bal",which = "both")
bal.plot(m.exact.m,var.name = "education",which = "both",type = "histogram")

# Probar otro método de emparejamiento y validar equilibrio

