# EL EFECTO DE LA EDUCACION SOBRE EL CRIMEN!
  
# Variables instrumentales
# La educacion reduce el crimen?
# Si es asi, gastar mas en educacion podria ser una herramienta de largo plazo para combatir el crimen
# El archivo inmates.dta (formato Stata) contiene informacion sobre el paper Lochner, L. y Moretti, E. (2004).
# "The Effect of Education on Crime: Evidence from Prison Inmates, Arrests, and Self-Reports",
# American Economic Review, 94(1):155-189 (http://www.nber.org/papers/w8605), donde se miden
# los efectos de la educacion sobre el crimen. 

# Las principales variables para el análisis son:
# prision: variable binaria igual a 1 si la persona esta en prision, 0 no
# educ: anos de escolaridad
# age: edad
# AfAm: variable binaria igual a 1 para afroamericano, 0 no
# ca8: variable binaria igual a 1 si la escolaridad obligatoria estatal es de 8 o menos anos
# ca9: variable binaria igual a 1 si la escolaridad obligatoria estatal es de 9 anos
# ca10: variable binaria igual a 1 si la escolaridad obligatoria estatal es de 10 anos
# ca11: variable binaria igual a 1 si la escolaridad obligatoria estatal es de 11 o mas anos

# Estime un modelo donde se relaciona el crimen con la educacion, la edad y su cuadrado, y si el
# individuo es afroamericano

library(haven); library(tidyverse); library(AER)

setwd("C:/Users/ggarci24/OneDrive - Universidad EAFIT/EAFIT/Cursos EAFIT/Econometria maestria EcoAplicada/Diapos/2019-II - 2021-II/Tema 9")
data <- read_dta("inmates.dta") |> 
  select(prison, educ, age, AfAm, ca8, ca9, ca10, ca11)

ols <- lm(prison ~ educ + age + I(age^2) + AfAm, data=data) 
summary(ols)

# Como la var dependiente es 1 y 0 puede interpretarse como la probabilidad de estar o no en prision.
# El beta de educ indica que un año adicional de educacion disminuye la probabilidad de estar en prision
# en 0.1 puntos porcentuales (pp)

# Ser afroamericano aumenta la probabilidad de estar preso en 2 pp, respecto a un no afroamericano

# Es la educacion exogena?
# No. Probablemente individuos con bajos niveles de educacion seran mas propensos a ser criminales, y
# aquellos individuos mas peligrosos tendran mas bajos niveles de educacion => causalidad reversa.

# Lochner y Moretti (2004) usan MC2E para mejorar la estimacion MCO. Ellos usan como instrumentos de la educación,
# la variaion sobre las leyes estatales de educacion obligatoria
# esas leyes no afectan directamente la prob de ir a prision y son relevantes

# Contrastando que la variable educ es realmente endogena
# Ho: educ es exogena 
iv <- ivreg(prison ~ educ + age + I(age^2) + AfAm |
                            age + I(age^2) + AfAm + ca9 + ca10 + ca11, data=data)

summary(iv)

# Diagnósticos de los instrumentos
# Relevancia de los instrumentos
# La idea es probar la significancia conjunta de los instrumentos en la primera etapa
# Se calcula entonces el F estadístico de la primera etapa

fstage <- lm(educ ~ age + I(age^2) + AfAm + ca9 + ca10 + ca11, data=data)
summary(fstage)

linearHypothesis(fstage, 
                 c("ca9 = 0", "ca10 = 0", "ca11 = 0"))

summary(iv, diagnostics=TRUE)
# Weak instruments: La H0 es que los instrumentos son débiles, así que un rechazo
# significa que los instrumento no son débiles, lo cual es bueno

# El F es bastante grando con los cual rehazamos H0 que los instrumentos no tienen efecto
# sobre la educación, con lo cual son relevantes

# Wu-Hausman: Es un test de endogeneidad, donde H0: Cov(educ, error) = 0. Rechazando H0
# indicate la existencia de endogeniedad la necesidad por variables instrumentales
# En otras palabras, prueba la consistencia de las estimaciones OLS bajo el supuesto que el
# IV es consistente. Cuando se rechaza H0, indica que OLS es no cosistente, sugiriendo
# que la endogeneidad es presente. Si no se rechaza H0, significa que OLS y IV son similares
# y la endogeneidad no es un problema

# Sargan: sirve para probar la validez de los instrumentos (los instrumentos no están
# correlacionados con los errores). Este test sólo puede calcularse si los instrumentos
# exceden el número de variables endógenas. Este test también es llamado test de
# restricciones de sobre-identificación. H0: Cov(z,error)=0. Lo bueno sería no rechazar