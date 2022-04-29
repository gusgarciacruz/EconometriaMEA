# HETEROCEDASTICIDAD

# Detectando heterocedasticidad
# Inspección visual

library(haven)
data <- read_stata("https://gusgarciacruz.github.io/EconometriaMEA/Tema8/elemapi2.dta")
View(data)

modelo.ols <- lm(api00~meals+ell+emer, data=data)
summary(modelo.ols)

# Hacer una inspección visual de los residuales estimados ploteados contra los valores estimados, 
# después de estimar el modelo, o los residuales contra cada predictor, para detectar cu?l ser?a 
# la posible variable explicativa causante de la heteroscedasticidad

data$resi <- modelo.ols$residuals
data$yfit <- modelo.ols$fit
View(data[,c('api00','yfit','resi')])

library(ggplot2)
ggplot(data = data, aes(y = resi, x = yfit)) + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data = data, aes(y = resi, x = meals)) + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data = data, aes(y = resi, x = ell)) + geom_point(col = 'blue') + geom_abline(slope = 0)
ggplot(data = data, aes(y = resi, x = emer)) + geom_point(col = 'blue') + geom_abline(slope = 0)

# Probando la existencia de heteroscedasticidad
# Test de Breusch-Pagan
# Manual
var.func <- lm(resi^2 ~ meals+ell+emer, data = data)
summary(var.func)
# n*R2
0.02098*nrow(data)
# Chi2 de la tabla
qchisq(.95, df = 1)

# Por función
install.packages('lmtest')
library(lmtest)
bptest(modelo.ols)

# Test de White
# U^2 contra Xs, sus cuadrado y productos cruzados
bptest(modelo.ols, ~ meals + ell + emer
                    + I(meals*ell) + I(meals*emer) + I(ell*emer)  
                    + I(meals^2) + I(ell^2) + I(emer^2), data = data)

# U^2 contra Y estimado y su cuadrado
bptest(modelo.ols, ~ yfit + I(yfit^2), data = data)

# Corrigiendo la heterocedasticidad
# Mínimos Cuadrados Ponderados
# Asumiendo que la variable explicativa meals es la causante  de la heterocedasticidad y 
# que la estructura de la Var(U) = (sigma^2)*meals.  Entonces para llegar a un modelo con 
# errores homoscedasticos se debe dividir  el modelo por raiz_cuadrada(meals)

summary(data$meals)

modelo.mcp <- lm(api00 ~ meals + ell + emer, weights = 1/meals, 
                 data = data, subset=data$meals>0)
summary(modelo.mcp)

# Capturando las observaciones con las que se estimó el modelo
nobs(modelo.mcp)
datamodelo <- data[rownames(modelo.mcp$model), ]
nrow(datamodelo)
summary(datamodelo[,c('api00', 'meals','ell','emer')])
sd(datamodelo$api00)

# MCG factibles
# 1. Estime el modelo y obtenga los residuales
# 2. Obtener log(u^2)
data$lresi2 = log(data$resi^2) 
  
# 3. Estime log(u^2) contra las Xs y obtener los valores predichos, gi
modelo.aux <- lm(lresi2 ~ meals+ell+emer, data=data)
summary(modelo.aux)
data$gi <- modelo.aux$fit
summary(data$gi)

# 4. Calcula hi = exp(gi)
data$hi = exp(data$gi)

# 5. Estimar la ecuación original por MCP usando como poderador 1/hi
modelo.mcg <- lm(api00 ~ meals + ell + emer, weights = 1/hi, data = data)
summary(modelo.mcg)

# Estimadores robustos a la heteroscedasticidad
library('sandwich')
modelo.robust <- coeftest(modelo.ols, vcov = vcovHC(modelo.ols, "HC1"))
modelo.robust
