load('dataset_reg.RData')

library(dplyr)
library(ggplot2)

dataset_reg <- dataset %>%
  mutate(
    ln_GDP_pc = log(GDP_pc),
    ln_Pop = log(Pop)
  ) %>% tibble()

dataset_reg %>%
  select(-c(Country, Code, Continent, Year)) %>%
  plot()

dataset_reg %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = LifeE)) +
  geom_boxplot()

dataset_reg %>%
  filter(Year >= 1950) %>%
  mutate(Year = factor(Year)) %>%
  ggplot(aes(x = Year, y = LifeE)) +
  geom_boxplot()

dataset_reg_c <- dataset_reg

dataset_reg <- dataset_reg %>%
  filter(Year >= 1950) %>%
  mutate(Year = factor(Year))

fitm1 <- lm(LifeE ~ ln_GDP_pc + ln_Pop, data = dataset_reg)
summary(fitm1)

fitm2 <- lm(LifeE ~ ln_GDP_pc + ln_Pop + Continent, data = dataset_reg)
summary(fitm2)

summary(aov(fitm2))
step(fitm2)

e<-residuals(fitm2)
sfit<-summary(fitm2)
d<-e/sfit$sigma
print(round(d,2))

par(mfrow=c(2,2))
# el histograma de los residuos, superponiendo una densidad normal:
hist(d,probability=T,xlab="Residuos estandarizados",main="",
     cex.lab=1.5, cex.axis=1.75, cex = 2, pch = 19, cex.main = 2)
d.seq<-seq(-3,3,length=50)
lines(d.seq,dnorm(d.seq,mean(d),sd(d)), col = 'red')

# El gráfico de residuos versus el predictor:
plot(dataset_reg$ln_GDP_pc,d,xlab="LN PIB PC",ylab="Residuos Estandarizados",cex.lab=1.5, cex.axis=1.75, cex = 1, pch = 19, cex.main = 2)

# y se ajusta con una curva suavizada la tendencia:
lines(lowess(dataset_reg$ln_GDP_pc,d),col="red")

# La secuencia temporal de residuos se obtiene con:
plot(d,type="b",ylab="Residuos estandarizados", cex.lab=1.5, cex.axis=1.75, cex = 1, pch = 19, cex.main = 2)

# y el gráfico de residuos versus el anterior es:
n<-length(d)
plot(d[1:(n-1)],d[2:n],xlab="Residuo i",ylab="Residuo i-1",cex.lab=1.5, cex.axis=1.75, cex = 1, pch = 19, cex.main = 2)

# y se ajusta con una curva suavizada la tendencia:
lines(lowess(d[1:(n-1)],d[2:n]),col="red")

###
r<-rstandard(fitm2)
opar<-par(mfrow=c(1,2))
# los residuos solos
plot(r,ylab='Residuos estudentizados', cex.lab=1.5, cex.axis=1.75, cex = 1, pch = 19, cex.main = 2)
title(sub="(a)")
# los residuos versus los valores ajustados
plot(fitted(fitm2),r,xlab='Valores ajustados',
     ylab='Residuos estudentizados', cex.lab=1.5, cex.axis=1.75, cex = 1, pch = 19, cex.main = 2)
title(sub="(b)")
par(opar)

library(lmtest)
# El test de Breusch-Pagan se obtiene con (supuesto de HOMOCEDASTICIDAD)
bptest(fitm2)

opar<-par(mfrow=c(1,2))
# Los gráficos de normalidad para los residuos son
qqnorm(fitm2$resid, cex.lab=1.5, cex.axis=1.75, cex = 1, pch = 19, cex.main = 2)
qqline(fitm2$resid)
# Eln histograma y superponer una densidad normal para los residuos estandarizados o estudentizados
r<-rstandard(fitm2)
hist(r,prob=T,xlim=c(-3,3),xlab="Res.estudentizados",main="Histograma",
     cex.lab=1.5, cex.axis=1.75, cex = 2, pch = 19, cex.main = 2)
lines(xseq<-seq(-3,3,length=100), dnorm(xseq,mean(r),sd(r)),
      col = 'red')
par(opar)

# Shapiro-Wilks:
shapiro.test(r)
# Kolmogorov-Smirnov:
ks.test(r,pnorm)
# Incorrelación
dwtest(fitm2)

## probemos abordando la heterocedasticidad incluyendo un parámetro de ponderación:
res<-residuals(fitm2)
wt <- 1 / lm(abs(fitm2$residuals) ~ fitm2$fitted.values)$fitted.values^2

# Ya ajustamos el modelo por mcp:
fitm5<-lm(LifeE ~ ln_GDP_pc + ln_Pop + Continent, data = dataset_reg, weights=wt)

# y dibujamos los residuos para apreciar si hemos corregido el problema de heterocedasticidad:
r<-rstandard(fitm5)
opar<-par(mfrow=c(2,2))
plot(r,ylab='Res.estudentizados')
plot(fitted(fitm5),r,xlab='Valores ajustados', ylab='Res.estudentizados')
plot(r~dataset_reg$Continent,xlab='Continent',ylab='Res.estudentizados')
par(opar)

bptest(fitm5)
ks.test(rstandard(fitm5),pnorm)
dwtest(fitm5,alternative='two.sided')

n<-length(r)
d <- sum((r[2:n] - r[1:(n-1)])^2)/(sum(r^2))
d

# Encontremos el p-valor
D_observed <- d

# Necesitamos hacer una aproximación mediante simulación de Monte Carlo
# Número de simulaciones Monte Carlo
n_simulations <- 10000

# Inicializar un contador para contar cuántas veces D simulado es mayor que D observado
count <- 0

# Realizar simulaciones Monte Carlo
for (i in 1:n_simulations) {
  # Generar residuos simulados (de la misma longitud que tus datos reales)
  simulated_residuals <- rnorm(n = length(r), mean = 0, sd = 1)
  
  # Calcular D simulado
  D_simulated <- sum(diff(simulated_residuals)^2) / sum(simulated_residuals^2)
  
  # Comprobar si D simulado es mayor que D observado
  if (D_simulated > D_observed) {
    count <- count + 1
  }
}

# Calcular el valor p basado en las simulaciones
p_value <- count / n_simulations

# Imprimir el valor p
cat("Valor p:", p_value, "\n")

# Transformación de box cox

library(MASS)
boxcox(fitm2,plotit=T)
bc<-boxcox(fitm2,plotit=F)
lambda<-bc$x[which.max(bc$y)]; lambda

library(labstatR)
z <- (dataset_reg$LifeE^lambda-1)/(lambda*meang(dataset_reg$LifeE)^(lambda-1))

# y el nuevo ajuste con dicha variable
fit.bc <- lm(z ~ ln_GDP_pc + ln_Pop + Continent, data = dataset_reg)
summary(fit.bc)

# Debemos buscar otra alternativa
# Vamos a probar reagrupando datos:

df_reg_a <- dataset_reg %>%
  group_by(Country, Code, Continent) %>%
  summarise(
    LifeE = mean(LifeE),
    ln_GDP_pc = log(mean(GDP_pc)),
    ln_Pop = log(mean(Pop)),
    .groups = 'drop'
  )

fitma <- lm(LifeE ~ ln_GDP_pc + ln_Pop + Continent, data = df_reg_a)
summary(fitma)
summary(aov(fitma))

step(fitma)

fitma2 <- lm(LifeE ~ ln_GDP_pc + Continent, data = df_reg_a)
summary(fitma2)

bptest(fitma2)
ks.test(rstandard(fitma2), pnorm)
dwtest(fitma2, alternative='two.sided')

df_reg_a %>%
  mutate(res = residuals(fitma2)) %>%
  ggplot(aes(x = Continent, y = res)) +
  geom_boxplot()

# Ponderemos:
resm2 <- residuals(fitma2)
wtm2 <- 1 / lm(abs(fitma2$residuals) ~ fitma2$fitted.values)$fitted.values^2

# Ya ajustamos el modelo por mcp:
fitma3 <- lm(LifeE ~ ln_GDP_pc + Continent, data = df_reg_a, weights = wtm2)
summary(fitma3)

bptest(fitma3)
ks.test(rstandard(fitma3),pnorm)

rst2 <- rstandard(fitma3)
n<-length(rst2)

dm2 <- sum((rst2[2:n] - rst2[1:(n-1)])^2)/(sum(rst2^2))
dm2
# Encontremos el p-valor
D_observed <- dm2

# Necesitamos hacer una aproximación mediante simulación de Monte Carlo
# Número de simulaciones Monte Carlo
n_simulations <- 10000

# Inicializar un contador para contar cuántas veces D simulado es mayor que D observado
count <- 0

# Realizar simulaciones Monte Carlo
for (i in 1:n_simulations) {
  # Generar residuos simulados (de la misma longitud que tus datos reales)
  simulated_residuals <- rnorm(n = length(rst2), mean = 0, sd = 1)
  
  # Calcular D simulado
  D_simulated <- sum(diff(simulated_residuals)^2) / sum(simulated_residuals^2)
  
  # Comprobar si D simulado es mayor que D observado
  if (D_simulated > D_observed) {
    count <- count + 1
  }
}

# Calcular el valor p basado en las simulaciones
p_value <- count / n_simulations

# Imprimir el valor p
cat("Valor p:", p_value, "\n")

########### 

newdata <- dataset_reg_c %>%
  filter(Year == 2015) %>%
  dplyr::select(ln_GDP_pc, Continent)

dataset_reg_c %>%
  filter(Year == 2015) %>%
  mutate(
    pred = predict(fitma3, newdata),
    err = pred-LifeE
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = ln_GDP_pc, color = Continent)) +
  ggplot2::geom_point(ggplot2::aes(y = pred,   shape = "Predicción"), size = 2) +
  ggplot2::geom_point(ggplot2::aes(y = LifeE,  shape = "Observado"),  size = 2) +
  ggplot2::scale_shape_manual(
    name   = "Data Set",
    values = c("Predicción" = 3, "Observado" = 19)
  ) +
  ggplot2::labs(x = "LN GDP PC", y = "Life E", color = "Continente") +
  ggplot2::theme_minimal()



