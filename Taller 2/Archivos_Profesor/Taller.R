## Vamos a cargar los datos:
load('datos_inferencia.RData')
head(df.ventas)

## Qué requerimos?
## Datos categóricos
## Veamos qué tenemos:
summary(df.ventas)

## saquemos una muestra
datos <- df.ventas # una copia para dejar los datos asegurados
# install.packages('samplingbook')
library(samplingbook)
n <- sample.size.mean(e=0.03*mean(datos$Revenue),
                      S=sd(datos$Revenue),
                      level = 0.97,
                      N = length(datos$Revenue))$n
n
muestra<- sample(1:nrow(datos), size=n, replace=FALSE)
m.datos<-datos[muestra,]
summary(m.datos)

# Contruyamos una variable que reepresente trimestres
m.datos$quarter <- cut(as.numeric(format(m.datos$Date, format="%m")),
    breaks = c(1,3,6,9,12), include.lowest = T)
summary(m.datos)

library(dplyr)
m.datos$quarter <- recode_factor(m.datos$quarter,
                                 '[1,3]' = '1QR',
                                 '(3,6]' = '2QR',
                                 '(6,9]' = '3QR',
                                 '(9,12]' = '4QR')
summary(m.datos)

# Ahora, categoricemos los ingresos
m.datos$Level_R <- cut(m.datos$Revenue,
                       breaks = c(0,300,500,max(m.datos$Revenue)),
                       include.lowest = T)

m.datos$Level_R <- factor(
  if_else(m.datos$Level_R == '[0,300]', 'Low',
        if_else(m.datos$Level_R == '(300,500]', 'Medium',
                'High')))

m.datos$Level_R <- factor(m.datos$Level_R,
                          levels = c('Low','Medium','High'))
summary(m.datos)

# Requerimos construir un ranking para la prueba de signos:
# Creemos el año para poder clasificar:

m.datos$Year <- as.numeric(format(m.datos$Date, format="%Y"))
summary(m.datos)

## Creemos los dos grupos (antes de 2017 y después de 2017)
## y clasifiquemos:

r.prod.a2017 <- aggregate(Revenue ~ ProductID,
                          data = m.datos[m.datos$Year <= 2017,],
                          FUN = mean)
r.prod.d2017 <- aggregate(Revenue ~ ProductID,
                          data = m.datos[m.datos$Year > 2017,],
                          FUN = mean)

r.prod.a2017$pos.a2017 <- rank(r.prod.a2017$Revenue)
r.prod.d2017$pos.d2017 <- rank(r.prod.d2017$Revenue)

r.prod <- r.prod.a2017[order(r.prod.a2017$pos.a2017,
                        r.prod.a2017$ProductID),]

# Tomemos el Top 50 antes de 2017 y comparemos con el otro grupo
top50 <- r.prod[1:50,c(1,3)]

top50 <- left_join(top50, r.prod.d2017[,c(1,3)], by = 'ProductID')

## Ahora hagamos las pruebas

## Bondad de ajuste

## Aunque no se especifica los tipos de productos, queremos saber
## si el registro de las ventas se da por estaciones (trimestre)
## o si el registro como tal es aleatorio.
## Primero, un gráfico exploratorio

quarter <- data.frame(table(m.datos$quarter))
names(quarter) <- c('quarter','Freq')
library(ggplot2)

ggplot(data = quarter, aes(x = quarter, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'darkred') +
  geom_text(aes(label = Freq), col = 'white',
            vjust = 1, size = 5) +
  labs(x = '', y = '') +
  theme(axis.text = element_text(size = 12))

## Ahora la prueba:

chisq.test(table(m.datos$quarter))

with(chisq.test(table(m.datos$quarter)),
     data.frame(observed,expected))

## Prueba de Independencia
## Se observan diferentes niveles de ventas y se quiere determinar
## si esos niveles muestran algún tipo de asociación con el periodo
## en el que ocurren las ventas

## De nuevo, un gráfico exploratorio, esta vez algo difetente:

addmargins(table(m.datos$quarter,m.datos$Level_R))

t.ventas <- data.frame(table(m.datos$Level_R,
                             m.datos$quarter))
head(t.ventas)
names(t.ventas) <- c('Level_R','Quarter', 'Freq')

ggplot(data = t.ventas, aes(x = Level_R, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'darkred') +
  geom_text(aes(label = Freq), col = 'white',
            vjust = 1, size = 4) +
  labs(x = '', y = '') +
  facet_wrap(~Quarter) +
  theme(axis.text = element_text(size = 12))

## Ahora la prueba:
chisq.test(m.datos$quarter, m.datos$Level_R)
# Como tabla:
# chisq.test(table(m.datos$quarter,m.datos$Level_R))

# Prueba de signos
## Vamos a comprobar si en la clasificación que hicimos
## se observan cambios

top50

# install.packages('coin')
library(coin)

wilcoxsign_test(pos.a2017 ~ pos.d2017, data = top50,
                distribution = "exact")
