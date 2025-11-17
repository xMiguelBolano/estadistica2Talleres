library(readr)
dataset <- read_csv("life-expectancy-vs-gdp-per-capita.csv", 
                    col_types = cols(`145446-annotations` = col_skip()))
head(dataset)

library(dplyr)

dataset <- dataset %>%
  mutate_if(is.character,factor)

head(dataset)

continents_regions <- read_csv("continents_regions.csv")
head(continents_regions)
continents_regions <- continents_regions %>%
  mutate_if(is.character,factor)

head(continents_regions)

dataset <- left_join(dataset,
                     continents_regions[,c('Code','Continent')],
                     by = 'Code')
head(dataset)

dataset$Continent <- if_else(is.na(dataset$Continent.x),
                             dataset$Continent.y,dataset$Continent.x)
dataset$Continent.x <- NULL
dataset$Continent.y <- NULL
head(dataset)

dataset <- data.frame(dataset)
names(dataset) <- c('Country','Code','Year',
                    'LifeE','GDP_pc','Pop','Continent')
head(dataset)

dataset <- na.omit(dataset)
head(dataset)
summary(dataset)
var(dataset[,c('LifeE','GDP_pc','Pop')])
cor(dataset[,c('LifeE','GDP_pc','Pop')])

library(ggplot2)

ggplot(data = dataset, aes(x = GDP_pc, y = LifeE)) +
  geom_point(aes(col = Continent),size = 3) + 
  geom_smooth(se = F, color = "black") + 
  labs(x = 'LN GDP per capita', y = 'Life Expectation', col = '') +
  theme(legend.position = 'bottom',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.text=element_text(size = 14),
        strip.text = element_text(size = 14))

ggplot(data = dataset, aes(x = log(GDP_pc), y = LifeE)) +
  geom_point(aes(col = Continent),size = 3) + 
  geom_smooth(se = F, color = "black") + 
  labs(x = 'Ln GDP per capita', y = 'Life Expectation', col = '') +
  theme(legend.position = 'bottom',
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        legend.title=element_text(size=14),
        legend.text=element_text(size = 14),
        strip.text = element_text(size = 14))

cor(dataset[,c('LifeE','GDP_pc','Pop')])
cor(dataset[,c('LifeE','GDP_pc','Pop')], method = 'kendall')
cor(dataset[,c('LifeE','GDP_pc','Pop')], method = 'spearman')

with(dataset, cor.test(LifeE,GDP_pc))
with(dataset, cor.test(LifeE,log(GDP_pc)))

library("Hmisc")
rcorr(as.matrix(dataset[,c('LifeE','GDP_pc','Pop')]))

#Funcion para calcular el Coeficiente de correlacion multiple.
rho.mult<-function(datos)
  # datos: matriz con las variables del problema.
  # La primera columna debe ser la variable respuesta.
  # Las restantes p-1 columnas son las variables explicativas.
{
  matriz<-var(datos)
  # calculo
  n<-nrow(datos)
  p<-ncol(matriz)
  sxx<-matriz[2:p,2:p]
  syx<-matrix(matriz[1,2:p],nrow=1)
  sxy<-t(syx)
  #coeficiente
  rho.mult<-sqrt(syx%*%solve(sxx)%*%sxy)/sqrt(matriz[1,1])
  cat("\n Coeficiente de correlacion multiple: ",rho.mult,"\n")
  #estadistico
  if(abs(rho.mult)==1)
    stop("Imposible resolver contraste.
Coeficiente de correlacion multiple igual a 1", call. = FALSE)
  else{
    est<-((n-(p-1)-1)*rho.mult)/((p-1)*(1-rho.mult^2))
    #grafico
    par(mfrow=c(1,1))
    x<-seq(0,qf(0.999,p-1,n-(p-1)-1),length=500)
    plot(x,df(x,p-1,n-(p-1)-1),type="l",ylab="densidad",
         main="Contraste Correlacion Multiple")
    abline(v=qf(0.975,p-1,n-(p-1)-1),col="red")
    abline(v=qf(0.025,p-1,n-(p-1)-1),col="red")
    abline(v=est,col="blue")
    abline(h=0)
    legend(qf(0.98,p-1,n-(p-1)-1),pf((p+3)/(n-p),p-1,n-(p-1)-1),
           c("Estadistico","Region Critica"),lty=rep(1,2),
           col=c("blue","red"),bty="n")
    cat("\n Estadistico de contraste: ",round(est,3),"\n")
    cat("\n p-valor: ",round(2*(1-pf(est,p-1,n-(p-1)-1)),3),"\n\n")}
  return(invisible())
}

rho.mult(dataset[,c('LifeE','GDP_pc','Pop')])

library(ppcor) # Si no está instalada se usa: install.packages("ppcor")
pcor(dataset[,c('LifeE','GDP_pc','Pop')])

ggplot(data = dataset, aes(x = Continent, y = LifeE)) +
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=20, 
               size=6, color="red", fill="red") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

M<-cor(dataset[,c('LifeE','GDP_pc','Pop')])
round(M,2)
library(corrplot)
corrplot(M, method="circle", cl.cex = 1.5, number.cex = 2, 
         number.font = 3, tl.cex = 1.5)

corrplot(M, method="number", cl.cex = 1.5, number.cex = 2, 
         number.font = 3, tl.cex = 1.5)

cor_5 <- rcorr(as.matrix(dataset[,c('LifeE','GDP_pc','Pop')]))
M <- cor_5$r
p_mat <- cor_5$P
p_mat[is.na(p_mat)] <- 0
corrplot(M, type = "upper", order = "hclust", p.mat = p_mat, 
         sig.level = 0.01,
         cl.cex = 1.5, number.cex = 2,
         number.font = 3, tl.cex = 1.5)

library(GGally)
ggpairs(
  dataset[,c('LifeE','GDP_pc','Pop','Continent')],
  lower = list(continuous = "density", combo = "box_no_facet"),
  upper = list(continuous = "points", combo = "dot_no_facet")) + 
  theme(strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 12,
                                   angle = 90,
                                   vjust = 0.5,
                                   hjust = 0.5),
        axis.text.y = element_text(size = 12))
