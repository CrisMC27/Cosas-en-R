library(summarytools)

#generar variable de distribucion normal con media 60 y desviacion estandar 10
n1=rnorm(100,mean = 60,sd=10)
#generar una variable de distribucion weibull con parametro 1 y 1, comparar
m1=rweibull(100,shape = 1,scale = 1)
#poner estas 2 variables en data frame o base datos
base=data.frame(n1,m1)
View(base)
#calcular estadisticas discriptivas
descr(base$n1)
descr(base$m1)
#histograma de frecuencias
hist(base$n1,breaks = "Sturges",freq = FALSE, plot = TRUE,col = "blue",
     labels = TRUE, xlab = "Pesos en kilogramos", ylab = "Probabilidades",
     ylim = c(0,0.1), xlim = c(30, 90),main = "Histograma de pesos de mujeres")
curve(dnorm(x,mean=mean(base$n1), sd=sd(base$n1)),add = TRUE, col="black",lwd=2)
hist(base$m1,breaks = "Sturges",freq = FALSE, plot = TRUE,col = "brown",
     labels = TRUE, xlab = "Pesos en kilogramos", ylab = "Probabilidades"
     ,main = "Histograma de pesos de mujeres")
curve(dnorm(x,mean=mean(base$m1), sd=sd(base$m1)),add = TRUE, col="black",lwd=2)
