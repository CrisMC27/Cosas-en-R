#llamando librerías
library(readxl)
library(summarytools)
str(basePro)
attach(basePro)
#Histograma de edades
hist(basePro$Edad,breaks = "Sturges",freq =FALSE , right = TRUE,density = TRUE,col = "magenta",
     main = "Histograma de años de los encuestados",plot = TRUE,labels = FALSE,
     xlab = "Años",ylab = "Frec relativas")
curve(dnorm(x,mean=mean(basePro$Edad),sd=sd(basePro$Edad)), from=18,to=35,
      add=TRUE, col="blue", lwd=2)
#Histograma de semestre
hist(basePro$Semestre,breaks = "Sturges",freq =FALSE , right = TRUE,density = TRUE,col = "red",
     main = "Histograma de semestre de los encuestados",plot = TRUE,labels = FALSE,
     xlab = "Años",ylab = "Frec relativas")
curve(dnorm(x,mean=mean(basePro$Semestre),sd=sd(basePro$Semestre)), from=2,to=10,
      add=TRUE, col="blue", lwd=2)

#Frecuecia de la pregunta 1
frecp1=table(P1)
#Frecuencias relativas de la pregunta 1
frecrelat=prop.table(frecp1)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica circular de la pregunta 1
pie(porc, main="¿Con qué frecuencia utiliza inteligencia artificial para realizar trabajos o tareas asignadas por los docentes?",labels =etiq,col = c("red","gold","blue","green"),lwd = 1 )
legend("topright",col=c("red","gold","blue","green"),legend =c("Casi nunca","Casi siempre","Ocasionalemente","Siempre"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","green"))
#Frecuencia de la pregunta 2
frecp2=table(P2)
#Frecuencias relativas de la pregunta 2
frecrelat=prop.table(frecp2)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica circular de la pregunta 2
pie(porc, main="¿Intenta realizar los trabajos de manera autónoma antes de recurrir a la ayuda de inteligencia artificial?",labels =etiq,col = c("red","gold","blue","green"),lwd = 1 )
legend("topright",col=c("red","gold","blue","green"),legend =c("Casi nunca","Casi siempre","Ocasionalemente","Siempre"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","green"))
#Frecuencia de la pregunta 3
frecp3=table(P3)
#Frecuencias relativas de la pregunta 3
frecrelat=prop.table(frecp3)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica circular de la pregunta 3
pie(porc, main="¿Intenta realizar los trabajos de manera autónoma antes de recurrir a la ayuda de inteligencia artificial?",labels =etiq,col = c("red","gold","blue","green","skyblue","pink","brown","orange","thistle"),lwd = 1 )
legend("topright",col=c("red","gold","blue","green","skyblue","pink","brown","orange","thistle"),legend =c("Bard","Bing Chat","ChatGPT","ChatGPT, Bard","ChatGPT, Bard, Bing Chat",
                                                                                                           "ChatGPT, Bing Chat","ChatGPT, Bing Chat, Otra","ChatGPT, Otra","Otra"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","green","skyblue","pink","brown","orange","thistle"))
#Frecuencia de la pregunta 4
frecp4=table(P4)
#Frecuencias relativas de la pregunta 4
frecrelat=prop.table(frecp4)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 4
pie(porc, main="¿Considera que la inteligencia artificial le ha ayudado a ampliar sus conocimientos acerca del tema que está consultando?",labels =etiq,col = c("red","gold","blue","green"),lwd = 1 )
legend("topright",col=c("red","gold","blue","green"),legend =c("Casi nunca","Casi siempre","Ocasionalemente","Siempre"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","green"))
#Frecuencia de la pregunta 5
frecp5=table(P5)
#Frecuencias relativas de la pregunta 5
frecrelat=prop.table(frecp5)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 5
pie(porc, main="¿Cuál es su nivel de confianza al usar alguna inteligencia artificial para resolver los trabajos de la universidad?
",labels =etiq,col = c("red","gold","blue"),lwd = 1 )
legend("topright",col=c("red","gold","blue"),legend =c("Confiable","Muy confiable","Poco confiable"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue"))
#Frecuencia de la pregunta 6
frecp6=table(P6)
#Frecuencias relativas de la pregunta 6
frecrelat=prop.table(frecp6)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 6
pie(porc, main="¿La inteligencia artificial ha reducido su carga académica y le ha dejado tiempo para realizar otras actividades?",labels =etiq,col = c("red","gold","blue","purple","brown"),lwd = 1 )
legend("topright",col=c("red","gold","blue","purple","brown"),legend =c("Casi nunca","Casi siempre","Nunca","Ocasionalmente","Siempre"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","purple","brown"))
#Frecuencia de la pregunta 7
frecp7=table(P7)
#Frecuencias relativas de la pregunta 7
frecrelat=prop.table(frecp7)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Graficas de la pregunta 7
pie(porc, main="¿Con el uso de la inteligencia artificial logra obtener una mejor retroalimentación de sus trabajos o proyectos universitarios por parte del docente?",labels =etiq,col = c("red","gold","blue","purple","brown"),lwd = 1 )
legend("topright",col=c("red","gold","blue","purple","brown"),legend =c("Casi nunca","Casi siempre","Nunca","Ocasionalmente","Siempre"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","purple","brown"))
#Frecuencia de la pregunta 8
frecp8=table(P8)
#Frecuencias relativas de la pregunta 8
frecrelat=prop.table(frecp8)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 8
pie(porc, main="¿Considera que la integración de la inteligencia artificial en la educación universitaria debería ser una prioridad para las instituciones educativas?
",labels =etiq,col = c("red","gold"),lwd = 1 )
legend("topright",col=c("red","gold"),legend =c("No","Si"), bty = "n",cex = 0.6,
       fill = c("red","gold"))
#Frecuencia de la pregunta 9
frecp9=table(P9)
#Frecuencias relativas de la pregunta 9
frecrelat=prop.table(frecp9)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 9
pie(porc, main="¿Le gustaría tener acceso a cursos gratuitos de inteligencia artificial como complemento a su educación universitaria?
",labels =etiq,col = c("red","blue"),lwd = 1 )
legend("topright",col=c("red","blue"),legend =c("No","Si"), bty = "n",cex = 0.6,
       fill = c("red","blue"))
#Frecuencia de la pregunta 10
frecp10=table(P10)
#Frecuencias relativas de la pregunta 10
frecrelat=prop.table(frecp10)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 10
pie(porc, main="¿En qué tipo de trabajos recurre al uso de la inteligencias artificiales?",
    labels =etiq,col = c("red","gold","blue","green"),lwd = 1 )
legend("topright",col=c("red","gold","blue","green"),
       legend =c("Proyectos de la asignatura","Proyectos de la asignatura, Talleres","Talleres","Talleres, Quices"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","green"))
#Frecuencia de la pregunta 10
frecp11=table(P11)
#Frecuencias relativas de la pregunta 10
frecrelat=prop.table(frecp11)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Grafica de la pregunta 11
pie(porc, main="¿En qué tipo de disciplina hace uso de la inteligencia artificial?",
    labels =etiq,col = c("red","gold","blue","green","purple","orange","black","pink","white","brown","skyblue","magenta","grey","#4A7C0E"),lwd = 1 )
legend("topright",col=c("red","gold","blue","green","purple","orange","black","pink","white","brown","skyblue","magenta","grey","#4A7C0E"),
       legend =c("Ciencias básicas","Ciencias básicas, Electivas","Ciencias básicas, Electivas, Fundamentación específica","Ciencias básicas, Electivas, Fundamentación específica, Fundamentación humanística","Ciencias básicas, Fundamentación específica","Ciencias básicas, Fundamentación específica, Fundamentación humanística","Ciencias básicas, Fundamentación humanística","Electivas","Electivas, Fundamentación específica","Electivas, Fundamentación específica, Fundamentación humanística","Electivas, Fundamentación humanística","Fundamentación específica","Fundamentación específica, Fundamentación humanística"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","green","purple","orange","black","pink","white","brown","skyblue","magenta","grey","#4A7C0E"))
#Frecuencia de la pregunta 12
frecp12=table(P12)
#Frecuencias relativas de la pregunta 12
frecrelat=prop.table(frecp12)
frecrelat1=round(frecrelat,2)
porc=frecrelat1*100
porc
#Etiquetas
etiq=paste(names(porc),porc,"%")
etiq
#Graficas de la pregunta 12
pie(porc, main="¿Cuando utiliza inteligencia artificial su nota sube?",
    labels =etiq,col = c("red","gold","blue","purple","brown"),lwd = 1 )
legend("topright",col=c("red","gold","blue","purple","brown"),
       legend =c("Casi nunca","Casi siempre","Nunca","Ocasionalmente","Siempre"), bty = "n",cex = 0.6,
       fill = c("red","gold","blue","purple","brown"))



