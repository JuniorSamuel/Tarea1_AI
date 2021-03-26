library(readxl)
url <- "https://github.com/JuniorSamuel/Tarea1_AI/raw/main/Datos/Empleabilidad%20de%20los%20estudiantes%20del%20ITLA.xlsx"
destfile <- "dataset.xlsx"
curl::curl_download(url, destfile)
dataset <- read_excel(destfile, col_types = c("date", 
                                              "text", "text", "text", "text", "numeric", 
                                              "text", "text", "text", "text", "skip", 
                                              "text", "text", "text", "text", "numeric", 
                                              "numeric", "numeric", "text", "text", 
                                              "text", "text", "text", "skip", "skip"))
#Generando dataFrame
data <- data.frame(dataset)

#Grafico de barra: Carrera 
carreraFactor <- factor(data$Carrera.)
leyendaCarrera <- levels(carreraFactor)
leyendaCarreraTemp <- c("A.C", "D.S", "D.I", "M.A", "Mec", "Mul", "R.I", "S.I", "Son", "M.D.M")
levels(carreraFactor) <- leyendaCarreraTemp

carreraGrafico <- barplot(table(carreraFactor), 
        main = "Carrera", 
        legend.text = TRUE,
        xlab = "Carrera de los encuatados", 
        ylab = "Frecuancia",
        col=palette("Pastel 1")
)

print("Interpretacion: En la siguiente tabla y grafico podemos observar que a los estudiantes la carrera de Desarrollo de software tuvieron mas participacion en la encuesta, seguido por Seguridad Informatica. Las carreras con menor participación en la encuesta son Diseño Industrial y Manufactura Automatizada, tambien podemos ver que no todas las carrera participaron.")


#Grafico de Barra: Sexo
sexoFactor <- factor(data$Genero)

sexoTable <- table(sexoFactor)
sexoGrafico <- barplot(sexoTable, 
        main = "Sexo",
        xlab = "Sexo de participante",
        ylab = "Frecuencia", 
        col = palette("Pastel 1"),
        legend.text = TRUE
)
print("Tabla de Sexo y Carrera de encuestados.")
#TAbla
table(carreraFactor, sexoFactor )

print("Interpretación: Los encuestado era mayoritariamente hombres, representando y las mujeres. En la tabla podemos observar que en las carrera son principalmente hombres, menos en la carrera de multimedia donde las mujeres doblan la cantidad de participantes hombres.")

#Grafico histograma: edad
edadGrafico <- hist(data$Edad, 
                    breaks='Sturges',                
                    main = "Edad de encuestados", 
                    xlab = "Edad", 
                    ylab = "Frecuencia",
                    col = palette("Pastel 1")
)
legend( x = "topright", legend = c("16-18","18-20","20-22","22-24","24-26","26-28","28-30", "30-32","32-34", "34-36", "36-38", "38-40"), fill = palette("Pastel 1"), title = "Leyenda") 


table(factor(data$Edad), sexoFactor);

print("Interpretación: La mayor parte de los encuestados se encuentra entre 16 al 20 años de edad.")


#Grafico de pastel"
trabajoFactor <- data$X.Actualmente.trabajas.
trabajoTable <- table(trabajoFactor)
trabajoGrafico <- pie(trabajoTable, main = "Estatus laboral", col = palette("Pastel 1"))

proporciones <- ftable(data$X.Actualmente.trabajas.)[1,]
pct <- round(proporciones/sum(proporciones)*100)
eti <- paste(pct, "%", sep = "")


legend( x = "topright", legend = eti, fill = palette("Pastel 1"), title = "Porcentaje")

table(trabajoFactor, sexoFactor)
print("Intepretacion: Solo 30 de los encuestados actualmente trabaja, 113 de los  encuestado no trabaja actualmente pero asegura que tiene interés en trabajar, los demás solo indicaron que no trabajar en la actualidad y no mostraron interés en trabajar. ")

#Grafico de pastel
pie(table(factor(data$Si.actualmente.trabajas..cuál.es.tu.salario)), 
    main = "Ingresos de encuastados",
    col = palette("Pastel 1"))

legend( x = "topright", legend = eti, fill = palette("Pastel 1"), title = "Porcentaje")

print("Intepretación: El 82% de los participante que actualmente no trabajan. El 2% gana menos de $5,000 pesos, el 5% gana entre RD$ 5,000 a RD$ 15,000 pesos, el salario del 6% esta entre $15,000 a RD$ 25,000 peso, el restante 4% esta sobre lo RD$ 25,000 peso.")