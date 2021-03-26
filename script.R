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

#Tabla:


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

#TAbla
table(carreraFactor, sexoFactor )

#Grafico histograma: edad
edadGrafico <- hist(data$Edad, 
                    breaks='Sturges',                
                    main = "Edad de encuestados", 
                    xlab = "Edad", 
                    ylab = "Frecuencia",
                    col = palette("Pastel 1")
)
legend( x = "topright", legend = c("16-18","18-20","20-22","22-24","24-26","26-28","28-30", "30-32","32-34", "34-36", "36-38", "38-40"), fill = palette("Pastel 1"), title = "Leyenda") 

#Grafico de pastel"
trabajoFactor <- data$X.Actualmente.trabajas.
trabajoTable <- table(trabajoFactor)
trabajoGrafico <- pie(trabajoTable, main = "Estatus laboral", col = palette("Pastel 1"))

proporciones <- ftable(data$X.Actualmente.trabajas.)[1,]
pct <- round(proporciones/sum(proporciones)*100)
eti <- paste(pct, "%", sep = "")


legend( x = "topright", legend = eti, fill = palette("Pastel 1"), title = "Porcentaje")

#Grafico de pastel
pie(table(factor(data$Si.actualmente.trabajas..cuál.es.tu.salario)), 
    main = "Ingresos de encuastados",
    col = palette("Pastel 1"))

legend( x = "topright", legend = eti, fill = palette("Pastel 1"), title = "Porcentaje")