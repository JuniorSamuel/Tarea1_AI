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
        xlab = "Carrera de los encuatados", 
        ylab = "Frecuancia",
        col=palette("Pastel 1")
)
#leyenda
legend( x = "topright", legend =  leyendaCarreraTemp, fill = palette("Pastel 1"), title = "Leyenda", )


#Grafico de Barra: Sexo
sexoFactor <- factor(data$Genero)

sexoTable <- table(sexoFactor)
sexoGrafico <- barplot(sexoTable, 
        main = "Sexo",
        xlab = "Sexo de participante",
        ylab = "Frecuencia", 
        col = palette("Pastel 1")
)

#leyenda
legend( x = "topright", legend = levels(sexoFactor), fill =  palette("Pastel 1"))


#Grafico histograma: edad
edadGrafico <- hist(data$Edad, 
     main = "Edad de encuestados", 
     xlab = "Edad", 
     ylab = "Frecuencia", 
     col = palette("Pastel 1")
)

#Grafico de pastel"
trabajoFactor <- data$X.Actualmente.trabajas.
trabajoTable <- table(trabajoFactor)
trabajoGrafico <- pie(trabajoTable, main = "Estatus laboral", col = palette("Pastel 1"))

#leyenda
#legend(x = "topright", legend = levels(trabajoFactor), fill = palette("Pastel 1"))
#Grafico 

#Grafico de pastel
pie(table(factor(data$Si.actualmente.trabajas..cuál.es.tu.salario)), 
    main = "Ingresos de encuastados",
    col = palette("Pastel 1"))