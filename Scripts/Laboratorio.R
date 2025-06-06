#-------------------------------------------------------#
#---- Universidad del valle: Escuela de Estadistica ----#
#---- Asignatura: Probabilidad y Estadistica -----------#
#---- Profesor: Ivan Mauricio Bermudez Vera ------------#
#-------------------------------------------------------#

#-------------------------------------------------------#
#---- Laborario R --------------------------------------#
#---- Integrantes: -------------------------------------#
#---- John Jaider Ramos Gaviria - 2370742 --------------#
#---- Kevin Ariel Ramirez Amaya - 2324793 --------------#
#---- Escuela de Ingenieria de Sistemas Y computacion --#
#-------------------------------------------------------#

# 1. Carga de librerias requeridas #

# Cargar libreria easypackages

library(easypackages)

# Listado de librerias requeridas

lib_req <- c("lubridate",
            "dplyr",
            "visdat",
            "missMDA",
            "mice",
            "DMwR2",
            "editrules",
            "corrplot",
            "ggplot2",
            "here",
            "skimr")

# Instalar y cargar librerias requeridas

easypackages::packages(lib_req)

#--------------------------------------------------------------------#

# 2. Preprocesamiento de datos #

# 1.  Lea la hoja de datos y adecúe el formato de cada variable, verificando que dispone de una hoja de datos técnicamente correcta

# Lectura de la hoja de datos

Data <- read.table(here("Data","BD_huella.txt"), header = TRUE, sep = "\t")

# Inspeccionar los niveles para las variables tipo Factor

table(Data$genero)
table(Data$zona)
table(Data$grado)
table(Data$comp_HHD)
table(Data$comp_HHI)

# Declaración de niveles correctos para las variables tipo Factor
level_genero <- c("1" = 1,
                  "2" = 2,
                  femenino = 1, 
                  Femenino = 1, 
                  FEMENINO = 1, 
                  masculino = 2, 
                  Masculino = 2,
                  MASCULINO = 2
                  )

level_zona <- c("1" = 1,
                "2"= 2,
                Urbano = 1, 
                URBANO = 1,
                Rural= 2, 
                RURAL= 2 
                )

level_grado <- c("6" = 6, 
                 "7" = 7, 
                 "8" = 8, 
                 "9" = 9, 
                 "10" = 10, 
                 "11" = 11,
                 sexto = 6,
                 SEXTO = 6,
                 septimo = 7, 
                 SEPTIMO = 7,
                 octavo = 8, 
                 OCTAVO = 8,
                 noveno = 9, 
                 NOVENO = 9, 
                 decimo = 10, 
                 DECIMO = 10,
                 once = 11, 
                 ONCE = 11 
                 )
level_comp_HHD <- c(Lavado.ropa = "Lavado.ropa",
                    Riego.jardin = "Riego.jardin",
                    Uso.baño = "Uso.baño",
                    USO.BAÑO = "Uso.baño",
                    Uso_baño = "Uso.baño",
                    Uso.cocina = "Uso.cocina")
level_comp_HHI <- c(Café = "Café",
                    CAFÉ = "Café",
                    Carne = "Carne",
                    CARNE = "Carne",
                    Fruta = "Frutas")

## Modificación del formato y transformación de variables
Data <- transform(Data, 
                  genero=factor(dplyr::recode(genero, !!!level_genero)),
                  zona=factor(dplyr::recode(zona, !!!level_zona)),
                  grado = factor(dplyr::recode(grado,!!!level_grado)),
                  comp_HHD = factor(dplyr::recode(comp_HHD, !!!level_comp_HHD)),
                  comp_HHI = factor(dplyr::recode(comp_HHI, !!!level_comp_HHI)))

# Inspeccionar los niveles para las variables tipo Factor

table(Data$genero)
table(Data$zona)
table(Data$grado)
table(Data$comp_HHD)
table(Data$comp_HHI)

#Verificacion de la estructura

str(Data)

# 2.  Construya el archivo: consistencia.txt, en el cual incluya las ecuaciones que usted considera necesarias para verificar la consistencia de los datos en el conjunto de variables.
cat("HHD >= 0\nHHI >= 0\nedad >= 10 \nper.hog >= 1", file = "consistencia.txt")


# 3.  Aplique estas reglas sobre la hoja de datos y genere un pequeño reporte de sus resultados.

Rules <- editrules::editfile("consistencia.txt")
Rules

plot(Rules)

Valid_Data = editrules::violatedEdits(Rules,Data)
Valid_Data

summary(Valid_Data)

which(Valid_Data)
matrix(data=1:55, 5, 11)

windows()
plot(Valid_Data)

# 4. Visualice e identifique los registros que presentan datos faltantes.

is.na(Data) # Creamos una matriz con los datos faltantes

visdat::vis_miss(Data) # Visualizamos en grafico los datos faltantes

# Función que evalua e identifica los datos faltantes por variable e individuo.

miss<-function(Data,plot=T){  
  n=nrow(Data);p=ncol(Data)
  names.obs<-rownames(Data)
  
  
  nobs.comp=sum(complete.cases(Data))         # Cuenta los registros completos
  Obs.comp=which(complete.cases(Data))        # Identifica los registros completos
  nobs.miss = sum(!complete.cases(Data))      # Identifica los registros con datos faltantes.
  Obs.miss=which(!complete.cases(Data))       # Identifica los registros con datos faltantes.
  
  Data.NA<-is.na(Data)
  Var_Num<- sort(colSums(Data.NA),decreasing=T)
  Var_per<-round(Var_Num/n,3)
  Obs_Num<-rowSums(Data.NA)
  names(Obs_Num)<-names.obs
  Obs_Num<-sort(Obs_Num,decreasing=T)
  Obs_per<-round(Obs_Num/p,3)
  lista<-list(n.row = n, n.col = p,n.comp = nobs.comp,Obs.comp = Obs.comp,n.miss = nobs.miss,Obs.miss = Obs.miss, Var.n = Var_Num , Var.p = Var_per, Obs.n= Obs_Num, Obs.per= Obs_per)
  
  if(plot){
    windows(height=10,width=15)
    par(mfrow=c(1,2))
    coord<-barplot(Var_per,plot=F)
    barplot(Var_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por variable", col="blue")
    axis(2,at=coord,labels=names(Var_per), cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2),pos=0)
    
    coord<-barplot(Obs_per,plot=F)
    barplot(Obs_per,xaxt="n",horiz=T,yaxt="n",xlim=c(-0.2,1), ylim=c(0,max(coord)+1),main= "% datos faltantes por registro", col="green")
    axis(2,at=coord,labels=names(Obs_per),cex.axis=0.5,pos=0,las=2)
    axis(1,seq(0,1,0.2),seq(0,1,0.2))
  }
  return(invisible(lista))
}

Summary.NA = miss(Data) 

# 5.  Sobre el conjunto de variables cuantitativas, realice un diagnóstico de datos atípicos

# Visuaizar datos atipicos

#Huella Hidrica Directa
x11()
par(mfrow=c(3,1))
with(Data,{
  hist(HHD,freq=F,col="blue",breaks=13)
  boxplot(HHD,horizontal=T,col="blue")
  hist(scale(HHD),freq=F,col="blue",breaks=13)
}
)

# Huella Hidrica Indirecta
x11()
par(mfrow=c(3,1))
with(Data,{
  hist(HHI,freq=F,col="blue",breaks=13)
  boxplot(HHI,horizontal=T,col="blue")
  hist(scale(HHI),freq=F,col="blue",breaks=13)
}
)

# Personas en hogar
x11()
par(mfrow=c(2,1))
with(Data,{
  barplot(per.hog,freq=F,col="blue",breaks=13)
  boxplot(per.hog,horizontal=T,col="blue")
}
)

# Identificar datos atipicos
id.out.uni=function(x,method=c("Standarized","Tukey","Cook")){
  id.out=NULL
  if(method=="Standarized"){id.out=which(abs(scale(x))>3)}
  else if(method=="Tukey"){id.out=which(x%in%(boxplot.stats(x)$out))}
  else if(method=="Cook"){model=lm(x~1);CD=cooks.distance(model)
  id.out=unname(which(CD>4*mean(CD)))}
  return(id.out)
}

# HHD
id.out.uni(Data$HHD,method="Standarized")
id.out.uni(Data$HHD,method="Tukey")
id.out.uni(Data$HHD,method="Cook")

# HHI
id.out.uni(Data$HHI,method="Standarized")
id.out.uni(Data$HHI,method="Tukey")
id.out.uni(Data$HHI,method="Cook")

# per.hog
id.out.uni(Data$per.hog,method="Standarized")
id.out.uni(Data$per.hog,method="Tukey")
id.out.uni(Data$per.hog,method="Cook")


# 6.  Con los resultados de los puntos anteriores, usted dispone del listado con registros inconsistentes y con datos faltantes. Es necesario corregirlo.

# Imputacion por media

imputM = mice::mice(Data, method = "mean")
Data_ImputM = mice::complete(imputM)


# 7. Genere un resumen de los cambios realizados en la hoja de datos. ReporteCambios.txt

# Guardar cambios realizados

write.table(setdiff(Data_ImputM, Data), "ReporteCambios.txt", sep="\t", row.names=FALSE)


# Guárdela en un archivo nuevo clean_huella.txt
write.table(Data_ImputM, file = "clean_huella.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# 8. i.  Genere una nueva variable denominada huella hídrica total (HHT), que equivale a la suma entre HHD y HHI.

# HHT: Huella Hídrica Total

Data_ImputM$HHT <- Data_ImputM$HHD + Data_ImputM$HHI

# ii.  Sobre la nueva variable calculada (HHT), clasifíquela (HHT_clas) en 3 grupos que cumplan con las siguientes condiciones:
  
# Grupo-Rango de clasificación
# bajo-si HHT ≤ 1789
# medio-si 1789 < HHT ≤ 1887
# alto-si HHT > 1887

# Clasificación
Data_ImputM$HHT_clas <- cut(
 Data_ImputM$HHT,
  breaks = c(-Inf, 1789, 1887, Inf),
  labels = c("bajo", "medio", "alto")
)

# 9. Utilice su pericia en el procesamiento de datos para resumir los datos en uno o pocos tableros gráficos.

# i. Presente en una sola ventana gráfica las distribuciones de:
#    - Clasificación de la huella hídrica total, 
#    - Componente de la huella hídrica directa,
#    - Componente de la huella hídrica indirecta.

# Mostrar todo en una sola ventana
x11()
par(mfrow = c(3, 1))

# 1. Distribución de HHT_clas
barplot(table(Data_ImputM$HHT_clas),
        main = "Clasificación de la Huella Hídrica Total",
        col = "skyblue",
        xlab = "Clasificación", ylab = "Frecuencia")

# 2. Componente de la HHD
barplot(table(Data_ImputM$comp_HHD),
        main = "Componente de la Huella Hídrica Directa",
        col = "salmon",
        xlab = "Componente", ylab = "Frecuencia")

# 3. Componente de la HHI
barplot(table(Data_ImputM$comp_HHI),
        main = "Componente de la Huella Hídrica Indirecta",
        col = "lightgreen",
        xlab = "Componente", ylab = "Frecuencia")

# ii. Presente en una sola ventana grafica el comportamiento de los puntajes de la huella hídrica directa e indirecta por cada uno de los factores de estudio (sexo, grado escolar y zona).

# Abrir nueva ventana de gráficos
x11(width = 12, height = 10)

# Configurar 3 filas x 2 columnas y márgenes más amplios abajo
par(mfrow = c(3, 2), mar = c(6, 4, 4, 2) + 0.1)

# 1. HHD por Sexo
boxplot(HHD ~ genero, data = Data_ImputM,
        main = "HHD por Sexo",
        col = "yellow",
        xlab = "Sexo : 1. Femenino, 2. Masculino", ylab = "HHD",
        las = 1)

# 2. HHI por Sexo
boxplot(HHI ~ genero, data = Data_ImputM,
        main = "HHI por Sexo",
        col = "yellow",
        xlab = "Sexo : 1. Femenino, 2. Masculino", ylab = "HHI",
        las = 1)

# 3. HHD por Grado Escolar
boxplot(HHD ~ grado, data = Data_ImputM,
        main = "HHD por Grado Escolar",
        col = "blue",
        xlab = "Grado : 6. Sexto, 7. Septimo, 8. Octavo, 9. Noveno, 10. Decimo, 11.Once", ylab = "HHD",
        las = 2)  # Texto vertical para mejor ajuste

# 4. HHI por Grado Escolar
boxplot(HHI ~ grado, data = Data_ImputM,
        main = "HHI por Grado Escolar",
        col = "blue",
        xlab = "Grado : 6. Sexto, 7. Septimo, 8. Octavo, 9. Noveno, 10. Decimo, 11.Once", ylab = "HHI",
        las = 2)

# 5. HHD por Zona
boxplot(HHD ~ zona, data = Data_ImputM,
        main = "HHD por Zona",
        col = "red",
        xlab = "Zona: 1. Urbana, 2. Rural", ylab = "HHD",
        las = 1)

# 6. HHI por Zona
boxplot(HHI ~ zona, data = Data_ImputM,
        main = "HHI por Zona",
        col = "red",
        xlab = "Zona: 1. Urbana, 2. Rural", ylab = "HHI",
        las = 1)

# iii.  Presente un resumen de los principales indicadores descriptivos de las variables cuantitativas por cada uno de los factores (sexo, grado escolar y zona).

# Resumen por género
resumen_genero <- Data_ImputM %>%
  group_by(genero) %>%
  summarise(
    edad_media = mean(edad),
    edad_mediana = median(edad),
    edad_sd = sd(edad),
    HHD_media = mean(HHD),
    HHD_mediana = median(HHD),
    HHD_sd = sd(HHD),
    HHI_media = mean(HHI),
    HHI_mediana = median(HHI),
    HHI_sd = sd(HHI),
    per_hog_media = mean(per.hog),
    per_hog_mediana = median(per.hog),
    per_hog_sd = sd(per.hog)
  )
View(resumen_genero)

# Resumen por grado escolar
resumen_grado <- Data_ImputM %>%
  group_by(grado) %>%
  summarise(
    edad_media = mean(edad),
    edad_mediana = median(edad),
    edad_sd = sd(edad),
    HHD_media = mean(HHD),
    HHD_mediana = median(HHD),
    HHD_sd = sd(HHD),
    HHI_media = mean(HHI),
    HHI_mediana = median(HHI),
    HHI_sd = sd(HHI),
    per_hog_media = mean(per.hog),
    per_hog_mediana = median(per.hog),
    per_hog_sd = sd(per.hog)
  )

View(resumen_grado)

# Resumen por zona
resumen_zona <- Data_ImputM %>%
  group_by(zona) %>%
  summarise(
    edad_media = mean(edad),
    edad_mediana = median(edad),
    edad_sd = sd(edad),
    HHD_media = mean(HHD),
    HHD_mediana = median(HHD),
    HHD_sd = sd(HHD),
    HHI_media = mean(HHI),
    HHI_mediana = median(HHI),
    HHI_sd = sd(HHI),
    per_hog_media = mean(per.hog),
    per_hog_mediana = median(per.hog),
    per_hog_sd = sd(per.hog)
  )
 View(resumen_zona)
 