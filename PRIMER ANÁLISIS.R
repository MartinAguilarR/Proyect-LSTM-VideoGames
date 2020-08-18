# PROYECTO REALIZADO CON BASE DE DATOS: https://drive.google.com/file/d/1MdN8yLFzUi9KiqnUWnYxXETvVYY_UrCJ/view?usp=sharing
# DATASET N°2 - Ventas de Videojuegos


#instalación tiddyverse
install.packages("tidyverse")

#Libraries
library("tidyverse")
library ("dplyr")
library ("readxl")
library("ggplot2")
library("gganimate")
library("fmsb")

#Leer DataBase
Videogames <- read_csv("vgsales.csv")
Videogames


#       LIMPIEZA Y TRANSFORMACION DE LA DATA

#Identificar las variables del dataset
names(Videogames)

#Examinar las variables y sus campos: 
str(Videogames) 
#resultados: la variable YEAR la tomo como un character(texto),
#el resto esta bien.

#cantidad de NA por variable
sapply(Videogames, function(x) sum(is.na(x)))
#cantidad de NULL por variable
sapply(Videogames, function(x) sum(is.null(x)))

#voy a convertir years de character a dobble
Videogames$Year <- as.integer(Videogames$Year)
str(Videogames)
#se perdieron 271 datos. 

#Eliminar las filas con NA
Videogames <-Videogames[!is.na(Videogames$Year),]

#crea la nueva variable nuemero de letras
Videogames <- mutate(Videogames, N_letras = nchar(Videogames$Name))


################ FEATURES RELEVANTES ####################

#Genre
table(Videogames$Genre)
tab_genre <- table(Videogames$Genre)
mean(tab_genre)

#global sales
Videogames %>% group_by(Genre) %>% summarise(promedio=mean(Global_Sales))

#N_letras
filter(Videogames, N_letras >=100)
#solo 15 tienen mas de 100 letras


#############  ANALISIS EXPLORATORIO  ####################### 

#sacar los valores max y minimos de las variables cuantitativas
summarise(Videogames, min(Year),max(Year))
summarise(Videogames, min(Rank),max(Rank))
summarise(Videogames, min(NA_Sales),max(NA_Sales))
summarise(Videogames, min(EU_Sales),max(EU_Sales))
summarise(Videogames, min(JP_Sales),max(JP_Sales))
summarise(Videogames, min(Other_Sales),max(Other_Sales))
summarise(Videogames, min(Global_Sales),max(Global_Sales))
summarise(Videogames, min(N_letras),max(N_letras))

# 1. PLATFORM
ggplot(data = Videogames) + geom_bar(mapping = aes(x=Platform), stat = "count") + ggtitle("Histórico de copias demandadas por consola")
#PS2 y WII son las plataformas con mas juegos vendidos que superaron las 100.000 ventas

# 2. YEAR
ggplot(data=Videogames) + geom_bar(mapping = aes(x=Year), stat="count") + ggtitle("Demanda histórica 1980 - 2020")

# [1980 - 1990]
Rango_uno <- Videogames %>% filter ((Year >= 1980 & Year <= 1990), Global_Sales) 
Rango_uno
ggplot(data = Rango_uno) + geom_bar(mapping = aes(x=Year), stat = "count") + ggtitle("Demanda 1980 - 1990")

#[1990 - 2000]
Rango_dos <- Videogames %>% filter((Year > 1990 & Year <= 2000), Global_Sales)
Rango_dos
ggplot(data = Rango_dos) + geom_bar(mapping = aes(x=Year), stat = "count") + ggtitle("Demanda 1990 - 2000")

#[2000- 2010]
Rango_tres <-Videogames %>% filter((Year >= 2000 & Year < 2010), Global_Sales)
Rango_tres
ggplot(data = Rango_tres) + geom_bar(mapping = aes(x=Year), stat = "count") + ggtitle("Demanda 2000 - 2010")

#[2010 - 2020]
Rango_cuatro <- Videogames %>% filter((Year >= 2010 & Year <= 2017), Global_Sales) 
Rango_cuatro 
ggplot( data= Rango_cuatro) + geom_bar(mapping = aes(x=Year), stat="count") + ggtitle("Demanda 2010 - 2020")

# 3. GENRE 
ggplot(data = Videogames) + geom_bar(mapping = aes(x=Genre), stat = "count") + ggtitle("Demanda histórica de Videojuegos por género")

#Cantidad de generos de videojuegos
n_generos <- unique(Videogames$Genre)
length(n_generos)


####  CHART HISTORICO GÉNERO Y CONSOLA  ####
# TRAMO 1 MAYOR A 2000
Videogames_ps2 <- filter( Videogames, Platform == "PS2")
ggplot(data = Videogames_ps2, aes(x=Genre, y=Global_Sales)) + geom_bar(stat="identity") + ggtitle("Tendencia Histórica de Videojuegos Ps2")

Videogames_Ds <- filter( Videogames, Platform == "DS")
ggplot(data = Videogames_Ds , aes(x=Genre, y=Global_Sales)) + geom_bar(stat="identity")+ ggtitle("Tendencia Histórica de Videojuegos Ds")

# Basado en escala métrica del 1 al 4, donde;
# 4 es alto numero de ventas de copias ([0 - 4000]) por género, y 1 bajo número de copias

data_historico <- data.frame(Action = c (4,0,3,2),
                             Fighting = c (4,0,1,0),
                             Sports = c (4,0,3,1),
                             Racing = c (4,0,2,1),
                             Plataform = c(4,0,1,2),
                             Strategy = c (4,0,0,1),
                             Puzzle = c (4,0,0,2),
                             Adventure = c (4,0,0,1),
                             Misc = c (4,0,1,3),
                             RolePlaying = c(4,0,1,3),
                             Shooter = c (4,0,1,0),
                             row.names = c("max", "min" , "Ps2", "Ds"))

colors_fill <- c(scales::alpha("darkblue", 0.7),
                 scales::alpha("gold", 0.5))

colors_line <- c(scales::alpha("darkblue", 0.8),
                 scales::alpha("yellow", 0.7))

radarchart(data_historico, 
           seg= 3 , #Número de segmentos del eje
           title = "Tendencia histórica de géneros demandados por consolas 1" , 
           pcol = colors_line ,
           pfcol = colors_fill ,
           plwd = 2)

legend(x=1.8,
       y=1.35,
       legend = rownames(data_historico[-c(1,2),]),
       bty = "n", pch=20, col= colors_line, cex= 1.2, pt.cex = 3)


# TRAMO 2 [1000 - 1500]
Videogames_ps3 <- filter( Videogames, Platform == "PS3")
ggplot(data = Videogames_ps3, aes(x=Genre, y=Global_Sales)) + geom_bar(stat="identity")+ ggtitle("Tendencia Histórica de Videojuegos Ps3")

Videogames_X360 <- filter( Videogames, Platform == "X360")
ggplot(data = Videogames_X360, aes(x=Genre, y=Global_Sales)) + geom_bar(stat="identity")+ ggtitle("Tendencia Histórica de Videojuegos X360")

Videogames_Wii <- filter( Videogames, Platform == "Wii")
ggplot(data = Videogames_Wii , aes(x=Genre, y=Global_Sales)) + geom_bar(stat="identity")+ ggtitle("Tendencia Histórica de Videojuegos Wii")


data_historico2 <- data.frame(Action = c (4,0,3,3,1),
                              Fighting = c (4,0,1,1,0),
                              Sports = c (4,0,2,2,3),
                              Racing = c (4,0,1,1,1),
                              Plataform = c(4,0,0,0,1),
                              Strategy = c (4,0,0,0,0),
                              Puzzle = c (4,0,0,0,0),
                              Adventure = c (4,0,0,0,0),
                              Misc = c (4,0,0,1,3),
                              RolePlaying = c(4,0,1,1,0),
                              Shooter = c (4,0,2,3,1),
                              row.names = c("max","min" ,"PS3", "X360", "Wii"))

colors_fill <- c(scales::alpha("red", 0.5),
                 scales::alpha("darkblue", 0.5),
                 scales::alpha("yellow", 0.5))


colors_line <- c(scales::alpha("red", 0.8),
                 scales::alpha("darkblue", 0.7),
                 scales::alpha("yellow", 0.7))


radarchart(data_historico2, 
           seg= 3 , #Número de segmentos del eje
           title = "Tendencia histórica de géneros demandados por consolas 2" , 
           pcol = colors_line ,
           pfcol = colors_fill ,
           plwd = 3)

legend(x=1.8,
       y=1.35,
       legend = rownames(data_historico2[-c(1,2),]),
       bty = "n", pch=20, col= colors_line, cex= 1.2, pt.cex = 3)

# 4. PUBLISHER:
n_distinct(Videogames$Publisher)
#577 categorias distintas

#Editoras con más copias vendias
editores_factor <-factor(Videogames$Publisher)
plot(editores_factor)
summary(editores_factor)
Ventas_editor <- summary(editores_factor)
Ventas_editor


######## VENTAS POR ZONA GEOGRÁFICA ##############
# GLOBAL_SALES 
ggplot(data=Videogames, aes(x=Genre, y=Global_Sales)) + geom_bar(stat="identity") + ggtitle("Histórico demandado de Videojuegos por género")

ggplot(data=Videogames, aes(x=Platform, y=Global_Sales, fill=Genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("red", "blue","green","yellow","Pink","Black","purple","orange","brown","violet","light blue","light green")) + ggtitle("Ventas Históricas con Tendencia Plataforma-Género")
#En demanda global de géneros predomina Acción,sports y shooter
#De igual manera las plataformas mas relevantes a nivel global son Ps2, Ps3, X360, Wii, DS


# 5. NA_SALES 
ggplot(data=Videogames, aes(x=Genre, y=NA_Sales)) + geom_bar(stat="identity") + ggtitle("Tendencia de Videojuegos globales demandados por género NA")

ggplot(data=Videogames, aes(x=Platform, y=NA_Sales, fill=Genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("red", "blue","green","yellow","Pink","Black","purple","orange","brown","violet","light blue","light green")) + ggtitle("Ventas Históricas en NA con Tendencia Plataforma-Género")

#en norteamerica los juego mas vendidos pertenecen a las categoria action, sport y shooter.
# de igual forma las plataformas más relevantes son PS2, PS3, X360, WII y DS

# 6. EU_SALES 
ggplot(data = Videogames, aes(x=Genre, y=EU_Sales)) + geom_bar(stat="identity")+ ggtitle("Histórico demandado de Videojuegos por género EU")

ggplot(data=Videogames, aes(x=Platform, y=EU_Sales, fill=Genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("red", "blue","green","yellow","Pink","Black","purple","orange","brown","violet","light blue","light green")) + ggtitle("Ventas Históricas en EU con Tendencia Plataforma-Género")
#En Europa predominan los juegos de action, sport, shooter y misc
# de igual forma la plataformas más relevantes son PS2, PS3, X360,WII Y DS

# 7. JP_SALES
ggplot(data=Videogames, aes(x=Genre, y=JP_Sales)) + geom_bar(stat="identity") + ggtitle("Histórico demandado de Videojuegos por género JP")

ggplot(data=Videogames, aes(x=Platform, y=JP_Sales, fill=Genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("red", "blue","green","yellow","Pink","Black","purple","orange","brown","violet","light blue","light green")) + ggtitle("Ventas Históricas en JP con Tendencia Plataforma-Género")
#En japón predominan los juegos de role-playing, action y sport
# Dde igual forma las plataformas mas relevantes son Ds, Ps, Ps2, SNes

#8. OTHER_SALES 
ggplot(data=Videogames, aes(x=Genre, y=Other_Sales)) + geom_bar(stat="identity") + ggtitle("Histórico demandado de Videojuegos por género Otros Mercados")

ggplot(data=Videogames, aes(x=Platform, y=Other_Sales, fill=Genre)) + 
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("red", "blue","green","yellow","Pink","Black","purple","orange","brown","violet","light blue","light green")) + ggtitle("Ventas Históricas en Otros Mercados con Tendencia Plataforma-Género")
#En Otros mercados los juegos predominantes son acción, sport y shooter
#De igual manera las plataformas mas relevantes son  Ps2 , Ps3


# 9.NUMERO DE LETRAS
mean(Videogames$N_letras)
ggplot(data = Videogames, aes(x = Genre, y = N_letras)) + 
  geom_jitter(aes(color = Genre), size = 0.8, alpha = 0.7) +
  geom_boxplot(aes(color = Genre), alpha = 0.7) + 
  xlab('Género') + 
  ylab('Numero de letras en el nombre de los videojuegos') +
  ggtitle('Numero de letras en los titulos por géneros') + 
  theme_minimal()


#                  PREGUNTAS DEL NEGOCIO

# 1. ¿En qué mercado se vende la mayor cantidad de videojuegos?
ventas_Gl <- sum(Videogames$Global_Sales) 
ventas_Gl

ventas_NA <- sum(Videogames$NA_Sales)
ventas_NA
ventas_EU <- sum(Videogames$EU_Sales)
ventas_EU
ventas_JP <- sum(Videogames$JP_Sales)
ventas_JP
ventas_Others <- sum(Videogames$Other_Sales)
ventas_Others
#En Norteamerica se vende la mayor cantidad de videojuegos

# 2. ¿Existe evidencia de que los videojuegos con títulos más cortos venden más?
hist(x=Videogames$N_letras)
# la mayor cantidad de videojuegos tiene entre 10 y 30 letras

# 3.¿Los editores con mayores ventas se especializan en pocos géneros o distribuyen una  alta variedad de videojuegos?
tab_editores <- filter(Videogames, Publisher %in% c("Electronic Arts","Activision","Namco Bandai Games","Ubisoft","Konami Digital Entertainment","THQ","Nintendo","Sony Computer Entertainment","Sega"))
tab_editores
tabla_publisher <- table(tab_editores$Genre, tab_editores$Publisher)
tabla_publisher

ggplot(data = tab_editores, aes(x=Publisher, fill = as.factor(Publisher))) + 
  geom_bar() +
  xlab("Editores") +
  ylab("Cantidad de juegos") +
  ggtitle("Cantidad de juegos que vendieron más de 100.000 copias por editor ")

