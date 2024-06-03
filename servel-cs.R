#@heelgueta, 30/07/2021
#resumir datos votaciones FA/CS de mayo x mesa

#logré juntar los archivos de votaciones de diferentes años a partir de los archivos que compartieron
#eso sí muchas mesas y locales de votación cambian, por lo que la cantidad de mesas disponibles para analizarse baja considerablemente
#como encontré problemas con algunos de los archivos que se nos compartió decidí partir desde cero y volver a bajar los archivos de las elecciones de mayo de 2021 desde servel.cl
#encontré los archivos de mayo 21 para descargar acá https://www.servel.cl/resultados-definitivos-elecciones-de-convencionales-constituyentes-gobernadores-regionales-alcaldes-y-concejales/
#despues termino otra sintaxis para resumir x comuna? + gráficos bonitos
#seguimos!

#ojo: (requiere tener instalado tidyverse)
#install.packages("tidyverse")

###################################
#1 descargar los archivos de nuevo#
###################################
#para descargarlos se pueden ejecutar las siguientes lineas de código
download.file("https://www.servel.cl/wp-content/uploads/2021/06/resultados_provisorios_mrs_ccg.csv.zip","resultados_provisorios_mrs_ccg.csv.zip")
download.file("https://www.servel.cl/wp-content/uploads/2021/06/resultados_provisorios_mrs_gore.csv.zip","resultados_provisorios_mrs_gore.csv.zip")
download.file("https://www.servel.cl/wp-content/uploads/2021/06/resultados_provisorios_mrs_alc.csv.zip","resultados_provisorios_mrs_alc.csv.zip")
download.file("https://www.servel.cl/wp-content/uploads/2021/06/resultados_provisorios_mrs_conc.csv.zip","resultados_provisorios_mrs_conc.csv.zip")
#(cabe mencionar que los archivos tenían los mismos problemas, pero... aquí vamos)

##############################################################################################
#2 cargar, corregir, extraer datos resumidos x mesa respecto a votaciones x constituyentes FA#
##############################################################################################
#cargar datos ccge, ojo, no es necesario descomprimir el zip :)
ccge2021 <- readr::read_csv(file = "resultados_provisorios_mrs_ccg.csv.zip")
#quitar filas con datos redundantes 
ccge2021 <- subset(ccge2021, nvoto != 904 & nvoto != 905) 
#corregir celdas sin votos que dicen NA en vez de 0, ojo con esto, quizás es problemático
ccge2021$votos_provisorio[is.na(ccge2021$votos_provisorio)] <- 0
#lista de variables
cat(colnames(ccge2021), sep="\n")
#crear variable idmesa para preservar info de ubicacion de cada mesa
ccge2021$idmesa <- paste(ccge2021$region,";",ccge2021$distrito,";",ccge2021$comuna,";",ccge2021$circunscripcion,";",ccge2021$local_nombre,";",ccge2021$mesa_numero)
#crear nueva matriz ccge2021r (r: resumen) con el no de electores x mesa
ccge2021r <- data.frame(with(ccge2021, tapply(mesa_electores, list(idmesa), FUN=max,na.rm=TRUE)))
ccge2021r <- tibble::rownames_to_column(ccge2021r)
ccge2021r <- dplyr::rename(ccge2021r, nelect = with.ccge2021..tapply.mesa_electores..list.idmesa...FUN...max..)
ccge2021r <- dplyr::rename(ccge2021r, idmesa = rowname)
ccge2021r <- tibble::as_tibble(ccge2021r)
#añadir a ccge2021r cantidad de votos x mesa
ccge2021r$totalv <- with(ccge2021,tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE))
#crear dfs ccge2021s y ccge2021sr (s: subset, r: resumen) como pasos para extraer votos de partidos del frente amplio, luego incorporarlos a ccge2021r y luego quitar dfs extra 
ccge2021s <- subset(ccge2021,partido_politico=="CONVERGENCIA SOCIAL" | partido_politico=="REVOLUCION DEMOCRATICA" | partido_politico=="COMUNES")
ccge2021sr <- data.frame(with(ccge2021s, tapply(votos_provisorio, list(idmesa), FUN=sum)))
ccge2021sr <- tibble::rownames_to_column(ccge2021sr)
ccge2021sr <- tibble::as_tibble(ccge2021sr)
ccge2021sr <- dplyr::rename(ccge2021sr, frente = with.ccge2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
ccge2021sr <- dplyr::rename(ccge2021sr, idmesa = rowname)
ccge2021r <- plyr::join(ccge2021r,ccge2021sr,by=c("idmesa"))
rm(ccge2021s,ccge2021sr)
#crear dfs ccge2021s y ccge2021sr (s: subset, r: resumen) como pasos para extraer votos de CS, luego incorporarlos a ccge2021r y luego quitar dfs extra 
ccge2021s <- subset(ccge2021,partido_politico=="CONVERGENCIA SOCIAL")
ccge2021sr <- data.frame(with(ccge2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
ccge2021sr <- tibble::rownames_to_column(ccge2021sr)
ccge2021sr <- tibble::as_tibble(ccge2021sr)
ccge2021sr <- dplyr::rename(ccge2021sr, convso = with.ccge2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
ccge2021sr <- dplyr::rename(ccge2021sr, idmesa = rowname)
ccge2021r <- plyr::join(ccge2021r,ccge2021sr,by=c("idmesa"))
rm(ccge2021s,ccge2021sr)
#crear dfs ccge2021s y ccge2021sr (s: subset, r: resumen) como pasos para extraer votos de RD, luego incorporarlos a ccge2021r y luego quitar dfs extra 
ccge2021s <- subset(ccge2021,partido_politico=="REVOLUCION DEMOCRATICA")
ccge2021sr <- data.frame(with(ccge2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
ccge2021sr <- tibble::rownames_to_column(ccge2021sr)
ccge2021sr <- tibble::as_tibble(ccge2021sr)
ccge2021sr <- dplyr::rename(ccge2021sr, revdem = with.ccge2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
ccge2021sr <- dplyr::rename(ccge2021sr, idmesa = rowname)
ccge2021r <- plyr::join(ccge2021r,ccge2021sr,by=c("idmesa"))
rm(ccge2021s,ccge2021sr)
#crear dfs ccge2021s y ccge2021sr (s: subset, r: resumen) como pasos para extraer votos de comunes, luego incorporarlos a ccge2021r y luego quitar dfs extra 
ccge2021s <- subset(ccge2021,partido_politico=="COMUNES")
ccge2021sr <- data.frame(with(ccge2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
ccge2021sr <- tibble::rownames_to_column(ccge2021sr)
ccge2021sr <- tibble::as_tibble(ccge2021sr)
ccge2021sr <- dplyr::rename(ccge2021sr, comune = with.ccge2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
ccge2021sr <- dplyr::rename(ccge2021sr, idmesa = rowname)
ccge2021r <- plyr::join(ccge2021r,ccge2021sr,by=c("idmesa"))
rm(ccge2021s,ccge2021sr)
#quitar df original para liberar ram, sólo dejando df resumen
rm(ccge2021)

#####################################################################################
#3 cargar, corregir, extraer datos resumidos x mesa respecto a votaciones x gores FA#
#####################################################################################
gore2021 <- readr::read_csv(file = "resultados_provisorios_mrs_gore.csv.zip")
#quitar filas con datos redundantes 
gore2021 <- subset(gore2021, nvoto != 904 & nvoto != 905) 
#corregir celdas sin votos que dicen NA en vez de 0, ojo con esto, quizás es problemático
gore2021$votos_provisorio[is.na(gore2021$votos_provisorio)] <- 0
#lista de variables
cat(colnames(gore2021), sep="\n")
#crear variable idmesa para preservar info de ubicacion de cada mesa
gore2021$idmesa <- paste(gore2021$region,";",gore2021$distrito,";",gore2021$comuna,";",gore2021$circunscripcion,";",gore2021$local_nombre,";",gore2021$mesa_numero)
#crear nueva matriz gore2021r (r: resumen) con el no de electores x mesa
gore2021r <- data.frame(with(gore2021, tapply(mesa_electores, list(idmesa), FUN=max,na.rm=TRUE)))
gore2021r <- tibble::rownames_to_column(gore2021r)
gore2021r <- dplyr::rename(gore2021r, nelect = with.gore2021..tapply.mesa_electores..list.idmesa...FUN...max..)
gore2021r <- dplyr::rename(gore2021r, idmesa = rowname)
gore2021r <- tibble::as_tibble(gore2021r)
#añadir a gore2021r cantidad de votos x mesa
gore2021r$totalv <- with(gore2021,tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE))
#crear dfs gore2021s y gore2021sr (s: subset, r: resumen) como pasos para extraer votos de partidos del frente amplio, luego incorporarlos a gore2021r y luego quitar dfs extra 
gore2021s <- subset(gore2021,pacto=="FRENTE AMPLIO")
gore2021sr <- data.frame(with(gore2021s, tapply(votos_provisorio, list(idmesa), FUN=sum)))
gore2021sr <- tibble::rownames_to_column(gore2021sr)
gore2021sr <- tibble::as_tibble(gore2021sr)
gore2021sr <- dplyr::rename(gore2021sr, frente = with.gore2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
gore2021sr <- dplyr::rename(gore2021sr, idmesa = rowname)
gore2021r <- plyr::join(gore2021r,gore2021sr,by=c("idmesa"))
rm(gore2021s,gore2021sr)
#crear dfs gore2021s y gore2021sr (s: subset, r: resumen) como pasos para extraer votos de CS, luego incorporarlos a gore2021r y luego quitar dfs extra 
gore2021s <- subset(gore2021,partido_politico=="CONVERGENCIA SOCIAL")
gore2021sr <- data.frame(with(gore2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
gore2021sr <- tibble::rownames_to_column(gore2021sr)
gore2021sr <- tibble::as_tibble(gore2021sr)
gore2021sr <- dplyr::rename(gore2021sr, convso = with.gore2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
gore2021sr <- dplyr::rename(gore2021sr, idmesa = rowname)
gore2021r <- plyr::join(gore2021r,gore2021sr,by=c("idmesa"))
rm(gore2021s,gore2021sr)
#crear dfs gore2021s y gore2021sr (s: subset, r: resumen) como pasos para extraer votos de RD, luego incorporarlos a gore2021r y luego quitar dfs extra 
gore2021s <- subset(gore2021,partido_politico=="REVOLUCION DEMOCRATICA")
gore2021sr <- data.frame(with(gore2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
gore2021sr <- tibble::rownames_to_column(gore2021sr)
gore2021sr <- tibble::as_tibble(gore2021sr)
gore2021sr <- dplyr::rename(gore2021sr, revdem = with.gore2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
gore2021sr <- dplyr::rename(gore2021sr, idmesa = rowname)
gore2021r <- plyr::join(gore2021r,gore2021sr,by=c("idmesa"))
rm(gore2021s,gore2021sr)
#crear dfs gore2021s y gore2021sr (s: subset, r: resumen) como pasos para extraer votos de comunes, luego incorporarlos a gore2021r y luego quitar dfs extra 
gore2021s <- subset(gore2021,partido_politico=="COMUNES")
gore2021sr <- data.frame(with(gore2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
gore2021sr <- tibble::rownames_to_column(gore2021sr)
gore2021sr <- tibble::as_tibble(gore2021sr)
gore2021sr <- dplyr::rename(gore2021sr, comune = with.gore2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
gore2021sr <- dplyr::rename(gore2021sr, idmesa = rowname)
gore2021r <- plyr::join(gore2021r,gore2021sr,by=c("idmesa"))
rm(gore2021s,gore2021sr)
#quitar df original para liberar ram, sólo dejando resumen
rm(gore2021)

########################################################################################
#4 cargar, corregir, extraer datos resumidos x mesa respecto a votaciones x alcaldes FA#
########################################################################################
alca2021 <- readr::read_csv(file = "resultados_provisorios_mrs_alc.csv.zip")
#quitar filas con datos redundantes 
alca2021 <- subset(alca2021, nvoto != 904 & nvoto != 905) 
#corregir celdas sin votos que dicen NA en vez de 0, ojo con esto, quizás es problemático
alca2021$votos_provisorio[is.na(alca2021$votos_provisorio)] <- 0
#lista de variables
cat(colnames(alca2021), sep="\n")
#crear variable idmesa para preservar info de ubicacion de cada mesa
alca2021$idmesa <- paste(alca2021$region,";",alca2021$distrito,";",alca2021$comuna,";",alca2021$circunscripcion,";",alca2021$local_nombre,";",alca2021$mesa_numero)
#crear nueva matriz alca2021r (r: resumen) con el no de electores x mesa
alca2021r <- data.frame(with(alca2021, tapply(mesa_electores, list(idmesa), FUN=max,na.rm=TRUE)))
alca2021r <- tibble::rownames_to_column(alca2021r)
alca2021r <- dplyr::rename(alca2021r, nelect = with.alca2021..tapply.mesa_electores..list.idmesa...FUN...max..)
alca2021r <- dplyr::rename(alca2021r, idmesa = rowname)
alca2021r <- tibble::as_tibble(alca2021r)
#añadir a alca2021r cantidad de votos x mesa
alca2021r$totalv <- with(alca2021,tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE))
#crear dfs alca2021s y alca2021sr (s: subset, r: resumen) como pasos para extraer votos de partidos del frente amplio, luego incorporarlos a alca2021r y luego quitar dfs extra 
alca2021s <- subset(alca2021,pacto=="FRENTE AMPLIO")
alca2021sr <- data.frame(with(alca2021s, tapply(votos_provisorio, list(idmesa), FUN=sum)))
alca2021sr <- tibble::rownames_to_column(alca2021sr)
alca2021sr <- tibble::as_tibble(alca2021sr)
alca2021sr <- dplyr::rename(alca2021sr, frente = with.alca2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
alca2021sr <- dplyr::rename(alca2021sr, idmesa = rowname)
alca2021r <- plyr::join(alca2021r,alca2021sr,by=c("idmesa"))
rm(alca2021s,alca2021sr)
#crear dfs alca2021s y alca2021sr (s: subset, r: resumen) como pasos para extraer votos de CS, luego incorporarlos a alca2021r y luego quitar dfs extra 
alca2021s <- subset(alca2021,partido_politico=="CONVERGENCIA SOCIAL")
alca2021sr <- data.frame(with(alca2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
alca2021sr <- tibble::rownames_to_column(alca2021sr)
alca2021sr <- tibble::as_tibble(alca2021sr)
alca2021sr <- dplyr::rename(alca2021sr, convso = with.alca2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
alca2021sr <- dplyr::rename(alca2021sr, idmesa = rowname)
alca2021r <- plyr::join(alca2021r,alca2021sr,by=c("idmesa"))
rm(alca2021s,alca2021sr)
#crear dfs alca2021s y alca2021sr (s: subset, r: resumen) como pasos para extraer votos de RD, luego incorporarlos a alca2021r y luego quitar dfs extra 
alca2021s <- subset(alca2021,partido_politico=="REVOLUCION DEMOCRATICA")
alca2021sr <- data.frame(with(alca2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
alca2021sr <- tibble::rownames_to_column(alca2021sr)
alca2021sr <- tibble::as_tibble(alca2021sr)
alca2021sr <- dplyr::rename(alca2021sr, revdem = with.alca2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
alca2021sr <- dplyr::rename(alca2021sr, idmesa = rowname)
alca2021r <- plyr::join(alca2021r,alca2021sr,by=c("idmesa"))
rm(alca2021s,alca2021sr)
#crear dfs alca2021s y alca2021sr (s: subset, r: resumen) como pasos para extraer votos de comunes, luego incorporarlos a alca2021r y luego quitar dfs extra 
alca2021s <- subset(alca2021,partido_politico=="COMUNES")
alca2021sr <- data.frame(with(alca2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
alca2021sr <- tibble::rownames_to_column(alca2021sr)
alca2021sr <- tibble::as_tibble(alca2021sr)
alca2021sr <- dplyr::rename(alca2021sr, comune = with.alca2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
alca2021sr <- dplyr::rename(alca2021sr, idmesa = rowname)
alca2021r <- plyr::join(alca2021r,alca2021sr,by=c("idmesa"))
rm(alca2021s,alca2021sr)
#quitar df original para liberar ram, sólo dejando resumen
rm(alca2021)

##########################################################################################
#5 cargar, corregir, extraer datos resumidos x mesa respecto a votaciones x concejales FA#
##########################################################################################
conc2021 <- readr::read_csv(file = "resultados_provisorios_mrs_conc.csv.zip")
#quitar filas con datos redundantes 
conc2021 <- subset(conc2021, nvoto != 904 & nvoto != 905) 
#corregir celdas sin votos que dicen NA en vez de 0, ojo con esto, quizás es problemático
conc2021$votos_provisorio[is.na(conc2021$votos_provisorio)] <- 0
#lista de variables
cat(colnames(conc2021), sep="\n")
#crear variable idmesa para preservar info de ubicacion de cada mesa
conc2021$idmesa <- paste(conc2021$region,";",conc2021$distrito,";",conc2021$comuna,";",conc2021$circunscripcion,";",conc2021$local_nombre,";",conc2021$mesa_numero)
#crear nueva matriz conc2021r (r: resumen) con el no de electores x mesa
conc2021r <- data.frame(with(conc2021, tapply(mesa_electores, list(idmesa), FUN=max,na.rm=TRUE)))
conc2021r <- tibble::rownames_to_column(conc2021r)
conc2021r <- dplyr::rename(conc2021r, nelect = with.conc2021..tapply.mesa_electores..list.idmesa...FUN...max..)
conc2021r <- dplyr::rename(conc2021r, idmesa = rowname)
conc2021r <- tibble::as_tibble(conc2021r)
#añadir a conc2021r cantidad de votos x mesa
conc2021r$totalv <- with(conc2021,tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE))
#crear dfs conc2021s y conc2021sr (s: subset, r: resumen) como pasos para extraer votos de partidos del frente amplio, luego incorporarlos a conc2021r y luego quitar dfs extra 
conc2021s <- subset(conc2021,pacto=="FRENTE AMPLIO")
conc2021sr <- data.frame(with(conc2021s, tapply(votos_provisorio, list(idmesa), FUN=sum)))
conc2021sr <- tibble::rownames_to_column(conc2021sr)
conc2021sr <- tibble::as_tibble(conc2021sr)
conc2021sr <- dplyr::rename(conc2021sr, frente = with.conc2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
conc2021sr <- dplyr::rename(conc2021sr, idmesa = rowname)
conc2021r <- plyr::join(conc2021r,conc2021sr,by=c("idmesa"))
rm(conc2021s,conc2021sr)
#crear dfs conc2021s y conc2021sr (s: subset, r: resumen) como pasos para extraer votos de CS, luego incorporarlos a conc2021r y luego quitar dfs extra 
conc2021s <- subset(conc2021,partido_politico=="CONVERGENCIA SOCIAL")
conc2021sr <- data.frame(with(conc2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
conc2021sr <- tibble::rownames_to_column(conc2021sr)
conc2021sr <- tibble::as_tibble(conc2021sr)
conc2021sr <- dplyr::rename(conc2021sr, convso = with.conc2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
conc2021sr <- dplyr::rename(conc2021sr, idmesa = rowname)
conc2021r <- plyr::join(conc2021r,conc2021sr,by=c("idmesa"))
rm(conc2021s,conc2021sr)
#crear dfs conc2021s y conc2021sr (s: subset, r: resumen) como pasos para extraer votos de RD, luego incorporarlos a conc2021r y luego quitar dfs extra 
conc2021s <- subset(conc2021,partido_politico=="REVOLUCION DEMOCRATICA")
conc2021sr <- data.frame(with(conc2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
conc2021sr <- tibble::rownames_to_column(conc2021sr)
conc2021sr <- tibble::as_tibble(conc2021sr)
conc2021sr <- dplyr::rename(conc2021sr, revdem = with.conc2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
conc2021sr <- dplyr::rename(conc2021sr, idmesa = rowname)
conc2021r <- plyr::join(conc2021r,conc2021sr,by=c("idmesa"))
rm(conc2021s,conc2021sr)
#crear dfs conc2021s y conc2021sr (s: subset, r: resumen) como pasos para extraer votos de comunes, luego incorporarlos a conc2021r y luego quitar dfs extra 
conc2021s <- subset(conc2021,partido_politico=="COMUNES")
conc2021sr <- data.frame(with(conc2021s, tapply(votos_provisorio, list(idmesa), FUN=sum,na.rm=TRUE)))
conc2021sr <- tibble::rownames_to_column(conc2021sr)
conc2021sr <- tibble::as_tibble(conc2021sr)
conc2021sr <- dplyr::rename(conc2021sr, comune = with.conc2021s..tapply.votos_provisorio..list.idmesa...FUN...sum..)
conc2021sr <- dplyr::rename(conc2021sr, idmesa = rowname)
conc2021r <- plyr::join(conc2021r,conc2021sr,by=c("idmesa"))
rm(conc2021s,conc2021sr)
#quitar df original para liberar ram, sólo dejando resumen
rm(conc2021)

##################################################################################
#6 juntar en una sola matriz las votaciones x cc, gores, alcaldes y concejales FA#
##################################################################################
#un pequeño arreglo de los nombres un poco redundante, sorry
colnames(ccge2021r) <- paste("ccge2021",colnames(ccge2021r),sep="_")
colnames(gore2021r) <- paste("gore2021",colnames(gore2021r),sep="_")
colnames(alca2021r) <- paste("alca2021",colnames(alca2021r),sep="_")
colnames(conc2021r) <- paste("conc2021",colnames(conc2021r),sep="_")
ccge2021r <- dplyr::rename(ccge2021r,idmesa=ccge2021_idmesa)
gore2021r <- dplyr::rename(gore2021r,idmesa=gore2021_idmesa)
alca2021r <- dplyr::rename(alca2021r,idmesa=alca2021_idmesa)
conc2021r <- dplyr::rename(conc2021r,idmesa=conc2021_idmesa)
#merge them all!
elec2021r <- plyr::join(ccge2021r,gore2021r,by=c("idmesa"),type="full")
elec2021r <- plyr::join(elec2021r,alca2021r,by=c("idmesa"),type="full")
elec2021r <- plyr::join(elec2021r,conc2021r,by=c("idmesa"),type="full")
#recover info from idmesa
elec2021r <- tidyr::separate(data = elec2021r,col = idmesa,into = c("region","distri","comuna","circun","nlocal","mesano"),sep=";")
#guardar archivo como csv
write.table(elec2021r,"elec2021r.csv",sep=";")
#ta-dá