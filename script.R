#!/usr/bin/Rscript

###### LIBRARIES
library(reshape2) # load reshape - bringing holy pivot table functionality to R
library(plyr)
#library(zoo) # for month of year calculation - at wiederholfaktura

######################### LADEN UND FORMATIEREN ############################### 
# Input Zeiten laden
leistungen <- read.csv(file="input/volle_leistung.csv",head=TRUE,sep=";")
names(leistungen) <- sub(" ", ".", names(leistungen)) ## replace spaces in column names

## Datum konvertieren VON BMD - "12.09.2013"
leistungen$Leistungsdatum <- as.Date(leistungen$Leistungsdatum, "%d.%m.%Y")

## Stunden in Millisekunden umwandeln - "03:58"
leistungen$duration <- as.numeric(sapply(strsplit(as.character(leistungen$Std),":"), "[",1))*60*60*1000+as.numeric(sapply(strsplit(as.character(leistungen$Std),":"), "[",2))*60*1000 # format in millisekunden umwandeln
leistungen <- leistungen[!is.na("Tät.Nr.Art.Nr"),] ## leere Tätigkeitsfelder rausstreichen

## Sample Data zum Ausprobieren
leistungen$begin <- sample(0:59, nrow(leistungen), replace=T)
leistungen$end <- sample(0:59, nrow(leistungen), replace=T)


## Uhrzeiten verstehen
#year$Start.time <- format(year$Start.time, format="%H:%M")
#year$End.time <- format(year$End.time, format="%H:%M")
#year$Start.time_now <- as.POSIXct(year$Start.time, format="%H:%M:%S") ## posixct nimmt das aktuelle Datum, wenn nichts angegeben ist
#year$End.time_now <- as.POSIXct(year$End.time, format="%H:%M:%S")


# Reduktion der Daten
leistungen <- leistungen[,c("Tät.Nr.Art.Nr","duration","begin","end")]
colnames(leistungen)[1] <- "Tätigkeit" #rename


############### Analyse pro Minute ################
for (i in 0:59 ) {
	column  <- as.character(i) # zusätzliche Spalten pro Minute anlegen
	leistungen[,column] <- ifelse( 
		leistungen$begin <= leistungen$end, ## wenn das Ende nicht in der folgenden Stunde liegt - von Minute 3 bis Minute 41
		ifelse( 
			leistungen$begin <= i & i <= leistungen$end, ## wenn es drinnen ist, zwischen Minute 3 und Minute 41
			1,
			0
		),
		ifelse(
			leistungen$begin <= i | leistungen$end <= i, ## wenn es drinnen ist, zwischen Minute 41 und Minute 3
			1,
			0
		)
	)
}
#leistungen[1:5,]
leistungen <- melt(leistungen, id.vars=c("Tätigkeit","duration","begin","end"), variable.name = "Minute", value.name = "DrinnenKZ") #melt minuten columns in 
check <- dcast(leistungen,Minute+Tätigkeit~"Anzahl",sum,value.var="DrinnenKZ",na.rm=TRUE)
#plot(check[check$Minute==0,"Tätigkeit"],check[check$Minute==0,"Anzahl"]) ##debug

#Summe pro Minute & Normalisierung
for (i in unique(check$Minute)) {
	check[check$Minute==i,"Summe"] <- sum(check[check$Minute==i,"Anzahl"]) #Summe pro Minute
	check[check$Minute==i,"Normalisation.factor"] <- 100/check[check$Minute==i,"Summe"] #Normalisierung mit der Summe auf 100
}
check$normalized <- check[,"Anzahl"]*check[,"Normalisation.factor"]


## Durchschnitt - for DEBUG 
#average <- dcast(leistungen,Tätigkeit~"Anzahl",sum,value.var="DrinnenKZ",na.rm=TRUE) #average$Anzahl  <-  average$Anzahl/60 ### größe rechtecke
#cast(year[year$drinnen_kz == 1,], Project~.) #Anzahl der Zeiten


############ Check for best match #################
# combinationen rechen pro kategorie (ohne Wiederholung, mit 0 der anderen)
# optimieren der differenz
# flächen aus dem pool für zukünftige kategorien rausschmeißen

#hypothese: die lösung mit der niedrigsten anzahl an flächen liefert die optimale fläche
# generell muss man isch entscheiden, auf was man optimiert: auf die gesamte fläche oder die einzelnen matches. 

target <- 30
sepp <- c(50,30,18,12)

#combinations
sepp <- append(sepp,rep(0, length(sepp))) #add zero so it repeats while the other stuff is just coming up once. number of zeros is the same of the numbers of areas 
#gtools::combinations(length(sepp), r=2L, v=sepp, repeats.allowed=TRUE)
maty <- gtools::combinations(length(sepp), 4, v=sepp, set=FALSE, repeats.allowed=FALSE)
maty <- unique(maty) # remove duplicate rows, duplicates because of the 0s
#maty ##debug

#get the index of the minima
diff <- abs(target-rowSums(maty)) #the absolute difference for each combination
min.index <- which(diff == min(diff)) #index for the minima solution
#min.index
maty[min.index,] ##selected solutions for min
