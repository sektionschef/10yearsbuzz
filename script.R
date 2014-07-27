#!/usr/bin/Rscript

###### LIBRARIES
library(reshape2) # load reshape - bringing holy pivot table functionality to R
library(plyr)
#library(zoo) # for month of year calculation - at wiederholfaktura

# Input Zeiten laden
leistungen <- read.csv(file="input/volle_leistung.csv",head=TRUE,sep=";")
names(leistungen) <- sub(" ", ".", names(leistungen)) ## replace spaces in column names

## Datum konvertieren VON BMD - "12.09.2013"
leistungen$Leistungsdatum <- as.Date(leistungen$Leistungsdatum, "%d.%m.%Y")

## Stunden in Millisekunden umwandeln - "03:58"
leistungen$duration <- as.numeric(sapply(strsplit(as.character(leistungen$Std),":"), "[",1))*60*60*1000+as.numeric(sapply(strsplit(as.character(leistungen$Std),":"), "[",2))*60*1000 # format in millisekunden umwandeln
leistungen <- leistungen[!is.na("Tät.Nr.Art.Nr"),] ## leere Tätigkeitsfelder rausstreichen

#check <- dcast(leistungen,Tät.Nr.Art.Nr~.,sum,value.var="duration",na.rm=TRUE) #debug


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


# Analyse pro Minute
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
leistungen <- melt(leistungen, id.vars=c("Tät.Nr.Art.Nr","duration","begin","end"), variable.name = "Minute", value.name = "DrinnenKZ") #melt 
check <- dcast(leistungen,Minute+Tät.Nr.Art.Nr~"Anzahl",sum,value.var="DrinnenKZ",na.rm=TRUE)
#plot(check[check$Minute==0,"Tät.Nr.Art.Nr"],check[check$Minute==0,"Anzahl"]) ##debug

#Summe pro Minute & Normalisierung
for (i in unique(check$Minute)) {
	check[check$Minute==i,"Summe"] <- sum(check[check$Minute==i,"Anzahl"])
	check[check$Minute==i,"Normalisation.factor"] <- 100/check[check$Minute==i,"Summe"]
}
check$normalized <- check[,"Anzahl"]*check[,"Normalisation.factor"]

## Durchschnitt - DEBUG 
#average <- dcast(leistungen,Tät.Nr.Art.Nr~"Anzahl",sum,value.var="DrinnenKZ",na.rm=TRUE) #average$Anzahl  <-  average$Anzahl/60 ### größe rechtecke
#cast(year[year$drinnen_kz == 1,], Project~.) #Anzahl der Zeiten



######### NICE INPUT ###################
# http://blog.revolutionanalytics.com/2009/07/because-its-friday-the-knapsack-problem.html 
appetizer.solution <- local (
function (target) {
  app <- c(2.15, 2.75, 3.35, 3.55, 4.20, 5.80)
  r <- 2L
  repeat {
	c <- gtools::combinations(length(app), r=r, v=app, repeats.allowed=TRUE)
	s <- rowSums(c)
	if ( all(s > target) ) {
	  print("No solution found")
	  break
	}
	x <- which( abs(s-target) < 1e-4 )
	if ( length(x) > 0L ) {
	  print("Solution found")
	  print(c[x,])
	  break
	}
	r <- r + 1L #L tells it to define it as an integer, which makes it faster
  }
})
#######################################

# combinationen rechen pro kategorie (ohne Wiederholung, mit 0 der anderen)
# optimieren der differenz
# flächen aus dem pool für zukünftige kategorien rausschmeißen

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
