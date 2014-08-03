#!/usr/bin/Rscript

###### LIBRARIES
library(reshape2) # load reshape - bringing holy pivot table functionality to R
library(plyr)
#library(zoo) # for month of year calculation - at wiederholfaktura

######################### LADEN UND FORMATIEREN ############################### 
# Input Zeiten laden
leistungen <- read.csv(file="input/volle_leistung.csv",head=TRUE,sep=";")
names(leistungen) <- sub(" ", ".", names(leistungen)) ## replace spaces in column names

# Zuteilung der Tätigkeiten
kategorien <- read.csv(file="input/kategorisierung.csv", head=TRUE,sep=",")
#table(kategorien[,"Kategorie"]) ##debug, Kategorie is factor

## Datum konvertieren VON BMD - "12.09.2013"
leistungen$Leistungsdatum <- as.Date(leistungen$Leistungsdatum, "%d.%m.%Y")

## Stunden in Millisekunden umwandeln - "03:58"
leistungen$duration <- as.numeric(sapply(strsplit(as.character(leistungen$Std),":"), "[",1))*60*60*1000+as.numeric(sapply(strsplit(as.character(leistungen$Std),":"), "[",2))*60*1000 # format in millisekunden umwandeln
leistungen <- leistungen[!is.na("Tät.Nr.Art.Nr"),] ## leere Tätigkeitsfelder rausstreichen

## Sample Data zum Ausprobieren
#leistungen$begin <- sample(0:59, nrow(leistungen), replace=T)
#leistungen$end <- sample(0:59, nrow(leistungen), replace=T)

## Uhrzeiten verstehen
#leistungen$begin <- format(leistungen$Von.Zeit, format="%H:%M")
#leistungen$end <- format(leistungen$Bis.Zeit, format="%H:%M")
#leistungen$begin <- as.POSIXct(leistungen$Von.Zeit, format="%H:%M") ## posixct nimmt das aktuelle Datum, wenn nichts angegeben ist
#leistungen$end <- as.POSIXct(leistungen$Bis.Zeit, format="%H:%M")
leistungen$begin <- as.numeric(sapply(strsplit(as.character(leistungen$Von.Zeit),":"), "[",1))*60+as.numeric(sapply(strsplit(as.character(leistungen$Von.Zeit),":"), "[",2)) #Stunden in Minuten umrechnen 
leistungen$end <- as.numeric(sapply(strsplit(as.character(leistungen$Bis.Zeit),":"), "[",1))*60+as.numeric(sapply(strsplit(as.character(leistungen$Bis.Zeit),":"), "[",2)) #Stunden in Minuten umrechnen 

# Reduktion der Daten
leistungen <- leistungen[,c("Tät.Nr.Art.Nr","begin","end")]
colnames(leistungen)[1] <- "Tätigkeit" #rename


#nrow(leistungen)
leistungen <- leistungen[!is.na(leistungen$begin),]
leistungen <- leistungen[!is.na(leistungen$end),]
leistungen <- leistungen[!is.na(leistungen$Tätigkeit),]
#nrow(leistungen)


############### Analyse pro Minute ################

leistungen <- leistungen[which(leistungen$begin != leistungen$end),] # remove entries where start time equals end time, they ruin everything
#leistungen <- leistungen[which(leistungen$begin >= leistungen$end),]##debug
leistungen$diff <- ifelse(leistungen$begin <= leistungen$end,
			  leistungen$end - leistungen$begin+1,
			  1439 - leistungen$begin + leistungen$end +1+1 # 2 times +1 for correcting the substraction
			  )

leistungen <- leistungen[rep(row.names(leistungen), leistungen$diff), ] # repeat the rows
start_base <- grep("\\.",rownames(leistungen), invert = TRUE) # search for rownames without period in rowname, to start the count from, index for start of series
override <- ifelse(leistungen$begin <= leistungen$end, # create the entries for the series; special treatment when the time ends on the next day
	unlist(sapply(start_base, function(i) seq(leistungen$begin[i], leistungen$end[i]))), 
	unlist(sapply(start_base, function(i) c(seq(leistungen$begin[i], 1439),seq(0,leistungen$end[i]))))
	)

#leistungen$Minute[1:nrow(leistungen)] <- override[1:nrow(leistungen)] ##debug
#write.csv(leistungen,"oida.csv") ##debug
leistungen$Minute <- override
#nrow(leistungen) ##debug &info


## Adding Categories
leistungen <- merge(leistungen,kategorien[kategorien$Kategorie!="",c("Tätigkeit","Kategorie")],by.x="Tätigkeit",by.y="Tätigkeit",all=FALSE) ## add manual chosen Categories for Tätigkeiten. merge with only the rows containing Categories & only selected columns. this way the entries without categories are removed.

leistungen$Kategorie <- factor(leistungen$Kategorie) #removing the "" factor

## Aggregate
timeboard <- dcast(leistungen,Minute+Kategorie~"Anzahl")
sum.timeboard <- dcast(leistungen, Minute~"Summe") # sum per minuted, needed for normalisation. beware: leistungen only contains values with categories. 
#plot(sum.timeboard$Minute/60,sum.timeboard$Summe) ##debug
tätboard <- dcast(leistungen,Kategorie~"Anzahl")
#write.csv(tätboard,"tätboard.csv")


#################### Normalization to sum of each minute ######################
sum.timeboard$normalize.factor <- 100/sum.timeboard$Summe #Normalizing the sum to 100

timeboard <- merge(timeboard,sum.timeboard,by.x="Minute",by.y="Minute",all.x=TRUE) #merge in order to normalize
timeboard$Normzahl <- timeboard$Anzahl * timeboard$normalize.factor



for (x in levels(timeboard$Kategorie)) {
	jpeg(paste("/home/stefan/Desktop/export/agg/",x,".jpg"), width=800, height=800)
	plot(timeboard$Minute[timeboard$Kategorie==x]/60,timeboard$Normzahl[timeboard$Kategorie==x], main=x, ylim=c(0,40)) ##debug
	dev.off()
}

for (i in 1:1439) {
	jpeg(paste("/home/stefan/Desktop/export/",i,".jpg"), width=800, height=800)
	plot(timeboard[timeboard$Minute==i,"Kategorie"],timeboard[timeboard$Minute==i, "Normzahl"],main=i, ylim=c(0,40))
	dev.off()
}
asdfaf



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
X <- maty[min.index,] ##selected solutions for min
X
sepp <- which(X!=0,arr.ind = T) ## which cells fit the condition, result is a matrix
sepp
X[sepp]
X[sepp[1,]]

X[sepp[2,]]
