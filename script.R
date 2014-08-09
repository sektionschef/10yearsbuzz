#!/usr/bin/Rscript

###### LIBRARIES
library(reshape2) # load reshape - bringing holy pivot table functionality to R
library(plyr)



####temporary stuff

timeboard <- read.csv(file="timeboard.csv",head=TRUE)
#timeboard[1:6,]

target_example <- 32

#areas <- c(50,30,18,12,32,44,2,6,9,45,32,36,19,23,27,29,15,11,17,39,28,23,11,19,1,3,8,33,23,1,5,9,24,14,11,9,44,21)

areas <- data.frame(size = c(50,30,18,12,32,44,2,6,9,45,32,36,19,23,27,29,15,11,17,39,28,23,11,19,1,3,8,33,23,1,5,9,24,14,11,9,44,21))
areas$id <- paste("e",1:length(areas$size),sep="")
write.csv(areas,"areas.csv")
#areas


lightup <- data.frame(Minute=unique(timeboard$Minute)) #create the plan which element to light up in each minute
lightup[,as.character(areas$id)] <- 0 ## fill it up
#lightup[1:7,]


knappsackn <- function(target,areas,areas.id) {
	#combinations
	possible.combinations <- append(areas,rep(0, 4)) #add zero so it repeats while the other stuff is just coming up once. number of repeats means how many areas can be selected, making it more and more complex.

	possible.combinations <- gtools::combinations(length(possible.combinations), 4, v=possible.combinations, set=FALSE, repeats.allowed=FALSE) ## second parameter is the dimensions (number of elements to add up), faster to exand.grid

	possible.combinations <- unique(possible.combinations) # remove duplicate rows, duplicates because of the 0s
	#maty ##debug

	#get the index of the minima
	diff <- abs(target-rowSums(possible.combinations)) #the absolute difference for each combination
	min.index <- which(diff == min(diff)) #index for the minima solution
	#min.index
	X <- possible.combinations[min.index,] ## all solutions for minimal difference between area and time value
	#X ##debug

	selection <- X[which(rowSums(X!=0) == max(rowSums(X!=0)))[1],] #rowSums counts numbers not zero; select the first row that meets the condition of having the fewest zeroes.


	areas.removed <- areas # initialize areas.removed in order to remove from it each time the loop is run
	areas.selected <- vector()
	for (i in 1:length(selection)) { ## remove the selected element for this category for the next iteration of the next category
		areas.removed <- areas.removed[-which(areas.removed==selection[i])[1]] #remove just once and only the first time found. if two same sized areas are part of the same solution both are removed.
		areas.selected[length(areas.selected)+1] <- areas.id[which(areas==selection[i])[1]]
	}
	areas.selected	
}


normzahl.per.minute <- sapply(unique(timeboard$Minute), function(i) timeboard[timeboard$Minute==i,"Normzahl"]) # Normzahl per Minute per category
#length(normzahl.per.minute)## all the minutes
#normzahl.per.minute[[1]][1:length(normzahl.per.minute[[1]])] ##select the first minute


#debug(knappsackn)
for (i in 1:length(normzahl.per.minute)) { #loop since knappsackn function needs one value not a vector like in lapply
	for (a in 1:length(normzahl.per.minute[[i]])) {
		#print(normzahl.per.minute[[i]][a])
		selected.elements <- knappsackn(normzahl.per.minute[[i]][a],areas$size,areas$id)
		#lightup[i,selected.elemenst] <- 1
		print(selected.elements)

		print(paste("Minute: ",timeboard$Minute[i+a-1]," - Normzahl der ",a,". Kategorie: ",normzahl.per.minute[[i]][a]," ausgewähltes Element: ",selected.elements,sep=""))
	}
}
#timeboard$Normzahl

afsasf


















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

## convert time to minutes
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


############### Analysis per minute ################

leistungen <- leistungen[which(leistungen$begin != leistungen$end),] # remove entries where start time equals end time, they ruin everything
#leistungen <- leistungen[which(leistungen$begin >= leistungen$end),]##debug

leistungen$diff <- ifelse(leistungen$begin <= leistungen$end, ## difference between start and end. if the task extends midnight, we have to deal with it separately
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
sum.timeboard <- dcast(leistungen, Minute~"Summe") # sum per minute, needed for normalisation. beware: leistungen only contains values with categories. 
tätboard <- dcast(leistungen,Kategorie~"Anzahl")
#write.csv(tätboard,"tätboard.csv") ##debug


#################### Normalization to sum of each minute ######################
sum.timeboard$normalize.factor <- 100/sum.timeboard$Summe #Normalizing the sum to 100
timeboard <- merge(timeboard,sum.timeboard,by.x="Minute",by.y="Minute",all.x=TRUE) #merge in order to normalize
timeboard$Normzahl <- timeboard$Anzahl * timeboard$normalize.factor

write.csv(timeboard,"timeboard.csv")
asfasfaf

####### Graphical Output ###############
# the whole day
#jpeg(paste("/home/stefan/Desktop/export/agg/day.jpg"), width=800, height=800)
#plot(sum.timeboard$Minute/60,sum.timeboard$Summe) ##debug
#dev.off()


#for (x in levels(timeboard$Kategorie)) {
#	jpeg(paste("/home/stefan/Desktop/export/agg/",x,".jpg"), width=800, height=800)
#	plot(timeboard$Minute[timeboard$Kategorie==x]/60,timeboard$Normzahl[timeboard$Kategorie==x], main=x, ylim=c(0,40)) ##debug
#	dev.off()
#}

#for (i in 1:1439) {
#	jpeg(paste("/home/stefan/Desktop/export/",i,".jpg"), width=800, height=800)
#	plot(timeboard[timeboard$Minute==i,"Kategorie"],timeboard[timeboard$Minute==i, "Normzahl"],main=i, ylim=c(0,40))
#	dev.off()
#}


############ Check for best match #################
# combinationen rechen pro kategorie (ohne Wiederholung, mit 0 der anderen)
# optimieren der differenz
# flächen aus dem pool für zukünftige kategorien rausschmeißen

#hypothese: die lösung mit der niedrigsten anzahl an flächen liefert die optimale fläche
# generell muss man isch entscheiden, auf was man optimiert: auf die gesamte fläche oder die einzelnen matches. 

target <- timeboard[timeboard$Minute==550 & timeboard$Kategorie=="Consulting","Normzahl"] 
#target <- 30

sepp <- c(50,30,18,12,32,44,2,6,9,45,32,36,19,23,27,29,15,11,17,39,28,23,11,19,1,3,8)
length(sepp)

#combinations
sepp <- append(sepp,rep(0, length(sepp))) #add zero so it repeats while the other stuff is just coming up once. number of zeros is the same of the numbers of areas 
#gtools::combinations(length(sepp), r=2L, v=sepp, repeats.allowed=TRUE)
maty <- gtools::combinations(length(sepp), 4, v=sepp, set=FALSE, repeats.allowed=FALSE) ## 4 is the size of the vector
maty <- unique(maty) # remove duplicate rows, duplicates because of the 0s
#maty ##debug

#get the index of the minima
diff <- abs(target-rowSums(maty)) #the absolute difference for each combination
min.index <- which(diff == min(diff)) #index for the minima solution
#min.index
X <- maty[min.index,] ##selected solutions for min
X
#sepp <- which(X!=0,arr.ind = T) ## which cells fit the condition, result is a matrix
#sepp
#X[sepp]
#X[sepp[1,]]

#X[sepp[2,]]
