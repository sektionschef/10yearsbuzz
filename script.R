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
#write.csv(areas,"areas.csv")
#areas





lightup <- timeboard[,c("Minute","Kategorie")] #where the lights will be placed
lightup[,as.character(areas$id)] <- 0 ## fill it up with ids of areas
#lightup[1:7,]


knappsackn <- function(target,areas.removed) {

	## create all possible combinations
	number.areas.for.combinations <- 4
	possible.combinations <- append(areas.removed$size,rep(0, number.areas.for.combinations)) #add zero so it repeats while the other stuff is just coming up once. number of repeats means how many areas can be selected, making it more and more complex.

	possible.combinations <- gtools::combinations(length(possible.combinations), number.areas.for.combinations, v=possible.combinations, set=FALSE, repeats.allowed=FALSE) ## second parameter is the dimensions (number of elements to add up), faster to exand.grid

	#possible.combinations <- unique(possible.combinations) # takes a lot of time - remove duplicate rows, duplicates because of the 0s
	possible.combinations <- possible.combinations[-which(rowSums(possible.combinations!=0)==0),] ##remove combinations full of zeroes. there must be at least one area.

	
	#get the index of the minima
	diff <- abs(target-rowSums(possible.combinations)) #the absolute difference for each combination
	solutions <- possible.combinations[which(diff == min(diff)),] ## all solutions for minimal difference between area and time value
	#X ##debug

	zero.count <- ifelse(nrow(solutions)>1,apply(solutions,1,function(s) sum(s!=0)),1) # the condition equals 0 or 1, so the sum counts the elements not zero. if there is jsut one row, this one is taken

	selection <- solutions[which(zero.count == max(zero.count))[1],] #select the first row that meets the max.  
	selection <- selection[selection!=0] # remove the zeroes, if there are some.

	for (i in 1:length(selection)) { ## remove the selected element for this category for the next iteration of the next category
		areas.selected[length(areas.selected)+1] <- areas.removed$id[which(areas.removed$size==selection[i])[1]] # save the id of the used area
		areas.removed <- areas.removed[-which(areas.removed$size==selection[i])[1],] #remove just once and only the first time found. if two same sized areas are part of the same solution both are removed.
	}
	list(areas.selected,areas.removed) # return two vectors as list	
}


#debug(knappsackn)
#timeboard[timeboard$Minute==361,]

#for (i in 361:362) { #debug 
for (i in 1:length(unique(timeboard$Minute))) { # per Minute - loop since knappsackn function needs one value not a vector like in lapply
	areas.removed <- areas # initialize areas.removed in order to remove from it each time the loop is run
	areas.selected <- vector() # create new vector for the selected elements
	
	for (a in 1:length(timeboard$Kategorie[timeboard$Minute==i-1])) { # -1 because minute starts at 0
	       	returned.list <- knappsackn(timeboard[timeboard$Minute==i-1,"Normzahl"][[a]],areas.removed)
		selected.elements <- as.vector(returned.list[[1]])
		lightup[lightup$Minute==i-1,][a,selected.elements] <- 1
		areas.removed <- as.vector(returned.list[[2]])
		print(paste("Minute: ",i-1," - Normzahl der ",a,". Kategorie: ",timeboard[timeboard$Minute==i-1,"Normzahl"][[a]]," ausgewähltes Element: ",selected.elements,sep="")) ##debug
	}
}
#timeboard$Normzahl

write.csv(lightup[1:4,],"lightup.csv")
#MInute 360 - Normzahl der 1. Kategorie 9.859.. - in max(zero.count) no non-missing arguments to max; returning -Inf
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
