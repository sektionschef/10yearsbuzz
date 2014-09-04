#!/usr/bin/Rscript

###### LIBRARIES
library(reshape2) # load reshape - bringing holy pivot table functionality to R
library(plyr)


####temporary stuff
timeboard <- read.csv(file="timeboard.csv",head=TRUE) ##debug shortcut
#timeboard[1:6,]
#lightup <- read.csv(file="lightup.csv",head=TRUE) ##debug shortcut
#lightup[1:4,]

#example stuff
#target_example <- 32 ##target for knappsack
#areas <- data.frame(size = c(50,30,18,12,32,44,2,6,9,45,32,36,19,23,27,29,15,11,17,39,28,23,11,19,1,3,8,33,23,1,5,9,24,14,11,9,44,21))
#areas$id <- paste("e",1:length(areas$size),sep="")


number.areas.for.combinations <- 5 ##more for better results - 30^7

category.colors <- data.frame(
		category = c(
		"Consulting",
		"JFX",
		"Red. Betreuung",
		"PJM",
		"Red. Leitung & Content Creation",
		"Konzeption"),
		color = c(
		"FF62FC74",
		"FFFCF262",
		"FF6289FC",
		"FFBF62FC",
		"FFFC8962",
		"FF62FCE3"))


######################### load ############################### 
# Input Zeiten laden
leistungen <- read.csv(file="input/volle_leistung.csv",head=TRUE,sep=";")
names(leistungen) <- sub(" ", ".", names(leistungen)) ## replace spaces in column names

#load sizes of canvas elements
areas <- read.csv(file="calculate size/areas.csv",head=TRUE)
#areas$normalized

# Zuteilung der Tätigkeiten
kategorien <- read.csv(file="input/kategorisierung.csv", head=TRUE,sep=",")
#table(kategorien[,"Kategorie"]) ##debug, Kategorie is factor


### beautiful look at the data
#plot(areas$normalized)
#plot(timeboard$Normzahl)  ## 5-35

#####
################## create areas for the knapsack to be less hungry ############
#####
#create a random categories for all elements
#buzzo <- data.frame("ids"=paste("e",sample(1:133, 133, replace=F),sep="")) # create areas
#buzzo$class <- paste("a",sample(1:30, 133, replace=T),sep="")

#buzzo ##debug
#plot(table(buzzo$class)) ##debug
#write.csv(buzzo, "buzzo.csv", row.names=F) #save in csv


buzzo <- read.csv(file="buzzo.csv")
areas <- merge(areas,buzzo,by.x="ids",by.y="ids")

areas.konsl <- dcast(areas, class~"normalized", sum, value.var="normalized")
areas.konsl$class <- as.character(areas.konsl$class)
#areas.konsl ##debug
#plot(areas.konsl) ## size of the categories


if (FALSE) { ##devotage
###################### format ##########################
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

}


######### create the output format of the solution #########
lightup <- data.frame("Minute" = unique(timeboard$Minute)) #where the lights will be placed
lightup[,as.character(areas$id)] <- 0 ## fill it up with ids of areas
#lightup[1:7,]


########### Knappsack function - allocating areas to categories #################
knappsackn <- function(target,areas.removed) { #finding optimal areas for each category for each minute, maximizing for the best match of individual categories and areas (not the smallest difference for each minute) and maximizing the amount of areas used.

	## create all possible combinations
	possible.combinations <- append(areas.removed$normalized,rep(0, number.areas.for.combinations)) #add zero so it repeats while the other stuff is just coming up once. necessary to be able to choose less than 4 areas. number of areas means how many areas can be selected, making it more and more complex.

	possible.combinations <- gtools::combinations(length(possible.combinations), number.areas.for.combinations, v=possible.combinations, set=FALSE, repeats.allowed=FALSE) ## all combinations for the areas, second parameter is the dimensions (number of elements to add up), faster to exand.grid

	#possible.combinations <- unique(possible.combinations) # takes a lot of time - remove duplicate rows, duplicates because of the added 0s
	possible.combinations <- possible.combinations[-which(rowSums(possible.combinations!=0)==0),] ##remove combinations full of zeroes. there must be at least one area chosen.
	
	#get the index of the minima
	diff <- abs(target-rowSums(possible.combinations)) #the absolute difference for each combination and the target
	solutions <- possible.combinations[which(diff == min(diff)),] ## all solutions for minimal difference between area and time value

	# if there are more than one optimal solutions
	#zero.count <- ifelse(nrow(solutions)>1,apply(solutions,1,function(s) sum(s!=0)),1) # the condition equals 0 or 1, so the sum counts the elements not zero. if there is jsut one row, this one is taken
	#selection <- ifelse(max(zero.count) > 0, solutions[which(zero.count == max(zero.count))[1],], solutions) #select the first row that meets the criteria of having the least zeroes, (min of zeroes)  - if there are no zeroes, take the first solution

	if (is.null(nrow(solutions))) { ## if there is more than one row the first one is chosen - note: ifelse() just gives me the first value
		selection <- solutions
	} else {
		selection <- solutions[1,]
	}

	selection <- selection[selection!=0] # remove the zeroes, if there are some. zero means no area.

	for (i in 1:length(selection)) { ## remove the selected element chosen for the current category so the next iteration can't chose the same area again.
		areas.selected[length(areas.selected)+1] <- areas.removed[areas.removed$normalized==selection[i],"class"][1] # save the class of the used area
		areas.removed <- areas.removed[-which(areas.removed$normalized==selection[i])[1],] #remove just once and only the first time found. if two same sized areas are part of the same solution both are removed.
	}
	list(areas.selected,areas.removed) # return two vectors as list	
}

#debug(knappsackn) ##debug
#timeboard[timeboard$Minute==361,] ##debug

for (i in 1:150) { #debug 
#for (i in 1:length(unique(timeboard$Minute))) { # per Minute - loop since knappsackn function needs one value not a vector like in lapply
	areas.removed <- areas.konsl # initialize the areas. all areas are available for a new minute
	areas.selected <- vector() # create new vector for the selected elements
	
	for (a in 1:length(timeboard$Kategorie[timeboard$Minute==i-1])) { # -1 because minute starts at 0
	       	returned.list <- knappsackn(timeboard[timeboard$Minute==i-1,"Normzahl"][[a]],areas.removed)
		selected.elements <- as.vector(returned.list[[1]]) #the returned selected areas
		#print(selected.elements)
		selected.elements <- as.vector(areas[selected.elements==areas$class,"ids"]) #convert areas to elements
		#lightup[lightup$Minute==i-1,selected.elements][timeboard$Kategorie[a] == category.colors$category,] <- as.character(category.colors$color[a]) # mark the corresponding columns with 1, if area selected
		lightup[lightup$Minute==i-1,selected.elements] <- as.character(category.colors[timeboard$Kategorie[a] == category.colors$category, "color"]) ## new line adding the colors per category for each line- NEW
		areas.removed <- as.vector(returned.list[[2]]) #the remaining areas
		print(paste("Minute: ",i-1," - Normzahl der ",a,". Kategorie: ",timeboard[timeboard$Minute==i-1,"Normzahl"][[a]]," ausgewähltes Element: ",selected.elements,sep="")) ##debug
	}
}


write.csv(lightup,"lightup.csv")


