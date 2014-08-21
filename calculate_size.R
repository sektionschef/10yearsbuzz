#!/usr/bin/r

require(XML)

file <- "canvas.svg"
areas <- data.frame(id = c("e1","e2","e3"))

xml.data <- xmlInternalTreeParse(file) #parse the svg

## Function to get coordinates of vetices from svg for each element
get.polyordinates <- function(xml,id) {
	#xpathSApply(xml.data, "//*[@id='path2985']",xmlValue) ## get a value
	polyordinates <- xpathSApply(xml, paste("//*[@id='",eval(id),"']",sep=""),xmlGetAttr, "d") # get the path's coordinates
	polyordinates <- strsplit(polyordinates, " ") ## split for the coordinates
	polyordinates <- polyordinates[[1]][2:length(polyordinates[[1]])] ## remove the "m"
	polyordinates <- strsplit(as.character(polyordinates), ",") ## remove the comma between x and y
	polyordinates <- matrix(as.numeric(unlist(polyordinates)), ncol=2, nrow=length(polyordinates),byrow=TRUE) # save as matrix
	polyordinates
}
#polyordinates <- get.polyordinates(xml.data,ids[1])


## calculate the area of polygon
calculate.sum <- function(polyordinates) {
	sum.x <- polyordinates[nrow(polyordinates),1]*polyordinates[1,2] #last row with first row
	sum.y <- polyordinates[nrow(polyordinates),2]*polyordinates[1,1]

	for (i in 1:(nrow(polyordinates)-1)) #
	{ # not until the last row, because there is no next row
		#print(i) ##debug
		sum.x <- sum.x + polyordinates[i,1]*polyordinates[i+1,2] #x value with y 
		sum.y <- sum.y + polyordinates[i,2]*polyordinates[i+1,1]
	} 
	sum <- abs((sum.x - sum.y)/2)
	sum
}


polyordinates <- sapply(areas$id, function(i) get.polyordinates(xml.data,i))
areas$size <- sapply(areas$id, function(i) calculate.sum(polyordinates[[i]]))
#areas$size

#polyordinates[[2]]
#plot(polyordinates[[2]]) ## polygon is upside down


normalize <- function(sizes) { #normalize to 100%
	normalize.factor <- 100/sum(sizes)
	normalized <- normalize.factor * sizes
	normalized
}

areas$normalized <- normalize(areas$size)
#areas


write.csv(areas, "areas.csv")
