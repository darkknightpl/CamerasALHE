
generateRandomChamber<-function(lengthMax, lengthMin, rects)
{
	if(rects < 0)
		stop()
	chamber<-list()
	rectangle<-list()
	rectangle[[1]]<-c(0.0, 0.0)
	repeat
	{
		rectangle[[2]]<-runif(2, -lengthMax, lengthMax)
		lenX<-abs(rectangle[[1]][[1]]-rectangle[[2]][[1]])
		lenY<-abs(rectangle[[1]][[2]]-rectangle[[2]][[2]])
		if(lenX >= lengthMin && lenY >= lengthMin)
			break
	}
	chamber[[1]]<-rectangle
	if(rects == 1)
		return(chamber)
	horizontalList <- list()
	verticalList <- list()
	horVer <- getHorizontalVertical(rectangle[[1]], rectangle[[2]])
	horizontalList[[1]] <- horVer[[1]]
	horizontalList[[2]] <- horVer[[2]]
	verticalList[[1]] <- horVer[[3]]
	verticalList[[2]] <- horVer[[4]]
	for(i in 1:(rects-1))
	{
		minMax <- getMinMax(chamber[[length(chamber)]][[1]], chamber[[length(chamber)]][[2]])

		randNmb<-sample(1:10, 1)
		randNmb <- 2
		selectedIdxCoord<-(randNmb%%2)+1
		toSelectIdxCoord <- ((randNmb+1)%%2)+1
		selectedCoord <- minMax[[selectedIdxCoord*2]]
		toSelectBegin <- minMax[[(toSelectIdxCoord*2)-1]]
		toSelectEnd <- minMax[[toSelectIdxCoord*2]]
		repeat
		{
			selected<-runif(1, toSelectBegin, toSelectEnd)
			lenSBeginS<-selected - toSelectBegin
			lenSEndS<-toSelectEnd - selected
			if(lenSBeginS >= 0.25*lengthMin && lenSEndS >= 0.25*lengthMin)
				break
		}
		firstPoint<-c()
		firstPoint[[selectedIdxCoord]]<-selectedCoord
		firstPoint[[toSelectIdxCoord]]<-selected
		repeat
		{
			repeat
			{
				if(selectedIdxCoord == 1)
					nextX <- runif(1, firstPoint[[1]], firstPoint[[1]]+lengthMax)
				else
					nextX <- runif(1, firstPoint[[1]]-lengthMax, firstPoint[[1]]+lengthMax)
				lenX <- abs(nextX - firstPoint[[1]])
				if(lenX >= lengthMin)
					break
			}
			repeat
			{
				if(selectedIdxCoord == 2)
					nextY <- runif(1, firstPoint[[2]], firstPoint[[2]]+lengthMax)
				else
					nextY <- runif(1, firstPoint[[2]]-lengthMax, firstPoint[[2]]+lengthMax)
				lenY <- abs(nextY - firstPoint[[2]])
				if(lenY >= lengthMin)
					break
			}
			if(minMax[[1]] <= nextX && nextX <= minMax[[2]]
				&& minMax[[3]] <= nextY && nextY <= minMax[[4]])
				next
			rectangle <- list(firstPoint, c(nextX, nextY))
			rectHorVer<- getHorizontalVertical(rectangle[[1]], rectangle[[2]])
			rectHorizontal <- list(rectHorVer[[1]], rectHorVer[[2]])
			rectVertical <- list(rectHorVer[[3]], rectHorVer[[4]])
			if(!checkIntersect(horizontalList, rectVertical) && 
				!checkIntersect(rectHorizontal, verticalList))
				break
		}
		horizontalList[[length(horizontalList)+1]] <- rectHorizontal[[1]]
		horizontalList[[length(horizontalList)+1]] <- rectHorizontal[[2]]
		verticalList[[length(verticalList)+1]] <- rectVertical[[1]]
		verticalList[[length(verticalList)+1]] <- rectVertical[[2]]
		chamber[[length(chamber)+1]] <- rectangle
	}
	return(chamber)
}


checkIntersect <- function(horizontalList, verticalList)
{
	for(horizontal in horizontalList)
	{
		horizontalRanges <- getMinMax(horizontal[[1]], horizontal[[2]])
		constY <- horizontalRanges[[3]]
		for(vertical in verticalList)
		{
			verticalRanges <- getMinMax(vertical[[1]], vertical[[2]])
			constX <- verticalRanges[[1]]
			if(horizontalRanges[[1]] < constX && constX < horizontalRanges[[2]]
				&& verticalRanges[[3]] < constY && constY < verticalRanges[[4]])
				return(TRUE)
		}
	}
	return(FALSE)
}


getHorizontalVertical<-function(firstPoint, secondPoint)
{
	firstHor <- list(firstPoint, c(secondPoint[[1]], firstPoint[[2]]))
	secHor <- list(secondPoint, c(firstPoint[[1]], secondPoint[[2]]))
	firstVer <- list(firstPoint, c(firstPoint[[1]], secondPoint[[2]]))
	secVer <- list(secondPoint, c(secondPoint[[1]], firstPoint[[2]]))
	horVer <- list(firstHor, secHor, firstVer, secVer)
	return(horVer)
}

getMinMax<-function(firstPoint, secondPoint)
{
	if(firstPoint[[1]] < secondPoint[[1]])
	{
		minX<- firstPoint[[1]]
		maxX<- secondPoint[[1]]
	}
	else
	{
		minX <- secondPoint[[1]]
		maxX <- firstPoint[[1]]
	}
	if(firstPoint[[2]] < secondPoint[[2]])
	{
		minY <- firstPoint[[2]]
		maxY <- secondPoint[[2]]
	}
	else
	{
		minY <- secondPoint[[2]]
		maxY <- firstPoint[[2]]
	}
	minMax <- c(minX, maxX, minY, maxY)
	return(minMax)
}


drawChamber<- function(chamber)
{
	initMinMax<-getMinMax(chamber[[1]][[1]], chamber[[1]][[2]])
	minX <- initMinMax[[1]]
	maxX <- initMinMax[[2]]
	minY <- initMinMax[[3]]
	maxY <- initMinMax[[4]]
	toDraw <- list()
	for(rectangle in chamber)
	{
		minMax <- getMinMax(rectangle[[1]], rectangle[[2]])
		toDraw[[length(toDraw)+1]] <- c(minMax[[1]], minMax[[3]], minMax[[2]], minMax[[4]])
		if(minMax[[1]] < minX)
			minX <- minMax[[1]]
		if(minMax[[2]] > maxX)
			maxX <- minMax[[2]]
		if(minMax[[3]] < minY)
			minY <- minMax[[3]]
		if(minMax[[4]] > maxY)
			maxY <- minMax[[4]]
	}
	plot(c(minX, maxX), c(minY, maxY), type= "n", xlab = "", ylab = "")
	for(rectangle in toDraw)
		rect(rectangle[[1]], rectangle[[2]], rectangle[[3]],rectangle[[4]])
}


chamber <- generateRandomChamber(10, 7, 5)
drawChamber(chamber)