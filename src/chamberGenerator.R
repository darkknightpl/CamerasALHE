checkIntersect<-function(horizontalList, verticalList)
{
	for(horizontal in horizontalList)
	{
		for(vertical in verticalList)
		{
			if(horizontal[[1]][[1]] < vertical[[1]][[1]] 
				&& vertical[[1]][[1]] < horizontal[[2]][[1]]
				&& vertical[[1]][[2]] < horizontal[[1]][[2]] 
				&& horizontal[[1]][[2]] < vertical[[2]][[2]])
				return(TRUE)
		}
	}
	return(FALSE)
}


getHorizontalVertical<-function(firstPoint, secondPoint)
{
	firstHor <- preprocessPoints(firstPoint, c(secondPoint[[1]], firstPoint[[2]]))
	secHor <- preprocessPoints(secondPoint, c(firstPoint[[1]], secondPoint[[2]]))
	firstVer <- preprocessPoints(firstPoint, c(firstPoint[[1]], secondPoint[[2]]))
	secVer <- preprocessPoints(secondPoint, c(secondPoint[[1]], firstPoint[[2]]))
	return(list(list(firstHor, secHor), list(firstVer, secVer)))
}


preprocessPoints<-function(firstPoint, secondPoint)
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
	return(list(c(minX, minY), c(maxX, maxY)))
}


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
	rectangle<-preprocessPoints(rectangle[[1]], rectangle[[2]])
	chamber[[1]]<-rectangle
	if(rects == 1)
		return(chamber)
	horVer <- getHorizontalVertical(rectangle[[1]], rectangle[[2]])
	horizontalList<-c(horVer[[1]])
	verticalList<-c(horVer[[2]])
	for(i in 1:(rects-1))
	{
		randNmb<-sample(1:2,1)
		selectedIdxCoord<-(randNmb%%2)+1
		toSelectIdxCoord <- ((randNmb+1)%%2)+1
		selectedCoord<- chamber[[length(chamber)]][[2]][[selectedIdxCoord]]
		toSelectBegin<- chamber[[length(chamber)]][[1]][[toSelectIdxCoord]]
		toSelectEnd<- chamber[[length(chamber)]][[2]][[toSelectIdxCoord]]
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
			if(chamber[[length(chamber)]][[1]][[1]] <= nextX 
				&& nextX <= chamber[[length(chamber)]][[2]][[1]]
				&& chamber[[length(chamber)]][[1]][[2]] <= nextY 
				&& nextY <= chamber[[length(chamber)]][[2]][[2]])
				next
			rectangle <- list(firstPoint, c(nextX, nextY))
			rectangle <- preprocessPoints(rectangle[[1]], rectangle[[2]])
			rectHorVer<- getHorizontalVertical(rectangle[[1]], rectangle[[2]])
			if(!checkIntersect(horizontalList, rectHorVer[[2]]) && 
				!checkIntersect(rectHorVer[[1]], verticalList))
				break
		}
		horizontalList <- c(horizontalList, rectHorVer[[1]])
		verticalList<- c(verticalList, rectHorVer[[2]])
		chamber[[length(chamber)+1]] <- rectangle
	}
	return(chamber)
}


drawChamber<- function(chamber)
{
	minX <- chamber[[1]][[1]][[1]]
	maxX <- chamber[[1]][[2]][[1]]
	minY <- chamber[[1]][[1]][[2]]
	maxY <- chamber[[1]][[2]][[2]]
	for(rectangle in chamber)
	{
		if(rectangle[[1]][[1]] < minX)
			minX <- rectangle[[1]][[1]]
		if(rectangle[[2]][[1]] > maxX)
			maxX <- rectangle[[2]][[1]]
		if(rectangle[[1]][[2]] < minY)
			minY <- rectangle[[1]][[2]]
		if(rectangle[[2]][[2]] > maxY)
			maxY <- rectangle[[2]][[2]]
	}
	plot(c(minX, maxX), c(minY, maxY), type= "n", xlab = "", ylab = "")
	for(rectangle in chamber)
		rect(rectangle[[1]][[1]], rectangle[[1]][[2]], rectangle[[2]][[1]], rectangle[[2]][[2]])
}


chamber <- generateRandomChamber(10, 7, 5)
drawChamber(chamber)