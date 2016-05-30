require(plotrix)
require(logging)


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


drawChamber<-function(chamber)
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


calculateMaxCameras<-function(chamber, radius)
{
  totalCameras<-0
  for (rectangle in chamber)
  {
    xLen <-rectangle[[2]][[1]] - rectangle[[1]][[1]]
    yLen <-rectangle[[2]][[2]] - rectangle[[1]][[2]]
    cameras<-(ceiling(xLen/radius)+1)*(ceiling(yLen/radius)+1)
    totalCameras<-totalCameras + cameras
  }
  return(totalCameras)
}


calculateMinCameras<-function(chamber, radius)
{
  totalCameras <- 0
  for (rectangle in chamber)
  {
    xLen <-rectangle[[2]][[1]] - rectangle[[1]][[1]]
    yLen <-rectangle[[2]][[2]] - rectangle[[1]][[2]]
    recArea<-xLen*yLen
    cameraArea<-pi*radius*radius
    cameras <- ceiling(recArea/cameraArea)
    totalCameras <- totalCameras + cameras
  }
  return(totalCameras)
}


generateStartPoint<-function(chamber, maxCameras, minCameras)
{
  camerasList<-list()
  for(i in 1:maxCameras)
    camerasList[[i]]<-NA
  totalArea<-0
  for(rectangle in chamber)
  {
    xLen <-rectangle[[2]][[1]] - rectangle[[1]][[1]]
    yLen <-rectangle[[2]][[2]] - rectangle[[1]][[2]]
    recArea<-xLen*yLen
    totalArea<-totalArea + recArea
  }
  idx<-1
  for(rectangle in chamber)
  {
    xLen <-rectangle[[2]][[1]] - rectangle[[1]][[1]]
    yLen <-rectangle[[2]][[2]] - rectangle[[1]][[2]]
    recArea<-xLen*yLen
    rectCameras<-ceiling((recArea/totalArea)*minCameras)
    for(i in 1:rectCameras)
    {
      camX<-runif(1, rectangle[[1]][[1]], rectangle[[2]][[1]])
      camY<- runif(1, rectangle[[1]][[2]], rectangle[[2]][[2]])
      camerasList[[idx]]<-c(camX, camY)
      idx<-idx + 1
    }
  }
  return(list(camerasList, "quality"=-1))
}


getDummyChamber<-function()
{
  chamber <- list()
  rect <- list(c(1,2), c(9,8))
  chamber[[length(chamber)+1]] <- rect
  return(chamber)
}


SimulatedAnnealling<-function(startSolution, chamber, radius, A, B, initTemp, maxIter)
{
  logReset()
  basicConfig()
  addHandler(writeToFile, file="cameraslog.log", level='INFO')
  with(getLogger(), names(handlers))  
  history<-initHistory(startSolution)
  history[[1]][["quality"]]<-evaluate(history[[1]], chamber, radius, A, B)
  model <- initModel(history, initTemp)
  while(!conditionFulfilled(model, maxIter))
  {
    selectedPoint <- selection(history, model)
    loginfo('quality %f point %s', selectedPoint[["quality"]], selectedPoint[[1]])
    newPoint<- variation(selectedPoint, chamber, radius)
    newPoint[["quality"]]<-evaluate(newPoint, chamber, radius, A, B)
    history <- pushBack(history, newPoint)
    model <- updateModel(model, history)
  }
  return(selection(history, model))
}


initHistory<-function(startSolution)
{
  return(list(startSolution))
}


evaluate<-function(solution, chamber, radius, A, B)
{
  cameras <- sum(!is.na(solution[[1]]))
  noCovered <- 0
  for(rectangle in chamber)
  {
    rectNoCovered <- doSample(100, rectangle, solution[[1]], radius)
    noCovered <- noCovered + rectNoCovered
  }
  quality <- A * cameras + B * noCovered
  return(quality)
}


dist<-function(x1, x2)
{
  return(sqrt(sum((x1 - x2) ^ 2)))
}


doSample<-function(numberOfSamplesPerUnit, rectangle, cameras, radius)
{
  lenX <-rectangle[[2]][[1]] - rectangle[[1]][[1]]
  lenY <-rectangle[[2]][[2]] - rectangle[[1]][[2]]
  field <- lenX * lenY
  totalNumberOfSamples <- ceiling(numberOfSamplesPerUnit * field)
  onTarget <- 0
  for(i in 1:totalNumberOfSamples)
  {
    x <- runif(1, rectangle[[1]][[1]], rectangle[[2]][[1]])
    y <- runif(1, rectangle[[1]][[2]], rectangle[[2]][[2]])
    point <- c(x,y)
    for(j in 1:length(cameras))
    {
      if(is.na(cameras[[j]]))
        next
      distance <- dist(point,cameras[[j]])
      if(distance < radius)
      {
        onTarget <- onTarget + 1
        break;
      }
    }
  }
  hitRatio <- onTarget / totalNumberOfSamples
  missRatio <- 1 - hitRatio
  return(missRatio*field)
}


initModel<-function(history, initTemp)
{
  return(list("best"=1,"iter"=1,"initTemp"=initTemp))
}


selection<-function(history, model)
{
  return(history[[model[["best"]]]])
}


conditionFulfilled<-function(model, maxIter)
{
  if(model["iter"] > maxIter)
    return(TRUE)
  return(FALSE)
}


updateModel<-function(model, history)
{
  model[["iter"]]<- model[["iter"]] + 1
  bestQuality<- history[[model[["best"]]]][["quality"]]
  if(bestQuality > history[[length(history)]][["quality"]])
  {
    model[["best"]]<- length(history)
    return(model)
  }
  temp <- model[["initTemp"]] / model[["iter"]]
  pa <- exp(-(history[[length(history)]][["quality"]]-bestQuality)/temp)
  if(runif(1,0,1) < pa)
    model[["best"]]<- length(history)
  return(model)
}


pushBack <- function(history, newPoint)
{
  history[[length(history)+1]] <- newPoint
  return(history)
}


normVariation<-function(point, radius, chamber)
{
  repeat
  {
    newX<- point[[1]] + rnorm(1)*radius
    newY<- point[[2]] + rnorm(1)*radius
    insideRect<-FALSE
    for(rectangle in chamber)
    {
      if(rectangle[[1]][[1]] <= newX && newX <= rectangle[[2]][[1]]
        && rectangle[[1]][[2]] <= newY && newY <= rectangle[[2]][[2]])
      {
        insideRect<-TRUE
        break
      }
    }
    if(insideRect)
      break
  }
  point[[1]]<-newX
  point[[2]]<-newY
  return(point)
}


variation<-function(selectedPoint, chamber, radius)
{
  index <- sample(1:length(selectedPoint), 1)
  selectedElement <- selectedPoint[[1]][[index]]
  if(is.na(selectedElement))
  {
    rectIndex <- sample(1:length(chamber), 1)
    rectangle <- chamber[[rectIndex]]
    x <- runif(1, rectangle[[1]][[1]], rectangle[[2]][[1]])
    y <- runif(1, rectangle[[1]][[2]], rectangle[[2]][[2]])
    point <- c(x,y)
    selectedPoint[[1]][[index]] <- point
  }
  else
  {
    decision <- sample(1:2, 1)
    if(decision == 1)
      selectedPoint[[1]][[index]] <- normVariation(selectedPoint[[1]][[index]], radius, chamber)
    else
      selectedPoint[[1]][[index]] <- NA
  }
  return(selectedPoint)
}


drawSolution<-function(chamber, solution, radius)
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
  deltaX <- maxX - minX
  deltaY <- maxY - minY
  if(deltaY > deltaX)
    lenMax <- deltaY
  else
    lenMax <- deltaX
  plot(minX:(minX+lenMax), minY:(minY+lenMax), type= "n", xlab = "", ylab = "")
  for(rectangle in chamber)
    rect(rectangle[[1]][[1]], rectangle[[1]][[2]], rectangle[[2]][[1]], rectangle[[2]][[2]])
  for(circle in solution[[1]])
  {
    if(length(circle) == 1)
      next
    draw.circle(circle[[1]],circle[[2]],radius,border="green",lty=1,lwd=1)
  }
}


run<-function(radius, A, B, initTemp, rectNumber, maxLengh, minLength, maxIter)
{
  chamber<-generateRandomChamber(maxLengh, minLength, rectNumber)
  maxCameras<-calculateMaxCameras(chamber, radius)
  minCameras<-calculateMinCameras(chamber, radius)
  solution<-generateStartPoint(chamber, maxCameras, minCameras)
  bestSolution<-SimulatedAnnealling(solution, chamber, radius, A, B, initTemp, maxIter)
  drawSolution(chamber, solution, radius)
}

run(1, 0.5, 0.5, 10, 2, 5, 2, 1000)