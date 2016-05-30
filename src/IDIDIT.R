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
  circleList<-list()
  for(i in 1:maxCameras)
    circleList[[i]]<-NA
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
      circleList[[idx]]<-c(camX, camY)
      idx<-idx + 1
    }
  }
  return(list(circleList, "quality"=-1))
}


getDummyChamber<-function()
{
  chamber <- list()
  rect <- list(c(1,2), c(9,8))
  chamber[[length(chamber)+1]] <- rect
  return(chamber)
}



SimulatedAnnealling<-function(startSolution, chamber, radius, A, B)
{
  history<-initHistory(startSolution)
  history[[1]][["quality"]]<-evaluate(history[[1]][[1]], chamber, radius, A, B)
  #print(history[[1]])
  #stop()
  model <- initModel(history)
  while(!conditionFulfilled(model))
  {
    selectedPoint <- selection(history, model)
    print(selectedPoint[["quality"]])
    
    
    #print(selectedPoint)
    #print("Teraz sam 1")
    #print(selectedPoint[[1]])
    #print(selectedPoint[["quality"]])
    newPoint<- variation(selectedPoint[[1]], chamber)
    #print(newPoint)
    newPoint <- list(newPoint, "quality"=evaluate(newPoint, chamber, radius, A, B))
    #print(newPoint)
    #print(newPoint)

    history <- pushBack(history, newPoint)
    model <- updateModel(model, history)
  }
}

initHistory<-function(startSolution)
{
  return(list(startSolution))
}

evaluate2<-function(circleList, chamber, radius, A, B)
{
  cameras <- 0
  for(circle in circleList)
  {
    if(length(circle) != 1)
    {
      cameras <- cameras + 1
      #print("AKUKU:")
      #print(cameras)
    }
    #print(cameras)
  }
  #print(cameras)
  #print("nowy")
  #print(cameras)
  #print(sum(!is.na(circleList)))
}

evaluate<-function(circleList, chamber, radius, A, B)
{

  
  cameras <- sum(!is.na(circleList))
  noCovered <- 0
  for(rectangle in chamber)
  {
    rectNoCovered <- doSample(100, rectangle, circleList, radius)
    noCovered <- noCovered + rectNoCovered
  }
  #print(cameras)
  #print(noCovered)
  quality <- A * cameras + B * noCovered
  return(quality)
}

dist <- function(x1, x2)
{
  return(sqrt(sum((x1 - x2) ^ 2)))
}

doSample <- function(numberOfSamplesPerUnit, rectangle, cameras, radius)
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
      if(length(cameras[[j]]) == 1)
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

initModel <- function(history)
{
  return(list("best"=1,"iter"=1,"temp"=10))
}

selection <- function(history, model)
{
  return(history[[model[["best"]]]])
}

conditionFulfilled<-function(model)
{
  if(model["iter"] > 1000)
    return(TRUE)
  return(FALSE)
}

updateModel <- function(model, history)
{
  model[["iter"]]<- model[["iter"]] + 1
  model[["temp"]]<- model[["temp"]]/model[["iter"]]
  # dorobic sprawdzanie zgodnie z temperatura

  bestQuality<- history[[model[["best"]]]][["quality"]]
  #print(bestQuality)
  #print("Jestem tu")
  x <- history[[length(history)]]
  #print(history)
  #print("Jestem tu")
  if(bestQuality > history[[length(history)]][["quality"]])
    model[["best"]]<- length(history)
  #print("jestem")
  return(model)
}


pushBack <- function(history, newPoint)
{
  # dostaje zly new point!
  #print(newPoint)
  history[[length(history)+1]] <- newPoint
  #print(history)
  return(history)
}


normVariation<-function(point)
{
  #print(point)
  point[[1]]<- (1+rnorm(1))*point[[1]]
  point[[2]]<- (1+rnorm(1))*point[[2]]
  return(point)
}

variation <- function(selectedPoint, chamber)
{
  index <- sample(1:length(selectedPoint), 1)
#print("jestem tu")
  #print(selectedPoint)

  selectedElement <- selectedPoint[[index]]
  #print(selectedPoint)
  #print(index)
  #print(selectedElement)
  #print("teraz tam")
  if(is.na(selectedElement))
  {
    #print("NA")
    #select random rectangle from room
    rectIndex <- sample(1:length(chamber), 1)
    rectangle <- chamber[[rectIndex]]
    #print("Rect:")
    #print(rectIndex)
    #create new camera at random point
    x <- runif(1, rectangle[[1]][[1]], rectangle[[2]][[1]])
    y <- runif(1, rectangle[[1]][[2]], rectangle[[2]][[2]])
    point <- c(x,y)
    #print("New camera at point:")
    #print(point)
    selectedPoint[[index]] <- point
  }
  else
  {
    decision <- sample(1:2, 1)
    if(decision == 1)
    {
      #print("modify")
      #modify
      selectedPoint[[index]] <- normVariation(selectedPoint[[index]])
    }
    else
    {
      #print("remove")
      #remove
      #print(selectedPoint)
      selectedPoint[[index]] <- NA
      #print(selectedPoint)
    }
  }
  #print("KONCZE")
  #print(selectedPoint)
  return(selectedPoint)
}


radius<- 1
chamber <- getDummyChamber()
maxCameras<-calculateMaxCameras(chamber, radius)
minCameras<-calculateMinCameras(chamber, radius)
solution<- generateStartPoint(chamber, maxCameras, minCameras)

SimulatedAnnealling(solution, chamber, radius, 5,6)