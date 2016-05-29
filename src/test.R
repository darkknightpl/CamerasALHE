getDummyChamber<-function()
{
  chamber <- list()
  rect <- list(c(0,0), c(2,2))
  chamber[[length(chamber)+1]] <- rect
  rect <- list(c(2,1), c(4,8))
  chamber[[length(chamber)+1]] <- rect
  return(chamber)
}


SimulatedAnnealling<- function(startPoint, chamber, A, B, radius)
{
  #inicu=jujemy historie jednym punktem poczatkowym

  #pytanier czy chcemy inicjowac jednym czy wieloma,
  # wydaje mis sie ze 1 

  history <- initHistory(startPoints)
  # wywolanie funkcji oceniajacej wartosci punktow
  # evaluate to funkcja oceniajace 1 punkt
  history <- evaluatePoints(history, evaluate)
  # inicjujemy model, bedzie w nim zapamietany indeks
  #najlepszegp punktu w historii
  model <- initModel(history)
  #glowna petla dopoki warunek nie jest spelniony
  while(!conditionFulfilled(model, history))
  {
    # zwraca po prosatu punkt z histori ktorego 
    # indeks ma zapamietany
    selectedPoint <- selection(history, model)
    # generujemy jednego losowego sasiada
    newPoint <- variation(selectedPoint)
    newPoint <- evaluate(newPoint)
    history <- pushBack(history, newPoint)
    model <- updateModel(model, history)
  }
}


initHistory<-function(startSolution)
{
  return(list(startSolution))
}

dist <- function(x1, x2)
{
  sqrt(sum((x1 - x2) ^ 2))
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
  #missRatio represents percentage of surface uncovered by cameras view
  missRatio <- 1 - hitRatio
  return(missRatio*field)
}

evaluate <- function(circleList, chamber, radius, A, B)
{
  cameras<-0
  for(circle in circleList)
  {
    if(length(circle) != 1)
      cameras<- cameras + 1
  }
  noCovered <- 0
  #print(cameras)
  for(rectangle in chamber)
  {
    rectNoCovered <- doSample(100, rectangle, circleList, radius)
    noCovered <- noCovered + rectNoCovered
  }
  #print(noCovered)

  quality <- A * cameras + B * noCovered
  #print(quality)
  return(quality)
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



radius <- 1
chamber <- getDummyChamber()

maxCameras<- calculateMaxCameras(chamber,radius)
minCameras<- calculateMinCameras(chamber, radius)

solution<- generateStartPoint(chamber, maxCameras, minCameras)
#print(solution)


# plot(c(0,0), c(2,2), type= "n", xlab = "", ylab = "")
# draw.circle(1,1,0.5,border="purple",lty=1,lwd=1)

require(plotrix)

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
  plot(c(minX, maxX), c(minY, maxY), type= "n", xlab = "", ylab = "")
  for(rectangle in chamber)
    rect(rectangle[[1]][[1]], rectangle[[1]][[2]], rectangle[[2]][[1]], rectangle[[2]][[2]])
  for(circle in solution)
  {
    #print(circle)
    if(length(circle) == 1)
      next
    draw.circle(circle[[1]],circle[[2]],radius,border="green",lty=1,lwd=1)
  }
}


radius<- 1
chamber <- getDummyChamber()
maxCameras<-calculateMaxCameras(chamber, radius)
minCameras<-calculateMinCameras(chamber, radius)
solution<- generateStartPoint(chamber, maxCameras, minCameras)
print(evaluate(solution[[1]], chamber, radius, 5,7))

#print(solution[[1]])

#solution[["quality"]] <- -1
#print(solution[[1]])
#solution[[length(solution)+1]]<- 666

#for(a in solution)
#  print(a)

a <- c(1,2)
b <- c(4,5)

print(a-b)