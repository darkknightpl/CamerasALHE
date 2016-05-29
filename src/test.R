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

evaluate<-function(solution, A, B)
{
}

evaluate <- function(solution, chamber)
{
  coverage <- doSample(point, chamber)
  
  for()

  quality <- A * cameras + B * coverate
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
  solution<-list()
  for(i in 1:maxCameras)
    solution[[i]]<-NA
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
      solution[[idx]]<-c(camX, camY)
      idx<-idx + 1
    }
  }
  return(solution)
}



radius <- 1
chamber <- getDummyChamber()

maxCameras<- calculateMaxCameras(chamber,radius)
minCameras<- calculateMinCameras(chamber, radius)

solution<- generateStartPoint(chamber, maxCameras, minCameras)
print(solution)


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
  plot(-1:10,type="n",xlab="",ylab="",main="Test draw.circle")
  for(rectangle in chamber)
    rect(rectangle[[1]][[1]], rectangle[[1]][[2]], rectangle[[2]][[1]], rectangle[[2]][[2]])
  for(circle in solution)
  {
    print(circle)
    if(length(circle) == 1)
      next
    draw.circle(circle[[1]],circle[[2]],radius,border="green",lty=1,lwd=1)
  }
}


# radius<- 1
# chamber <- getDummyChamber()
# maxCameras<-calculateMaxCameras(chamber, radius)
# minCameras<-calculateMinCameras(chamber, radius)
# solution<- generateStartPoint(chamber, maxCameras, minCameras)
# drawSolution(chamber, solution, radius)