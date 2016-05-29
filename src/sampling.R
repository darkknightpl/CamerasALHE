dist <- function(x1, x2)
{
  sqrt(sum((x1 - x2) ^ 2))
}

doSample <- function(numberOfSamplesPerUnit, rectangle, cameras, radius)
{
  
  lenX<-abs(rectangle[[1]][[1]] - rectangle[[2]][[1]])
  lenY<-abs(rectangle[[1]][[2]] - rectangle[[2]][[2]])
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
  return(missRatio)
}

testDoSample <- function()
{
  rectangle <- list()
  rectangle[[length(rectangle)+1]] <- c(0,0)
  rectangle[[length(rectangle)+1]] <- c(4,2)
  cameras <- list()
  cameras[[length(cameras)+1]] <- c(0,0)
  cameras[[length(cameras)+1]] <- c(0,2)
  radius <- 1
  print(doSample(1000,rectangle,cameras,radius)) 
}

testDoSample()