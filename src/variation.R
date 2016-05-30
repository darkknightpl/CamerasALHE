modify <- function()
{
  #TODO
}

variation <- function(selectedPoint, chamber)
{
  newPoint <- 0
  index <- sample(1:length(selectedPoint), 1)
  selectedElement <- selectedPoint[[index]]
  if(is.na(selectedElement))
  {
    print("NA")
    #select random rectangle from room
    rectIndex <- sample(1:length(chamber), 1)
    rectangle <- chamber[[rectIndex]]
    print("Rect:")
    print(rectIndex)
    #create new camera at random point
    x <- runif(1, rectangle[[1]][[1]], rectangle[[2]][[1]])
    y <- runif(1, rectangle[[1]][[2]], rectangle[[2]][[2]])
    point <- c(x,y)
    print("New camera at point:")
    print(point)
    selectedPoint[[index]] <- point
  }
  else
  {
    decision <- sample(1:2, 1)
    if(decision == 1)
    {
      print("modify")
      #modify
      modify()
    }
    else
    {
      print("remove")
      #remove
      selectedPoint[[index]] <- NA
    }
  }
  return(newPoint)
}

testVariation <- function()
{
  rectangle <- list()
  rectangle[[length(rectangle)+1]] <- c(0,0)
  rectangle[[length(rectangle)+1]] <- c(4,2)
  
  rectangle2 <- list()
  rectangle2[[length(rectangle2)+1]] <- c(2,2)
  rectangle2[[length(rectangle2)+1]] <- c(6,4)
  
  chamber <- list()
  chamber[[length(chamber)+1]] <- rectangle
  chamber[[length(chamber)+1]] <- rectangle2
  
  selectedPoint <- list()
  selectedPoint[[length(selectedPoint)+1]] <- c(0,0)
  selectedPoint[[length(selectedPoint)+1]] <- c(0,2)
  selectedPoint[[length(selectedPoint)+1]] <- NA
  selectedPoint[[length(selectedPoint)+1]] <- NA
  selectedPoint[[length(selectedPoint)+1]] <- NA
  
  variation(selectedPoint, chamber)
  
}

testVariation()