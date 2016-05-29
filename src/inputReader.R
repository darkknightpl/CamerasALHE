readinput <- function()
{
  print("Enter following parameters")
  numberOfCameras <- readline(prompt="Enter number of cameras: ")
  cameraRadius <- readline(prompt="Enter camera radius: ")
  numberOfRectangles <- readline(prompt="Enter number of rectangles: ")
  rectMinLength <- readline(prompt="Enter minimum length of rectangle side: ")
  rectMaxLength <- readline(prompt="Enter maximum length of rectangle side: ")
  constParameterA <- readline(prompt="Enter A parameter value: ")
  constParameterB <- readline(prompt="Enter B parameter value: ")
}

readRectangles <- function()
{
  chamber <- list()
  print("Enter coordinates of rectangles")
  numberOfRectangles <- readline(prompt="Enter number of rectangles: ")
  for(i in 1:numberOfRectangles)
  {
    rectangle <- list()
    x1 <- readline(prompt="First point: Enter x coordinate: ")
    y1 <- readline(prompt="First point: Enter y coordinate: ")
    x2 <- readline(prompt="Second point: Enter x coordinate: ")
    y2 <- readline(prompt="Second point: Enter y coordinate: ")
    rectangle[[length(rectangle)+1]] <- c(x1,y1)
    rectangle[[length(rectangle)+1]] <- c(x2,y2)
    chamber[[length(chamber)+1]] <- rectangle
  }
  return(chamber)
}

testReadRectangles <- function()
{
  print(readRectangles())  
}

testReadRectangles()
