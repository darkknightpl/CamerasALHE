print("Hello World")



# Funkcja ktora powinna generowac pomieszczenie
generateRandomChamber <- function()
{
	rectangle <- list()
	rectangle[[length(rectangle)+1]] <- sample(1:10,2)
	rectangle[[length(rectangle)+1]] <- sample(1:10,2)
	names <- c("first", "second")
	names(rectangle) <- names
	
	chamber <- list()
	chamber[[length(chamber)+1]] <- rectangle
	return(chamber)
}

# Funkcja tworzaca pomieszczenie zadane przez uzytkownika
generateUserChamber <- function()
{
	chamber <- list()
	rect <- list()
	rect[[length(rect)+1]] <- c(1,2)
	rect[[length(rect)+1]] <- c(9,8)
	chamber[[length(chamber)+1]] <- rect
	return(chamber)
}


# Funkcja obliczajaca dlugosc wektora
calculateMaxLenght <- function(chamber, radius)
{
	cameras <- 0
	for (rect in chamber)
	{
		xLen <- abs(rect[[1]][[1]] - rect[[2]][[1]])
		yLen <- abs(rect[[1]][[2]] - rect[[2]][[2]])

		singleRec <- ceiling(xLen/radius) * ceiling(yLen/radius)
		cameras <- cameras + singleRec
	}
	return(cameras)
}

# Funkcja obliczajaca minimalna liczbe kamer
calculateMinLength <- function(chamber, radius)
{
	cameras <- 0
	for (rect in chamber)
	{
		xLen <- abs(rect[[1]][[1]] - rect[[2]][[1]])
		yLen <- abs(rect[[1]][[2]] - rect[[2]][[2]])

		singleRec <- ceiling((xLen * yLen)/radius)
		cameras <- cameras + singleRec
	}
}


generateStartPoint <- function(maxCameras, minCameras)
{
	solution <- list()
	# wektor o dlugosci maxCameras samych NA
	for (i in 1: maxCameras)
	{
		solution[[i]] = NA
	}

	for (i in 1: minCameras)
	{
		# tu bedzie cos konkretnego, jakis punktu ktory 
		# nalezy do pomieszczenia
		solution[[i]] <- c(1,2)
	}
}


SimulatedAnnealling() <- function()
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


updateModel <- function(model, history)
{
	newModel <- 0
	# tu ogolnie tak
	# 1) uaktualniamy numer iteracji
	# 2) n podstawie tego obliczamy temperatre dla kolejnej iteracji
	# 3) sprawdzamy czy ostatni punkt z historii jest lepszy od besta,
	# jak tak to podmieniamy, jak nie to niedeterminizm wynikajacy z temperatury
	return(newModel)
}


pushBack <- function(history, newPoint)
{
	history[[length(history)+1]] <- newPoint
	return(history)
}



variation <- function(selectedPoint)
{
	newPoint <- 0
	# tu musimy jakos losowo wygenerowac punkt,
	# mozemy np
	# 1) na poczatku losujemy co chcemy robic
	# dodawac, odejmowac, modyfikowac
	# ) potem losujemy popzycje na ktorej to robimy
	# 2) najpierw losujemy pozycje do zmiany
	# potem jak zmieniamy (przy czym gdy jest zawsze mamy 2 opcje)
	# NA -> mozemy tylko dodac
	# konkretny -> mozemy usunac lub zmodyfikowac
	return(newPoint)
}


initModel <- function(history)
{
	# w modelu trzymamy:
	# 1) najlepszy punkt
	# 2) numer iteracji
	# 3) aktualna temperatore
	return(0)
}


conditionFulfilled <- function(model, history)
{
	# trzeba wymyslic jaki warunek,
	# najprosciej po jakiejs liczbie iteracji lub
	# gy dzialanie nie przynosi efektu
}


selection <- function(history, model)
{
	selectedPoint <- history[model$best]
	return(selectedPoint)
}


evaluatePoints <- function(points, evaluate, chamber)
{
	for(idx in 1:length(points))
	{
		points[[idx]]$quality <- evaluate(points[[idx]], chamber)
	}
	return(points)
}

evaluate <- function(point, chamber)
{
	coverage <- doSample(point, chamber)
	cameras <- sum(!is.na(points))

	quality <- A * cameras + B * coverate
	return(quality)
}

doSample <- function(point, chamber)
{
	success <- 0
	failed <- 0
	# tu musimy zaimplementowac probkowanie
	# czyli
	# 1) losujemy jakis punkt(x,y) ktory lezy w obrebie pomieszczenia
	# 2) przegladamy nasz punkt(wektor), sprawdzamy czy istnieje
    # w nim taki punkt, ktorego odleglosc od wylosowanego punktu jest 
    # mniejsza rowna promien, jak jest to success += 1, jak nie to failure += 1
    result <- success/(success + failure)
    return(result)
}

# chyba po prostu tyle
initHistory <- function(startPoints)
{
	return(startPoints)
}

