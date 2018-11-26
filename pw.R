## Евклидово расстояние
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

##Функции ядра
kernelEP = function(r)
{
  return ((3/4*(1-abs(r)^2)*(abs(r)<=1)))
}

kernelR = function(r)
{
  return ((0.5*abs(r))* (abs(r) <= 1))    
}

kernelT = function(r)
{
  return ((1 - abs(r)) * (abs(r) <= 1))
}

kernelQ = function(r)
{
  return ((15 / 16) * (1 - abs(r) ^ 2) ^ 2 * (abs(r) <= 1))
}

kernelG = function(r)
{
  return (((2*pi)^(-1/2)) * exp(-1/2*abs(r)^2) * (abs(r) <= 1))
}

## Сортируем объекты согласно расстояния до объекта z

sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  
  ## Создаём матрицу расстояний
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  ##print(distances)
  ## Сортируем
  orderedXl <- xl[order(distances[, 2]), ]
  ##print(orderedXl)
  return (orderedXl);
}

dstFunc <- function(xl, z, metricFunction =
                         euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  dstnc <- c()
    dsst <- distances[, 2]
    ##print(dsst)
    dstnc <- order(dsst)
  ##print(dstnc)
 
    ddsstt <- distances[dstnc, 2]
    print(ddsstt)
  
  return(ddsstt);
}

## Применяем метод kwNN
pw <- function(xl, z, k, h)
{
  
  ## Сортируем выборку согласно классифицируемого объекта
  orderedXl <- sortObjectsByDist(xl, z)
  ddsstt <- dstFunc(xl, z)
  n <- dim(orderedXl)[2] - 1
  
  for(i in 1:k){
    orderedXl[i, 4] <- kernelG(ddsstt[i]/h)
  }
  print(orderedXl)
  types <- c("setosa", "versicolor", "virginica")
  mat <- matrix(data=0, nrow=1, ncol=3)
  colnames(mat) <- types
  
  a=n+1
  b=n+2
  classes <- orderedXl[1:k, a:b]
  
  mat[1,1] <- sum(classes[classes$Species=="setosa",2])
  mat[1,2] <- sum(classes[classes$Species=="versicolor",2])
  mat[1,3] <- sum(classes[classes$Species=="virginica",2])
  
  print(mat)
  
  nmbr <- which.max(mat)
  print(nmbr)
  class <- types[nmbr]
  print(class)
  return (class)
}

## Рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green3",
            "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col =
       colors[iris$Species], asp = 1)

## Классификация одного заданного объекта
z <- c(5, 2.5)
xl <- iris[, 3:5]
class <- pw(xl, z, k=4, h=1)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)