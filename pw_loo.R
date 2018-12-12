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
  return (((2*pi)^(-1/2)) * exp(-1/2*r^2))
}

## Сортируем объекты согласно расстояния до объекта z
xl <- iris[, 3:5]
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
  ##print(ddsstt)
  
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
    orderedXl[i, 4] <- kernelQ(ddsstt[i]/h)
  }
  ##print(orderedXl)
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
  ##print(nmbr)
  class <- types[nmbr]
  
  if(mat[1,1] == 0 & mat[1,2] == 0 & mat[1,3] == 0){
    class <- 0
  }
  
  ##print(class)
  return (class)
}

Loo <- function(h,xl)
{
  sum <- 0
  for(i in 1:150){
    if(i==1){
      tmpXL <- xl[2:150,]
    }
    else if (i==150) {
      tmpXL <- xl[1:149,]
    }
    else {
      
      tmpXL <- rbind(xl[1:i-1, ], xl[i+1:150,])
    }
    
    xi <- c(xl[i,1], xl[i,2])
    class <-pw(tmpXL,xi,k,h)
    if(class != xl[i,3])
      sum <- sum+1
  }
  sum <- sum/150
  Oy <- c(Oy, sum)
  print(sum)
  return(sum)
}


h=0.3
k=4
xl <- iris[, 3:5]
z <- c(5,1.4)
class <- pw(xl,z,k,h)


  
  plot(NULL, NULL, type = "l", xlim = c(0, 15), ylim = c(0, 1), xlab = 'k', ylab = 'LOO')
  Ox <- seq(from = 1, to = 15, by = 5)
  Oy <- c()
  
  print(Ox)
  print(Oy)
  
  
  lines(Ox, Oy, pch = 8, bg = "black", col = "blue")
  points(h, sum, pch = 22, bg = "black", col = "black")


