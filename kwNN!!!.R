## Евклидово расстояние
euclideanDistance <- function(u, v)
{
sqrt(sum((u - v)^2))
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

## Сортируем
orderedXl <- xl[order(distances[, 2]), ]
return (orderedXl);
}

## Применяем метод kNN
kwNN <- function(xl, z, k, q)
{

## Сортируем выборку согласно классифицируемого объекта
orderedXl <- sortObjectsByDist(xl, z)
n <- dim(orderedXl)[2] - 1

for(i in i:k){
orderedXl[i, 4] <- q^i
}
types <- c("setosa", "versicolor", "virginica")
mat <- matrix(data=0, nrow=2, ncol=3)
colnames(mat) <- types

a=n+1
b=n+2
classes <- orderedXl[1:k, a:b]

mat[1,1] <- sum(classes[classes$Species=="setosa",2])
mat[1,2] <- sum(classes[classes$Species=="versicolor",2])
mat[1,3] <- sum(classes[classes$Species=="virginica",2])

class <- names(which.max(mat))
return (class)
}

## Рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green3",
"virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col =
colors[iris$Species], asp = 1)

## Классификация одного заданного объекта
z <- c(2.7, 1)
q <- 0.5
xl <- iris[, 3:5]
class <- kwNN(xl, z, k=6, q)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)