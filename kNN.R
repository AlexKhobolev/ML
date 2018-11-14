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
kNN <- function(xl, z, k)
{
## Сортируем выборку согласно классифицируемого объекта
orderedXl <- sortObjectsByDist(xl, z)
n <- dim(orderedXl)[2] - 1
## Получаем классы первых k соседей
classes <- orderedXl[1:k, n + 1]
## Составляем таблицу встречаемости каждого класса
counts <- table(classes)
## Находим класс, который доминирует среди первых k соседей
class <- names(which.max(counts))
return (class)
}
## Рисуем выборку
colors <- c("setosa" = "red", "versicolor" = "green3",
"virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col =
colors[iris$Species], asp = 1)
## Классификация одного заданного объекта
z <- c(2.7, 1)
xl <- iris[, 3:5]
class <- kNN(xl, z, k=6)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)