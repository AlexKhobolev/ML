## Евклидово расстояние
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
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
  ## Сортируем
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}
## Применяем метод kNN
knn <- function(xl, z, k)
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
  print(class)
  return (class)
}

class <- iris[, 5]
xl <- iris[, 3:5]


plot(NULL, NULL, type = "l", xlim = c(0, 150), ylim = c(0, 1), xlab = 'k', ylab = 'LOO')
Ox <- seq(from = 1, to = 150, by = 5)
Oy <- c()
 
LOO_opt <- 1
k_opt <- 1
 
for(k in Ox) {
  l <- dim(xl)[1]
  R <- 0
  for(i in 1:l) {
    irisNew <- iris[-i, 3:5]
    z <- iris[i, 3:4]
    if(knn(irisNew, z, k) != iris[i, 5]) {
      R <- R + 1
    }
  }
  LOO <- R/l
  Oy <- c(Oy, LOO)

  if(LOO < LOO_opt) {
    LOO_opt <- LOO
    k_opt <- k
  }
}

print(Ox)
print(Oy)
print(k_opt)

lines(Ox, Oy, pch = 8, bg = "black", col = "blue")
points(k_opt, LOO_opt, pch = 22, bg = "black", col = "black")