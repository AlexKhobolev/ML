## ��������� ����������
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}

## ��������� ������� �������� ���������� �� ������� z
xl <- iris[, 3:5]
sortObjectsByDist <- function(xl, z, metricFunction =
                                euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  ## ������ ������� ����������
  distances <- matrix(NA, l, 2)
  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
  ## ���������
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}
## ��������� ����� kNN
knn <- function(xl, z, k)
{
  ## ��������� ������� �������� ����������������� �������
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  ## �������� ������ ������ k �������
  classes <- orderedXl[1:k, n + 1]
  ## ���������� ������� ������������� ������� ������
  counts <- table(classes)
  ## ������� �����, ������� ���������� ����� ������ k �������
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