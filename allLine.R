## ������������ ��������� �������
trainingSampleNormalization <- function(xl)
{
  n <- dim(xl)[2] - 1
  for(i in 1:n)
  {
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])
  }
  return (xl)
}
## ���������� ������� ��� �� -1 ��� w0
trainingSamplePrepare <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  xl <- cbind(xl[, 1:n], seq(from = -1, to = -1, length.out = l), xl[, n + 1])
}
## ������������ ������� ������
lossQuad <- function(x)
{
  return ((x-1)^2)
}

## �������������� �������� ��� ADALINE
sg.ADALINE <- function(xl, eta = 1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## �������������� Q
  Q <- 0
  for (i in 1:l)
  {
    ## ��������� ��������� ������������ <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## ��������� ������
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossQuad(margin)
  }
  repeat
  {
    ## ��������� ������� ��� ���� �������� ����. �������
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## �������� ��������� �������
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # �������� ��������� ������ �� ������
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      ## ��������� ��������� ������������ <w,xi>
      wx <- sum(w * xi)
      ## ������ ��� ���������
      margin <- wx * yi
      ## ��������� ������
      ex <- lossQuad(margin)
      eta <- 1 / sqrt(sum(xi * xi))
      w <- w - eta * (wx - yi) * xi
      ## ��������� Q
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
    }
    else
    {
      break
    }
  }
  return (w)
}
## ������� ������ ��� ������� �����
lossPerceptron <- function(x)
{
  return (max(-x, 0))
}

## �������������� �������� � �������� �����
sg.Hebb <- function(xl, eta = 0.1, lambda = 1/6)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  ## �������������� Q
  Q <- 0
  for (i in 1:l)
  {
    ## ��������� ��������� ������������ <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## ��������� ������
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossPerceptron(margin)
  }
  repeat
  {
    ## ����� ��������� ��������
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
    ## ������� ��������� �������
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # ������� ��������� ��������� ������
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      w <- w + eta * yi * xi
    }
    else
      break;
  }
  return (w)
}
## ��������������� ������� ������
lossLog <- function(x)
{
  return (log2(1 + exp(-x)))
}

## ���������� �������
sigmoidFunction <- function(z)
{
  return (1 / (1 + exp(-z)))
}

## �������������� �������� ��� ������������� ���������
sg.LogRegression <- function(xl)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- c(1/2, 1/2, 1/2)
  iterCount <- 0
  lambda <- 1/l
  ## �������������� Q
  Q <- 0
  for (i in 1:l)
  {
    ## ��������� ��������� ������������ <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## ��������� ������
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossSigmoid(margin)
  }
  repeat
  {
    # �������� ��������� ������ �� ������
    i <- sample(1:l, 1)
    iterCount <- iterCount + 1
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    ## ��������� ��������� ������������ <w,xi>
    wx <- sum(w * xi)
    ## ������ ����������� ���
    margin <- wx * yi
    ex <- lossSigmoid(margin)
    eta <- 0.3#1 / iterCount
    w <- w + eta * xi * yi * sigmoidFunction(-wx * yi)
    ## ��������� ��� Q
    Qprev <- Q
    Q <- (1 - lambda) * Q + lambda * ex
    if (abs(Qprev - Q) / abs(max(Qprev, Q)) < 1e-5)
      break
  }
  return (w)
}
# ���-�� �������� � ������ ������
ObjectsCountOfEachClass <- 100
## ���������� ��������� ������
library(MASS)
Sigma1 <- matrix(c(2, 0, 0, 10), 2, 2)
Sigma2 <- matrix(c(4, 1, 1, 2), 2, 2)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, c(0, 0), Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, c(10, -10),
               Sigma2)
xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))
colors <- c(rgb(255/255, 255/255, 0/255), "white",
            rgb(0/255, 200/255, 0/255))
## ������������ ������
xlNorm <- trainingSampleNormalization(xl)
xlNorm <- trainingSamplePrepare(xlNorm)
## ����������� ������
## ADALINE
plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3]
                                                     + 2], asp = 1, xlab = "x1", ylab = "x2", main = "��������
��������������")
w <- sg.ADALINE(xlNorm)
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col =
         "blue")
## ������� �����
w <- sg.Hebb(xlNorm)
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col =
         "green3")
## ������������� ���������
w <- sg.LogRegression(xlNorm)
abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col =
         "red")
# legend("bottomleft", c("ADALINE", "������� �����",
#                        "������������� ���������"), pch = c(15,15,15), col =
#          c("blue", "green3", "red"))