# Machine Learning

+ [Метрические алгоритмы классификации](#Метрические-алгоритмы-классификации) 
  + [1NN](#Описание-алгоритма-1NN)
  + [KNN](#Описание-алгоритма-kNN)
  + [KWNN](#Описание-алгоритма-kwNN)
  + [Метод парзеновского окна](#Метод-парзеновского-окна)
  + [Метод потенциальных функций](#Метод-потенциальных-функций)
+ [Байесовские алгоритмы классификации](#Байесовские-алгоритмы-классификации)
  + [Линии уровня](#Линии-уровня-нормального-распределения)
  + [Наивный нормальный байесовский классификатор](#Наивный-нормальный-байесовский-классификатор)
  + [Plug-in](#Подстановочный-алгоритм-plug-in)
  + [Линейный дискриминант Фишера](#Линейный-дискриминант-Фишера)
+ [Линейные алгоритмы классификации](#Линейные-алгоритмы-классификации)
  + [Метод стохастического градиента](#Метод-стохастического-градиента) 
  + [Алгоритм классификации ADALINE](#Алгоритм-классификации-ADALINE) 
  + [Персептрон Розенблатта](#Персептрон-Розенблатта)
  + [Логистическая регрессия](#Логистическая-регрессия)  
  
## Метрические алгоритмы классификации
## Описание алгоритма 1NN

  Для решения задачи классификации с помощью алгоритма нахождения одного ближайшего соседа нам потребуется: обучающая выборка, 
классифицируемый объект и метрика, в которой мы будем подсчитывать расстояния между точками. Целью алгоритма 
1NN является классификация объекта. 
Шаги алгоритма:
1) Определяем координаты точки, которую будем классифицировать. 
```html
z <- c(4.9, 1.7)
```
2) ВЫбираем метрику (использую евклидову).
```html
euclideanDistance <- function(u, v)
{
  sqrt(sum((u - v)^2))
}
```
3) Потом расчитываем расстояния от классифицируемой точки до каждой точки обучающей выборки. 
4) Выбираем одну точку обучающей выборки, расстояние до которой от классифицируемой наименьшее, получаем информацию о ее классовой принадлежности. 
5) Определяем классифицируемою точку таким же классом, что и наиближайшая точка. 

Код программы:

```html
ONN <- function(xl, z)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1

  classes <- orderedXl[1, n + 1]
  counts <- table(classes)

  class <- names(which.max(counts))
  return (class)
}
```

Результат работы программы для классифицируемой точки с координатами (4.9, 1.7) прикрепляю для того, чтобы можно было сравнить с результатом работы программы kNN, т.к. результаты могут отличаться из-за того, что на точку влияет уже не один ближайший объект.

![onn](https://user-images.githubusercontent.com/44859059/48857418-defafc00-edc9-11e8-9d8d-1dc2b1603ebb.png)


## Описание алгоритма kNN

Для решения задачи классификации с помощью алгоритма нахождения k ближайших соседей нам потребуется: обучающая выборка, 
классифицируемый объект и метрика, в которой мы будем подсчитывать расстояния между точками. Целью алгоритма kNN является классификация объекта. 
Шаги алгоритма: 
1) Определяем координаты точки, которую будем классифицировать. 
2) ВЫбираем метрику (использую евклидову).
3) Потом расчитываем расстояния от классифицируемой точки до каждой точки обучающей выборки.
4) Сортируем точки по возрастанию расстояния.
```html
sortObjectsByDist <- function(xl, z, metricFunction =
euclideanDistance)
{
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  distances <- matrix(NA, l, 2)

  for (i in 1:l)
  {
    distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
  }
                      
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}
```
5) Выбираем k (в случае этой программы k=3) точек обучающей выборки, расстояния до которых от классифицируемой наименьшие,  получаем информацию о классе точек доминирующих среди k бижайших (какого класса точек больше среди k ближайших). 
6) Определяем классифицируемою точку таким же классом, что и k ближайших точек.

Код программы:

```html
kNN <- function(xl, z, k)
{
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)

  class <- names(which.max(counts))
  return (class)
}
```
Глядя на результат работы алгоритма kNN, можем заметить, что одна и та же точка (4.9, 1.7) классифицировалась по разному в 1NN и kNN. На такой результат повлияло то, что в алгоритме 1NN на точку влияет лишь один объект, а в kNN (при k=3 в нашем случае) среди 3-х ближайших точек две из них принадлежат к другому классу (кроме самой близкой), соответственно этот класс доминирует и точка классифицируется другим классом.

![knn](https://user-images.githubusercontent.com/44859059/48864484-bda40b00-eddd-11e8-9200-02eca25547e8.png)

Результат работы алгоритма kNN для k=4 и классифицируемой точки (2.5, 0.7). Далее мы сравним его с результатом работы алгоритма kwNN с тем же k и той же точкой.

![knn k 4](https://user-images.githubusercontent.com/44859059/48863749-79177000-eddb-11e8-9967-8fc145049c53.png)


## Описание алгоритма kwNN

Для решения задачи классификации с помощью алгоритма kwNN нам потребуется: обучающая выборка, классифицируемый объект, 
весовая функция для каждого элемента (вычисляется по формуле w=q^i, где q - произвольная величина, i - порядковый номер объекта) и метрика, в которой мы будем подсчитывать расстояния между точками. Целью алгоритма kwNN является классификация объекта. 
Шаги алгоритма: 
1) Определяем координаты точки, которую будем классифицировать. 
2) ВЫбираем метрику (использую евклидову).
3) Потом расчитываем расстояния от классифицируемой точки до каждой точки обучающей выборки. 
4) Выбираем k точек обучающей выборки, расстояния до которых от классифицируемой наименьшие,  получаем информацию о классе точек доминирующих среди k бижайших с помощью весовой функции. Считаем сумму весов точек одного класса среди k ближайших. 
5) Определяем классифицируемою точку таким же классом, что и класс ближайших точек с наибольшей суммой весов.

Код программы:

```html
kwNN <- function(xl, z, k, q)
{

  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1

 for(i in 1:k){
    orderedXl[i, 4] <- q^i
}
 types <- c("setosa", "versicolor", "virginica")
 mat <- matrix(data=0, nrow=1, ncol=3)
 colnames(mat) <- types

 a=n+1
 b=n+2
 classes <- orderedXl[1:k, a:b]

 mat[1,1] <- sum(classes[classes$Species=="setosa",2])
 mat[1,2] <- sum(classes[classes$Species=="versicolor",2])
 mat[1,3] <- sum(classes[classes$Species=="virginica",2])

nmbr <- which.max(mat)

class <- types[nmbr]

return (class)
}
```
Преимуществом алгоритма kwNN над kNN является наличие весовой функции, она показывает такой факт - чем ближе объект обучающей выборки распологается по отношению к классифицируемому объекту, тем сильнее он влияет на его классификацию. А так же в случае, когда среди k ближайших соседей имеется одинаковое количество объектов разных классов (n объектов одного класса и n другого) и они оба доминирующие, алгоритм kNN будет классифицировать объект неверно, а в случае алгоритма kwNN в процесс классификации включается номер соседа, от чего зависит весовая функция, и в результате определения класса будет отсутствовать неопределенность.  

Результат работы алгоритма kwNN для k=4 и классифицируемой точки (2.5, 0.7). Сравним его с работой алгоритма kNN с набором тех же данных. Можно заметить, что точка классифицировалась иначе из-за того, что общая сумма весов у класса *versicolor*(зеленый цвет) больше, чем у класса *setosa*(красный цвет).

![kwnn n 4](https://user-images.githubusercontent.com/44859059/48864388-761d7f00-eddd-11e8-9135-729d55e39093.png)

**Описание алгоритма LOO**

Алгоритм LOO (leave-one-out) является алгоритмом, который проверяет алгоритмы на точность. 
Шаги алгоритма: 
1) Исключаем по очереди по 1 элементу из выборки. 
2) Обучаем алгоритм на оставшихся элементах выборки.
3) Классификацируем извлеченный элемент (в случае задач классификации). 
4) Возвращаем его. 
5) Извлекаем следующий и так далее со всеми элементами выборки.
6) В результате всех проведенных действий мы можем заметить, где алгоритм ошибся.

Код программы:

```html
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
```
Это изображение показывает, что для k=6 (обозначено на графике черной точкой) алгоритм kNN имеет наименьшее число ошибок.

![loo_knn](https://user-images.githubusercontent.com/44859059/48860382-d0b0de00-edd1-11e8-9e08-e155cf2cc823.png)

## Метод парзеновского окна

В методе парзеновского окна оценкой близости яляется функция, которая называется функцией ядра и зависитот переменной, которая зависит от h и расстояния от классифицируемой точки до k-го ближайшего объекта выборки (r=dist/h). Чаще используют 5 видов ядер Епанечникова, прямоугольное, треугольное, квадрическое и гауссовское - единственное, которое не ограничивает переменную (во всех остальных переменная имеет ограничение по модулю меньше или равное еденице). Фиксированная ширина окна h работает неудачно, когда плотность распределения обучающей выборки неравномерна.При плотном расположении объектов желательно использование более узкого окна, а в разреженном случае - более широкого. Выбор функции ядра слабо влияет на классификацию в отличии от h. Так же параметр h влияет на сам факт классификации, будет он произведен или нет. Если в результате подбора, переменная r по модулю превосходит значение 1, то объект не будет классифицирован.

Шаги алгоритма парзеновского окна очень схожи с алгоритмом kwNN, отличаются они различными функциями ядра и наличием такой переменной, как ширина окна.

Отсутствием классификации в тех точках, для которых переменная функции ядра больше 1 по модулю, удалось достичь с помощью такого кода:

```html
##Если каждый класс имеет нулевое значение веса, точка не классифицируется
if(mat[1,1] == 0 & mat[1,2] == 0 & mat[1,3] == 0){
    class <- 'white'
  }
``` 

Карта классификации метода парзеновского окна с использованием квадрического ядра. 

![pw_map](https://user-images.githubusercontent.com/44859059/49109338-7e713080-f29b-11e8-8468-5ab70f77f3ab.png)

Loo для треугольного ядра:

```html
kernelT = function(r)
{
  return ((1 - abs(r)) * (abs(r) <= 1))
}
```

![loopw](https://user-images.githubusercontent.com/44859059/50244640-837c5800-03e1-11e9-91e9-aff25ad3c53b.png)


Loo для Епанечникова ядра:

```html
kernelEP = function(r)
{
  return ((3/4*(1-abs(r)^2)*(abs(r)<=1)))
}
```

![loopwep](https://user-images.githubusercontent.com/44859059/50244692-a3138080-03e1-11e9-877f-9660b000c3d1.png)

## Метод потенциальных функций

Метод потенциальных функций так же является алгоримом классификации. В этом методе мы можем представить, что мы по очереди находимся в каждой точке обучающей выборки и думать, что каждый объект обучающей выборки распространяет вокруг себя потенциал своего класса. Мерой важности объекта при классификации будет служить величина его "заряда" и расстояние до классифицируемого объекта.


## Байесовские алгоритмы классификации

Байесовский метод классификации опирается на теорему о том, что если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде. 
Для классификации точки нужно вычислить функции правдоподобия каждого из классов, после вычислить апостериорные вероятности классов. Классифицируемый объект относится к классу, апостериорная вероятность которого максимальна.

## Линии уровня нормального распределения

N-мерным нормальным распределением будет называться распределение с плотностью ![linesform](https://user-images.githubusercontent.com/44859059/50251637-90a34200-03f5-11e9-8c17-2fc8cf8fcb9b.png)с математическим ожиданием (мю) и матрицей ковариации (сигма). 

Рассмотрим следующие случаи:

1) Если признаки объектов независимы (некоррелированы), то линнии уровня будут распространятся от центра, обладать формой элипса и оси будут параллельны осям координат.

Примеры:

![lines2](https://user-images.githubusercontent.com/44859059/50252040-cd236d80-03f6-11e9-9984-5682493ed698.png)

![lines3](https://user-images.githubusercontent.com/44859059/50252060-da405c80-03f6-11e9-97ba-779d8decbc7a.png)

2) Если признаки имеют одинаковые дисперсии (разброс значений), линии уровня будут образовывать окружности.

Пример: 

![lines1](https://user-images.githubusercontent.com/44859059/50252136-1ecbf800-03f7-11e9-8f9e-1c548895db22.png)

3) Если признаки зависимы, то матрица перестает быть диагональной, линнии уровня образуют форму елипсов, которые наклонены.

Пример:

![lines4](https://user-images.githubusercontent.com/44859059/50252279-83875280-03f7-11e9-8884-dd2555ee467d.png)


## Наивный нормальный байесовский классификатор

Наивный нормальный байесовский классификатор основан на теореме Байеса, в которой он говорит, что признаки объектов независимы. Другими словами: наличие признака в классе не связано с наличием какого-либо другого признака.

Преимущества алгоритма: 1) высокая скорость классификации, т.к. низкие вычислительные затраты; 2) когда признаки независимы, наивный байесовский классификатор оптимален.

Недостатки: 1) низкое качество классификации.

Шаги алгоритма: 
1) Определяем априорные вероятности появления каждого класса;
2) Восстаналиваем матрицы математического ожидания и ковариационной матрицы;

```html
mu = matrix(0, nrow=m, ncol=n)
sigm = matrix(0, nrow=m, ncol=n)
for(i in 1:m){
  for(j in 1:n){
    temp=xl[xl[,3]==classes[i],][,j] #j-й столбец содержащий только признаки i-го класса
    mu[i,j]<-mean(temp)#матрица мат ожиданий
    sigm[i,j]<-sqrt(var(temp))#матрица отклонений
  }
}
```

3) Проводим подсчет эмпирических оценок;

```html
N=1/sqrt(2*pi)/sigm[i,j]*exp(-1/2*(x[j]-mu[i,j])^2/sigm[i,j]^2)
      scores[i] = scores[i] * N
```
4) Определяем объект таким классом, эмпирическая оценка которого наибольшая.

Карта классификации:

![nnbk](https://user-images.githubusercontent.com/44859059/50241993-1618f900-03da-11e9-88ac-9bc6ef3fffe8.png)

## Подстановочный алгоритм plug-in

Это один из вариантов байесовского классификатора, который обладает нормальным многомерным распределением. В алгоритме plug-in, восстанавливая параметры нормального распределения для каждого класса, подставляем в формулу оптимального байесовского классификатора восстановленные плотности.

В случае, когда ковариационные матрицы равны для всех классов, получится Линейный Дискриминант Фишера.

Шаги алгоритма:

1) Генерируем тестовые данные;

```html
Sigma1 <- matrix(c(10, 0, 0, 1), 2, 2)
Sigma2 <- matrix(c(1, 0, 0, 5), 2, 2)
Mu1 <- c(1, 5)
Mu2 <- c(5, 0)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
```

2) Соединяем два класса в одну выборку;

```html
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
```

3) Восстанавливаем центр нормального распределения;

```html
estimateMu <- function(objects)
{
  ## mu = 1 / m * sum_{i=1}^m(objects_i)
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  mu <- matrix(NA, 1, cols)
  for (col in 1:cols)
  {
    mu[1, col] = mean(objects[,col])
  }
  return(mu)
}
```

4) Восстанавливаем ковариационные матрицы;

```html
estimateCovarianceMatrix <- function(objects, mu)
{
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  sigma <- matrix(0, cols, cols)
  for (i in 1:rows)
  {
    sigma <- sigma + (t(objects[i,] - mu) %*% (objects[i,] - mu)) / (rows - 1)
  }
  return (sigma)
}
```

5) Получаем коэффициенты подстановочного алгоритма для построения разделяющей кривой имеющей вид:

a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2 + f = 0

Если будем выбирать различные центры и ковариационные матрицы, будем получать разные виды дискриминантной функции.

![pluginokr](https://user-images.githubusercontent.com/44859059/50253590-b895a400-03fb-11e9-9f33-07219059d974.png)

![plugin2](https://user-images.githubusercontent.com/44859059/50253605-c519fc80-03fb-11e9-9eec-d476a3be6585.png)

![plugin3](https://user-images.githubusercontent.com/44859059/50253627-d400af00-03fb-11e9-8914-a98f0870369f.png)

![plugin4](https://user-images.githubusercontent.com/44859059/50253639-dfec7100-03fb-11e9-99ef-6254abed9a47.png)


## Линейный дискриминант Фишера

Линейный дискриминант Фишера отличается от подстановочного алгоритма тем, что ковариационные матрицы в Линейном дискриминате Фишера равны. И для их восстановления нужно использовать все объекты всех классов обучающей выборки.

Шаги алгоритма:

1) Генерируем тестовые данные с одинаковыми ковариационными матрицами

```html
Sigma1 <- matrix(c(5, 0, 0, 5), 2, 2)
Sigma2 <- matrix(c(5, 0, 0, 5), 2, 2)
Mu1 <- c(0, 0)
Mu2 <- c(16, 0)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
```

2) Соединяем два класса в одну выборку

3) Восстанавливаем ковариационные матрицы

```html
for (i in 1:rows1)
  {
    sigma <- sigma + (t(objects1[i,] - mu1) %*%
                        (objects1[i,] - mu1)) / (rows + 2)
  }
  for (i in 1:rows2)
  {
    sigma <- sigma + (t(objects2[i,] - mu2) %*%
                        (objects2[i,] - mu2)) / (rows + 2)
  }
```

4) Получаем коэффициенты разделяющей прямой по формуле

![default](https://user-images.githubusercontent.com/44859059/50403018-5e566380-07ac-11e9-914d-e20d49fb8d73.png)

```html
inverseSigma <- solve(Sigma)
alpha <- inverseSigma %*% t(mu1 - mu2)
mu_st <- (mu1 + mu2) / 2
beta <- mu_st %*% alpha
```

5) Рисуем прямую

```html
abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col = "red", lwd = 3)
```

Результаты работы алгоритма:

![ldf](https://user-images.githubusercontent.com/44859059/50403029-78904180-07ac-11e9-8229-24994095b0cf.png)

![ldf3](https://user-images.githubusercontent.com/44859059/50447918-7c989c80-092f-11e9-9624-e02b4e9db79c.png)

Преимуществом алгоритма ЛДФ над подстановочным алгоритмом является повышенная четкоссть работы, когда в выборке небольшое число элементов и подсчет коэфициентов прямой идет легче, т. к. в ЛДФ нет квадратов. А преимуществом подстановочного является универсальность, т. к. кривая за счет своего изгиба может огибать некоторые точки, в то время как в ЛДФ они будут выбросами.


## Линейные алгоритмы классификации

Линейный классификатор основан на принципе разделения классов. Будем рассматривать задачу классификации с двумя видами классов ![lin](https://user-images.githubusercontent.com/44859059/50448534-ed8d8380-0932-11e9-94c8-bd751fadd03c.png). Алгоритм ![lin2](https://user-images.githubusercontent.com/44859059/50448620-4e1cc080-0933-11e9-861f-fad7685c32ee.png) будет называться линейным алгоритмом классификации, который отделяет класс -1 от класса +1. Целью алгоритма является построение разделяющей прямой между классами. Если объект распологается по одну сторону разделяющей прямой, то они принадлежат к одному классу, а если по другую - к другому. Уравнение *f(x, w)=0* задает разделяющую прямую, где *w* - вектор параметров.

Так же в линейных алгоритмах классификации присутствует такая величина как *M(w)=yf(x, w)*, которая называется отступом. Если отступ объекта меньше нуля на объекте *x*, то алгоритм на этом объекте ошибается. Чем больше величина отступа тем лучше работает алгоритм.

Формула ![default](https://user-images.githubusercontent.com/44859059/50449956-b40d4600-093b-11e9-9a60-1ad3b88cf14f.png) определяет эмпирический риск или функцию потерь - количество ошибок на обучающей выборке. Функция *L(M)* называется фукцией потерь, она непрерывна и неотрицательна. Наша цель это минимизация функции потерь.

Часто используемые функции потерь *L(M)*:

<img width="578" alt="_" src="https://user-images.githubusercontent.com/44859059/50449833-db174800-093a-11e9-8567-b4d62a71a4f8.png">

Оказывается, что конструкция работы линейных алгоритмов классификации очень схожа с принципом работы нейронов человека.

![default](https://user-images.githubusercontent.com/44859059/50450119-d2277600-093c-11e9-9924-1dd4410ef0cd.png)

Как же работает человеческий нейрон? 

Синапсы - это часть клетки, к которой подходят отростки других клеток. В синапсах в результате биологических процессов начинает концентрироваться отрицательный заряд и он переходит внутрь клетки, в ее ядро. И там, как только происходит концентрация слишком большого отрицательного заряда, который пришел от всех синапсов, клетка генерирует электрический импульс, который бежит по аксону (в человеческом организме встречаются аксоны длиной до метра). Там происходит повышение концентрации отрицательно заряженных ионов и порождается волна возбуждения, она передается другой клетке, она возбуждается и так далее. Клетка это такой механизм, который на входе получил импульсы, суммировал их и передал. Можем заметить аналогию: величина заряда, которая приходит в клетку через синапсы - это наши признаки, синаптические связи - это веса, а коэффициент *w_0* это тот предел, который заставляет клетку генерировать импульс.

Ниже представлена модель с сумматором, функцией, которая переобразовывает результаты в значения -1 или +1: 

![summator](https://user-images.githubusercontent.com/44859059/51426753-2eaa4800-1c00-11e9-9e9e-1128c3f94bad.png)



## Метод стохастического градиента

Метод стохастического градиента - это метод градиентного спуска численных методов. Это итерационный процесс, на котором мы, взяв какое-то начальное приближение, должны каждый следующий раз идти в направлении антиградиента. Градиент функции нам показывает направление ее возрастания, а, т. к. мы стремимся минимизировать функцию потерь, мы идем в направлении самого быстрого убывания - антиградиента. При чем вычисление градиента проводится не на всех объектах выборки, а на некоторых, выбранных случайным образом. Именно по-этому алгоритм и называется "стохастическим".

Численная минимизация методом градиентного спуска:

![default](https://user-images.githubusercontent.com/44859059/50452445-289bb100-094b-11e9-9a20-202d87dfd42b.png), где ![default](https://user-images.githubusercontent.com/44859059/50452479-584ab900-094b-11e9-902d-086eb264ee21.png) - это градиентный шаг или темп обучения, *Q* - это градиент.

Формула используемая нами:

![2](https://user-images.githubusercontent.com/44859059/50452563-cb542f80-094b-11e9-9e27-be20096c0fc6.png)

Преимуществом метода стохастического градиента является возможность обучаться не на всей выборке, т. е. он эффективен в обучении на больших данных. Например нам дана выборка из миллиона объектов и, просмотрев первые случайные несколько тысяч объектов, мы уже имеем неплохой вектор весов. Т.е. он может обучиться даже не просмотрев все данные.

Недостатки: возможна расходимость или медленная сходимость; алгоритм останавливается в локальных минимумах.

Ход алгоритма:

1) Инициализировать веса случайным образом в промежутке (-1/2n, 1/2n);

2) Инициализировать текущую оценку функционала *Q*;

Повторять следующие шаги:

3) Выбираем объект из выборки;

4) Вычисляем потерю;

5) Делаем градиентный шаг;

6) Оцениваем значение функционала *Q*;

Пока зачение *Q* и/или веса *w* не стабилизируется. 

Данный алгоритм используется во всех нижепредставленных линейных классификаторах.

## Алгоритм классификации ADALINE

В линейном классификаторе ADALINE в качестве функции потерь берется квадратичная функция ![1](https://user-images.githubusercontent.com/44859059/50454742-8f749680-095a-11e9-8521-612953a7b0a9.png). Возьмем производную по переменной *w* и получим правило обновления весов (дельта-правило) на каждом шаге, которое будет выглядеть следующим образом: ![2](https://user-images.githubusercontent.com/44859059/50455022-68b75f80-095c-11e9-9e51-1c30b43ecdd4.png)

Квадратичная функция потерь выглядит следующим образом: 

```html
lossQuad <- function(x)
{
  return ((x-1)^2)
}
```

Ход алгоритма:

1) Нормализуем данные обучающей выборки: 

```html
trainingSampleNormalization <- function(xl)
{
  n <- dim(xl)[2] - 1
  for(i in 1:n)
  {
    xl[, i] <- (xl[, i] - mean(xl[, i])) / sd(xl[, i])
  }
  return (xl)
}
```

2) Определяем функцию потерь. Здесь она квадратичная и представлена выше;

3) Стохастический градиент для ADALINE:

4) Инициализируем веса;

```html
 Q <- 0
  for (i in 1:l)
  {
    ## вычисляем скалярное произведение <w,x>
    wx <- sum(w * xl[i, 1:n])
    ## вычисляем отступ
    margin <- wx * xl[i, n + 1]
    Q <- Q + lossQuad(margin)
  }
```

5) Вычисляем отступы для всех объектов обучающей выборки;

```html
    margins <- array(dim = l)
    for (i in 1:l)
    {
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      margins[i] <- crossprod(w, xi) * yi
    }
```

6) Делаем градиентный шаг;

7) Вычисляем ошибку;

8) Обновляем вектор весов.

```html
     Qprev <- Q
     Q <- (1 - lambda) * Q + lambda * ex
```

Пример работы ADALINE: 

![adaline](https://user-images.githubusercontent.com/44859059/51426428-c5c0d100-1bfb-11e9-9808-77a8229befe0.png)


## Персептрон Розенблатта 

В Персептроне Розенблатта используется кусочно-линейная функция потерь: ![perc1](https://user-images.githubusercontent.com/44859059/51425764-580fa780-1bf1-11e9-8842-557d5416abb3.png).

Алгоритм основан на правиле Хэбба, которое гласит: если ![1](https://user-images.githubusercontent.com/44859059/50455417-f72ce080-095e-11e9-9b72-b9fabb728103.png) то ![2](https://user-images.githubusercontent.com/44859059/50455469-2b080600-095f-11e9-88a8-366538be12cb.png) Это новое правило обновления весов.

Персептрон Розенблатта отличается от ADALINE лишь функцией потерь и правилом обновления весов, ход алгоритма здесь такой же.

Пример работы Персептрона Розенблатта: 

![perceptron](https://user-images.githubusercontent.com/44859059/51426435-e2f59f80-1bfb-11e9-8c1b-b8115ef6eab8.png)

## Логистическая регрессия

Здесь используется логистическая функция потерь - ![log1](https://user-images.githubusercontent.com/44859059/51425799-e8e68300-1bf1-11e9-83d8-5e4006bb419e.png). 

Правило обновления весов для логистической регрессии - ![log2](https://user-images.githubusercontent.com/44859059/51425821-4bd81a00-1bf2-11e9-8a14-2b2dafc02e99.png). Где сигмоидная функция такова: ![log3](https://user-images.githubusercontent.com/44859059/51425846-9b1e4a80-1bf2-11e9-9049-1bece04b1cfc.png)

Логистическа регрессия также выполняется с помощью стохастического градиента.

Пример работы логистической регрессии:

![logreg](https://user-images.githubusercontent.com/44859059/51426440-fb65ba00-1bfb-11e9-9422-b915b860c68c.png)

Как правило, логистическая регрессия дает результаты лучше, чем правило Хэбба или дельта-правило, т. к. она оспользует более "удачную" функцию потерь.

Сравнение работы всех трёх алгоритмов, где синий - ADALINE, зеленый - Персептрон Розенблатта (правило Хэбба), красный - логистичекая регрессия:

![allline](https://user-images.githubusercontent.com/44859059/51426449-189a8880-1bfc-11e9-9b00-f9f4bc31bba8.png)

[Вверх](#Machine-Learning)
