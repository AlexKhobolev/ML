# ML
**Описание алгоритма 1NN**

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


**Описание алгоритма kNN**

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


**Описание алгоритма kwNN**

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

