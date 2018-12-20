# ML
## Метрические алгоритмы
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

**Метод парзеновского окна**

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

Лоо для треугольного ядра:

```html
kernelT = function(r)
{
  return ((1 - abs(r)) * (abs(r) <= 1))
}
```

![loopw](https://user-images.githubusercontent.com/44859059/50244640-837c5800-03e1-11e9-91e9-aff25ad3c53b.png)


Лоо для Епанечникова ядра:

```html
kernelEP = function(r)
{
  return ((3/4*(1-abs(r)^2)*(abs(r)<=1)))
}
```

![loopwep](https://user-images.githubusercontent.com/44859059/50244692-a3138080-03e1-11e9-877f-9660b000c3d1.png)

**Метод потенциальных функций**

Метод потенциальных функций так же является алгоримом классификации. В этом методе мы можем представить, что мы по очереди находимся в каждой точке обучающей выборки и думать, что каждый объект обучающей выборки распространяет вокруг себя потенциал своего класса. Мерой важности объекта при классификации будет служить величина его "заряда" и расстояние до классифицируемого объекта.


## Байесовские классификаторы

Байесовский метод классификации опирается на теорему о том, что если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде. 
Для классификации точки нужно вычислить функции правдоподобия каждого из классов, после вычислить апостериорные вероятности классов. Классифицируемый объект относится к классу, апостериорная вероятность которого максимальна.

**Линии уровня нормального распределения**

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


**Наивный нормальный байесовский классификатор**

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

**Подстановочный алгоритм plug-in**

Это один из вариантов байесовского классификатора, который обладает нормальным многомерным распределением. В алгоритме plug-in, восстанавливая параметры нормального распределения для каждого класса, подставляем в формулу оптимального байесовского классификатора восстановленные плотности.

В случае, когда ковариационные матрицы равны для всех классов, получится Линейный Дискриминант Фишера.

Если будем выбирать различные центры и ковариационные матрицы, будем получать разные виды дискриминантной функции.

![pluginokr](https://user-images.githubusercontent.com/44859059/50253590-b895a400-03fb-11e9-9f33-07219059d974.png)

![plugin2](https://user-images.githubusercontent.com/44859059/50253605-c519fc80-03fb-11e9-9eec-d476a3be6585.png)

![plugin3](https://user-images.githubusercontent.com/44859059/50253627-d400af00-03fb-11e9-8914-a98f0870369f.png)

![plugin4](https://user-images.githubusercontent.com/44859059/50253639-dfec7100-03fb-11e9-99ef-6254abed9a47.png)

