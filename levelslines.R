norm = function(x, y, mu, sigma) {
  x = matrix(c(x, y), 2, 1)
  k = 1 / sqrt((2 * pi) ^ 2 * det(sigma))
  e = exp(-0.5 * t(x - mu) %*% solve(sigma) %*% x - mu)
  return(k * e)
}
par(bg = 'white', fg = 'black')
a = matrix(c(5,7,7,10), nrow = 2, ncol = 2 ) 
q=10;
plot(-q:q, -q:q, type = "n",asp=1)
x=seq(-q, q, 0.1)
y=seq(-q, q, 0.1)


for(i in x){
  for(j in y){
    z = c(i, j)
    plot = norm(i,j,0,a)
    color = adjustcolor("red", plot*2 )
    points(z[1], z[2], pch = 21,col=color,bg=color)
  }
}



z = outer(x, y, function(x, y) {
  sapply(1:length(x), function(i) norm(x[i], y[i], 0, a))
  
})

contour(x,y,z,add=T ,asp=1,lwd = 1)