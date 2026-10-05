erlB = function(ro,n) {
  i=0:n
  ro^n/factorial(n)/sum(ro^i/factorial(i))
}