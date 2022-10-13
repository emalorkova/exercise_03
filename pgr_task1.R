IndexOfMin <- function(array, first, last) {
  index <- first
  for (k in (first + 1):last) {
    if (array[k] < array[index]) {
      index <- k
    }
  }
  return(index)
}


IndexOfMin(c(5,6,7,9,4,3,2,1), 1, 7)



SelectionSort <- function(a, n) {
  for i ← 1 to n − 1
    j <- IndexOfMin(a, i, n)
    Swap elements a(i) and a(j)
  return a
}