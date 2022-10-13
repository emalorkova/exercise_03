IndexOfMin <- function(array, first, last) {
  index <- first
  for (k in (first + 1):last) {
    if (array[k] < array[index]) {
      index <- k
    }
  }
  return(index)
}


SelectionSort <- function(array, n) {
  for (i in 1:(n-1)) {
    j <- IndexOfMin(array, i, n)
    #Swap elements a(i) and a(j)
    temp <- array[i]
    array[i] <- array[j]
    array[j] <- temp
  }
  return(array)
}


RecursiveSelectionSort <- function(array, first, last) {
  if (first < last) {
    index <-IndexOfMin(array, first, last)
    #Swap array(first) with array(index)
    temp <- array[first]
    array[first] <- array[index]
    array[index] <- temp
    array <- RecursiveSelectionSort(array, first + 1, last)
  }
return(array)
}


RecursiveSelectionSort(c(5,6,7,9,4,3,2,1, 10, 17), 1, 8)
