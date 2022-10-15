Chocolate <- function(M, r, c) {
  if (r == nrow(M)) {
    return(M[r, c])
  }
  else
    choco_count <- M[r, c]
    choco_r <- Chocolate(M, r + 1, c)
    choco_c <- Chocolate(M, r + 1, c + 1)
    return(max(choco_r, choco_c) + choco_count)
}


Chocolate(matrix(1:9, nrow = 3, ncol = 3), 1, 1)
