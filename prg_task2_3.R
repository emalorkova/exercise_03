CZKChange <- function(M) {
  mince <- c()
#Give the integer part of M/50 50 CZK coins to customer.
#Let remainder be the remaining amount due the customer.
  pocet_50 <- as.integer(M/50)
  mince50 <- append(mince, replicate(pocet_50, 50))
  M <- M - pocet_50*50

#Give the integer part of remainder/20 20 CZK coins to customer.
#Let remainder be the remaining amount due the customer.
  pocet_20 <- as.integer(M/20)
  mince20 <- append(mince50, replicate(pocet_20, 20))
  M <- M - pocet_20*20

#Give the integer part of remainder/10 10 CZK coins to customer.
#Let remainder be the remaining amount due the customer.
  pocet_10 <- as.integer(M/10)
  mince10 <- append(mince20, replicate(pocet_10, 10))
  M <- M - pocet_10*10

#Give the integer part of remainder/5 5 CZK coins to customer.
#Let remainder be the remaining amount due the customer.
  pocet_5 <- as.integer(M/5)
  mince5 <- append(mince10, replicate(pocet_5, 5))
  M <- M - pocet_5*5

#Give remainder in 1 CZK coins to customer.
  mince_final <- append(mince5, replicate(M, 1))

return(unlist(mince_final))
}


CZKChange(1567)



CoinChange <- function(M, coins) {
  change <- c()
  for (c in coins) {
    while (M >= c) {
      change <- append(change, c)
      M <- M - c
    }
  }
  return(change)
}


CoinChange(127, c(50, 20, 10, 5, 1))
