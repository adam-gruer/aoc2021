lanternfish <- function(fish, day){
  
  breeders <- fish[1]
  fish <- fish[2:9]
  fish[7] <- fish[7] + breeders
  fish[9] <- breeders
  fish
}


fish <- scan("06_day/data/data", sep =",") |>
  factor( levels = 0:8) |>
  table() |>
  as.numeric()

options(scipen = 999)
Reduce(lanternfish, x = 1:256, fish) |>
  sum()
options(scipen = 0)

  