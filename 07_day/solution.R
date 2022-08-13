crab_positions <- scan("07_day/data/data", sep = ",")

grid <- 0:max(crab_positions)
incremental_fuel <- function(i) (1 + i) * i / 2

part1 <-  min(sapply(grid, function(pos) abs(pos - crab_positions) |> sum()) )
part2 <- min(sapply(grid, function(pos) incremental_fuel(abs(pos - crab_positions)) |> sum()) )




