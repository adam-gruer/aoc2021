diagnostic_report_test <- read.fwf("03_day/data/testdata", widths = rep(1,5))
diagnostic_report <- read.fwf("03_day/data/data.txt", widths = rep(1,12))

count <- function(x, ties = 0){
 # browser()
contingency_table <- table(x)
if (ties == 1 & length(contingency_table) == 2){
  contingency_table <- contingency_table[c(2,1)]
}

most_common <- which.max(contingency_table) 
names(most_common) |> as.integer()

}

intToi <- function(x, base = 0L){
  paste0( x, collapse = "") |> 
    strtoi(base)
}

gamma_binary <- sapply(diagnostic_report, count)
epsilon_binary <- (!gamma_binary) |> as.integer()

gamma_decimal <-intToi(gamma_binary, base = 2)
epsilon_decimal <- intToi(epsilon_binary, base = 2)

power_consumption <- gamma_decimal * epsilon_decimal



#part2

oxygen_generator <- function(report, bit_location){
  filter_bit <- count(report[,bit_location], ties = 1)
  report[report[bit_location] == filter_bit, ]
}

co2_scrubber <- function(report, bit_location){
  browser()
  if(nrow(report) == 1) return(report)
  filter_bit <- count(report[,bit_location], ties = 0)
  filter_bit <- (!filter_bit) |> as.integer()
  
  report[report[bit_location] == filter_bit, ]
}
oxygen_binary <- Reduce(oxygen_generator, seq(ncol(diagnostic_report_test)), init = diagnostic_report_test)
co2_scrubber_binary <- Reduce(co2_scrubber, seq(ncol(diagnostic_report_test)), init = diagnostic_report_test)

x <- co2_scrubber(diagnostic_report_test,1)
x2 <- co2_scrubber(x,2)
x3 <- co2_scrubber(x2, 3)

i <- 1
gamma_binary <- count(diagnostic_report_test[,i],ties = 1)


x <- diagnostic_report_test[diagnostic_report_test[i] == gamma_binary[i], ]

i <- 2
gamma_binary <- count(x[,i],ties = 1)
x <- x[x[i] == gamma_binary, ]

i <- 3
gamma_binary <- count(x[,i],ties = 1)
x <- x[x[i] == gamma_binary, ]


i <- 4
gamma_binary <- count(x[,i],ties = 1)
x <- x[x[i] == gamma_binary, ]


i <- 5
gamma_binary <- count(x[,i],ties = 1)
x <- x[x[i] == gamma_binary, ]

