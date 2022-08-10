diagnostic_report_test <- read.fwf("03_day/data/testdata", widths = rep(1,5))
diagnostic_report <- read.fwf("03_day/data/data.txt", widths = rep(1,12))

count <- function(x){
 # browser()
contingency_table <- table(x)
if ( length(contingency_table) == 2){
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
  filter_bit <- count(report[,bit_location])
  report[report[bit_location] == filter_bit, ]
}

co2_scrubber <- function(report, bit_location){
  #browser()
  if(nrow(report) == 1) return(report)
  filter_bit <- count(report[,bit_location])
  filter_bit <- (!filter_bit) |> as.integer()
  
  report[report[bit_location] == filter_bit, ]
}

#part 2 test
oxygen_binary <- Reduce(oxygen_generator, seq(ncol(diagnostic_report_test)), init = diagnostic_report_test)
co2_scrubber_binary <- Reduce(co2_scrubber, seq(ncol(diagnostic_report_test)), init = diagnostic_report_test)

oxygen_generator_rating <- intToi(oxygen_binary,2L)
co2_scrubber_rating <- intToi(co2_scrubber_binary,2L)
(life_support_rating <- oxygen_generator_rating * co2_scrubber_rating )

#part 2 prod


oxygen_binary <- Reduce(oxygen_generator, seq(ncol(diagnostic_report)), init = diagnostic_report)
co2_scrubber_binary <- Reduce(co2_scrubber, seq(ncol(diagnostic_report)), init = diagnostic_report)

oxygen_generator_rating <- intToi(oxygen_binary,2L)
co2_scrubber_rating <- intToi(co2_scrubber_binary,2L)
(life_support_rating <- oxygen_generator_rating * co2_scrubber_rating )
