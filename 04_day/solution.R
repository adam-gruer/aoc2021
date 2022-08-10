#part 1
#test
call <- scan("04_day/data/test_data", sep = ",", nlines = 1)
boards <- scan("04_day/data/test_data", skip = 1)

boards <- array(boards, dim = c(5,5,3))

boards_bool <- boards == call[1]
