#part 1
#test
call <- scan("04_day/data/test_data", sep = ",", nlines = 1)
boards_vector <- scan("04_day/data/test_data", skip = 1)
board_size <- c(5,5)
n_boards <- length(boards_vector) /prod(board_size)
boards <- array(boards_vector, dim = c(board_size,n_boards))
#boards <- aperm(boards, c(2,1,3))
marked_boards_init <- array(FALSE, dim = dim(boards) )

#boards_bool <- boards == call[1]
#boards_bool <- boards_bool + (boards == call[2])
#apply(boards_bool, c(1,3), all)


mark_boards <- function(marked_boards, number){
  marked_boards + (boards == number)
}

check_bingo <- function(marked_boards){
  
  bingo_board <- c(check_rows(marked_boards),
  check_columns(marked_boards))[2]

  sum(boards[,,bingo_board][!as.logical(marked_boards[,,bingo_board])])
  
  
}

check_rows <- function(marked_boards){
  apply(marked_boards,c(1,3), all) |>
    which(arr.ind = TRUE)
}

check_columns <- function(marked_boards){
  apply(marked_boards,c(2,3), all) |>
    which(arr.ind = TRUE)
}

check_bingo(marked_boards)

bingo <- NA_integer_
marked_boards <- marked_boards_init
i <- 0
while(is.na(bingo)){
  i <- i + 1
  marked_boards <- mark_boards(marked_boards, call[i] )
  bingo <- check_bingo(marked_boards)
}

call[i] * bingo 
