#part 1
#test




mark_boards <- function(marked_boards, number, boards){

  marked_boards + (boards == number)
}

check_bingo <- function(marked_boards){
 
  if(length(dim(marked_boards)) == 2){
    c(check_rows(marked_boards),
      check_columns(marked_boards))[1]} else {
  
   rbind(check_rows(marked_boards),
  check_columns(marked_boards))[,2]}
}

score_board <- function(boards, marked_boards, bingo_board){
  if(length(dim(boards)) == 2){
    sum(boards[!as.logical(marked_boards)])
  } else
  sum(boards[,,bingo_board][!as.logical(marked_boards[,,bingo_board])])
  }

check_rows <- function(marked_boards){
  margins <- c(1,3)
  if(length(dim(marked_boards)) < 3) {margins <- 1}
  apply(marked_boards,margins, all) |>
    which(arr.ind = TRUE)
}

check_columns <- function(marked_boards){
  margins <- c(2,3)
  if(length(dim(marked_boards)) < 3) {margins <- 2}
  apply(marked_boards,margins, all) |>
    which(arr.ind = TRUE)
}


#Play
play <- function(path){

  call <- scan(path, sep = ",", nlines = 1)
  boards_vector <- scan(path, skip = 1)
  board_size <- c(5,5)
  n_boards <- length(boards_vector) /prod(board_size)
  boards <- array(boards_vector, dim = c(board_size,n_boards))
  boards <- aperm(boards, c(2,1,3))
  marked_boards_init <- array(FALSE, dim = dim(boards) )
  
  
  
bingo <- integer()
marked_boards <- marked_boards_init
i <- 0
while(length(bingo) == 0){
 
  i <- i + 1
  marked_boards <- mark_boards(marked_boards, call[i], boards )
  bingo <- check_bingo(marked_boards)
  
}

call[i] * score_board(boards, marked_boards , bingo) 
}
play("04_day/data/data.txt")

#part 2
play2 <- function(path){
 
  call <- scan(path, sep = ",", nlines = 1)
  boards_vector <- scan(path, skip = 1)
  board_size <- c(5,5)
  n_boards <- length(boards_vector) /prod(board_size)
  boards <- array(boards_vector, dim = c(board_size,n_boards))
  boards <- aperm(boards, c(2,1,3))
  marked_boards_init <- array(FALSE, dim = dim(boards) )
  
  
  
  bingo <- NA_integer_
  marked_boards <- marked_boards_init
  i <- 0

   while(n_boards >= 1){

    i <- i + 1
    marked_boards <- mark_boards(marked_boards, call[i], boards )
    bingo <- check_bingo(marked_boards)

    if(!length(bingo) == 0){
     
      if (n_boards == 1) break
      
      marked_boards <- marked_boards[,,-bingo]
      boards <- boards[,,-bingo]
      n_boards <- dim(marked_boards)[3]
      if(is.na(n_boards)) {n_boards <- 1 }
    }
  }
  
  call[i] * score_board(boards, marked_boards, bingo) 
}
play2("04_day/data/test_data")
play2("04_day/data/data.txt")
