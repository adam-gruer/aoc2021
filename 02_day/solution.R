

parser <- function(command){
  direction <- str2lang(strsplit(command, " ")[[1]][[1]])
  distance <- str2lang(strsplit(command, " ")[[1]][[2]])
  substitute(direction(dist = distance))
}

move <- function(position, command){
  expr <- parser(command)
  expr$position <- position
  eval(expr)
}

  
forward <- function(position = list(x = 0, y = 0), dist ){
  position$x <- position$x + dist
  position
}

down <- function(position = list(x = 0, y = 0), dist ){
  position$y <- position$y + dist
  position
}

up <- function(position = list(x = 0, y = 0), dist ){
  position$y <- position$y - dist
  position
}

#test
test <- c("forward 5",
"down 5",
"forward 8",
"up 3",
"down 8",
"forward 2")

Reduce(move, x = test, init = list(x = 0, y = 0))

#part 1
x <- readLines("02_day/data/part1.txt")
final_pos <- Reduce(move, x = x, init = list(x = 0, y = 0))
final_pos$x
final_pos$y

final_pos$x *
final_pos$y



#part 2
forward <- function(position = list(x = 0, y = 0, aim = 0), dist ){
  position$x <- position$x + dist
  position$y <- position$y + (dist * position$aim)
  position
}

down <- function(position = list(x = 0, y = 0, aim = 0), dist ){
  position$aim <- position$aim + dist
  position
}

up <- function(position = list(x = 0, y = 0, aim = 0), dist ){
  position$aim <- position$aim - dist
  position
}

#test

Reduce(move, x = test, init = list(x = 0, y = 0, aim = 0))

# part 2 answer

final_pos <- Reduce(move, x = x, init = list(x = 0, y = 0, aim = 0))
final_pos$x
final_pos$y

final_pos$x *
  final_pos$y


