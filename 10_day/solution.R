word <- c("{([(<{}[<>[]}>{[]{[(<()>") |>
  stringr::str_split("", simplify = TRUE)
word <- c("[(()[<>])]({[<{<<[]>>(") |>
  stringr::str_split("", simplify = TRUE)

words <- readLines("10_day/data/data") |>
  stringr::str_split("", simplify = TRUE)

openr <- c("(", "{", "[", "<")
closr <- openr
names(closr) <- c(")", "}", "]", ">")

illegal <- c(3,57, 1197, 25137 ) 
names(illegal) <- c(")", "]", "}", ">")

closing <- c(1, 2, 3, 4)
names(closing) <- c("(", "[", "{", "<" ) 

tokens <- character()

scan_tokens_1 <- function(word, tokens){
 
  
 if(length(word) == 0) return(0)  
 if(word[1] %in% openr){
   tokens <- c(word[1], tokens)

   return(list(word = word[-1], tokens =  tokens))
 }
 if (closr[word[1]] == tokens[1]){
   return(list(word = word[-1], tokens = tokens[-1]))
 }

 return(illegal[word[1]])
}


 parse_1 <- function(word){
word <- word[!word == ""]
x <- list(word = word, tokens = character())
while(is.list(x)){
  x <- scan_tokens_1(x$word, x$tokens)
  
}
x
 }
 
 
sum(apply(words, 1, parse_1))



scan_tokens_2 <- function(word, tokens){
  
 
  if(length(word) == 0 && length(tokens) == 0) return(integer())
  if(length(word) == 0) return(completion_score(tokens))
  if(word[1] %in% openr){
    tokens <- c(word[1], tokens)
    
    return(list(word = word[-1], tokens =  tokens))
  }
  if (closr[word[1]] == tokens[1]){
    return(list(word = word[-1], tokens = tokens[-1]))
  }
  
  return(integer())
}


parse_2 <- function(word){
  word <- word[!word == ""]
  x <- list(word = word, tokens = character())
  while(is.list(x)){
    x <- scan_tokens_2(x$word, x$tokens)
    
  }
  x
}

completion_score <- function(tokens){
   point_values <- closing[tokens]
   
 
     Reduce(function(total, x){total * 5 + x}, point_values, init = 0)
}

options(scipen = 999) 
 apply(words, 1, parse_2) |> unlist()  |>
  median()

 


