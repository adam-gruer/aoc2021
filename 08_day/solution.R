data <- scan("08_day/data/data", what = character(), sep = "|")
data <- gsub("^\\s|\\s$","",data)
data <- matrix(data, ncol = 2, byrow = TRUE)



strsplit(data[,2], " ") |>
  lapply(nchar) |>
  sapply(function(x) x %in% c(2,3,4,7)) |>
  sum()




is_1 <- function(x) nchar(x) == 2
is_4 <- function(x) nchar(x) == 4
is_7 <- function(x) nchar(x) == 3
is_8 <- function(x) nchar(x) == 7

is_a <- function(patterns){
  
  one <- patterns[is_1(patterns)] |>
    stringr::str_split("",simplify = TRUE)
  
  seven <- patterns[is_7(patterns)] |>
    stringr::str_split("",simplify = TRUE)
  
  setdiff(seven, one)
  
}

is_g <- function(patterns, a){
  over_4_segments <- patterns[nchar(patterns) > 4]
  gsub(a,"", over_4_segments) |>
    paste0(collapse = "") |> 
    stringr::str_split("",simplify = TRUE) |>
    table() |>
    which.max() |>
    names()


  
}

is_e <- function(patterns, a, g){
  eight <- patterns[is_8(patterns)] |>
    stringr::str_split("", simplify = TRUE )
  four <- patterns[is_4(patterns)] |>
    stringr::str_split("", simplify = TRUE )
  setdiff(eight, c(four, a, g))

  
}

is_9 <- function(patterns, e){
  eight <- patterns[is_8(patterns)] |>
    stringr::str_split("", simplify = TRUE )
  nine <- setdiff(eight, e)
  stringr::str_split(patterns,"",simplify = TRUE) |>
    apply(1,function(set){setequal(set[nzchar(set)],nine)})

}

is_0 <- function(patterns,e,g){
  seven <- patterns[is_7(patterns)]|>
    stringr::str_split("", simplify = TRUE )
  
  x <- c(seven, e,g)
  zero_or_six <- patterns[nchar(patterns) == 6 & !is_9(patterns,e)]
  zero <- zero_or_six[stringr::str_split(zero_or_six,"",simplify = TRUE) |>
    apply(1,function(set){length(setdiff(set[nzchar(set)],x)) == 1})]
 patterns == zero
  
}

is_6 <- function(patterns,e,g){
  nchar(patterns) == 6 &
    !is_9(patterns,e) & 
    !is_0(patterns, e, g)
  
}

is_3 <- function(patterns){
  two_three_five <-  patterns[nchar(patterns) == 5 ] |>
    stringr::str_split("", simplify = TRUE )
  one <- patterns[is_1(patterns)] |>
    stringr::str_split("", simplify = TRUE )
 three <-  apply(two_three_five,1,function(set){
    length(intersect(one, set)) == 2
  })
  three <- paste0(two_three_five[three,], collapse = "")
  patterns == three
}


is_2 <- function(patterns, e){
   nchar(patterns) == 5  &
      !is_3(patterns) &
      stringr::str_detect(patterns, e)
}

is_5 <- function(patterns, e){
  nchar(patterns) == 5  &
    !is_3(patterns) &
    !stringr::str_detect(patterns, e)
}

is_1(data[1,1])
data[1,1]

patterns <- stringr::str_split(data[1,1],
                               " ",
                               simplify = TRUE )

rearrange <- function(word){
  stringr::str_split(word, "", simplify = TRUE) |>
    sort() |>
    paste0(collapse = "")
}

decode <- function(patterns){
  a <- is_a(patterns)
  g <- is_g(patterns, a)
  e <- is_e(patterns, a, g)
  
  lookup <- 0:9
  names(lookup) <- sapply(c( patterns[is_0(patterns, e, g)],
                      patterns[is_1(patterns)],
                      patterns[is_2(patterns, e)],
                      patterns[is_3(patterns)],
                      patterns[is_4(patterns)],
                      patterns[is_5(patterns, e)],
                      patterns[is_6(patterns, e, g)],
                      patterns[is_7(patterns)],
                      patterns[is_8(patterns)],
                      patterns[is_9(patterns, e)]),
                      rearrange)
  lookup
}


get_display <- function(data){
patterns <- stringr::str_split(data[1],
                     " ",
                     simplify = TRUE )

lookup <- decode(patterns)

display <- stringr::str_split(data[2],
                              " ",
                              simplify = TRUE ) |>
            sapply(rearrange)

value <- lookup[display]
value[1] * 1000 +
  value[2] * 100 +
  value[3]* 10 +
  value[4]
}

apply(data, 1, get_display) |>
  sum()



