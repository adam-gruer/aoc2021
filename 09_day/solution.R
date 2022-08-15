input <- readLines("09_day/data/data") |>
  stringr::str_split("", simplify = TRUE) 

c <- ncol(input)
r<- nrow(input)

input <-   as.integer(input)|>
  matrix(ncol = c)



top_row <- seq(1, by = r, length.out = c)
right_column <- seq(from = r * (c-1) + 1,
                    to = c*r)
left_column <- seq(1, r)
bottom_row <- seq(r, by = r, length.out = c)



is_minimum <- function(i, input){
 
  up <-  if(i %in% top_row){0} else i - 1
  right <- if(i %in% right_column){0} else i+r
  down <- if(i %in% bottom_row ){0}else{i + 1}
  left <- if(i %in% left_column ){0}else{i - r}
  
  x <- input[i]
  u <- input[up]
  r <- input[right]
  d <- input[down]
  l <- input[left]
  
  which.min(c(x,u,r,d,l)) == 1
 
}

input[-1, ]
(input[sapply(seq_along(input), 
             is_minimum, 
             input = input)] + 1)  |>
  sum()



# For a given matrix, generate a matrix shifted by one row/column
# up/down or left/right.
shift <- function(m, direction = c("up", "down", "left", "right",
                                   "upleft", "upright", "downleft", "downright"),
                  x = Inf) {
  direction <- match.arg(direction)
  
  m <- switch(direction,
              up    = rbind(m[-1, ], x),
              down  = rbind(x, m[-nrow(m), ]),
              right = cbind(m[, -1], x),
              left  = cbind(x, m[, -ncol(m)]),
              upleft = cbind(rbind(m[-1, -1], x), x),
              upright = cbind(x, rbind(m[-1, -ncol(m)], x)),
              downleft = cbind(rbind(x, m[-nrow(m), -1]), x),
              downright = cbind(x, rbind(x, m[-nrow(m), -ncol(m)])))
  
  rownames(m) <- colnames(m) <- NULL
  m
}

# Detect lowpoints by finding those positions of a matrix which carry a number
# smaller then all four shifted matrices.
find_lowpoints <- function(heightmap) {
 
  (
    
    heightmap < shift(heightmap, "up") &
      heightmap < shift(heightmap, "down") &
      heightmap < shift(heightmap, "right") &
      heightmap < shift(heightmap, "left")
  ) |>
    which(arr.ind = TRUE)
}
sum(input[find_lowpoints(input)] + 1)

