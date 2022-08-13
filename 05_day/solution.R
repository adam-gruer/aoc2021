 
  lines <- pipe('sed "s/->//g; s/  /,/g" 05_day/data/data') |> 
    scan(sep = ",") |>
    matrix(ncol = 4, byrow = TRUE)
 
 

isHorizontal <- function(line){
  line[2] == line[4]
  
}

isVertical <-  function(line){
  line[1] == line[3]
}

isHorizontalOrVertical <- function(line){
  isHorizontal(line) || isVertical(line)
}

points <- function(line){
  if (isHorizontal(line)){
    xs <- line[1]:line[3]
    ys <- rep_len(line[2], length(xs))
    
    
  } else if(isVertical(line)){
    ys <- line[2]:line[4]
    xs <- rep_len(line[1], length(ys))
  }
  
  matrix(c(xs, ys), ncol = 2)
  
}

 

points.m <- lines[apply(lines, 1, isHorizontalOrVertical), ] |>
  apply(1, points) |>
  purrr::reduce(rbind)
  duplicated(points.m) -> dupes

  nrow(unique(points.m[dupes, ]))
  
  

