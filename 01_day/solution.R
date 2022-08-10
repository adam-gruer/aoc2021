x <- scan("01_day/data/part1.txt")
# part 1

sum(diff(x, 1) > 0)

# oart 2
n <- length(x)

sum(diff(sapply(1:(n-2), function(i) {sum(x[i:(i+2)])}),1) > 0)
sum(diff(x, lag = 3) > 0)
t <- c(199 ,
200 ,
208 ,
210 ,
200 ,
207 ,
240 ,
269 ,
260 ,
263)

1:3
t[1:3]
t[2:4]
t[3:5]
sum(diff(sapply(1:8, function(x) {sum(t[x:(x+2)])}),1) > 0)
