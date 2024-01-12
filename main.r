# takes list returns int(s)
modes <- function(v) {
  ux <- unique(v)
  tab <- tabulate(match(v, ux))
  ux[tab == max(tab)]
}

# takes two ints return int
skip_number <- function(N, n){
  return (round(N/n))
}

"how often something happens divided by all outcomes
won 9 games from a total of 12 games played
frequency of winning is 9
relative frequency of winning is 9/12 = 75%
takes two ints returns int"
relative_frequency <- function(class_freq, total_freq){
  return (round(class_freq/total_freq))
}

# (R) - takes list return int
range <- function(v){
  return (max(v) - min(v))
}

# (s^2) - takes list returns int
var <- function(v){
  summation <- 0
  x_bar = mean(v)
  for (x in v){
    summation = summation + ((x - x_bar) ^ 2)
  }
  return (summation/(length(v) - 1))
}

# (s) - takes list returns int
std <- function(v){
  return (sqrt(var(v)))
}

# sample (z) = (x - mean)/std 
# population (Z) = (x - mu)/sigma
z_score <- function(x, v){
  return ((x - mean(v))/std(v))
}

iqr <- function(v){
  if(is.unsorted(v)){
    v <- sort(v)
  }
  return (v[3 * (length(v) + 1)/4] - v[(length(v) + 1)/4])
}

v <- c(2, 1, 2, 3, 1, 2, 3, 4, 1, 5, 5, 3, 2, 3)

result <- modes(v)
print(result)

