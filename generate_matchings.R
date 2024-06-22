## https://math.stackexchange.com/questions/3093225/an-efficient-approach-to-combinations-of-pairs-in-groups-without-repetitions



# Function to generate matchings

generate_matchings <- function(n) {
  
  ## create a vector of labels
  labels <- c("∞", 0:(n-2))
  labels <- c(n, seq_len(n-1)) - 1
  
  ## initialize an empty list to store matchings
  matchings <- vector("list", n-1)
  
  ## check if n is odd
  if (n %% 2 != 0) {
    
    ## generate the first grouping for odd number of students
    first_grouping <- list(
      c(n-1, 0),
      c(1, (n-2)),
      c(2, (n-3)),
      c(3, 4, 5)
    )
    
    ## store the first grouping
    matchings[[1]] <- first_grouping
    
    ## generate subsequent groupings by rotating the labels
    for (i in 1:(n-2)) {
      previous_grouping <- matchings[[i]]
      new_grouping <- lapply(previous_grouping, function(pair) {
        sapply(pair, function(label) {
          if (label == (n-1)) {
            return(n-1)
          } else {
            return((as.numeric(label) + 1) %% (n-1))
          }
        })
      })
      matchings[[i + 1]] <- new_grouping
    }
  } else {
    
    ## generate matchings for even number of students
    for (i in 0:(n-2)) {
      matching <- list()
      matching[[1]] <- c(n-1, i)
      for (k in 1:((n-2) / 2)) {
        student1 <- (i + k) %% (n-1)
        student2 <- (i - k) %% (n-1)
        if (student2 < 0) student2 <- student2 + (n-1)
        matching[[length(matching) + 1]] <- c(student1, student2)
      }
      matchings[[i + 1]] <- matching
    }
  }
  ## convert to base 1 index
  matchings <- lapply(matchings, function(x){
    
    lapply(x, function(y) y + 1)
  })
  return(matchings)
}



# Function to print matchings

print_matchings <- function(matchings) {
  for (i in 1:length(matchings)) {
    cat("i =", i-1, ":")
    for (group in matchings[[i]]) {
      cat(" (", paste(group, collapse=","), ")", sep = "")
    }
    cat("\n")
  }
}



## example usage
n <- 9
matchings <- generate_matchings(n)
print_matchings(matchings)

## check correctness
print(all(sapply(matchings, function(x){
  
  setequal(ux <- unlist(x), seq_len(max(ux)))
})))
