## https://math.stackexchange.com/questions/3093225/an-efficient-approach-to-combinations-of-pairs-in-groups-without-repetitions



# Function to generate matchings

generate_matchings <- function(n) {
  
  # Create a vector of labels
  labels <- c("∞", 0:(n-2))
  
  # Initialize an empty list to store matchings
  matchings <- vector("list", n-1)
  
  # Check if n is odd
  if (n %% 2 != 0) {
    # Generate the first grouping for odd number of students
    first_grouping <- list(
      c("∞", 0),
      c(1, (n-2)),
      c(2, (n-3)),
      c(3, 4, 5)
    )
    
    # Store the first grouping
    matchings[[1]] <- first_grouping
    
    # Generate subsequent groupings by rotating the labels
    for (i in 1:(n-2)) {
      previous_grouping <- matchings[[i]]
      new_grouping <- lapply(previous_grouping, function(pair) {
        sapply(pair, function(label) {
          if (label == "∞") {
            return("∞")
          } else {
            return((as.numeric(label) + 1) %% (n-1))
          }
        })
      })
      matchings[[i + 1]] <- new_grouping
    }
  } else {
    # Generate matchings for even number of students
    for (i in 0:(n-2)) {
      matching <- list()
      matching[[1]] <- c("∞", i)
      for (k in 1:((n-2) / 2)) {
        student1 <- (i + k) %% (n-1)
        student2 <- (i - k) %% (n-1)
        if (student2 < 0) student2 <- student2 + (n-1)
        matching[[length(matching) + 1]] <- c(student1, student2)
      }
      matchings[[i + 1]] <- matching
    }
  }
  
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

# Example usage
n <- 9
matchings <- generate_matchings(n)
print_matchings(matchings)
