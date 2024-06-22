## https://math.stackexchange.com/questions/3093225/an-efficient-approach-to-combinations-of-pairs-in-groups-without-repetitions



# Function to generate matchings

generate_matchings <- function(n) {
  
  ## require n >= 4
  if (n < 4){
    
    stop("Trivial for n < 4.")
  }
  ## create a vector of labels
  labels <- c(n, seq_len(n-1)) - 1
  
  ## initialize an empty list to store matchings
  matchings <- vector("list", n-1)
  
  ## check if n is odd
  if (n %% 2 != 0) {
    
    ## generate the first grouping for odd number of students
    first_grouping <- list(
      c(n-1, 0)
    )
    for (i in seq_len(n-1)){
      
      remainder <- setdiff(seq(0, n-1), unlist(first_grouping))
      
      if (length(remainder) == 3) break
      
      first_grouping <- c(first_grouping, list(
        c(i, n-i-1)
      ))
    }
    first_grouping <- c(first_grouping, list(
      remainder
    ))
    
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



## create vector of names of people
people <- sprintf("Name%s", seq_len(19))

## convert to matchings by getting index groups and converting to names
matchings <- lapply(generate_matchings(length(people)), function(x){
  
  lapply(x, function(y){
    
    people[y]
  })
})
# print_matchings(matchings)

## get number of columns (2 if even, 3 if odd)
ncols <- max(sapply(matchings, function(x) sapply(x, length)))

## convert matchings to table format
match_table <- do.call(rbind, lapply(seq_len(length(matchings)), function(x){
  
  ## create one row for each group, with blank value if necessary to match dimensions
  dfx <- do.call(rbind, lapply(matchings[[x]], function(y){
    
    ## add blank values to get length ncols
    if (length(y) < ncols){
      
      return(c(y, rep("", ncols - length(y))))
      
    } else{
      
      return(y)
    }
  }))
  ## add header on top: blank column, and a row with title "Matchings #"
  dfx <- rbind(matrix(c(rep("", ncols),
                        sprintf("Matchings %s", x), rep("", ncols - 1)), 
                      ncol = ncols, byrow = TRUE),
               dfx)
  
  return(dfx)
}))

## add people to the top section of match_table, with blank buffer
match_table <- rbind(cbind(matrix(c("People", people), ncol = 1),
                           matrix(rep("", (ncols - 1) * (length(people) + 1)), ncol = ncols - 1)),
                     match_table)

print(match_table)
