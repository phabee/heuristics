#' return either best greedy combination of objects with highest value per weight 
#' or the most valuable single item fitting into the knapsack. 
#'
#' @param knapsack 
#' @param capacity 
#'
#' @return the solution
mod_greedy <- function(knapsack, capacity) {
  # 1) "Smarter greedy"
  # sort by value per weight
  knapsack <- knapsack[order(knapsack$vpw, decreasing = TRUE),]
  # now fill knapsack with all items taken in this sequence
  # and still fitting into the knapsack
  packed_items <- c()
  packed_weight <- 0
  packed_value <- 0
  for (id in 1:nrow(knapsack)) {
    item <- knapsack[id, ]
    left_capacity <- capacity - packed_weight
    # check if next item in the sequence still fits into knapsack
    if (left_capacity > 0 && item$weight <= left_capacity) {
      packed_items <- c(packed_items, item$item_id)
      packed_weight <- packed_weight + item$weight
      packed_value <- packed_value + item$value
    } 
  }

  # 2) Addition to "modified greedy"
  # determine most valuable single item fitting into knapsack
  knapsack <- knapsack[order(knapsack$value, decreasing = TRUE),]
  best_single_item <- 0
  best_single_value <- 0
  best_single_weight <- 0
  for (id in 1:nrow(knapsack)) {
    item <- knapsack[id, ]
    # check if next item in the sequence still fits into knapsack
    if (item$weight <= capacity) {
      best_single_item <- item$item_id
      best_single_value <- item$value
      best_single_weight <- item$weight
      break
    } 
  }
  
  # now return best configuration
  if (best_single_value < packed_value) {
    ret_val <- list(items_packed = packed_items, total_value = packed_value,
                    packed_weight = packed_weight)
  } else {
    ret_val <- list(items_packed = c(best_single_item), total_value = best_single_value,
                    packed_weight = best_single_weight)
  }
  return(ret_val)
}

# load knapsack instance KSP8
dat <- read.table(file = "KS_P08.txt")
n <- dat[1,1]
capacity <- dat[2,1]
weights <- dat[3:(n+2),1]
values <- dat[(n+3):nrow(dat),1]

knapsack <- data.frame(item_id=1:n, weight=weights, value=values, 
                       vpw=values/weights)

# solve it: mod greedy
a <- mod_greedy(knapsack = knapsack, capacity = capacity)
print(a)
