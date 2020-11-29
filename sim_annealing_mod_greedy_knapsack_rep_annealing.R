#' apply simulated annealing to mod greedy knapsack algorithm. 
#'
#' @param knapsack the knapsack instance
#' @param capacity the capacity of the knapsack
#' @param T0 initial temperature
#' @param alpha the temperature reduction factor per iteration
#'
#' @return the solution
#' @export
sim_annealing_mod_greedy <- function(knapsack, capacity, T0 = 1e2, alpha = 0.9) {
  # code see slide 53 from week 10
  moves_allowed <- c("replace", "remove", "add")
  cur_sol <- mod_greedy(knapsack = knapsack, capacity = capacity)
  best_sol <- cur_sol
  T <- T0
  cnt_non_improving_subsequent_iterations <- 0
  while(TRUE) {
    # choose move randomly
    move <- sample(moves_allowed, size = 1)
    # apply move
    new_sol <- apply_move(cur_sol, move, knapsack, capacity)
    dist <- new_sol$total_value - cur_sol$total_value
    # the other way round, since we maximize value rather than minimize it
    if (dist > 0) {
      # improving moves are always allowed
      cur_sol <- new_sol
    } else {
      # Generate random number in [0,1]
      u <- runif(1)
      if (exp(dist/T) > u) {
        # if Temp allows accepting bad value
        cur_sol <- new_sol
      }
    }
    if (cur_sol$total_value > best_sol$total_value) {
      best_sol <- cur_sol
      # reset unsuccessful iteration counter
      cnt_non_improving_subsequent_iterations <- 0
    } else {
      # increase unsuccessful iteration counter
      cnt_non_improving_subsequent_iterations <- 
        cnt_non_improving_subsequent_iterations +1
    }
    T <- T / (1 + alpha*T)
    if (cnt_non_improving_subsequent_iterations > 300)
      break
  }
  return(best_sol)
}

#' applies the move move to the current solution
#'
#' @param cur_sol the current solution
#' @param move any of c("replace", "remove", "add")
#' @param knapsack the knapsack instance
#' @param capacity the knapsack capacity
#'
#' @return the modified solution
#' @export
apply_move <- function(cur_sol, move, knapsack, capacity) {
  if (move == "replace" && length(cur_sol$items_packed)>0) {
    # choose an arbitrary upper limit for unsuccessful tries to prevent 
    # dead loop
    for (i in 1:50) {
      # exit, if no other element left
      if (length(knapsack$item_id[-cur_sol$items_packed]) == 0)
        break
      # determine item to remove
      el_to_replace <- cur_sol$items_packed[sample(1:length(cur_sol$items_packed),
                                                   1, replace = F)]
      obj_to_replace <- knapsack[knapsack$item_id == el_to_replace, ]
      # determine new item to insert
      el_to_insert <- sample(knapsack$item_id[-cur_sol$items_packed], 1)
      obj_to_insert <- knapsack[knapsack$item_id == el_to_insert, ]
      # now insert new element at old position
      items_packed <- replace(cur_sol$items_packed, 
                                      cur_sol$items_packed == 
                                      el_to_replace, el_to_insert)
      total_value <- cur_sol$total_value - obj_to_replace$value + 
                             obj_to_insert$value
      packed_weight <- cur_sol$packed_weight - obj_to_replace$weight + 
                             obj_to_insert$weight
      # make sure capacity is not exceeded
      if (packed_weight <= capacity) {
        cur_sol$items_packed <- items_packed
        cur_sol$total_value <- total_value
        cur_sol$packed_weight <- packed_weight
        break
      }
    } 
  } else if (move == "remove" && length(cur_sol$items_packed)>0) {
    # determine item to remove
    el_to_replace <- cur_sol$items_packed[sample(1:length(cur_sol$items_packed),
                                                 1, replace = F)]
    obj_to_replace <- knapsack[knapsack$item_id == el_to_replace, ]
    cur_sol$items_packed <- cur_sol$items_packed[cur_sol$items_packed != el_to_replace]
    cur_sol$total_value <- cur_sol$total_value - obj_to_replace$value
    cur_sol$packed_weight <- cur_sol$packed_weight - obj_to_replace$weight
  } else if (move == "add" && cur_sol$packed_weight < capacity ) {
    # choose an arbitrary upper limit for unsuccessful tries to prevent 
    # dead loop
    for (i in 1:50) {
      # exit, if no other element left
      if (length(knapsack$item_id[-cur_sol$items_packed]) == 0)
        break
      # determine new item to insert
      el_to_insert <- sample(knapsack$item_id[-cur_sol$items_packed], 1)
      obj_to_insert <- knapsack[knapsack$item_id == el_to_insert, ]
      # now insert new element at old position
      items_packed <- c(cur_sol$items_packed, el_to_insert)
      total_value <- cur_sol$total_value + obj_to_insert$value
      packed_weight <- cur_sol$packed_weight + obj_to_insert$weight
      # make sure capacity is not exceeded
      if (packed_weight <= capacity) {
        cur_sol$items_packed <- items_packed
        cur_sol$total_value <- total_value
        cur_sol$packed_weight <- packed_weight
        break
      }
    } 
  }
  return (cur_sol)
}


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

rep_annealing <- function (knapsack, capacity, numrep = 100) {
  best_sol <- NULL
  for (i in 1:numrep) {
    cur_sol <- sim_annealing_mod_greedy(knapsack = knapsack, capacity = capacity)
    if (is.null(best_sol) || best_sol$total_value < cur_sol$total_value) {
      best_sol <- cur_sol
    }
    print(paste0(i, "-", best_sol$total_value))
  }
  return (best_sol)
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

# solve it: annealing
a <- sim_annealing_mod_greedy(knapsack = knapsack, capacity = capacity)
print(a)

# solve it: rep-annealing
a <- rep_annealing(knapsack = knapsack, capacity = capacity, numrep = 100)
print(a)
