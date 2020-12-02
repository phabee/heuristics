#' start a tabu search with steepest descent and a chessboard-neighborhood 
#' within the given data-field applying best improvement strategy
#'
#' @param dat the data-field
#' @param x0 start x-coordinate in data-field where search is started
#' @param y0 start y-coordinate in data-field where search is started
#' @param maxit maximal number of iterations
#' @param minimizaation TRUE to minimize values, FALSE else
#' @param tabu_size size of memory to prevent opposite moves
#'
#' @return the best solution found
#' @export
tabu_search <- function(dat, x0, y0, maxit = 100, minimization = TRUE, tabu_size = 1) {
  num_it <- 0
  cur_cand <- list(x=x0, y=y0, val=get_mat_value(dat, x0, y0), op=0)
  best_cand <- cur_cand
  last_ops <- c()
  while(num_it < maxit) {
    # cur_val <- get_mat_value(dat, cur_x, cur_y)
    # best_val <- cur_val
    # calc neighborhood
    nb <- neighborhood(dat, cur_cand$x, cur_cand$y)
    next_cand <- NULL
    # check every neighbor solution attainable f
    for (i in 1:length(nb)) {
      cand <- nb[[i]]
      # check, if operation not in tabu list
      if (!get_opposite_move(cand$op) %in% tail(last_ops, tabu_size)) {
        # move allowed
        if (is.null(next_cand))
          next_cand <- cand
        # check if it's better than tha last 'next' candidate and update, if so
        # then check if it's better than best candiate and update that, if so
        if (minimization) {
          if (cand$val < next_cand$val) 
            next_cand <- cand
          if (cand$val < best_cand$val) 
            best_cand <- cand
        } else {
          if (cand$val > next_cand$val) 
            next_cand <- cand
          if (cand$val > best_cand$val) 
            best_cand <- cand
        }
      }  
    }
    cur_cand <- next_cand
    last_ops <- tail(c(last_ops, cur_cand$op), tabu_size)
    cat("next val: ", cur_cand$val, "\n")
    num_it <- num_it + 1
  }
  return(best_cand)
}

#' calculate opposite move
#'
#' @param move one of c(1,2,3,4)
#'
#' @return the opposite move
#' @export
get_opposite_move <- function(move) {
  opp_move <- c(3, 4, 1, 2)
  return(opp_move[move])
}

#' calculate the neighborhood of the given coordinate 
#'
#' @param dat field of data
#' @param x x-coord of current point
#' @param y y-coord of current point
#'
#' @return a list of lists containing the neighborhood
#' @export
neighborhood <- function(dat, x = -7, y = 7) {
  x_id <- which(dat[1,] == x)
  y_id <- which(dat[,1] == y)
  cnt <- 1
  nb <- list()
  if (length(x_id) == 0 || length(y_id) == 0) 
    stop("didn't find location in matrix provided.")
  for (op in 1:4) {
    x_temp_id <- x_id; y_temp_id <- y_id
    if (op == 1) {
      x_temp_id <- x_temp_id + 1
    } else if (op == 2) {
      y_temp_id <- y_temp_id - 1
    } else if (op == 3) {
      x_temp_id <- x_temp_id - 1
    } else {
      y_temp_id <- y_temp_id + 1
    }
    if (x_temp_id > 1 && y_temp_id > 1 && x_temp_id <= dim(dat)[2] && 
        y_temp_id <= dim(dat)[1]) {
      nb[[cnt]] <- list(x=dat[1, x_temp_id], y=dat[y_temp_id, 1], 
                              val=dat[y_temp_id, x_temp_id], op=op)
      cnt <- cnt + 1
    }
  }
  return(nb)
}

#' return matrix value at position indicated by x, y if x and y were taken as 
#' coordinates found in the first column and first row of the given matrix
#'
#' @param dat the matrix / data-field
#' @param x the x coordinate of the value to be returned
#' @param y the y coordinate of the value to be returned
#'
#' @return the value at coordinate x/y of the data-field
#' @export
get_mat_value <- function(dat, x, y) {
  x_id <- which(dat[1,] == x)
  y_id <- which(dat[,1] == y)
  if (length(x_id) == 0 || length(y_id) == 0) 
    stop("didn't find location in matrix provided.")
  return (dat[y_id, x_id])
}

dat <- matrix(c(NA, -7, -6, - 5, - 4, - 3, - 2, - 1, 0, 1, 2, 3, 4, 5, 6, 7, 
                -6,248, 216, 210, 222, 230, 234, 256, 304, 336, 372, 428, 495,
                585, 650, 754, -5, 193, 175, 157, 166, 174, 181, 215, 249, 295,
                329, 382, 454, 539, 597, 707, -4, 138, 144, 126, 116, 124, 150,
                184, 194, 250, 305, 361, 425, 480, 566, 646, -3, 123, 89, 85, 97,
                105, 109, 129, 179, 209, 246, 302, 368, 458, 525, 627, -2, 92,
                58, 70, 70, 78, 94, 98, 148, 168, 223, 282, 339, 413, 510, 582,
                -1, 68, 34, 46, 46, 54, 70, 74, 124, 144, 199, 258, 315, 388,
                486, 558, 0, 51, 17, 14, 25, 33, 38, 57, 107, 136, 174, 230, 
                296, 386, 454, 555, 1, 18, 25, 5, -4,3, 29, 65, 74, 131, 185, 
                240, 305, 361, 445, 527, 2, 27, 6, -10, 0, 8, 13, 46, 83, 126,
                160, 213, 284, 371, 429, 539, 3, 33, 0, -3, 7, 15, 20, 39, 89, 
                118, 156, 212, 278, 368, 436, 537, 4, 33, 12, -4, 6, 14, 19, 52,
                89, 132, 166, 219, 290, 377, 435, 545, 5, 30, 37, 17, 7, 15, 41, 
                77, 86, 143, 197, 252, 317, 373, 457, 539, 6, 69, 35, 32, 43, 51,
                56, 75, 125, 154, 192, 248, 314, 404, 472, 573, 7, 92, 58, 70, 70,
                78, 94, 98, 148, 168, 223, 282, 339, 412, 510, 582), byrow=T, nrow=15)

# invoke tabu search heuristic
sol <- tabu_search(dat, x0 = -7, y0 = -6, tabu = 1, maxit = 25)
print(sol)
sol <- tabu_search(dat, x0 = -7, y0 = 7, tabu = 3, maxit = 25)
print(sol)
