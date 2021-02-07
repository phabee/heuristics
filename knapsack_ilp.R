library(lpSolve)

knapsack_ilp <- function(knapsack, capacity) {
  lpmod <- list(
    # objective function (maximize value)
    obj = knapsack$value,
    # constraints sum of weights < capacity
    con = matrix(knapsack$weight,
      nrow = 1,
      byrow = T
    ),
    # signs
    dir = c("<="),
    # 'right hand side' der Ungl.
    rhs = capacity,
    # enumerate binary decision variables  
    binary.vec = 1:nrow(knapsack)
  )
    lsg <-
      lpSolve::lp("max",
                  lpmod$obj,
                  lpmod$con,
                  lpmod$dir,
                  lpmod$rhs,
                  binary.vec = lpmod$binary.vec)
    
    return (lsg)
}
          
# load knapsack instance KSP8
dat <- read.table(file = "KS_P08.txt")
n <- dat[1,1]
capacity <- dat[2,1]
weights <- dat[3:(n+2),1]
values <- dat[(n+3):nrow(dat),1]

knapsack <- data.frame(item_id=1:n, weight=weights, value=values)

# solve it: ILP
a <- knapsack_ilp(knapsack = knapsack, capacity = capacity)
print(a)