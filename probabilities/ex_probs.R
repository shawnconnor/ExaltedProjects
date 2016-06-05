library(discreteRV)

build_a_die <- function(tn = 7, doubles = c(10), reroll_all = NA, reroll_once = NA) {
    face <- c(1:10)
    result <- c(0,0,0,0,0,0,0,0,0,0)
    odds <- c(0,0,0,0,0,0,0,0,0,0)
    die <- data.frame(face, result, odds)
    die[die$face >= tn,]$result <- 1
    die[die$face %in% doubles,]$result <- 2
    if (!is.na(reroll_all)) {
        die <- die[-reroll_all,]
  }
  
  die$odds <- 1/length(die$face)
  if (!is.na(reroll_once)) {
    die[die$face %in% reroll_once,]$odds <- 0
    die$odds <- die$odds+(sum(die$face %in% reroll_once)/length(die$face)^2)
    }
  return(die)
}

my_die <- build_a_die()
a_roll <- RV(my_die$result, my_die$odds)

results<-list()
results[[1]] <- data.frame(probs(a_roll))
foreach(i=2:25) %do% {
    results[[i]] <- data.frame(probs(SofIID(a_roll, n = i, fractions = FALSE)))
  }
