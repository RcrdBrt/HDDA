FIND_EXTREME_OBSERVARION <- function(x,sd_factor=3){
  which(x>mean(x)+sd_factor*sd(x) | x<mean(x)-sd_factor*sd(x))
}

out_idxs <- c()

for (i in 1:length(numeric_variables)){
  out_idxs <- c(out_idxs, FIND_EXTREME_OBSERVARION(acc[,numeric_variables[i]]))
}

out_idxs <- unique(out_idxs)

acc <- acc[-out_idxs,]
