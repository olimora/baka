train_set <- data.frame(praca = as.numeric(c(1:10)), 
                       gho = as.numeric(c(1:10)),
                       obl = as.numeric(c(1:10)))
chosen_one <- data.frame(praca = 50, gho = 50, obl = 50)
scale <- c(praca = 100, gho = 100, obl = 100)
diff <- c(1:10)

train_set <- cbind(train_set, diff)

tryy <- function(jebem) {
  return(1 + 1 + 1)
}

train_set$diff <- apply(train_set, 1, function(x) tryy(train_set) )


try <- function(cho, tr, pod_prac, pod_gho, pod_obl, scale) {
  ret <- (abs(cho[['praca']] - tr[['praca']]) * 100 / scale[['praca']])
  +
    (abs(cho[['gho']] - tr[['gho']]) * 100 / scale[['gho']]) 
  + 
    (abs(cho[['obl']] - tr[['obl']]) * 100 / scale[['obl']]) 
  return (ret)
}


try2 <- function() {
  ret <- (abs(chosen_one[['praca']] - train_set[['praca']]) * 100 / scale[['praca']])
  +
    (abs(chosen_one[['gho']] - train_set[['gho']]) * 100 / scale[['gho']]) 
  + 
    (abs(chosen_one[['obl']] - train_set[['obl']]) * 100 / scale[['obl']]) 
  return (ret)
}


