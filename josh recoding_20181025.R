n <- 100

dt <- data.frame(pt1 = sample(c(0,1), size = n, replace = T), 
                 ft1 = sample(c(0,1), size = n, replace = T),
                 pt2 = sample(c(0,1), size = n, replace = T), 
                 ft2 = sample(c(0,1), size = n, replace = T)
                 )

head(dt)


new_var <- function(pt_var, ft_var){
  ifelse(ft_var == 1, 2, 
         ifelse(pt_var == 1, 1, 0)
         )
}

for(i in 1:2){
  dt[,paste0("study_w",i)] <-  new_var(pt_var = dt[,paste0("pt",i)], ft_var = dt[,paste0("ft",i)])
}

head(dt)
