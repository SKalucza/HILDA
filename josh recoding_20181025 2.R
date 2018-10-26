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



ndt <- data.frame(parent = character(), child = character())

for(i in 1:nrow(M)){
  
  for(j in 2:ncol(M)){
    
    if(M[i,j] == 4){
      
      new_row <- data.frame(parent = M[i,1], child = colname(M)[j])
      
      ndt <- rbind(ndt, new_row)
      
    }
    
  }
  
}



