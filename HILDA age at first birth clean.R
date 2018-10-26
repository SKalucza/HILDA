# Load hilda1.rda

library(dplyr)

arg <- select(hilda1, xwaveid, contains("arg"))

arg <- select(arg, xwaveid, 192:211) # select relationship vars

ndt <- data.frame(xwaveid = character(), child = character())

for(i in 1:nrow(arg)){
  
  for(j in 2:ncol(arg)){
    
    if(arg[i,j] == 4){
      
      new_row <- data.frame(xwaveid = arg[i,1], child = colnames(arg)[j])
      
      ndt <- rbind(ndt, new_row)
      
    }
    
  }
  
}

rm(new_row)
rm(i)
rm(j)
rm(study_var)

# add on ages of children

ahgage <-  select(hilda1, xwaveid, contains("ahgage"))

par <- left_join(ndt, ahgage, by = "xwaveid")

# assign right age to right child

par$child.age[par$child == "arg01"] <- par$ahgage1[par$child == "arg01"]
par$child.age[par$child == "arg02"] <- par$ahgage2[par$child == "arg02"]
par$child.age[par$child == "arg03"] <- par$ahgage3[par$child == "arg03"]
par$child.age[par$child == "arg04"] <- par$ahgage4[par$child == "arg04"]
par$child.age[par$child == "arg05"] <- par$ahgage5[par$child == "arg05"]
par$child.age[par$child == "arg06"] <- par$ahgage6[par$child == "arg06"]
par$child.age[par$child == "arg07"] <- par$ahgage7[par$child == "arg07"]
par$child.age[par$child == "arg08"] <- par$ahgage8[par$child == "arg08"]
par$child.age[par$child == "arg09"] <- par$ahgage9[par$child == "arg09"]
par$child.age[par$child == "arg10"] <- par$ahgage10[par$child == "arg10"]
par$child.age[par$child == "arg11"] <- par$ahgage11[par$child == "arg11"]

# age at child

par$age.at.child <- par$ahgage - par$child.age

# Birth order
par$childorder <- ave(par$child.age, par$xwaveid, FUN=rank)

birthorder <- par %>% arrange(xwaveid, age.at.child) %>%
  group_by(xwaveid) %>% 
  mutate(rank = rank(age.at.child, ties.method = "first"))

# first child only

a1c <- birthorder %>% filter(rank == 1) %>% select(xwaveid, age.at.child)

# Clean up

rm(arg)
rm(ahgage)
rm(birthorder)
rm(ndt)

# add gender

sex <- select(hilda1, xwaveid, ahgsex)

a1c <- left_join(a1c, sex, by = "xwaveid")
