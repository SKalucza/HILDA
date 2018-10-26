# using a1c from HILDA age at first birth clean

table(a1c$age.at.child)

a1c$tp <- ifelse(a1c$age.at.child < 20, 1, 0)
a1c$tpyp <- ifelse(a1c$age.at.child < 25, 1, 0)
a1c$yp <- ifelse(a1c$tp != 1 & a1c$age.at.child < 25, 1, 0)

describe(a1c$tp) # 214 teenage parents (15-19)
describe(a1c$yp) # 1213 young parents (20-24)
describe(a1c$tpyp) # 1454 (15-24)

table(a1c$ahgsex, a1c$tp)
table(a1c$ahgsex, a1c$yp)
table(a1c$ahgsex, a1c$tpyp)
