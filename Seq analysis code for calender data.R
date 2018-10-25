# import data from HILDA calender clean.r

library(TraMineR)
library(TraMineRextras)

edu.seq <- select(study.seq, xwaveid, contains('study'))

edu.seq <- na.omit(edu.seq)

eduseq <- seqdef(edu.seq, var=2:52, cpal=c("#e5f5e0","#a1d99b","#31a354"))


seqIplot(eduseq, ylab=NA, yaxis=FALSE, legend.prop = 0.2, sortv="from.start")
