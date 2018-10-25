# ===== H I L D A calendar data. ====== #

# ------ NOTES --------------------------------------------------------------------------------------#
# from ada.notes.01343.pdf
#Variable                                      Description
#_capeft, _capept, _capj, _capune, _capnlf     Per cent time in last financial year spent in: 
#                                             - full-time education - part-time education 
#                                             - jobs - unemployed - not in the labour force
#_cafnj                                       Number of jobs in last financial year
#_cantp                                       Number of time periods answered in calendar
# --------------------------------------------------------------------------------------------------#

#--- Coding -----#
# 0- no  1- yes
# -1 Not asked question: skipped due to answer to a preceding question
# -2 Not applicable
# - 10 Non-responding individual


# load( H I L D A F I L E )

library(dplyr)
library(stringr)
library(Hmisc)

# All calender related data, wave 1 (a)
aca <- select(hilda1, xwaveid, contains("aca"))

  # Only full time study
  # acaeft - whether activity occurs
  
  aca.fstud <- select(aca, xwaveid, contains("acaeft"))
  head(aca.fstud)
  describe(aca.fstud$acaeft) # 8.4 % have been in full time study, 61.8 have not, 29.9 non responding person
  describe(aca.fstud$acaeft01)
  
  # recode
  aca.fstud[,3:53] <- lapply(aca.fstud[,3:53], 
                              FUN = function(x) recode(x, '-1' = '0', '-2' = '0', '1'='1', '0' = '0')) 
  # Part time study only
  # acaept - whether part time study
  
  aca.pstud <- select(aca, xwaveid, contains("acaept"))
  head(aca.pstud)
  describe(aca.pstud$acaept) # 5.6 % have been in part time study, 64.5 have not, 29.9 non responding person
  
  #recode
  aca.pstud[,3:53] <- lapply(aca.pstud[,3:53], 
                             FUN = function(x) recode(x, '-1' = '0', '-2' = '0', '1'='1', '0' = '0')) 
  
  #Make trial study sequence
  # 3 states: 0 - no study, 1 - part time study and 2 - full time study
  
  study.seq <- full_join(aca.fstud, aca.pstud, by = "xwaveid")
  
  study.seq$study01 <- ifelse(study.seq$acaeft01 == 1, 2,
                               ifelse(study.seq$acaept01 == 1, 1, 0))
  study.seq$study02 <- ifelse(study.seq$acaeft02 == 1, 2,
                              ifelse(study.seq$acaept02 == 1, 1, 0))
  study.seq$study03 <- ifelse(study.seq$acaeft03 == 1, 2,
                              ifelse(study.seq$acaept03 == 1, 1, 0))
  study.seq$study04 <- ifelse(study.seq$acaeft04 == 1, 2,
                              ifelse(study.seq$acaept04 == 1, 1, 0))
  study.seq$study05 <- ifelse(study.seq$acaeft05 == 1, 2,
                              ifelse(study.seq$acaept05 == 1, 1, 0))
  study.seq$study06 <- ifelse(study.seq$acaeft06 == 1, 2,
                              ifelse(study.seq$acaept06 == 1, 1, 0))
  study.seq$study07 <- ifelse(study.seq$acaeft07 == 1, 2,
                              ifelse(study.seq$acaept07 == 1, 1, 0))
  study.seq$study08 <- ifelse(study.seq$acaeft08 == 1, 2,
                              ifelse(study.seq$acaept08 == 1, 1, 0))
  study.seq$study09 <- ifelse(study.seq$acaeft09 == 1, 2,
                              ifelse(study.seq$acaept09 == 1, 1, 0))
  study.seq$study10 <- ifelse(study.seq$acaeft10 == 1, 2,
                              ifelse(study.seq$acaept10 == 1, 1, 0))
  study.seq$study11 <- ifelse(study.seq$acaeft11 == 1, 2,
                              ifelse(study.seq$acaept11 == 1, 1, 0))
  study.seq$study12 <- ifelse(study.seq$acaeft12 == 1, 2,
                              ifelse(study.seq$acaept12 == 1, 1, 0))
  study.seq$study13 <- ifelse(study.seq$acaeft13 == 1, 2,
                              ifelse(study.seq$acaept13 == 1, 1, 0))
  study.seq$study14 <- ifelse(study.seq$acaeft14 == 1, 2,
                              ifelse(study.seq$acaept14 == 1, 1, 0))
  study.seq$study15 <- ifelse(study.seq$acaeft15 == 1, 2,
                              ifelse(study.seq$acaept15 == 1, 1, 0))
  study.seq$study16 <- ifelse(study.seq$acaeft16 == 1, 2,
                              ifelse(study.seq$acaept16 == 1, 1, 0))
  study.seq$study17 <- ifelse(study.seq$acaeft17 == 1, 2,
                              ifelse(study.seq$acaept17 == 1, 1, 0))
  study.seq$study18 <- ifelse(study.seq$acaeft18 == 1, 2,
                              ifelse(study.seq$acaept18 == 1, 1, 0))
  study.seq$study19 <- ifelse(study.seq$acaeft19 == 1, 2,
                              ifelse(study.seq$acaept19 == 1, 1, 0))
  study.seq$study20 <- ifelse(study.seq$acaeft20 == 1, 2,
                              ifelse(study.seq$acaept20 == 1, 1, 0))
  study.seq$study21 <- ifelse(study.seq$acaeft21 == 1, 2,
                              ifelse(study.seq$acaept21 == 1, 1, 0))
  study.seq$study22 <- ifelse(study.seq$acaeft22 == 1, 2,
                              ifelse(study.seq$acaept22 == 1, 1, 0))
  study.seq$study23 <- ifelse(study.seq$acaeft23 == 1, 2,
                              ifelse(study.seq$acaept23 == 1, 1, 0))
  study.seq$study24 <- ifelse(study.seq$acaeft24 == 1, 2,
                              ifelse(study.seq$acaept24 == 1, 1, 0))
  study.seq$study25 <- ifelse(study.seq$acaeft25 == 1, 2,
                              ifelse(study.seq$acaept25 == 1, 1, 0))
  study.seq$study26 <- ifelse(study.seq$acaeft26 == 1, 2,
                              ifelse(study.seq$acaept26 == 1, 1, 0))
  study.seq$study27 <- ifelse(study.seq$acaeft27 == 1, 2,
                              ifelse(study.seq$acaept27 == 1, 1, 0))
  study.seq$study28 <- ifelse(study.seq$acaeft28 == 1, 2,
                              ifelse(study.seq$acaept28 == 1, 1, 0))
  study.seq$study29 <- ifelse(study.seq$acaeft29 == 1, 2,
                              ifelse(study.seq$acaept29 == 1, 1, 0))
  study.seq$study30 <- ifelse(study.seq$acaeft30 == 1, 2,
                              ifelse(study.seq$acaept30 == 1, 1, 0))
  study.seq$study31 <- ifelse(study.seq$acaeft31 == 1, 2,
                              ifelse(study.seq$acaept31 == 1, 1, 0))
  study.seq$study32 <- ifelse(study.seq$acaeft32 == 1, 2,
                              ifelse(study.seq$acaept32 == 1, 1, 0))
  study.seq$study33 <- ifelse(study.seq$acaeft33 == 1, 2,
                              ifelse(study.seq$acaept33 == 1, 1, 0))
  study.seq$study34 <- ifelse(study.seq$acaeft34 == 1, 2,
                              ifelse(study.seq$acaept34 == 1, 1, 0))
  study.seq$study35 <- ifelse(study.seq$acaeft35 == 1, 2,
                              ifelse(study.seq$acaept35 == 1, 1, 0))
  study.seq$study36 <- ifelse(study.seq$acaeft36 == 1, 2,
                              ifelse(study.seq$acaept36 == 1, 1, 0))
  study.seq$study37 <- ifelse(study.seq$acaeft37 == 1, 2,
                              ifelse(study.seq$acaept37 == 1, 1, 0))
  study.seq$study38 <- ifelse(study.seq$acaeft38 == 1, 2,
                              ifelse(study.seq$acaept38 == 1, 1, 0))
  study.seq$study39 <- ifelse(study.seq$acaeft39 == 1, 2,
                              ifelse(study.seq$acaept39 == 1, 1, 0))
  study.seq$study40 <- ifelse(study.seq$acaeft40 == 1, 2,
                              ifelse(study.seq$acaept40 == 1, 1, 0))
  study.seq$study41 <- ifelse(study.seq$acaeft41 == 1, 2,
                              ifelse(study.seq$acaept41 == 1, 1, 0))
  study.seq$study42 <- ifelse(study.seq$acaeft42 == 1, 2,
                              ifelse(study.seq$acaept42 == 1, 1, 0))
  study.seq$study43 <- ifelse(study.seq$acaeft43 == 1, 2,
                              ifelse(study.seq$acaept43 == 1, 1, 0))
  study.seq$study44 <- ifelse(study.seq$acaeft44 == 1, 2,
                              ifelse(study.seq$acaept44 == 1, 1, 0))
  study.seq$study45 <- ifelse(study.seq$acaeft45 == 1, 2,
                              ifelse(study.seq$acaept45 == 1, 1, 0))
  study.seq$study46 <- ifelse(study.seq$acaeft46 == 1, 2,
                              ifelse(study.seq$acaept46 == 1, 1, 0))
  study.seq$study47 <- ifelse(study.seq$acaeft47 == 1, 2,
                              ifelse(study.seq$acaept47 == 1, 1, 0))
  study.seq$study48 <- ifelse(study.seq$acaeft48 == 1, 2,
                              ifelse(study.seq$acaept48 == 1, 1, 0))
  study.seq$study49 <- ifelse(study.seq$acaeft49 == 1, 2,
                              ifelse(study.seq$acaept49 == 1, 1, 0))
  study.seq$study50 <- ifelse(study.seq$acaeft50 == 1, 2,
                              ifelse(study.seq$acaept50 == 1, 1, 0))
  study.seq$study51 <- ifelse(study.seq$acaeft51 == 1, 2,
                              ifelse(study.seq$acaept51 == 1, 1, 0))
