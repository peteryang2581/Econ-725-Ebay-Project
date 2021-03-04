library(haven)
library(tidyverse)
library(h2o)
#h2o.init(nthreads = -1)
h2o.init(nthreads = 4)
library(e1071)

ebaydatafinal <- read_dta("ebaydatafinal.dta")

ebay <- ebaydatafinal %>%
  mutate(ding_good = pmax(ding_barely, ding_minute, ding_negligible, ding_small, ding_limited, ding_almost, ding_minor, ding_little, ding_invisible)) %>%
  select(-c(ding_barely, ding_minute, ding_negligible, ding_small, ding_limited, ding_almost, ding_minor, ding_little, ding_invisible)) %>%
  mutate(ding_bad = pmax(ding_wide, ding_enormous, ding_noticeable, ding_large, ding_obvious, ding_major, ding_substantial, ding_visible, ding_huge, ding_medium, ding_big, ding_significant, ding_sizable, ding_vast)) %>%
  select(-c(ding_wide, ding_enormous, ding_noticeable, ding_large, ding_obvious, ding_major, ding_substantial, ding_visible, ding_huge, ding_medium, ding_big, ding_significant, ding_sizable, ding_vast)) %>%
  mutate(ding_knowledge = pmax(ding_apparent, ding_known)) %>%
  select(-c(ding_apparent, ding_known)) %>%
  mutate(ding_negation = pmax(ding_no, ding_free, ding_never, ding_nothing)) %>%
  select(-c(ding_no, ding_free, ding_never, ding_nothing)) %>%
  mutate(ding_low = pmax(ding_seldom, ding_one, ding_rarely, ding_only, ding_hardly, ding_couple)) %>%
  select(-c(ding_seldom, ding_one, ding_rarely, ding_only, ding_hardly, ding_couple)) %>%
  mutate(ding_high = pmax(ding_several, ding_much, ding_very, ding_extremely, ding_many, ding_some)) %>%
  select(-c(ding_several, ding_much, ding_very, ding_extremely, ding_many, ding_some)) %>%
  mutate(ding_pics = pmax(ding_pic, ding_photo)) %>%
  select(-c(ding_pic, ding_photo))


ebay$ding_group2 <- 0
ebay <- ebay %>%
  mutate(ding_group = ifelse(ding == 1 & ding_negation == 1, 1,
                             ifelse(ding == 1 & ding_negation == 0 & (ding_good == 1 | ding_low == 1) & ding_high == 0 & ding_bad == 0,2,
                                    ifelse(ding == 1 & ding_negation == 0 & ding_good == 0 & ding_low == 0 & ding_bad == 0 & ding_high == 0,3,
                                           ifelse(ding == 1 & ding_negation == 0 & (ding_bad == 1 | ding_high == 1) & ding_low == 0 & ding_good == 0,4,
                                                  ifelse(ding == 1 & ding_negation == 0 & ding_group2 == 0,5,0)))))) %>%
  select(-ding_group2)

###########################################################
################### Group 2 ###############################
###########################################################
ebay <- ebay %>%
  mutate(dent_good = pmax(dent_barely, dent_minute, dent_negligible, dent_small, dent_limited, dent_almost, dent_minor, dent_little, dent_invisible)) %>%
  select(-c(dent_barely, dent_minute, dent_negligible, dent_small, dent_limited, dent_almost, dent_minor, dent_little, dent_invisible)) %>%
  mutate(dent_bad = pmax(dent_wide, dent_enormous, dent_noticeable, dent_large, dent_obvious, dent_major, dent_substantial, dent_visible, dent_huge, dent_medium, dent_big, dent_significant, dent_sizable, dent_vast)) %>%
  select(-c(dent_wide, dent_enormous, dent_noticeable, dent_large, dent_obvious, dent_major, dent_substantial, dent_visible, dent_huge, dent_medium, dent_big, dent_significant, dent_sizable, dent_vast)) %>%
  mutate(dent_knowledge = pmax(dent_apparent, dent_known)) %>%
  select(-c(dent_apparent, dent_known)) %>%
  mutate(dent_negation = pmax(dent_no, dent_free, dent_never, dent_nothing)) %>%
  select(-c(dent_no,dent_free, dent_never, dent_nothing)) %>%
  mutate(dent_low = pmax(dent_seldom, dent_one, dent_rarely, dent_only, dent_hardly, dent_couple)) %>%
  select(-c(dent_seldom, dent_one, dent_rarely, dent_only, dent_hardly, dent_couple)) %>%
  mutate(dent_high = pmax(dent_several, dent_much, dent_very, dent_extremely, dent_many, dent_some)) %>%
  select(-c(dent_several, dent_much, dent_very, dent_extremely, dent_many, dent_some)) %>%
  mutate(dent_pics = pmax(dent_pic, dent_photo)) %>%
  select(-c(dent_pic, dent_photo))

ebay$dent_group2 <- 0
ebay <- ebay %>%
  mutate(dent_group = ifelse(dent == 1 & dent_negation == 1, 1,
                             ifelse(dent == 1 & dent_negation == 0 & (dent_good == 1 | dent_low == 1) & dent_high == 0 & dent_bad == 0,2,
                                    ifelse(dent == 1 & dent_negation == 0 & dent_good == 0 & dent_low == 0 & dent_bad == 0 & dent_high == 0,3,
                                           ifelse(dent == 1 & dent_negation == 0 & (dent_bad == 1 | dent_high == 1) & dent_low == 0 & dent_good == 0,4,
                                                  ifelse(dent == 1 & dent_negation == 0 & dent_group2 == 0,5,0)))))) %>%
  select(-dent_group2)

# Group 3
ebay <- ebay %>%
  mutate(crack_good = pmax(crack_barely, crack_minute, crack_negligible, crack_small, crack_limited, crack_almost, crack_minor, crack_little, crack_invisible)) %>%
  select(-c(crack_barely, crack_minute, crack_negligible, crack_small, crack_limited, crack_almost, crack_minor, crack_little, crack_invisible)) %>%
  mutate(crack_bad = pmax(crack_wide, crack_enormous, crack_noticeable, crack_large, crack_obvious, crack_major, crack_substantial, crack_visible, crack_huge, crack_medium, crack_big, crack_significant, crack_sizable, crack_vast)) %>%
  select(-c(crack_wide, crack_enormous, crack_noticeable, crack_large, crack_obvious, crack_major, crack_substantial, crack_visible, crack_huge, crack_medium, crack_big, crack_significant, crack_sizable, crack_vast)) %>%
  mutate(crack_knowledge = pmax(crack_apparent, crack_known)) %>%
  select(-c(crack_apparent, crack_known)) %>%
  mutate(crack_negation = pmax(crack_no, crack_free, crack_never, crack_nothing)) %>%
  select(-c(crack_no, crack_free, crack_never, crack_nothing)) %>%
  mutate(crack_low = pmax(crack_seldom, crack_one, crack_rarely, crack_only, crack_hardly, crack_couple)) %>%
  select(-c(crack_seldom, crack_one, crack_rarely, crack_only, crack_hardly, crack_couple)) %>%
  mutate(crack_high = pmax(crack_several, crack_much, crack_very, crack_extremely, crack_many, crack_some)) %>%
  select(-c(crack_several, crack_much, crack_very, crack_extremely, crack_many, crack_some)) %>%
  mutate(crack_pics = pmax(crack_pic, crack_photo)) %>%
  select(-c(crack_pic, crack_photo))

ebay$crack_group2 <- 0
ebay <- ebay %>%
  mutate(crack_group = ifelse(crack == 1 & crack_negation == 1, 1,
                              ifelse(crack == 1 & crack_negation == 0 & (crack_good == 1 | crack_low == 1) & crack_high == 0 & crack_bad == 0,2,
                                     ifelse(crack == 1 & crack_negation == 0 & crack_good == 0 & crack_low == 0 & crack_bad == 0 & crack_high == 0,3,
                                            ifelse(crack == 1 & crack_negation == 0 & (crack_bad == 1 | crack_high == 1) & crack_low == 0 & crack_good == 0,4,
                                                   ifelse(crack == 1 & crack_negation == 0 & crack_group2 == 0,5,0)))))) %>%
  select(-crack_group2)

#######################################################
##Group 4
ebay <- ebay %>% 
  mutate(problem_good = pmax(problem_barely, problem_minute, problem_negligible, problem_small, problem_limited, problem_almost, problem_minor, problem_little, problem_invisible)) %>% 
  select(-c(problem_barely, problem_minute, problem_negligible, problem_small, problem_limited, problem_almost, problem_minor, problem_little, problem_invisible)) %>%
  mutate(problem_bad = pmax(problem_wide, problem_enormous, problem_noticeable, problem_large, problem_obvious, problem_major, problem_substantial, problem_visible, problem_huge, problem_medium, problem_big, problem_significant, problem_sizable, problem_vast)) %>% 
  select(-c(problem_wide, problem_enormous, problem_noticeable, problem_large, problem_obvious, problem_major, problem_substantial, problem_visible, problem_huge, problem_medium, problem_big, problem_significant, problem_sizable, problem_vast)) %>%
  mutate(problem_knowledge = pmax(problem_apparent, problem_known)) %>% 
  select(-c(problem_apparent, problem_known)) %>%
  mutate(problem_negation = pmax(problem_no, problem_free, problem_never, problem_nothing)) %>% 
  select(-c(problem_no, problem_free, problem_never, problem_nothing)) %>%
  mutate(problem_low = pmax(problem_seldom, problem_one, problem_rarely, problem_only, problem_hardly, problem_couple)) %>% 
  select(-c(problem_seldom, problem_one, problem_rarely, problem_only, problem_hardly, problem_couple)) %>%
  mutate(problem_high = pmax(problem_several, problem_much, problem_very, problem_extremely, problem_many, problem_some)) %>% 
  select(-c(problem_several, problem_much, problem_very, problem_extremely, problem_many, problem_some)) %>%
  mutate(problem_pics = pmax(problem_pic, problem_photo)) %>% 
  select(-c(problem_pic, problem_photo))

ebay$problem_group2 = 0
ebay <- ebay %>%
  mutate(problem_group = ifelse(problem == 1 & problem_negation == 1,1, 
                                ifelse(problem == 1 & problem_negation == 0 & (problem_good == 1 | problem_low == 1) & problem_high == 0 & problem_bad == 0,2,
                                       ifelse(problem == 1 & problem_negation == 0 & problem_good == 0 & problem_low == 0 & problem_bad == 0 & problem_high == 0,3,
                                              ifelse(problem == 1 & problem_negation == 0 & (problem_bad == 1 | problem_high == 1) & problem_low == 0 & problem_good == 0,4,
                                                     ifelse(problem == 1 & problem_negation == 0,5,0)))))) %>%
  select(-problem_group2)
##Group 5
ebay <- ebay %>% 
  mutate(rust_good = pmax(rust_barely, rust_minute, rust_negligible, rust_small, rust_limited, rust_almost, rust_minor, rust_little, rust_invisible)) %>% 
  select(-c(rust_barely, rust_minute, rust_negligible, rust_small, rust_limited, rust_almost, rust_minor, rust_little, rust_invisible)) %>%
  mutate(rust_bad = pmax(rust_wide, rust_enormous, rust_noticeable, rust_large, rust_obvious, rust_major, rust_substantial, rust_visible, rust_huge, rust_medium, rust_big, rust_significant, rust_sizable, rust_vast)) %>% 
  select(-c(rust_wide, rust_enormous, rust_noticeable, rust_large, rust_obvious, rust_major, rust_substantial, rust_visible, rust_huge, rust_medium, rust_big, rust_significant, rust_sizable, rust_vast)) %>%
  mutate(rust_knowledge = pmax(rust_apparent, rust_known)) %>% 
  select(-c(rust_apparent, rust_known)) %>%
  mutate(rust_negation = pmax(rust_no, rust_free, rust_never, rust_nothing)) %>% 
  select(-c(rust_no, rust_free, rust_never, rust_nothing)) %>%
  mutate(rust_low = pmax(rust_seldom, rust_one, rust_rarely, rust_only, rust_hardly, rust_couple)) %>% 
  select(-c(rust_seldom, rust_one, rust_rarely, rust_only, rust_hardly, rust_couple)) %>%
  mutate(rust_high = pmax(rust_several, rust_much, rust_very, rust_extremely, rust_many, rust_some)) %>% 
  select(-c(rust_several, rust_much, rust_very, rust_extremely, rust_many, rust_some)) %>%
  mutate(rust_pics = pmax(rust_pic, rust_photo)) %>% 
  select(-c(rust_pic, rust_photo))

ebay$rust_group2 = 0
ebay <- ebay %>%
  mutate(rust_group = ifelse(rust == 1 & rust_negation == 1,1, 
                             ifelse(rust == 1 & rust_negation == 0 & (rust_good == 1 | rust_low == 1) & rust_high == 0 & rust_bad == 0,2,
                                    ifelse(rust == 1 & rust_negation == 0 & rust_good == 0 & rust_low == 0 & rust_bad == 0 & rust_high == 0,3,
                                           ifelse(rust == 1 & rust_negation == 0 & (rust_bad == 1 | rust_high == 1) & rust_low == 0 & rust_good == 0,4,
                                                  ifelse(rust == 1 & rust_negation == 0,5,0)))))) %>%
  select(-rust_group2)



##############################################################################################
##Group6
ebay <- ebay %>%
  mutate(scratch_good = pmax(scratch_barely, scratch_minute, scratch_negligible,scratch_small, scratch_limited, scratch_almost, scratch_minor,scratch_little, scratch_invisible)) %>%
  select(-c(scratch_barely, scratch_minute, scratch_negligible,scratch_small, scratch_limited,scratch_almost, scratch_minor, scratch_little,scratch_invisible) ) %>%
  mutate(scratch_bad = pmax(scratch_wide, scratch_enormous, scratch_noticeable, scratch_large, scratch_obvious, scratch_major, scratch_substantial, scratch_visible, scratch_huge, scratch_medium, scratch_big, scratch_significant, scratch_sizable, scratch_vast)) %>%
  select(-c(scratch_wide, scratch_enormous, scratch_noticeable, scratch_large, scratch_obvious, scratch_major,scratch_substantial, scratch_visible, scratch_huge, scratch_medium, scratch_big, scratch_significant,scratch_sizable, scratch_vast)) %>%
  mutate(scratch_knowledge = pmax(scratch_apparent, scratch_known)) %>% 
  select(-c(scratch_apparent, scratch_known)) %>%
  mutate(scratch_negation = pmax(scratch_no, scratch_free, scratch_never, scratch_nothing)) %>%
  select(-c(scratch_no, scratch_free, scratch_never, scratch_nothing)) %>%
  mutate(scratch_low = pmax(scratch_seldom, scratch_one, scratch_rarely, scratch_only, scratch_hardly, scratch_couple)) %>%
  select(-c(scratch_seldom, scratch_one, scratch_rarely, scratch_only, scratch_hardly, scratch_couple)) %>%
  mutate(scratch_high = pmax(scratch_several, scratch_much, scratch_very, scratch_extremely, scratch_many, scratch_some)) %>%
  select(-c(scratch_several, scratch_much, scratch_very, scratch_extremely, scratch_many, scratch_some)) %>%
  mutate(scratch_pics = pmax(scratch_pic, scratch_photo)) %>%
  select(-c(scratch_pic,scratch_photo))

ebay$scratch_group2 <- 0
ebay <- ebay %>%
  mutate(scratch_group = ifelse(scratch == 1 & scratch_negation == 1, 1,
                                ifelse(scratch == 1 & scratch_negation == 0 & (scratch_good == 1 | scratch_low == 1) & scratch_high == 0 & scratch_bad == 0, 2,
                                       ifelse(scratch == 1 & scratch_negation == 0 & scratch_good == 0 & scratch_low == 0 & scratch_bad == 0 & scratch_high == 0, 3,
                                              ifelse(scratch == 1 & scratch_negation == 0 & (scratch_bad == 1 | scratch_high == 1) & scratch_low == 0 & scratch_good == 0, 4,
                                                     ifelse(scratch == 1 & scratch_negation == 0 & scratch_group2 == 0, 5,0)))))) %>%
  select(-scratch_group2)

################################################################################################


###########################################################
################### Group 7 ###############################
###########################################################
ebay <- ebay %>%
  mutate(broken_good = pmax(broken_barely, broken_minute, broken_negligible, broken_small, broken_limited, broken_almost, broken_minor, broken_little, broken_invisible)) %>%
  select(-c(broken_barely,  broken_minute,  broken_negligible,  broken_small,  broken_limited , broken_almost , broken_minor , broken_little,  broken_invisible)) %>%
  mutate(broken_bad = pmax(broken_wide, broken_enormous, broken_noticeable, broken_large, broken_obvious, broken_major, broken_substantial, broken_visible, broken_huge, broken_medium, broken_big, broken_significant, broken_sizable, broken_vast)) %>%
  select(-c(broken_wide,  broken_enormous,  broken_noticeable,  broken_large , broken_obvious,  broken_major,  broken_substantial,  broken_visible,  broken_huge,  broken_medium,  broken_big,  broken_significant,  broken_sizable,  broken_vast)) %>%
  mutate(broken_knowledge = pmax(broken_apparent, broken_known)) %>%
  select(-c(broken_apparent,  broken_known)) %>%
  mutate(broken_negation = pmax(broken_no, broken_free, broken_never, broken_nothing)) %>%
  select(-c(broken_no,  broken_free,  broken_never,  broken_nothing)) %>%
  mutate(broken_low = pmax(broken_seldom, broken_one, broken_rarely, broken_only, broken_hardly, broken_couple)) %>%
  select(-c(broken_seldom,  broken_one,  broken_rarely,  broken_only,  broken_hardly,  broken_couple)) %>%
  mutate(broken_high = pmax(broken_several, broken_much, broken_very, broken_extremely, broken_many, broken_some)) %>%
  select(-c(broken_several, broken_much, broken_very,  broken_extremely,  broken_many,  broken_some)) %>%
  mutate(broken_pics = pmax(broken_pic, broken_photo)) %>%
  select(-c(broken_pic, broken_photo))

ebay$broken_group2 <- 0
ebay <- ebay %>%
  mutate(broken_group = ifelse(broken == 1 & broken_negation == 1, 1,
                               ifelse(broken == 1 & broken_negation == 0 & (broken_good == 1 | broken_low == 1) & broken_high == 0 & broken_bad == 0,2,
                                      ifelse(broken == 1 & broken_negation == 0 & broken_good == 0 & broken_low == 0 & broken_bad == 0 & broken_high == 0,3,
                                             ifelse(broken == 1 & broken_negation == 0 & (broken_bad == 1 | broken_high == 1) & broken_low == 0 & broken_good == 0,4,
                                                    ifelse(broken == 1 & broken_negation == 0 & broken_group2 == 0,5,0)))))) %>%
  select(-broken_group2)
#####################################################################################################################


ebay <- ebay %>%
  filter(trans != 1 & trans != 5) %>%
  filter(miles >= 0) %>%
  filter(miles != 0 | condition == 22) %>%
  filter(model != 1) %>%
  filter(doors != 1 & doors != 5) %>%
  filter(doors != 3 | model == 30) %>%
  filter(model != 27 | doors != 2) %>%
  filter(model != 28 | doors != 4) %>%
  filter(model != 29 | doors != 2) %>%
  filter(model != 32 | doors != 4) %>%
  filter(model != 33 | doors != 4) %>%
  filter(model != 38 | doors != 2 ) %>%
  filter(model != 39 | doors != 4 ) %>%
  filter(model != 40 | doors != 4) %>%
  filter(title == 17) %>%
  filter(condition == 23) %>%
  filter(software != "") %>%
  drop_na(age,software) %>%
  mutate(negpct = 100 - pctfdback)


ebay <- ebay %>%
  mutate(pro=ifelse(software == "carad",1,
                    ifelse(software == "auction123",1,
                           ifelse(software == "eBizAutos",1,0)))) %>%
  select(sell,biddy1,miles,buyitnow,photos,model,doors,trans,age,n,pro,featured,
         bookvalue,startbid,warranty,inspection,reserve,addedinfo,negpct,text,dealer,
         ding_group,scratch_group,crack_group,broken_group,dent_group,problem_group,rust_group,
         startdate,length,endsunday,endday) %>%
  mutate(startmon=match(substr(startdate,1,3),month.abb)) %>%
  select(-startdate) %>%
  group_by(model,doors,trans) %>%
  mutate(carmodel=cur_group_id()) 
ebay$biddy1[which(is.na(ebay$biddy1))] <- 0
ebay$photos[which(is.na(ebay$photos))] <- 0