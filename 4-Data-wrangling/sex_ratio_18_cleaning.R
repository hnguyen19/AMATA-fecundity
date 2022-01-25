### current, from one sheet  
library(emmeans)
library(here)
library(readr)
library(tidyverse) 


#### This script made  sexed18.csv, sexed19.csv

sexratio_2018 <- read_csv(here("2 Data/Raw data/Waterhemp sex ratio 2018.csv"),
                          col_types = cols(Sex = col_integer(), 
                                           Number = col_integer(),
                                           Date = col_date(format = "%m/%d/%y")),
                          na = "empty")

sexratio_2018$Crop_a <- substr(sexratio_2018$Crop,0,1) #abbreviated crops
sexratio_2018$Crop_a <- toupper(sexratio_2018$Crop_a)
sexratio_2018$Rot <- substr(sexratio_2018$Rotation,0,1)

sexratio_2018$Crop_ID <- factor(paste(sexratio_2018$Crop_a, sexratio_2018$Rot , sep = ""))



#add block
sexratio_2018<-sexratio_2018%>%mutate(Block=ifelse(Plot%in%c(11:19),"1",
                                                   ifelse(Plot%in%c(21:29),"2",
                                                          ifelse(Plot%in%c(31:39),"3","4"))))

table(sexratio_2018$Crop_ID, sexratio_2018$Block)

sexratio_2018$Year <- substr(sexratio_2018$Date,0,4)

tmp <- sexratio_2018%>%
  group_by(Plot, Side, Corn_weed_management, Crop_ID, Rotation, Crop, Block, Year) %>%
  summarize(Total=max(Number),
            Female = sum(Sex == 1),
            Male = Total - Female)

### Save sexed18.csv, with the total number of male and female per EU.
write.csv(tmp, here("2 Data/Raw data/sexed18.csv"), row.names = F)


## 2019  
### Questions
## Is the missing data at random or not at random? It is expected that the high efficacy 
## result in fewer weeds in quadrats but it was not intentional. Or is it completely at 
## random? Testing for MAR with a t-test can be problematic so visualizing the missing pattern
## is optimal, with package VIM


## WHindiv2019 is the data set at the end of the season, with individual plant sex, weight, cohort and
## dates of sampling, identified to quadrat ID within subplot
## the Color column is the toothpick column to be matched with the rows each of the entries
## in this set belong in the X2019_cohort data set. 
WHindiv2019 <- read_csv("2 Data/Raw data/WHindiv2019.csv",
                        col_types = cols(Plot = col_character(),
                                         Quadrat = col_character()),na = ".")
WHindiv2019$Color <- tolower(WHindiv2019$Color) # 2996 entries 
WHindiv2019<- WHindiv2019[complete.cases(WHindiv2019),] #2993 entries


## the counts below are identified by quadrat, cohort, side and plot (smallest to largest grouping factor)
## No NAs are in the following 3 sets  

# count the number of males
countm<-WHindiv2019%>%
  group_by(Plot,Side,Quadrat,Color)%>%
  filter(Sex=="M")%>%
  dplyr::summarise(Number_end_m=n())
dim(countm) # 348 records

#count the number of females
countf<-WHindiv2019%>%
  group_by(Plot,Side,Quadrat,Color)%>%
  filter(Sex=="F")%>%
  dplyr::summarise(Number_end_f=n())
dim(countf) # 394 records

#count the number of unknown
countu<-WHindiv2019%>%
  group_by(Plot,Side,Quadrat,Color)%>%
  filter(Sex=="U")%>%
  dplyr::summarise(Number_end_u=n())
dim(countu) # 254 records


# # count the total number of plants, including all the unknown sex
#numbers have to be matched by both quadrat and color because some cohorts were 
#identified with 2 colors due to toothpick shortage
count1<-WHindiv2019%>%
  group_by(Plot,Side,Quadrat,Color)%>%  
  dplyr::summarise(Number_end=n()) #%>%
#  mutate(Number_end = ifelse(Number_))
dim(count1) # 688 entries


## X2019_cohort is the data set created at the beginning of the season, to be matched by row with the 
## WHindiv2019 data set. This set contains all the identification factors.
## Cohorts are color coded in X2019_cohort data set to be matched with WHindiv2019
## The Number_end column is to be filled with the number of plant found at the end of the season
## in each quadrat from each cohort with the tallied number from the WHindiv2019 set. 
## Number_end == 0 if Number_begin == 0. 
X2019_cohort <- read_csv(here("2 Data/Raw data/2019_cohort.csv"), 
                         col_types = cols(Plot = col_character(),
                                          Quadrat = col_character(),
                                          Date_begin = col_date(format = "%m/%d/%y"), 
                                          Number_begin = col_integer(), 
                                          Date_end = col_date(format = "%m/%d/%y"), 
                                          Number_end = col_integer()))

dim(X2019_cohort)


# add block, Crop_ID
X2019_cohort<-X2019_cohort%>%mutate(Block=ifelse(Plot%in%c(11:19),"1",
                                                 ifelse(Plot%in%c(21:29),"2",
                                                        ifelse(Plot%in%c(31:39),"3","4")))) %>%
  mutate(Number_end = ifelse(Number_begin ==0, 0, "NA")) #if number_begin = 0, number_end = 0

X2019_cohort$Crop_a <- substr(X2019_cohort$Crop,0,1) #abbreviated crops
X2019_cohort$Crop_a <- toupper(X2019_cohort$Crop_a)
X2019_cohort$Rot <- substr(X2019_cohort$Rotation,0,1)

X2019_cohort$Crop_ID <- factor(paste(X2019_cohort$Crop_a , X2019_cohort$Rot , sep = ""))
X2019_cohort$Year <- substr(X2019_cohort$Date_begin,0,4)
X2019_cohort$Interval <- X2019_cohort$Date_end - X2019_cohort$Date_begin #how old a plant was at harvest


# check if the number of Crop_ID x Corn_herbicide are correct
check1 <- X2019_cohort%>%group_by(Plot, Side, Crop_ID, Corn_weed_management) %>%
  count()

table(check1$Crop_ID, check1$Corn_weed_management)
check1%>%
  filter(Crop_ID == "S2")

table(WHindiv2019$Sex, WHindiv2019$Sex) #check if the 3 sex categories are correct
##overall, the sex proportion was 1416 F, 962 M and 616 U. Most of the Unknown were too young

##Have a set with Number_end = F + M + U.  


# <https://stackoverflow.com/questions/48284524/left-join-r-dataframes-merging-two-columns-with-nas>
try1 <- X2019_cohort %>% left_join( count1,
                                    by=c("Plot","Side","Quadrat", "Color")) %>%
  mutate(Total = coalesce(as.integer(Number_end.x), as.integer(Number_end.y)))%>%
  mutate(Total = as.integer(Total),
         ID = paste(Plot,Side,Quadrat,Cohort, sep = "_"))
##When first join X2019_cohort and count1: 
#with Number_end.y = Number_end from "count1" and Number_end.x = Number_end from "X2019_cohort"
#Number_end.x = NA because the number of survived plants at Date_end was unknown at Date_begin
#Number_end.x = 0 because Number_begin = 0
#Number_end.y = NA because Number_begin = 0, so by the harvest day, no plant is found with a
# combined identity of Quadrat x color 
#Total = Number_end.y if Number_end.y is non-zero, or = Number_end.x if Number_end.y = NA

dim(try1) #3448 entries, at this step
### Total is filled with non-zeros from Number_end.x and Number_end.y 
### Error data points are where Number_end.y > Number_begin  

error_points <- try1 %>% filter(Number_end.y>Number_begin) 


corrected_points <- error_points %>%
  mutate(Total = ifelse(Number_end.x != Number_begin, "NA", Total))%>% # 17 points
  mutate(Total = as.integer(Total))


try2 <- try1 #make a copy of try1
# <https://stackoverflow.com/questions/30912136/replace-rows-in-one-data-frame-if-they-appear-in-another-data-frame>
try2[match(corrected_points$ID, try2$ID), ] <- corrected_points ## at this step
## Total: all the entries that Number_end.y > Number_begin has been replaced with NA
## all the NA in the Total column need to be imputed after the number of M and F are added

all19_indiv <- try2 # all the corrected data with NAs


all19_sexed <- plyr::join_all(list(all19_indiv,countf, countm, countu), 
                              by=c("Plot","Side","Quadrat","Color"),
                              type = "left") 

#check if nonNAs and NA add up to the whole data set: YES

all19_NAs <- all19_sexed %>% filter(is.na(Total)) %>% 
  mutate(Number_end_f = NA, 
         Number_end_m = NA,
         Number_end_u = NA)#of these, 
## all the 3 sex categories are NA if Total = NA
## all the non NAs rows in the 3 sex categories are replaced with NA because Total = NA 


all19_non_NAs <- all19_sexed %>% filter(Total != "NA") # of these, numbers in the 3 sex categories
## need correction because some of the rows summed to larger than Total 

all19_zero <- all19_non_NAs %>% 
  filter(Total == 0) %>%
  mutate(Number_end_f = 0,
         Number_end_m = 0, 
         Number_end_u = 0) ## replace all NAs in 3 sex categories with 0 since Total = 0 

all19_non_zero <- all19_non_NAs %>% filter(Total != 0) 

all19_blank0 <- all19_non_zero %>% 
  filter(!is.na(Number_end_m) & !is.na(Number_end_f) & !is.na(Number_end_u))  # 43 entries

# select(ID, Total, Number_end_f, Number_end_m, Number_end_u); to see all19_blank0 without distraction.

all19_blank1plus <- all19_non_zero[rowSums(is.na(all19_non_zero[,c(23:25)])) >= 1 , ] #565 entries

#the 2 lines below are to check all19_blank1plus
all19_blank1 <- all19_blank1plus[rowSums(is.na(all19_blank1plus[,c(23:25)])) == 1, ] # 203 entries

all19_blank2 <- all19_blank1plus[rowSums(is.na(all19_blank1plus[,c(23:25)])) == 2, ] # 362 entries

# ask overflow 

dat <- all19_blank1plus %>% 
  dplyr::select(ID, Total, Number_end_f, Number_end_m, Number_end_u)


nm1 <- grep("^Number", names(dat), value = TRUE)
i1 <- rowSums(dat[nm1], na.rm = TRUE) == dat$Total
i1[is.na(i1)] <- FALSE
i2 <- is.na(dat[i1, nm1])
dat[i1, nm1][i2] <- 0 #this has 565 entries
#<https://stackoverflow.com/questions/67187587/conditionally-replace-na-in-one-column-by-subtracting-the-sum-of-other-columns-f#67187607>
## replace all NAs in 3 sex categories with 0 if 

## put dat back to all19_blank1plus

all19_blank1plus_filled <- all19_blank1plus %>%
  left_join(dat, by = c("ID", "Total" = "Total",
                        "Number_end_f" = "Number_end_f",
                        "Number_end_m" = "Number_end_m",
                        "Number_end_u" = "Number_end_u")) # "left" data override "right"

all19_blank1plus_filled  <- all19_blank1plus %>%
  left_join(dat, by = "ID") %>%
  mutate(Total = coalesce(Total.x = Total.y),
         Number_end_f = coalesce(Number_end_f.x = Number_end_f.y),
         Number_end_m = coalesce(Number_end_m.x = Number_end_m.y),
         Number_end_u = coalesce(Number_end_u.x = Number_end_u.y)) %>%
  dplyr::select(-c(Total.x, Number_end_f.x:Number_end_u.y)) ## at this step
## all the rows with up to 2 NA sex categories have been filled. 

## see of zeros and NAs allign well. YES
test1 <- rbind(all19_zero, all19_NAs) # 755 + 2085 = 2840 entries

## put everything in all19_non_zero back together  
all19_non_zero_filled  <- rbind(all19_blank0, all19_blank1plus_filled)

### all non_NA and NA together  
all19_sexed_filled <- rbind(all19_non_zero_filled, test1) ## all fill-able cells have been filled; all NAs at their places.  Do not rbind 3 dataframes at once or numbers will be misplaced.  

names(all19_sexed_filled)[names(all19_sexed_filled) =="Number_end_m"] <- "Male"
names(all19_sexed_filled)[names(all19_sexed_filled) =="Number_end_f"] <- "Female"
names(all19_sexed_filled)[names(all19_sexed_filled) =="Number_end_u"] <- "Unknown"

### visualize the missing patterns  
sexed19 <- all19_sexed_filled%>%
  dplyr::select(Plot, Side, Quadrat, Cohort, Rotation, Block ,Herbicide, Corn_weed_management, Crop, Crop_ID, Total, Female, Male, Unknown, ID, Year, Number_begin) %>% 
  mutate(Total_sexed = Female + Male) #add the total sexed individual to reference as % sexed later.  

## Save sexed19_mis.csv, the raw data to be imputed. This set has the total number of male and female by quadrat
write.csv(sexed19, here("2 Data/Clean data/sexed19_mis.csv"), row.names = F) #with missing data


### imputation without order 
library(VIM)
mice_plot1 <- aggr(sexed19[,12:14] , col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(sexed19[,12:14]), 
                   cex.axis= 1,
                   cex.numbers = .2,
                   ylab=c("Missing data","Pattern"))

mice_plot1 ## data is either NA in all 3 categories, or known, no NA for 2 categories or  1 because Total = F + M + U

pattern1 <-  md.pattern(sexed19[,12:14]) #755 missing values out of 3448 entries. 


### impute without order, just numerical variables  

sexed19_imp1 <- mice(sexed19[,12:14], m=5, maxit = 50, method = 'pmm', seed = 500) #return a list of 5 data sets, because m = 5.  
summary(sexed19_imp1)

sexed19_imp1_df <- complete(sexed19_imp1, 2) #2 is the 2nd of 5 imputated sets

plot(sexed19_imp1)
summary(mice::complete((sexed19_imp1)))

## impute without order, full data set  
sexed19_imp2 <- mice(sexed19, m=50, maxit = 50, method = 'pmm', seed = 500) #return a list of 50 data sets, because m = 50.  
summary(sexed19_imp2)

sexed19_comp <- mice::complete(sexed19_imp2) #save imputed data set

# aggregate by experiment unit
sexed19_comp_eu <-  sexed19_comp %>% 
  group_by (Plot, Side, Corn_weed_management, Crop_ID, Rotation, Crop, Block, Year) %>%
  summarize(Female = sum(Female),
            Male = sum(Male))

### Question: no missing factor, no problem, can impute without sub-setting.  


write.csv(sexed19_comp_eu, here("2 Data/Clean data/sexed19_imp.csv"), row.names = F) #imputed data


#try4$Number_end_u[is.na(try4$Number_end_u)]<-0 #don't change to 0
try4$Number_end <- as.numeric(try4$Number_end)
#names(try4)[names(try4) =="Number_end.y"] <- "Number_end"
### sex ratio
try4$Number_end_sexed <- try4$Number_end - try4$Number_end_u
#keep the sexed, m and f only. 



try5 <- try4[,-c(9, 19, 22, 24)]


#try4$Number_end_f[is.na(try4$Number_end_f)]<-0 
#try4$Number_end_m[is.na(try4$Number_end_m)]<-0
try4$Number_end.x <- NULL

try4$Survival_rate_all <- try4$Number_end/try4$Number_begin

#calculate the pct of no-id sex 
sum(countu$Number_end_u, na.rm = T)/sum(try4$Number_end_sexed, na.rm = T)
sum(countf$Number_end_f,na.rm = T)
sum(countm$Number_end_m,na.rm = T)
#assuming that the sex ratio at the beginning of the season is 1:1


## M:F prop season-end

try4$M_prop_end <- try4$Number_end_m/try4$Number_end_sexed
try4$F_prop_end <- try4$Number_end_f/try4$Number_end_sexed
## Survival by sex, season-end
names(try4)[names(try4) =="Number_end_m"] <- "Male"
names(try4)[names(try4) =="Number_end_f"] <- "Female"
names(try4)[names(try4) =="Number_end_u"] <- "Unknown"

#keep complete cases only 
#try4 <- try4[complete.cases(try4), ]


#write.csv(try4,"2 Data/Clean data/census19_all.csv",row.names = F)



library(VIM)
mice_plot1 <- aggr(sex19.mis , col=c('navyblue','yellow'),
                   numbers=TRUE, sortVars=TRUE,
                   labels=names(sex.mis ), cex.axis=.7,
                   gap=3, ylab=c("Missing data","Pattern"))

## impute with MICE
library(mice)
try6 <- mice(try5[,10:11] , m=5, maxit = 50, method = 'pmm', seed = 500)
summary(try6 )

## Save sexed19.csv, the raw data set with total number of male and female per EU (not per quadrat)
write.csv(sexed19,"2 Data/Raw data/sexed19.csv",row.names = F) # with all the 72 plots

#example:
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
md.pattern(iris.mis)
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

