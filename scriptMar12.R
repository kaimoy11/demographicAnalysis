library(ggplot2)
#plot visual
#data must be as.character
ggplot(data = erimData) + 
  geom_bar(mapping = aes(x = ResType, fill = petOwnership ))



#subset of only single family homes who OWN house
singleFamilyOwns <- erimData[which(erimData$ResType == 3 & erimData$ResStatus == 1),]

#distribution of pet ownership in single family owned homes

#data must be as.character
ggplot(data = singleFamilyOwns) + 
  geom_bar(mapping = aes(x = petOwnership, fill = petOwnership )) +
  labs(title = "Pet Ownership in Single Family Homes that are Owned",
       )


#create linear regression with target variable as own house not rent so that they can
#install doggy door

#create data set with cleaned data

#prepare data for regression through standardization







#binary 0 = single 
# 1 = married
linearResStatus <- lm(ResStatus ~ HHInc + ResType + HHNbr
                         +MWrkHrs + FWrkHrs + MEdu + FEdu + MeanAge + Cable
                         + YogExp + DinExp, data = erimData)
summary(linearResStatus)

#assume
subCoupled
subSingle
#regression for number of pets of married ( relation = 1)
coupledPetsLinear <- lm(totalPets ~ HHInc + ResType + HHNbr
                      +MWrkHrs + FWrkHrs + MEdu + FEdu + MAge + FAge + Cable
                      + YogExp + DinExp, data = subCoupled)
summary(coupledPetsLinear)
#for sub single relation = 0
singlePetsLinear  <- lm(totalPets ~ HHInc + ResType + HHNbr
                      +MWrkHrs + FWrkHrs + MEdu + FEdu + MeanAge + Cable #note Mean Age
                      + YogExp + DinExp, data = subSingle)
summary(singlePetsLinear)
#calculate mean age in regards to missing variables.
#will use whichever age is present and ignore the other.



erimData$scaledMwrkHrs <- scale(erimData[ ,"MWrkHrs"])
erimData$scaledFwrkHrs <- scale(erimData[ ,"FWrkHrs"])
erimData$scaledYog <- scale(erimData[ ,"YogExp"])


subset$scaledYog <- scale(subset[ ,"YogExp"])

#regression without age variable
#regression for number of pets of married ( relation = 1)


numPetsRegression <- lm(ResStatus ~ ResType+ HHInc  + totalPets + HHNbr + 
                          AvgAge + Cable + scaledYog, data = erimData)

summary(numPetsRegression)



#create age variabel
currentYear <- as.numeric(format(Sys.time(), "%Y"))
#need to clean up so that they work when the complement is missing
erimData$MAge <- ifelse(erimData$MBirth > 0,currentYear- erimData$MBirth, NA) #difference between current year and birth year
erimData$FAge <- ifelse(erimData$FBirth > 0,currentYear- erimData$FBirth, NA)

erimData$AvgAge <- round(ifelse(is.na(erimData$MAge), erimData$FAge, ifelse(is.na(erimData$FAge), erimData$MAge, (erimData$MAge + erimData$FAge)/2)),0)

#mess around linear

messRegression <- lm(MWrkHrs ~ ResType+ HHInc  + totalPets + HHNbr + 
                          AvgAge + Cable + scaledYog, data = erimData)
summary(messRegression)


messRegression <- lm(YogExp ~ ResType+ HHInc  + totalPets + HHNbr + 
                       AvgAge + Cable , data = erimData)

subset <- myData[which(myData$ResType== 3 & myData$ResStatus== 1  
                       & myData$Cable == 1),]

# messRegression <- lm(totalPets ~ HHInc + HHNbr + 
#                        AvgAge  + scaledYog, data = subset)

messRegression <- lm(MWrkHrs ~ HHInc + HHNbr + 
                       AvgAge  + scaledYog, data = subset)
