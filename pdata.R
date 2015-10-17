library(readr)
library(xgboost)

set.seed(1)
## Loading data
train <- read_csv("input/train.csv")
test  <- read_csv("input/test.csv")
y = train$target
test_id = test$ID

#removing id and label from data
train = subset(train, select=-c(ID, target))
test = subset(test,select=-c(ID))
# removing NA columns
removing_column <- c("VAR_0205","VAR_0207","VAR_0213","VAR_0214","VAR_0840","VAR_0847",
"VAR_1428","VAR_0008","VAR_0009","VAR_0010","VAR_0011","VAR_0012",
"VAR_0018","VAR_0019","VAR_0020","VAR_0021","VAR_0022","VAR_0023",
"VAR_0024","VAR_0025","VAR_0026","VAR_0027","VAR_0028","VAR_0029",
"VAR_0030","VAR_0031","VAR_0032","VAR_0038","VAR_0039","VAR_0040",
"VAR_0041","VAR_0042","VAR_0043","VAR_0044","VAR_0188","VAR_0189",
"VAR_0190","VAR_0196","VAR_0197","VAR_0199","VAR_0202","VAR_0203",
"VAR_0215","VAR_0216","VAR_0221","VAR_0222","VAR_0223","VAR_0229",
"VAR_0239","VAR_1427","VAR_0246","VAR_0394","VAR_0438","VAR_0446",
"VAR_0527","VAR_0528","VAR_0530")
train=train[,!(names(train) %in% removing_column)]
test=test[,!(names(test) %in% removing_column)]
# fix city name
library(dplyr)
library(stringdist)
reviewDupes <- mutate(train, City = VAR_0200, State = VAR_0237, Zip=VAR_0241) %>% 
	select(City, State, Zip) %>%
	mutate(stateZip = paste(Zip, State, sep="_"),
         fullGeoID = paste(City, Zip, State, sep="_")) %>%
   distinct()
potentialDupes <- group_by(reviewDupes, stateZip) %>% 
  dplyr::summarise(n = n(), 
                   altName = first(City), # prettier: most common
                   altID = first(fullGeoID)) %>% 
  filter(n > 1)
dupes <- mutate(left_join(potentialDupes, reviewDupes, by="stateZip"), 
                dist=stringdist(altName, City)) %>% 
                filter(dist >= 1 & dist <= 2)
train <- mutate(train, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
train <- left_join(train, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
	mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
	select(-fullGeoID, -altName)
  
test <- mutate(test, fullGeoID = paste(VAR_0200, VAR_0241, VAR_0237, sep="_"))
test <- left_join(test, select(dupes, altName, fullGeoID), by="fullGeoID") %>%
	mutate(VAR_0200 = ifelse(is.na(altName), VAR_0200, altName)) %>%
	select(-fullGeoID, -altName)
### Processing Date
datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

train_cropped <- train[datecolumns]
train_cc <- data.frame(apply(train_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  train[dc] <- NULL
  train[dc] <- train_cc[dc]
}

train_cc <- NULL
train_cropped <- NULL
gc()

test_cropped <- test[datecolumns]
test_cc <- data.frame(apply(test_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  test[dc] <- NULL
  test[dc] <- test_cc[dc]
}

test_cc <- NULL
test_cropped <- NULL
gc()

#train[train==-1] = NA
train[train==""] = NA
train[train=="[]"] = NA 
#test[test==-1]=NA
test[test==""]=NA
test[test=="[]"]=NA

feature.names <- names(train)

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}
train[is.na(train)] <- -1
test[is.na(test)]   <- -1
not_dup_index = !duplicated(lapply(train, summary))
train = train[not_dup_index]
test = test[not_dup_index]
gc()

write_csv(train,"input/ptrain.csv")
write_csv(test,"input/ptest.csv")
