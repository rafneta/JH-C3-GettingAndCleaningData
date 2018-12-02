library(dplyr)


############# Begin Problem 1 #####################

# Read the data (test and train) from local folder

data.test <- read.table("test/X_test.txt")
data.train <- read.table("train/X_train.txt")


# Read label from the data (test and train) from local folder

data.test.label <- read.table("test/y_test.txt")
data.train.label <- read.table("train/y_train.txt")

# Create a new variable for train data, describe activity 

data.train.label.char <-character(length = dim(data.train.label)[1])
data.train.label.char[data.train.label==1]<-"WALKING"
data.train.label.char[data.train.label==2]<-"WALKING_UPSTAIRS"
data.train.label.char[data.train.label==3]<-"WALKING_DOWNSTAIRS"
data.train.label.char[data.train.label==4]<-"SITTING"
data.train.label.char[data.train.label==5]<-"STANDING"
data.train.label.char[data.train.label==6]<-"LAYING"

data.train.subject <- replicate(dim(data.train.label)[1],"train")

# Create a new variable for test data, describe activity 

data.test.label.char <-character(length = dim(data.test.label)[1])
data.test.label.char[data.test.label==1]<-"WALKING"
data.test.label.char[data.test.label==2]<-"WALKING_UPSTAIRS"
data.test.label.char[data.test.label==3]<-"WALKING_DOWNSTAIRS"
data.test.label.char[data.test.label==4]<-"SITTING"
data.test.label.char[data.test.label==5]<-"STANDING"
data.test.label.char[data.test.label==6]<-"LAYING"

data.test.subject <- replicate(dim(data.test.label)[1],"test")

# add varibles (describe the activity and subject) to train and test data 

data.train["activity"] = data.train.label.char
data.train["subject"]=data.train.subject
data.test["activity"] = data.test.label.char
data.test["subject"]=data.test.subject

# Merge the data

data.complete <- merge(data.train, data.test, all = TRUE)

################## End Problem 1 ########################

################### Begin Problem 2 #####################

# Read from features file as characters

features <- read.table("features.txt")
features.charvalues <- as.character(features$V2)

indices<-vector()
indices.names<-character()
mesure <- c("mean","std")

## search the strings in variable mesure, and save the index and the name 

for (i in 1:length(features.charvalues)) {
  desition <- grepl(paste(mesure, collapse = "|"),features.charvalues[i])
  if(desition){
    indices<-c(indices,i)
    indices.names<-c(indices.names,features.charvalues[i])
  }
}


data.complete.selec <- select(data.complete,indices,activity, subject)

###################### End Problem 2 ########################

###################### Begin Problem 3 ########################

# Thsi problem was solve in problem 1


###################### End Problem 3 ########################

###################### Begin Problem 4 ########################

# variable indies.names was create in problem 2,

data.complete.selec.rename <- data.complete.selec
names(data.complete.selec.rename) <- c(indices.names,"activity","subject")

###################### End Problem 4 ########################

###################### Begin Problem 5 ########################
dcsr <- data.complete.selec.rename
dcsr.as<-group_by(dcsr,activity,subject)
print(summarise_all(dcsr.as,mean))

###################### End Problem 5 ########################

