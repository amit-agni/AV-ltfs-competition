library(pacman)

p_load(caret)

str(iris)

temp <- iris
str(temp)

dummies <- dummyVars(~.,data=temp, fullRank = TRUE)

new_temp <- predict(dummies, newdata = temp)

new_temp
head(new_temp)

str(new_temp)
class(new_temp)


mtcars[,2]


temp <- data.frame(status=factor(c("Lo", "Hi", "Med", "Med", "Hi"), levels=c("Lo", "Med", "Hi"), ordered=TRUE))
str(temp)
temp

dummies <- dummyVars(~., data=temp)

new_temp <- predict(dummies,newdata = temp)

new_temp




temp <- data.frame(status=factor(c("Lo", "Hi", "Med", "Med", "Hi","VL","VH","VM"), levels=c("Lo", "Med", "Hi","VL","VH","VM"), ordered=TRUE))
str(temp)
temp
model.matrix(~., data = temp)


to do tomorow
change some features to ordered
    PERFORM_CNS.SCORE.DESCRIPTION
    AVERAGE.ACCT.AGE
    CREDIT.HISTORY.LENGTH
    NO.OF_INQUIRIES
    PRI.OVERDUE.ACCTS
dummyvars use fullrank = TRUE



unique(DT$Employee_code_ID)
