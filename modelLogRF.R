library(randomForest)

train = read.csv("train2016.csv")

test = read.csv("test2016.csv")
testID = test$USER_ID

train = train[,3:108]
test = test[,3:107]

modelRF = randomForest(Party ~ ., data=train)

predRF = predict(modelRF, newdata=test, type="prob")[,2]

colsImportant = c("Income", "HouseholdStatus", "EducationLevel", "Party", "Q124742", "Q121699",
                  "Q121700", "Q120194", "Q118232", "Q116197", "Q115611", "Q114517", "Q113181",
                  "Q112478", "Q112270", "Q108950", "Q109244", "Q108342", "Q102687", "Q101596", "Q100689",
                  "Q99716", "Q98869", "Q98578", "Q98197")

colsImportantTest = c("Income", "HouseholdStatus", "EducationLevel", "Q124742", "Q121699",
                      "Q121700", "Q120194", "Q118232", "Q116197", "Q115611", "Q114517", "Q113181",
                      "Q112478", "Q112270", "Q108950", "Q109244", "Q108342", "Q102687", "Q101596", "Q100689",
                      "Q99716", "Q98869", "Q98578", "Q98197")

train = train[, colsImportant]
test = test[, colsImportantTest]

modelLog = glm(Party ~ ., data=train, family=binomial)

predLog = predict(modelLog, newdata=test, type="response")

predLogRF = 0.5 * predLog + 0.5 * predRF

threshold = 0.5

PredTestLabels = as.factor(ifelse(predLogRF<threshold, "Democrat", "Republican"))

MySubmission = data.frame(USER_ID = testID, Predictions = PredTestLabels)

write.csv(MySubmission, "SubmissionModelLogRF55.csv", row.names=FALSE)
