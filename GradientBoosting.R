
#comment 11
library("glmnet")
library("gbm")  

temp <- tempfile()
download.file("ftp://ftp.nhtsa.dot.gov/GES/GES12/GES12_Flatfile.zip", temp, 
              quiet = TRUE)
accident_data_set <- read.delim(unz(temp, "PERSON.TXT"))
unlink(temp)
print(sort(colnames(accident_data_set)))
table(accident_data_set$INJSEV_IM)

accident_data_set <- accident_data_set[accident_data_set$INJSEV_IM != 6, ]

for (i in 1:ncol(accident_data_set)) {
  if (sum(as.numeric(is.na(accident_data_set[, i]))) > 0) {
    num_missing <- sum(as.numeric(is.na(accident_data_set[, i])))
    print(paste0(colnames(accident_data_set)[i], ":  ", num_missing))
    
  }
}

rows_to_drop <- which(apply(accident_data_set, 1, FUN = function(X) {
  return(sum(is.na(X)) > 0)
}))
data <- accident_data_set[-rows_to_drop, ]
data$INJ_SEV <- NULL

data$INJSEV_IM <- as.numeric(data$INJSEV_IM == 4)
target <- data$INJSEV_IM

train_rows <- sample(nrow(data), round(nrow(data) * 0.5))
traindf <- data[train_rows, ]
testdf <- data[-train_rows, ]
OLS_model <- lm(INJSEV_IM ~ ., data = traindf)
response_column <- which(colnames(traindf) == "INJSEV_IM")

trainy <- traindf$INJSEV_IM
gbm_formula <- as.formula(paste0("INJSEV_IM ~ ", paste(colnames(traindf[, -response_column]), 
                                                       collapse = " + ")))

gbm_model <- gbm(gbm_formula, traindf, distribution = "bernoulli", n.trees = 500, 
                 bag.fraction = 0.75, cv.folds = 5, interaction.depth = 3)

gbm_perf <- gbm.perf(gbm_model, method = "cv")
