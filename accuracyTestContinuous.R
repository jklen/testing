
# accuracy comparision for predicting continuous variable

# input1: inputVars - input variables names or index, if not provided,
#                     all except target variable and weights column will be used
# input2: targetVar - name or index of target variable to predict
# input3: compareVars - name or index of variables, which contain predicted values from other tools
# input4: filePathOrDF - string of input csv file, or dataframe name
# input5: algName - algorithm name
# input6: algParams - algorithm parameters as list, if not provided, default values will be used
#
# exclude rows with NA values
#
# regression - lm() internally one-hot encodes categorical variables
#     parameters:
#         normalize - if normalize continuous variables, default TRUE
#         fitIntercept - if intercept should be fitted, default TRUE
#         weightsCol - column name/index with weights, default FALSE
# decision trees - no data preparation needed

library(scales)
library(rpart)
library(dummy)

compare_continuous <- function(inputVars = NULL, targetVar, compareVars = NULL, filePathOrDF,
                               algName = 'regression', algParams = NULL){
  
  # read file to dataframe, or take input dataframe
  
  if (is.character(filePathOrDF)){
    df <- data.table::fread(filePathOrDF)
  } else {
    df <- filePathOrDF
  }
  
  # if defined variables are indexes, get their names
  
  if (is.integer(inputVars) | is.numeric(inputVars)){
    inputVars <- names(df)[inputVars]
    print(inputVars)
  }
  if (is.integer(targetVar) | is.numeric(targetVar)){
    targetVar <- names(df)[targetVar]
    print(targetVar)
  }
  if (is.integer(compareVars) | is.numeric(compareVars)){
    compareVars <- names(df)[compareVars]
    print(compareVars)
  }
  
  df <- as.data.frame(df)
  df <- na.omit(df)
  
  # filter out variables, so only target variable and input variables remain in dfToPredict
  
  dfToPredict <- df[, !(names(df) %in% compareVars)]
  print(names(dfToPredict))

  if (!is.null(inputVars)){
    print(c(targetVar, inputVars))
    dfToPredict <- dfToPredict[, c(targetVar, inputVars)]
  }
  
  # regression
  
  # parameters
  if (algName == 'regression'){
    regParams <- list()
    regParams$fitIntercept <- ifelse(is.null(algParams$fitIntercept), T, algParams$fitIntercept)
    regParams$normalize <- ifelse(is.null(algParams$normalize), T, algParams$normalize)
    
    if (!is.null(algParams$weightsCol)){
      if (is.integer(algParams$weightsCol) | is.numeric(algParams$weightsCol)){
        weightsColName <- names(df)[algParams$weightsCol]
        regParams$weightsCol <- df[, weightsColName]
      } else {
        regParams$weightsCol <- df[, algParams$weightsCol]
      }
    }
    
    # normalize continuous input variables
    if (regParams$normalize == T){
      dfToPredict[sapply(dfToPredict, function(x) is.numeric(x) | is.integer(x)) & names(dfToPredict) != targetVar] <- 
        lapply(dfToPredict[sapply(dfToPredict, function(x) is.numeric(x) | is.integer(x)) & names(dfToPredict) != targetVar], rescale)
    }
    
    # create formula
    if (regParams$fitIntercept){
      formula <- as.formula(paste(targetVar, '~ .'))
    } else {
      formula <- as.formula(paste(targetVar, '~ . - 1'))
    }
    
    regModel <- lm(formula = formula, data = dfToPredict, weights = regParams$weightsCol)
    
    return(regModel)
    
  }
  
}