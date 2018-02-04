
# creates plots for clustering comparision
# input1: x - dataframe
# input2: classVar - class variables, which contain clustering results, or classes which the clustering should found,
#     if NULL, 'cluster' will be considered as only one class variable
# chart1: barchart of cluster/class counts of each defined class variable by first class variable defined in input2
# charts2: barchart of category counts of each categorical variable by each cluster/class variable
# charts3: boxplot of each continuous variable by each cluster/class variable
# output: list with all generated plots

library(ggplot2)

makeCPlots <- function(x, classVar = 'cluster'){
  df <- x
  df[, classVar] <- lapply(df[, classVar, drop = F], factor)

  dfVars <- df[, !(names(df) %in% classVar), drop = F]
  dfClassVars <- df[, names(df) %in% classVar, drop = F]
  
  numCont <- sum(sapply(dfVars, function(x){is.numeric(x) | is.integer(x)}))
  numCats <- sum(sapply(dfVars, function(x){is.ordered(x) | is.factor(x) | is.character(x)}))
  namesCont <- names(dfVars[, sapply(dfVars, function(x){is.numeric(x) | is.integer(x)}), drop = F])
  namesCats <- names(dfVars[, sapply(dfVars, function(x){is.ordered(x) | is.factor(x) | is.character(x)}), drop = F])
  
  plotList <- list()
  
  # chart of first class variable vs. other class variables

  if (length(classVar) > 2){
    cVar1 <- classVar[1]
    
    dfToPlotReshaped <- gather(dfClassVars, 'variable', 'value', classVar[2:length(classVar)])
    plotClass <- ggplot(aes(x = as.factor(variable)), data = dfToPlotReshaped) +
      geom_bar(aes(fill = as.factor(value)), position = 'dodge') +
      facet_wrap(as.formula(paste('~', cVar1))) +
      ggtitle(paste('Categories count for class variables by ', cVar1)) +
      theme_bw()
    
    print(plotClass)
  } else{
    if (length(classVar) == 2){
      plotClass <- ggplot(aes_string(x = classVar[1]), data = dfClassVars) +
        geom_bar(aes_string(fill = classVar[2]), position = 'dodge') +
        ggtitle(paste('Categories count for ', classVar[2], 'by ', classVar[1])) +
        theme_bw()
      print(plotClass)
    }
  }
  
  if (exists('plotClass')){
    plotList <- append(plotList, list(plotClass))
  }
  
  for (cVar in classVar){
    
    # chart for continuous variables/measures

    if (numCont > 1){
      dfToPlot <- cbind(dfClassVars[, cVar, drop = F], dfVars[sapply(dfVars, function(x){is.numeric(x) | is.integer(x)})])
      dfToPlotReshaped <- gather(dfToPlot, 'variable', 'value', namesCont)
      
      plotCont <- ggplot(aes_string(x = cVar, y = 'value'), data = dfToPlotReshaped) +
        geom_boxplot(aes(fill = as.factor(variable))) +
        ggtitle(paste('Normalized measures by ', cVar)) +
        theme_bw()
      
      print(plotCont)
    } else {
      if(numCont == 1){
        dfToPlot <- cbind(dfClassVars[, cVar, drop = F], dfVars[,sapply(dfVars, function(x){is.numeric(x) | is.integer(x)}), drop = F])
        
        plotCont <- ggplot(aes_string(x = cVar, y = namesCont), data = dfToPlot) +
          geom_boxplot() +
          ggtitle(paste('Normalized ', namesCont, ' by', cVar))
        
        print(plotCont)
      }
    }
    
    # chart for factors/categorical variables
    
    if (numCats > 1){
      dfToPlot <- cbind(dfClassVars[, cVar, drop = F], dfVars[sapply(dfVars, function(x){is.ordered(x) | is.factor(x) | is.character(x)})])
      dfToPlotReshaped <- gather(dfToPlot, 'variable', 'value', namesCats)
      print(str(dfToPlotReshaped))
      print(dfToPlotReshaped)
      
      plotCats <- ggplot(aes(x = as.factor(variable)), data = dfToPlotReshaped) +
        geom_bar(aes(fill = as.factor(value)), position = 'dodge') +
        facet_wrap(as.formula(paste('~', cVar))) +
        ggtitle(paste('Categories count for dimensions by ', cVar)) +
        theme_bw()
      
      print(plotCats)
    } else {
      if(numCats == 1){
        dfToPlot <- cbind(dfClassVars[, cVar, drop = F], dfVars[,sapply(dfVars, function(x){is.ordered(x) | is.factor(x) | is.character(x)}), drop = F])
        
        plotCats <- ggplot(aes_string(x = cVar), data = dfToPlot) +
          geom_bar(aes_string(fill = namesCats), position = 'dodge') +
          ggtitle(paste('Categories count by', cVar)) +
          theme_bw()
        
        print(plotCats)
      }
    }
    
    if (exists('plotCont')){
      plotList <- append(plotList, list(plotCont))
    }
    if (exists('plotCats')){
      plotList <- append(plotList, list(plotCats))
    }
    
  }
  return(plotList)
  
}