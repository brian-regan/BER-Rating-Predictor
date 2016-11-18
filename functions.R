import <- function(file){
  data <- read.csv(file)
  return(data)
}

multiply_areas <- function(data){
  new_columns <- c("TotWallFlow","TotRoofFlow","TotWindowFlow","TotDoorFlow")
  heat_flow <- c("AvgWallU","AvgRoofU","AvgFloorU","AvgWindowU","AvgDoorU")
  item <- c("ExposedWallArea","ExposedRoofArea","TotFloorArea","TotWindowArea","TotDoorArea")
  #for(i in 1:length(heat_flow)){
  #  data[new_columns[i]] <- data[heat_flow[i]]*data[item[i]]
  #}
  data[c("TotWallFlow","TotRoofFlow","TotWindowFlow","TotDoorFlow")] = data[c("AvgWallU","AvgRoofU","AvgFloorU","AvgWindowU","AvgDoorU")] + data[c("ExposedWallArea","ExposedRoofArea","TotFloorArea","TotWindowArea","TotDoorArea")]
  return(data)
}

chief <- function(train, test){
  #print("cleaning")
  #cleaned <- full_clean(train, test)
  #print("cleaned")
  #train <- cleaned[[1]]
  #test <- cleaned[[2]]
  
  #Multiply areas
  train <- multiply_areas(train)
  test <- multiply_areas(test)
  
  # Only use integer/numeric columns
  classes <- sapply(train, class)
  cols <- colnames(train)[classes %in% c("numeric", "integer")]
  numeric_train <- train[, cols]
  numeric_test <- test[, cols]
  
  # Remove BuildingID from Train Data
  train <- select(train, -BuildingID)
  # Remove EnergyRatingCont and BuildingID from the Test Data
  test <- select(test, -c(BuildingID, EnergyRatingCont))
  
  
    
  # Run Linear Model
  print("predicting")
  reg_model <- lm( EnergyRatingCont~., numeric_train)
  print("predictions done")
  
  summary(reg_model)
  
  print("oh fuck here it comes")
  predictions <- predict(reg_model, newdata = numeric_test)
  print("fuuuuuuuuuuuck")
  
  
  
  return(predictions)
  
}













full_clean <- function(train, test){
  
  print("YES/NO")
  # Changes Yes and Nos to 1 and 0s
  train <- clean_yes_no(train)
  test <- clean_yes_no(test)
  
  print("Rem %")
  # Remove Percent
  train <- percent_remove(train)
  test <- percent_remove(test)
  
  print("Year2Age")
  train <- year2age(train)
  test <- year2age(test)
  
  print("RemSquareMetre")
  train <- cleanSquareMeter(train)
  test <- cleanSquareMeter(test)
  
  print("MSE Correct")
  # Change MSE to all numeric
  train <- MSE_convert(train)
  test <- MSE_convert(test)
  
  print("NA Averaging")
  # List of new train and test data set with estmated NAs
  na_removed <- na_remove(train, test)
  train <- na_removed[[1]]
  test <- na_removed[[2]]
  
  
  
  
  
  
  
  return(list(train, test))
  
}





clean_yes_no <- function(df){
  
  classes <- sapply(train, class)
  colmns <- colnames(df)[!(classes %in% c("numeric", "integer"))]
  
  
  for(col in colmns){
    df[,col] <- yes_no_convert(df[,col])
  }
  
  return(df)
  
}


yes_no_convert <- function(colmn){
  
  colmn[colmn == "YES"] <- 1
  colmn[colmn == "NO"] <- 0 
  return(colmn)
}

na_remove <- function(train, test){
  if(ncol(train) != ncol(test)){print("Not same amount of columns"); return(0)}
  
  classes <- sapply(train, class)
  numeric_indexs<- (1:length(classes))[classes == "numeric" | classes == "integer"]
  
  for(i in numeric_indexs){
    #print(i)
    train_indexs <- is.na(train[,i])
    test_indexs <- is.na(test[,i])
    
    
    train[train_indexs,i] <- mean(train[,i], na.rm = TRUE)
    test[test_indexs,i] <- mean(train[,i], na.rm = TRUE)
    
  }
  return(list(train, test))
}

MSE_convert <- function(df){
  colmn <- df[, "MainSpaceEnergy"]
  new_colmn <- as.numeric(colmn)
  df[, "MainSpaceEnergy"] <- new_colmn
  return(df)
}

percent_remove <- function(df){
  colmn <- df[, "PercLivingArea"]
  new_colmn <- sapply(colmn, function(a){as.numeric(gsub("%", "", a))})
  df[, "PercLivingArea"] <- new_colmn
  return(df)
}

#Remove y from year and change to age
year2age <- function(df){
  old_year <- df$Year
  new_year <- sapply(old_year, function(a){2017 - as.numeric(gsub("Y", "", a))})
  df$Year <- NULL
  df$Age <- new_year
  return(df)
}

  #Clear sq m from Ground Floor Area
  cleanSquareMeter <- function(df){
    GF <- df$GroundFloorArea
    new_GF <- sapply(GF, function(a){as.numeric(gsub(" sq. m", "", a))})
    df$GroundFloorArea <- new_GF
    return(df)
  }
  
  