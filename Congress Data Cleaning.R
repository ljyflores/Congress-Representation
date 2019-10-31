library(pbapply)
library(data.table)

file_list <- list.files(pattern = ".csv", recursive = TRUE)
fillTheBlanks <- function(x, missing=""){
  rle <- rle(as.character(x))
  empty <- which(rle$value==missing)
  rle$values[empty] <- rle$value[empty-1] 
  inverse.rle(rle)
}

suffixes <- c('I','II','III','IV','V','Jr.','Sr.')

cleanString <- function(x){
  x = gsub('[0-9]+', '', x)
  x = gsub('\\[', '', x)
  x = gsub('\\]', '', x)
  x = gsub(',', '', x)
  x = gsub('\\(', ' ', x)
  x = gsub('\\)', ' ', x)
  return(x)
}

for (file in file_list){
  print(file)
  data <- read.csv(file, as.is = T, skip = 1)
  if (colnames(data)[1] == 'Abra'){
    data <- read.csv(file, as.is = T)
  }

  if (is.na(data[1,3])){
    if ('Representative.' %in% colnames(data)){
      colnames(data)[colnames(data)=='Representative.'] = 'Representative'
    }
    if ('X.' %in% colnames(data)){
      colnames(data)[colnames(data)=='X.'] = 'X'
    }
    data$Representative <- data$X
    data <- subset(data, select = -c(X))
  }
  colnames(data) <- c("Province","District","Name","Party")
  
  data$Province <- as.factor(data$Province)
  data$District <- as.factor(data$District)
  data$Province <- fillTheBlanks(data$Province)
  data$Year <- rep(as.integer(substr(file,10,11)),length(data$Name))
  data$First <- rep(NA, length(data$Name))
  data$Last <- rep(NA, length(data$Name))
  
  for (i in 1:length(data$Name)){
    lst <- strsplit(data$Name[i], " ")
    first = ifelse(lst[[1]][1] == 'Ma.', lst[[1]][2], lst[[1]][1])
    last = ifelse(lst[[1]][length(lst[[1]])] %in% suffixes, lst[[1]][length(lst[[1]])-1], lst[[1]][length(lst[[1]])])
    data$First[i] = cleanString(first)
    data$Last[i] = cleanString(last)
  }
  
  fwrite(data, paste0("congress_processed_", substr(file,10,11),".csv"))
}
