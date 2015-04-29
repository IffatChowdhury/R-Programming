#pollutantmean <- function(directory, pollutant, id = 1:332) {
  #set the path
 # path = directory
  
  #get the file List in that directory
 # fileList = list.files(path)
  
  #extract the file names and store as numeric for comparison
 # file.names = as.numeric(sub("\\.csv$","",fileList))
  
  #select files to be imported based on the user input or default
 # selected.files = fileList[match(id,file.names)]
  
  #import data
 # Data = lapply(file.path(path,selected.files),read.csv)
  
  #convert into data frame
 # Data = do.call(rbind.data.frame,Data)
  
  #calculate mean
 # mean(Data[,pollutant],na.rm=TRUE)
  
#}


pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
		
		listOfFiles = list.files(directory)
		allFiles = as.numeric(sub("\\.csv$","",listOfFiles))
		idEdFiles = fileList[match(id,allFiles)]
		dataSet = lapply(file.path(directory,idEdFiles),read.csv)
		selectedData = do.call(rbind.data.frame,dataSet)
		mean(selectedData[,pollutant],na.rm=TRUE)
		
}