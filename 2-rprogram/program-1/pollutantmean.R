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
        ## NOTE: Do not round the result!
        
              
        df_all_monitors=data.frame()
                                        
        for (i in seq_along(id)){
                
                if (id[i]<100){
                        this_file<-formatC(id[i],2,flag="0")
                }
                else{
                        this_file=id[i]  
                }
                
                this_file<-paste(directory, "/", this_file, ".csv", sep="")
                                               
                df_this_monitor<-read.csv(this_file)
                df_all_monitors<-rbind(df_all_monitors, df_this_monitor)                                              
                                                
        }
        
       mean(df_all_monitors[, pollutant], na.rm=TRUE)
        
}
        
        
