complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
                                                     
        df_all_monitors<-data.frame()
                                         
        for (i in seq_along(id)){
                
                if (id[i]<100){
                        this_file<-formatC(id[i],2,flag="0")
                }
                else{
                      this_file=id[i]  
                }
                this_file<-paste(directory, "/", this_file, ".csv", sep="")
                                
                df_this_monitor<-read.csv(this_file)
                                                       
                df_this_complete<-df_this_monitor[complete.cases(df_this_monitor),]
                df_all_monitors<-rbind(df_all_monitors, c(id[i], nrow(df_this_complete)))
                
        }
        
        colnames(df_all_monitors)<-c("id", "nobs")
        df_all_monitors
                
}