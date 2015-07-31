corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        v_above_threshold_monitors<-numeric(length=0)
        
        id = list.files(directory, pattern="*.csv")
                       
        for (i in seq_along(id)){
                
                this_file<-paste(directory, "/", id[i], sep="")
                                
                df_this_monitor<-read.csv(this_file)
                df_this_complete<-df_this_monitor[complete.cases(df_this_monitor),]
                
                if (nrow(df_this_complete)>threshold){
                        
                        this_corr<-cor(df_this_complete[,"sulfate"], df_this_complete[,"nitrate"])
                        v_above_threshold_monitors<-c(v_above_threshold_monitors, this_corr)
                                               
                }
                                           
        }
        
        v_above_threshold_monitors
        
}