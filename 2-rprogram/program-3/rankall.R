rankall<-function(outcome, num="best") {
        
        ##Read outcome data
        dfcaremeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##The three valid outcomes and their column maps
        validoutcomes<-c("heart attack", "heart failure", "pneumonia")
        outcomecols<-c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        
        names(outcomecols)<-validoutcomes
        
        ## check if the input outcome is valid
        if (!is.na(outcome)){
                
                if (!is.element(outcome, validoutcomes)){
                        stop("invalid outcome")
                        
                }
                
        }
        
        #Eliminate NA and Not Availables
        dfcaremeasures<-subset(dfcaremeasures, dfcaremeasures[,outcomecols[outcome]]!="Not Available")
        
        ##Retrieve unique States
        uniquestates<-as.data.frame(unique(dfcaremeasures[,"State"], na.rm=TRUE))
        
        ##Sort states by names
        colnames(uniquestates)<-c("State")    
        uniquestates<-uniquestates[order(uniquestates[,"State"]),]
                    
        dfhospitalswithrank = data.frame(hospital=character(), state=character())
        
        for (i in 1:length(uniquestates)){
                
                state<-uniquestates[i]
                                     
        
                if (!is.na(state)){
                        
                        ## Subset the care meaasurs for the current state
                        dfthisstatemeasures<-subset(dfcaremeasures, dfcaremeasures$State==state)
                        
                        ##Clean the outcome data to ensure no errors in numeric
                        dfthisstatemeasures<-subset(dfthisstatemeasures, dfthisstatemeasures[,outcomecols[outcome]]!="Not Available")
                        
                        ##Set num for best and worst for the state  
                        thisstatenum<-num
                        
                        if (num=="worst") thisstatenum<-nrow(dfthisstatemeasures)
                        if (num=="best") thisstatenum<-1
                                                                                                              
                        ## Order hosipitals  by value of outcome
                        dfthisstatemeasures<-dfthisstatemeasures[order(as.numeric(dfthisstatemeasures[,outcomecols[outcome]]),dfthisstatemeasures[,"Hospital.Name"]),]
                        
                        ## Take the top hospital post ordering by hosiptal name 
                        hospitalwithrank<-dfthisstatemeasures[thisstatenum, "Hospital.Name"]
                                                
                        #Append to the data frame of hospitals and states
                        dfthishospital<-data.frame("hospital"= hospitalwithrank, "state"=state)
                        dfhospitalswithrank<-rbind(dfhospitalswithrank,dfthishospital)
                        
                }
                
                
        }
        
        ## Return hospital name in that state with the given rank
       
        return(dfhospitalswithrank)
        
        
}