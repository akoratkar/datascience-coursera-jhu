rankhospital<-function(state, outcome, num) {
        
        ##Read States from the Hospital Data
        dfallstates<-read.csv("hospital-data.csv", colClasses = "character")
        uniquestates<-unique(dfallstates[,"State"])
        
        ##Check if the input tate is valid else stop
        if (!is.na(state)){
                
                ##Convert state to upper case
                state<-toupper(state)
                if (!is.element(state, uniquestates)){
                        stop("invalid state")
                        
                }
                
        }
        
        ## Retrieve hospitals for the input state      
        hospitalsinstate<-subset(dfallstates, State==state, na.rm=TRUE)
        
        ## check if num is invalid i.e. greater than number of hospitals in state
        if (!is.na(num) && is.numeric(num)){
                
                if (num>nrow(hospitalsinstate)) return (NA)
                
        }
        
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
        
        ## Retrieve subset of the data for the input state      
        thisstatedata<-subset(dfcaremeasures, State==state, na.rm=TRUE)
                
        hospitalwithrank = character()
        
        if (nrow(thisstatedata)>1){
                
                #Eliminate NA and Not Availables
                thisstatedata<-subset(thisstatedata, thisstatedata[,outcomecols[outcome]]!="Not Available", na.rm=TRUE)
                 
                      
                ## Order state data by value of outcome
                thisstatedata<-thisstatedata[order(as.numeric(thisstatedata[,outcomecols[outcome]]),thisstatedata[,"Hospital.Name"]),]
                
                if (num=="worst") num<-nrow(thisstatedata)
                if (num=="best") num<-1
                
                ## Take the top hospital post ordering by hosiptal name 
                hospitalwithrank<-thisstatedata[num, "Hospital.Name"]
                
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        return(hospitalwithrank)
        
        
}