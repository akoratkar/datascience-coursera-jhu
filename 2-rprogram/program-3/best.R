best<-function(state, outcome) {
        
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
        
        besthospitalname = character(0)
        
        if (nrow(thisstatedata)>1){
                
                ## Calculate Min for the outcome
                outcomevalues<-thisstatedata[,outcomecols[outcome]]
                outomeevalues<-outcomevalues[complete.cases(outcomevalues)]
                outcomevalues<-outcomevalues[outcomevalues!="Not Available"]
                outcomevaluesclean<-as.numeric(outcomevalues)
                minoutcome<-min(outcomevaluesclean)
                
                minoutcome<-format(round(minoutcome, 2), nsmall = 1)
                                                        
                    
                ## Get the hospitals with the min
                thisstatedata<-subset(thisstatedata, thisstatedata[,outcomecols[outcome]]==minoutcome, select=Hospital.Name)
                
                ## Take the top hospital post ordering by hosiptal name 
                besthospitalname<-thisstatedata[order(1),][1]
                
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        return(besthospitalname)
                         

}