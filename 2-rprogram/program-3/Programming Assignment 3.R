## - Start of Q1 -

##read the care measures file
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

##retrieve col namesc
colnames(outcome)
names(outcome)

##retieve number of rows abd cols
nrow(outcome)
ncol(outcome)

## Retreive 30-days death rates [col 11]
ThiryDaysDeathRates<-as.numeric(outcome[, 11])

## Histogram of 30-days death rates [col 11]
hist(ThiryDaysDeathRates)

## - End of Q1 -

## - Start of Q2 -
best<-function(state, outcome) {
        
        ##Read outcome data
        dfcaremeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##The three valid outcomes
        validoutcomes<-c("heart attack", "heart failure", "pneumonia")
        
        ##Retrieve unique states from the outcome data frame
        states<-unique(dfcaremeasures[,"State"])
        
        ##Check if the input state is valid else stop
        if (!is.na(state)){
                
                ##Convert state to upper case
                state<-toupper(state)
                if (!is.element(state, states)){
                        stop("invalid state")
                        
                }
                               
        }
        
        ## check if the input outcome is valid
        if (!is.na(outcome)){
                
                if (!is.element(outcome, validoutcomes)){
                        stop("invalid outcome")
                        
                }
                
        }
                
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
}

