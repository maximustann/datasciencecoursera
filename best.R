best <- function(state, outcome) {
    if(is.null(my_data)){
        my_data <<- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
    }

    state_names <- as.data.frame(table(my_data[["State"]]))[, 1]
    if(!state %in% state_names) stop("Wrong state name")
    
    disease_dict <- list(11, 17, 23)
    names(disease_dict) <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% names(disease_dict)) stop("Outcome is not in the list")
    
    
}