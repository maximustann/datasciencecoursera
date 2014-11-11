best <- function(state, outcome) {
	if(!exists("my_data")){
        my_data <<- read.csv("outcome-of-care-measures.csv", 
                             colClasses = "character")
	}

    state_names <- as.data.frame(table(my_data[["State"]]))[, 1]
    if(!state %in% state_names) stop("Wrong state name")

    disease_dict <- list(11, 17, 23)
    names(disease_dict) <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% names(disease_dict)) stop("Outcome is not in the list")

	if(!exists("splited_data")){
		splited_data <<- split(my_data[, c(2, 11, 17, 23)], my_data[, 7])
	}
	
	if(outcome == "heart attack") {
		name_vector <- character()
		d <- as.numeric(splited_data[[state]][, 2])
		min_value <- min(d, na.rm = T)
		for(i in 1:length(splited_data[[state]][, 2])){
			value <- as.numeric(splited_data[[state]][, 2][i])
			if(is.na(value)){
				next
			}
			if(min_value == value){
				name_vector <- c(name_vector, splited_data[[state]][, 1][i])
			}
		}
		name_vector <- sort(name_vector)
		return(name_vector[1])
	}
	else if(outcome == "heart failure"){
		name_vector <- character()
		d <- as.numeric(splited_data[[state]][, 3])
		min_value <- min(d, na.rm = T)
		for(i in 1:length(splited_data[[state]][, 3])){
			value <- as.numeric(splited_data[[state]][, 3][i])
			if(is.na(value)){
				next
			}
			if(min_value == value){
				name_vector <- c(name_vector, splited_data[[state]][, 1][i])
			}
		}
		name_vector <- sort(name_vector)
		return(name_vector[1])
	}
	else {
		name_vector <- character()
		d <- as.numeric(splited_data[[state]][, 4])
		min_value <- min(d, na.rm = T)
		for(i in 1:length(splited_data[[state]][, 4])){
			value <- as.numeric(splited_data[[state]][, 4][i])
			if(is.na(value)){
				next
			}
			if(min_value == value){
				name_vector <- c(name_vector, splited_data[[state]][, 1][i])
			}
		}
		name_vector <- sort(name_vector)
		return(name_vector[1])
	}
	
}
