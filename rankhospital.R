rankhospital <- function(state, outcome, num = "best"){
	if(!exists("my_data")) {
		my_data <<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
		d <- as.numeric(splited_data[[state]][, 2])
		state_heart_attack <- data.frame(splited_data[[state]][, 1], d)
		state_heart_attack <- state_heart_attack[order(state_heart_attack[, 2]), ]
		if(num == "best"){
			return(as.character(state_heart_attack[1, ][[1]]))
		}
		else if(num == "worst"){
			state_heart_attack <- state_heart_attack[complete.cases(state_heart_attack), ]
			return(as.character(state_heart_attack[, 1][[length(state_heart_attack)]]))
		}
		else{
			if(num > length(state_heart_attack)){
				return(NA)
			}
			print(state_heart_attack[, 1])
			return(as.character(state_heart_attack[, 1][[num]]))
		}
	}
	else if(outcome == "heart failure"){
		d <- as.numeric(splited_data[[state]][, 3])
		state_heart_attack <- data.frame(splited_data[[state]][, 1], d)
		state_heart_attack <- state_heart_attack[order(state_heart_attack[, 2]), ]
		print(state_heart_attack)
		if(num == "best"){
			return(as.character(state_heart_attack[1, ][[1]]))
		}
		else if(num == "worst"){
			state_heart_attack <- state_heart_attack[complete.cases(state_heart_attack), ]
			return(as.character(state_heart_attack[, 1][[length(state_heart_attack)]]))
		}
		else{
			if(num > nrow(state_heart_attack)){
				return(NA)
			}
			return(as.character(state_heart_attack[, 1][[num]]))
		}
	}
	else{
		d <- as.numeric(splited_data[[state]][, 4])
		state_heart_attack <- data.frame(splited_data[[state]][, 1], d)
		state_heart_attack <- state_heart_attack[order(state_heart_attack[, 2]), ]
		if(num == "best"){
			return(as.character(state_heart_attack[1, ][[1]]))
		}
		else if(num == "worst"){
			state_heart_attack <- state_heart_attack[complete.cases(state_heart_attack), ]
			return(as.character(state_heart_attack[, 1][[length(state_heart_attack)]]))
		}
		else{
			if(num > length(state_heart_attack)){
				return(NA)
			}
			return(as.character(state_heart_attack[, 1][[num]]))
		}
	}

}
