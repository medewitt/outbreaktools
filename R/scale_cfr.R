#' Function to work out correction CFR
#' @param data_1_in the input data
#' @param delay_fun the delay function to use in reporting
#'
#' @export

scale_cfr <- function(data_1_in, delay_fun){
	if(!all(c("new_cases", "new_deaths") %in% names(data_1_in))){
		stop("Please pass data that have columns named `new_cases` and `new_deaths`")
	}
	case_incidence <- data_1_in[["new_cases"]]
	death_incidence <- data_1_in[["new_deaths"]]
	cumulative_known_t <- 0 # cumulative cases with known outcome at time tt
	# Sum over cases up to time tt
	for(ii in 1:nrow(data_1_in)){
		known_i <- 0 # number of cases with known outcome at time ii
		for(jj in 0:(ii - 1)){
			known_jj <- (case_incidence[ii - jj]*delay_fun(jj))
			known_i <- known_i + known_jj
		}
		cumulative_known_t <- cumulative_known_t + known_i # Tally cumulative known
	}
	# naive CFR value
	b_tt <- sum(death_incidence)/sum(case_incidence)
	# corrected CFR estimator
	p_tt <- sum(death_incidence)/cumulative_known_t
	data.frame(nCFR = b_tt,
						 cCFR = p_tt,
						 total_deaths = sum(death_incidence),
						 cum_known_t = round(cumulative_known_t),
						 total_cases = sum(case_incidence))
}
