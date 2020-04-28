#' Make Three Panel
#'
#'@param filename2 the name
#'@param percent_pop the population percentage
#'@param covariates the covariates to use
#'@param covariates_long if using the long covariates
#'@param countries the list of countries
#'@param data_country the data for the country
#'
make_three_pannel_plot <- function(filename2,
																	 percent_pop = FALSE,
																	 covariates,
																	 covariates_long = NULL,
																	 countries,
																	 data_country){

	if(!is.null(covariates_long)){
		covariates = read.csv(covariates_long, stringsAsFactors = FALSE)
		names_covariates = c('Schools + Universities','Self-isolating if ill', 'Public events',
												 'Lockdown', 'Social distancing encouraged')
		covariates <- covariates %>%
			dplyr::filter((Type %in% names_covariates))
		covariates <- covariates[,c(1,2,4)]
		covariates <- spread(covariates, Type, Date.effective)
		names(covariates) <- c('Country','lockdown', 'public_events', 'schools_universities','self_isolating_if_ill', 'social_distancing_encouraged')
		covariates <- covariates[c('Country','schools_universities', 'self_isolating_if_ill', 'public_events', 'lockdown', 'social_distancing_encouraged')]
		covariates$schools_universities <- as.Date(covariates$schools_universities, format = "%d.%m.%Y")
		covariates$lockdown <- as.Date(covariates$lockdown, format = "%d.%m.%Y")
		covariates$public_events <- as.Date(covariates$public_events, format = "%d.%m.%Y")
		covariates$self_isolating_if_ill <- as.Date(covariates$self_isolating_if_ill, format = "%d.%m.%Y")
		covariates$social_distancing_encouraged <- as.Date(covariates$social_distancing_encouraged, format = "%d.%m.%Y")

	}

	all_data <- data.frame()
	all_data_out <- data.frame()
	intervention_data <- data.frame()
	for(i in 1:length(countries)){
		print(i)
		N <- length(dates[[i]])
		country <- countries[[i]]

		predicted_cases <- colMeans(prediction[,1:N,i])
		predicted_cases_li <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.025)
		predicted_cases_ui <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.975)
		predicted_cases_li2 <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.25)
		predicted_cases_ui2 <- matrixStats::colQuantiles(prediction[,1:N,i], probs=.75)


		estimated_deaths <- colMeans(estimated.deaths[,1:N,i])
		estimated_deaths_li <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.025)
		estimated_deaths_ui <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.975)
		estimated_deaths_li2 <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.25)
		estimated_deaths_ui2 <- matrixStats::colQuantiles(estimated.deaths[,1:N,i], probs=.75)

		rt <- colMeans(out$Rt_adj[,1:N,i])
		rt_li <- matrixStats::colQuantiles(out$Rt_adj[,1:N,i],probs=.025)
		rt_ui <- matrixStats::colQuantiles(out$Rt_adj[,1:N,i],probs=.975)
		rt_li2 <- matrixStats::colQuantiles(out$Rt_adj[,1:N,i],probs=.25)
		rt_ui2 <- matrixStats::colQuantiles(out$Rt_adj[,1:N,i],probs=.75)


		# delete these 2 lines
		covariates_country <- covariates[which(covariates$Country == country), 2:6]
		covariates_country_long <- tidyr::gather(covariates_country,
																						 key = "key",
																			value = "value")
		covariates_country_long$x <- rep(NULL,
																		 length(covariates_country_long$key))
		un_dates <- unique(covariates_country_long$value)

		for (k in 1:length(un_dates)){
			idxs <- which(covariates_country_long$value == un_dates[k])
			max_val <- round(max(rt_ui)) + 0.3
			for (j in idxs){
				covariates_country_long$x[j] <- max_val
				max_val <- max_val - 0.3
			}
		}


		covariates_country_long$value <- lubridate::as_date(covariates_country_long$value)
		covariates_country_long$country <- rep(country,
																					 length(covariates_country_long$value))

		data_country <- data.frame("time" = lubridate::as_date(as.character(dates[[i]])),
															 "country" = rep(country, length(dates[[i]])),
															 "reported_cases" = reported_cases[[i]],
															 "reported_cases_c" = cumsum(reported_cases[[i]]),
															 "predicted_cases_c" = cumsum(predicted_cases),
															 "predicted_min_c" = cumsum(predicted_cases_li),
															 "predicted_max_c" = cumsum(predicted_cases_ui),
															 "predicted_cases" = predicted_cases,
															 "predicted_min" = predicted_cases_li,
															 "predicted_max" = predicted_cases_ui,
															 "predicted_min2" = predicted_cases_li2,
															 "predicted_max2" = predicted_cases_ui2,
															 "deaths" = deaths_by_country[[i]],
															 "deaths_c" = cumsum(deaths_by_country[[i]]),
															 "estimated_deaths_c" =  cumsum(estimated_deaths),
															 "death_min_c" = cumsum(estimated_deaths_li),
															 "death_max_c"= cumsum(estimated_deaths_ui),
															 "estimated_deaths" = estimated_deaths,
															 "death_min" = estimated_deaths_li,
															 "death_max"= estimated_deaths_ui,
															 "death_min2" = estimated_deaths_li2,
															 "death_max2"= estimated_deaths_ui2,
															 "rt" = rt,
															 "rt_min" = rt_li,
															 "rt_max" = rt_ui,
															 "rt_min2" = rt_li2,
															 "rt_max2" = rt_ui2)

		colnames_csv <- c("time","country", "reported_cases", "reported_cases_c",  "predicted_cases_c",
											"predicted_min_c","predicted_max_c", "predicted_cases","predicted_min", "predicted_max",
											"deaths", "deaths_c", "estimated_deaths_c", "death_min_c",  "death_max_c","estimated_deaths",
											"death_min", "death_max","rt", "rt_min","rt_max")
		data_country_out_temp <- data_country[,colnames_csv]
		colnames(data_country_out_temp) <- c("time","country", "reported_cases", "reported_cases_cumulative",  "predicted_infections_mean_cumulative",
																				 "predicted_infections_lower_CI_95_cumulative","predicted_infections_higher_CI_95_cumulative",
																				 "predicted_infections_mean","predicted_infections_lower_CI_95", "predicted_infections_higher_CI_95_cumulative",
																				 "reported_deaths", "reported_deaths_cumulative", "estimated_deaths_mean_cumulative",
																				 "estimated_deaths_lower_CI_95_cumulative",  "estimated_deaths_higher_CI_95_cumulative",
																				 "estimated_deaths_mean", "estimated_deaths_lower_CI_95", "estimated_deaths_higher_CI_95",
																				 "mean_time_varying_reproduction_number_R(t)", "time_varying_reproduction_number_R(t)_lower_CI_95",
																				 "time_varying_reproduction_number_R(t)_higher_CI_95")

		all_data <- rbind(all_data, data_country)
		all_data_out <- rbind(all_data_out, data_country_out_temp)
		intervention_data <- rbind(intervention_data, covariates_country_long)

		make_icl_plots(data_country = data_country,
							 covariates_country_long = covariates_country_long,
							 filename2 = filename2,
							 country = country,
							 percent_pop = percent_pop)

	}
	write.csv(all_data, paste0("results/", "base-plot.csv"))
	write.csv(intervention_data, paste0("results/", "base-intervention.csv"))
	write.csv(all_data_out, paste0("web/data/", "results.csv"))
}
