#' Stan Model Base General
#'
#' @param dat the dat sent to the model
#' @param iter 2400
#' @param warmup 1000
#' @param chains 4
#' @param control default controls
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @export
#'
#'
stan_icl_general <- function(dat,iter = 2400, warmup = 1000, chains  = 4,
														 control = list(adapt_delta = 0.95, max_treedepth = 13), ...){


	out <- rstan::sampling(stanmodels$icl_general,
												 data = dat,
												 iter = iter,
												 warmup = warmup,
												 chains  = chains,
												 control = control, ...)
	return(out)
}

#' Stan Model Speed
#'
#' @param dat the dat sent to the model
#' @param iter 2400
#' @param warmup 1000
#' @param chains 4
#' @param control default controls
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @export
#'
#'
stan_icl_speed <- function(dat,iter = 2400, warmup = 1000, chains  = 4,
														 control = list(adapt_delta = 0.95, max_treedepth = 13), ...){


	out <- rstan::sampling(stanmodels$icl_speed,
												 data = dat,
												 iter = iter,
												 warmup = warmup,
												 chains  = chains,
												 control = control, ...)
	return(out)
}

#' Stan Model Speed2
#'
#' @param dat the dat sent to the model
#' @param iter 2400
#' @param warmup 1000
#' @param chains 4
#' @param control default controls
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' @return An object of class `stanfit` returned by `rstan::sampling`
#' @export
#'
#'
stan_icl_speed2 <- function(dat,iter = 2400, warmup = 1000, chains  = 4,
													 control = list(adapt_delta = 0.95, max_treedepth = 13), ...){


	out <- rstan::sampling(stanmodels$icl_speed2,
												 data = dat,
												 iter = iter,
												 warmup = warmup,
												 chains  = chains,
												 control = control, ...)
	return(out)
}


