#' Make Imperial College London Plots
#'
#' Used internally within a function to make these plots
#'
#' @param data_country the country data
#' @param covariates_country_long the long form of covarites
#' @param filename2 the filename
#' @param country the country or unit of interest
#' @param percent_pop the population percent
#' @param file_ext the file extension to write out
#' @export

make_icl_plots <- function(data_country,
													 covariates_country_long,
													 filename2,
													 country, percent_pop, file_ext = "svg"){

	country <- gsub(pattern = "_", replacement = " ", country)

	data_cases_95 <- data.frame(data_country$time,
															data_country$predicted_min,
															data_country$predicted_max)

	names(data_cases_95) <- c("time", "cases_min", "cases_max")

	data_cases_95$key <- rep("nintyfive", length(data_cases_95$time))

	data_cases_50 <- data.frame(data_country$time,
															data_country$predicted_min2,
															data_country$predicted_max2)
	names(data_cases_50) <- c("time", "cases_min", "cases_max")

	data_cases_50$key <- rep("fifty", length(data_cases_50$time))
	data_cases <- rbind(data_cases_95, data_cases_50)
	levels(data_cases$key) <- c("ninetyfive", "fifty")

	p1 <- ggplot2::ggplot(data_country) +
		ggplot2::geom_bar(data = data_country,
											ggplot2::aes(x = time, y = reported_cases),
						 fill = "coral4", stat='identity', alpha=0.5) +
		ggplot2::geom_ribbon(data = data_cases,
								aes(x = time, ymin = cases_min,
										ymax = cases_max, fill = key)) +
		ggplot2::xlab("") +
		ggplot2::ylab("Daily number of infections\n") +
		ggplot2::scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
		ggplot2::scale_y_continuous(expand = c(0, 0), labels = comma) +
		ggplot2::scale_fill_manual(name = "", labels = c("50%", "95%"),
											values = c(alpha("deepskyblue4", 0.55),
																 alpha("deepskyblue4", 0.45))) +
		ggpubr::theme_pubr(base_family="sans") +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
					legend.position = "None") +
		ggplot2::ggtitle(country) +
		ggplot2::guides(fill=ggplot2::guide_legend(ncol=1))

	data_deaths_95 <- data.frame(data_country$time, data_country$death_min,
															 data_country$death_max)
	names(data_deaths_95) <- c("time", "death_min", "death_max")
	data_deaths_95$key <- rep("nintyfive", length(data_deaths_95$time))
	data_deaths_50 <- data.frame(data_country$time, data_country$death_min2,
															 data_country$death_max2)
	names(data_deaths_50) <- c("time", "death_min", "death_max")
	data_deaths_50$key <- rep("fifty", length(data_deaths_50$time))
	data_deaths <- rbind(data_deaths_95, data_deaths_50)
	levels(data_deaths$key) <- c("ninetyfive", "fifty")


	p2 <-   ggplot2::ggplot(data_country, ggplot2::aes(x = time)) +
		ggplot2::geom_bar(data = data_country,
											ggplot2::aes(y = deaths, fill = "reported"),
						 fill = "coral4", stat='identity', alpha=0.5) +
		ggplot2::geom_ribbon(
			data = data_deaths,
			ggplot2::aes(ymin = death_min, ymax = death_max, fill = key)) +
		ggplot2::scale_x_date(date_breaks = "weeks", labels = date_format("%e %b")) +
		ggplot2::scale_y_continuous(expand = c(0, 0), labels = comma) +
		ggplot2::scale_fill_manual(name = "", labels = c("50%", "95%"),
											values = c(alpha("deepskyblue4", 0.55),
																 alpha("deepskyblue4", 0.45))) +
		ggplot2::ylab("Daily number of deaths\n") +
		ggplot2::xlab("") +
		ggpubr::theme_pubr(base_family="sans") +
		ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1),
					legend.position = "None") +
		ggplot2::guides(fill=ggplot2::guide_legend(ncol=1))


	plot_labels <- c("Complete lockdown",
									 "Public events banned",
									 "School closure",
									 "Self isolation",
									 "Social distancing \n encouraged")

	# Plotting interventions
	data_rt_95 <- data.frame(data_country$time,
													 data_country$rt_min, data_country$rt_max)
	names(data_rt_95) <- c("time", "rt_min", "rt_max")
	data_rt_95$key <- rep("nintyfive", length(data_rt_95$time))
	data_rt_50 <- data.frame(data_country$time, data_country$rt_min2,
													 data_country$rt_max2)
	names(data_rt_50) <- c("time", "rt_min", "rt_max")
	data_rt_50$key <- rep("fifty", length(data_rt_50$time))
	data_rt <- rbind(data_rt_95, data_rt_50)
	levels(data_rt$key) <- c("ninetyfive", "fifth")

	p3 <- ggplot2::ggplot(data_country) +
		geom_stepribbon(data = data_rt,
										ggplot2::aes(x = time,
												ymin = rt_min, ymax = rt_max,
												group = key,
												fill = key)) +
		ggplot2::geom_hline(yintercept = 1, color = 'black', size = 0.1) +
		ggplot2::geom_segment(data = covariates_country_long,
													ggplot2::aes(x = value, y = 0, xend = value, yend = max(x)),
								 linetype = "dashed", colour = "grey", alpha = 0.75) +
		ggplot2::geom_point(data = covariates_country_long,
												aes(x = value,
														y = x,
														group = key,
														shape = key,
														col = key), size = 2) +
		ggplot2::xlab("") +
		ggplot2::ylab(expression(R[t])) +
		ggplot2::scale_fill_manual(name = "", labels = c("50%", "95%"),
											values = c(alpha("seagreen", 0.75), alpha("seagreen", 0.5))) +
		ggplot2::scale_shape_manual(name = "Interventions", labels = plot_labels,
											 values = c(21, 22, 23, 24, 25, 12)) +
		ggplot2::scale_colour_discrete(name = "Interventions", labels = plot_labels) +
		ggplot2::scale_x_date(date_breaks = "weeks", labels = date_format("%e %b"),
								 limits = c(data_country$time[1],
								 					 data_country$time[length(data_country$time)])) +
		ggplot2::scale_y_continuous(expand = expansion(mult=c(0,0.1))) +
		ggpubr::theme_pubr(base_family="sans") +
		ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
		ggplot2::theme(legend.position="right")

	# Special plot settings for mobile
	p3_mobile <- p3  +
		ggplot2::theme(legend.position="below")

	# Plots for Web, Desktop version
	dir.create("web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
	cowplot::save_plot(filename = paste0("web/figures/desktop/", country, "_infections", ".svg"),
						p1, base_height = 4, base_asp = 1.618)
	cowplot::save_plot(filename = paste0("web/figures/desktop/", country, "_deaths", ".svg"),
						p2, base_height = 4, base_asp = 1.618)
	cowplot::save_plot(filename = paste0("web/figures/desktop/", country, "_rt", ".svg"),
						p3, base_height = 4, base_asp = 1.618 * 2)

	# Plots for Web, Mobile version
	dir.create("web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
	cowplot::save_plot(filename = paste0("web/figures/mobile/", country, "_infections", ".",file_ext),
						p1, base_height = 4, base_asp = 1.1)
	cowplot::save_plot(filename = paste0("web/figures/mobile/", country, "_deaths", ".",file_ext),
						p2, base_height = 4, base_asp = 1.1)
	cowplot::save_plot(filename = paste0("web/figures/mobile/", country, "_rt", ".",file_ext),
						p3_mobile, base_height = 4, base_asp = 1.1)

	# Special plot settings for mobile
	p3_mobile <- p3  +
		theme(legend.position="below")

	# Plots for Web, Desktop version
	dir.create("web/figures/desktop/", showWarnings = FALSE, recursive = TRUE)
	cowplot::save_plot(filename = paste0("web/figures/desktop/", country, "_infections", ".",file_ext),
						p1, base_height = 4, base_asp = 1.618)
	cowplot::save_plot(filename = paste0("web/figures/desktop/", country, "_deaths", ".",file_ext),
						p2, base_height = 4, base_asp = 1.618)
	cowplot::save_plot(filename = paste0("web/figures/desktop/", country, "_rt", ".",file_ext),
						p3, base_height = 4, base_asp = 1.618 * 2)

	# Plots for Web, Mobile version
	dir.create("web/figures/mobile/", showWarnings = FALSE, recursive = TRUE)
	cowplot::save_plot(filename = paste0("web/figures/mobile/", country, "_infections", ".",file_ext),
						p1, base_height = 4, base_asp = 1.1)
	cowplot::save_plot(filename = paste0("web/figures/mobile/", country, "_deaths", ".",file_ext),
						p2, base_height = 4, base_asp = 1.1)
	cowplot::save_plot(filename = paste0("web/figures/mobile/", country, "_rt", ".",file_ext),
						p3_mobile, base_height = 4, base_asp = 1.1)

	p <- cowplot::plot_grid(p1, p2, p3, ncol = 3, rel_widths = c(1, 1, 2))
	cowplot::save_plot(filename = paste0("figures/", country, "_three_pannel_", filename2, ".png"),
						p, base_width = 14)
}
