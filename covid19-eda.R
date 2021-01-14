#!/usr/bin/env Rscript
##
## Exploratory Data Analysis through visualization on COVID-19 data concerning
## the number of confirmed cases and the number of deaths every day and per
## country.
## It was conducted for the course "Programming Tools and Technologies for Data
## Science" at NTUA on Fall semester 2020-2021.
##
## Christos Katsakioris <ckatsak [at] cslab [dot] ece [dot] ntua [dot] gr>
##
## Last modification: Thu 14 Jan 2021 08:01:35 PM EET

# =============================================================================
#
# Imports & global variables
#
# =============================================================================

library(data.table) # processing
library(lubridate)  # for `mdy()` during the processing
library(ggplot2)    # plotting

#' The GitHub URL where the (third-party) dataset of COVID-19 confirmed cases
#' can be found in CSV format. NOTE: Stripped from TLS to avoid a dependency.
CONFIRMED_URL <- "http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

#' The GitHub URL where the (third-party) dataset of COVID-19 deaths can be
#' found in CSV format. NOTE: Stripped from TLS to avoid an extra dependency.
DEATHS_URL <- "http://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

#' Control the verbosity of the execution.
VERBOSE <- TRUE


# =============================================================================
#
# Initial processing
#
# =============================================================================

#' The processing function retrieves and processes the initial datasets to
#' produce (and return) the total number of confirmed cases worldwide, the
#' total number of worldwide deaths and a data.table for further processing.
#' This processing is dictated by the assignment.
processing <- function() {
  # (Down)load the latest data into a new data.table, excluding some columns
  exclusions <- c("Province/State", "Lat", "Long")
  confirmed  <- fread(CONFIRMED_URL, header = TRUE, drop = exclusions)
  deaths     <- fread(DEATHS_URL,    header = TRUE, drop = exclusions)

  # Appropriately rename the variable related to the country
  setnames(confirmed, "Country/Region", "Country")
  setnames(deaths,    "Country/Region", "Country")

  # Reshape each data.table from wide to long format
  confirmed <- melt(confirmed, id.vars = c("Country"), variable.name = "date",
                    value.name = "confirmed", variable.factor = FALSE)
  deaths    <- melt(deaths,    id.vars = c("Country"), variable.name = "date",
                    value.name = "deaths",    variable.factor = FALSE)

  # Convert each date variable from character to a date object using `mdy()`
  confirmed <- confirmed[, date := mdy(date)]
  deaths    <- deaths[   , date := mdy(date)]

  # Group them by (Country, date) summing deaths
  confirmed <-confirmed[, .(confirmed = sum(confirmed)), by = .(Country, date)]
  deaths    <-deaths[   , .(deaths    = sum(deaths)),    by = .(Country, date)]

  # Merge the two datasets into one
  dt <- merge(confirmed, deaths)

  # Calculate the total number of confirmed cases as well as the total number
  # of deaths, worldwide
  #
  # NOTE: Since the recorded data are cumulative, σ(most_recent) -> aggregate
  confirmed <- sum(dt[date == max(date)][, confirmed])
  deaths    <- sum(dt[date == max(date)][, deaths])

  # Sort by country and date
  dt <- dt[order(Country, date)]

  # Calculate daily increases
  dt[, ":="(
       confirmed.ind = confirmed - shift(confirmed, 1, type = "lag", fill = 0),
       deaths.inc    = deaths    - shift(deaths,    1, type = "lag", fill = 0)
     ),
     by = .(Country)
  ]

  # Return the results as a list
  ret <- list(confirmed, deaths, dt)
  names(ret) <- c("confirmed", "deaths", "dt")
  ret
}


# =============================================================================
#
# Debug printing
#
# =============================================================================

#' Auxiliary function to print the results of the `processing()` function for
#' debugging purposes.
print_initial_processing_results <- function(results) {
  if (FALSE == VERBOSE) {
    return
  }
  print("--------------------------------------------------------------------")
  print(sprintf("Total worldwide confirmed cases = %d", results$confirmed))
  print(sprintf("Total worldwide deaths = %d", results$deaths))
  print("--------------------------------------------------------------------")
  print(sprintf("The data.table produced after processing:"))
  print(results$dt)
  print("--------------------------------------------------------------------")
}

#' Auxiliary function to print a data.table for debugging purposes.
debug_print_dt <- function(dt) {
  if (FALSE == VERBOSE) {
    return
  }
  print("--------------------------------------------------------------------")
  print(dt)
  print("--------------------------------------------------------------------")
}


# =============================================================================
#
# EDA-related stuff
#
# =============================================================================

#' The countries that constitute the European Union.
EU <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia",
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
        "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
        "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia",
        "Slovenia", "Spain", "Sweden")

#' The nordic countries.
NORDIC <- c("Denmark", "Finland", "Iceland", "Norway", "Sweden")

#' The countries that constitute BRICS.
BRICS <- c("Brazil", "Russia", "India", "China", "South Africa")

#' Countries that are mostly or partially (>= 25% of their area) in the Balkan
#' Peninsula according to Wikipedia (accessed on January 11th, 2021, at
#' https://en.wikipedia.org/wiki/Balkans#Balkan_Peninsula).
BALKANS <- c("Albania", "Bosnia and Herzegovina", "Bulgaria", "Croatia",
             "Greece", "Kosovo", "Montenegro", "North Macedonia", "Serbia",
             "Slovenia")

#' The countries that constitute the Arab states of the Persian Gulf, according
#' to Wikipedia (accessed on January 11th, 2021, at
#' https://en.wikipedia.org/wiki/Arab_states_of_the_Persian_Gulf).
GULF <- c("Bahrain", "Kuwait", "Iraq", "Oman", "Qatar", "Saudi Arabia",
          "United Arab Emirates")

#' TODO: Documentation
calc_summary <- function(dt) {
  countries <- unique(dt$Country)

  aggr_dt <- function(group) {
    dt[Country %in% group,
       .(
         total.deaths         = sum(deaths.inc),
         total.confirmed      = sum(confirmed.ind),
         total.ratio          = sum(deaths.inc) / sum(confirmed.ind),
         mean.daily.confirmed = mean(confirmed.ind),
         mean.daily.deaths    = mean(deaths.inc)
        ),
       by = .(Country)]
  }
  all_agdt   <- aggr_dt(countries)
  eu_agdt    <- all_agdt[Country %in% EU]
  nord_agdt  <- all_agdt[Country %in% NORDIC]
  brics_agdt <- all_agdt[Country %in% BRICS]
  balk_agdt  <- all_agdt[Country %in% BALKANS]
  gulf_agdt  <- all_agdt[Country %in% GULF]
  us_agdt    <- all_agdt["US"]
  ca_agdt    <- all_agdt["Canada"]
  uk_agdt    <- all_agdt["United Kingdom"]
  au_agdt    <- all_agdt["Australia"]
  br_agdt    <- all_agdt["Brazil"]
  debug_print_dt(eu_agdt)
  debug_print_dt(nord_agdt)
  debug_print_dt(brics_agdt)
  debug_print_dt(balk_agdt)
  debug_print_dt(us_agdt)
  debug_print_dt(uk_agdt)

  mean_dt <- function(name, group) {
    all_agdt[Country %in% group,
             .(
               Country              = name,
               total.deaths         = sum(total.deaths),
               total.confirmed      = sum(total.confirmed),
               total.ratio          = mean(total.ratio),
               mean.daily.confirmed = mean(mean.daily.confirmed),
               mean.daily.deaths    = mean(mean.daily.deaths)
              )]
  }
  world <- mean_dt("WORLD", countries)
  eu    <- mean_dt("EUROPEAN UNION", EU)
  nord  <- mean_dt("NORDIC COUNTRIES", NORDIC)
  brics <- mean_dt("BRICS", BRICS)
  balk  <- mean_dt("BALKAN COUNTRIES", BALKANS)
  gulf  <- mean_dt("GULF COUNTRIES", GULF)
  debug_print_dt(world)
  debug_print_dt(eu)
  debug_print_dt(nord)
  debug_print_dt(brics)
  debug_print_dt(balk)
  debug_print_dt(gulf)

  rbind(all_agdt, world, eu, nord, brics, balk, gulf)
}

#' Generate the plots for the summary statistics data.table.
plot_summary <- function(dt) {
  econ <- dt[Country %in% c(EU, BRICS, "US")
            ][, econ.grp := as.factor(ifelse(Country %in% EU,
                                             "European Union",
                                             ifelse(Country %in% BRICS,
                                                    "BRICS",
                                                    "US")))]
  ggplot(econ) +
    aes(x = econ.grp) +
    aes(y = total.confirmed) +
    aes(fill = Country) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = "Confirmed COVID-19 cases per economic group") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    labs(x = "") +
    labs(y = "Confirmed COVID-19 Cases") +
    theme_bw() +
    geom_text(aes(label = Country), position = position_stack(vjust=.5), size=2)
    #geom_text(aes(label = Country))
    #geom_text(stat='Country', aes(label = stat(x), color = 'blue', vjust=-1))
  ggsave("aggr_econ_conf.png")
  ggplot(econ) +
    aes(x = econ.grp) +
    aes(y = total.deaths) +
    aes(fill = Country) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = "COVID-19 deaths per economic group") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    labs(x = "") +
    labs(y = "COVID-19 Deaths") +
    theme_bw() +
    #theme(legend.position = "none") +
    geom_text(aes(label = Country), position = position_stack(vjust=.5), size=2)
    #geom_text(aes(label = Country))
    #geom_text(stat='Country', aes(label = stat(x), color = 'blue', vjust=-1))
  ggsave("aggr_econ_deaths.png")

  geogr <- dt[Country %in% c(BALKANS, NORDIC, GULF)
             ][, geo.grp := as.factor(ifelse(Country %in% BALKANS,
                                              "Balkan",
                                              ifelse(Country %in% NORDIC,
                                                     "Nordic",
                                                     "Gulf")))]
  ggplot(geogr) +
    aes(x = geo.grp) +
    aes(y = total.confirmed) +
    aes(fill = Country) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = "Confirmed COVID-19 cases per geographic group") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    labs(x = "") +
    labs(y = "Confirmed COVID-19 Cases") +
    theme_bw() +
    geom_text(aes(label = Country), position = position_stack(vjust=.5), size=2)
    #geom_text(aes(label = Country))
    #geom_text(stat='Country', aes(label = stat(x), color = 'blue', vjust=-1))
  ggsave("aggr_geo_conf.png")
  ggplot(geogr) +
    aes(x = geo.grp) +
    aes(y = total.deaths) +
    aes(fill = Country) +
    geom_bar(position = "stack", stat = "identity") +
    labs(title = "COVID-19 deaths per geographic group") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    labs(x = "") +
    labs(y = "COVID-19 Deaths") +
    theme_bw() +
    #theme(legend.position = "none") +
    geom_text(aes(label=Country), position=position_stack(vjust=.5), size=2)
    #geom_text(aes(label = Country))
    #geom_text(stat='Country', aes(label = stat(x), color='blue', vjust = -1))
  ggsave("aggr_geo_deaths.png")


  #debug_print_dt(dt)
  aggrs <- c("WORLD", "EUROPEAN UNION", "NORDIC COUNTRIES", "BRICS",
             "BALKAN COUNTRIES", "GULF COUNTRIES", "US")
  bottom25 <- dt[!Country %in% c("MS Zaandam", "Diamond Princess")
                ][order(-total.ratio)][1:25]
  ratioz <- rbind(dt[Country %in% aggrs], bottom25)[order(-total.ratio)]
  #debug_print_dt(bottom25)
  ratioz$Country <- reorder(ratioz$Country, ratioz$total.ratio)

  #ggplot(ratioz) +
  #  aes(x = reorder(Country, total.ratio)) +
  #  aes(y = total.ratio) +
  #  geom_bar(stat = "identity") +
  #  coord_flip() +
  #  geom_text(aes(label=sprintf("%d out of %d", total.deaths,total.confirmed)),
  #            vjust = .5, hjust = -.15, color="dark red", size = 1.5) +
  ggplot(ratioz) +
    # FIXME: This fucking shit doesn't get ordered by total.ratio here, no
    # matter what. I had to order the Countries as a factor right above
    # (credits to @geol for that).
    #aes(x = reorder(Country, total.ratio)) +
    aes(x = Country) +
    aes(y = total.ratio) +
    geom_segment(aes(x = Country, xend = Country, y = 0, yend = total.ratio),
                 color = ifelse(ratioz$Country %in% aggrs, "blue", "dark red"),
                 size  = ifelse(ratioz$Country %in% aggrs, 1.3, 0.7) ) +
    geom_text(aes(label=sprintf("%d out of %d", total.deaths,total.confirmed)),
              vjust = .5, hjust = -.15, color="magenta", size = 1.5) +
    ylim(c(0, .31)) +
    #geom_point(size = 2, color = "red", fill = alpha("blue", .3), alpha = .7,
    #           shape = 21, stroke = 2) +
    #geom_point() +
    geom_point(color = ifelse(ratioz$Country %in% aggrs, "blue", "dark red"),
               size  = ifelse(ratioz$Country %in% aggrs, 3, 1)) +
    coord_flip() +
    #geom_segment(x = Country, xend = Country, y = 0, yend = total.ratio) +
    labs(x = "") +
    labs(y = "Ratio") +
    labs(title = "Ratio of COVID-19 Deaths to Confirmed COVID-19 Cases") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    theme_bw()
  ggsave("aggr_ratio_lollipop.png")
}

#' Calculates (and adds as a new variable) the hemisphere (Northern, Southern
#' or Equator) in which each observed country is located, and returns the newly
#' formed data.table.
determine_hemispheres <- function(dt) {
  lats <- fread(DEATHS_URL,
                header = TRUE,
                select = c("Country/Region", "Lat")
    )[Lat != 0
     ][, .(lat = mean(Lat)), by = c("Country/Region")
      ][, Hemisphere := as.factor(ifelse(lat > 10,
                                         "Northern",
                                         ifelse(lat < -10,
                                                "Southern",
                                                "Equator")))
       ][, lat := NULL]
  setnames(lats, "Country/Region", "Country")

  merge(dt, lats, by = c("Country"))
}

#' TODO: Documentation
#' https://github.com/CSSEGISandData/COVID-19/issues/3484
plot_seasons <- function(dt) {
  hem_series <- dt[,
                   .(confirmed.ind = sum(confirmed.ind),
                     deaths.inc    = sum(deaths.inc)),
                   by = .(date, Hemisphere)
                  ][,
                    ":="(confirmed = cumsum(confirmed.ind),
                         deaths    = cumsum(deaths.inc)),
                    by = .(Hemisphere)]
  hem_sum <- hem_series[,
                        .(confirmed = sum(confirmed.ind),
                          deaths    = sum(deaths.inc)),
                        by = .(Hemisphere)]

  first_date <- hem_series[date == min(date), date][1]
  last_date <- hem_series[date == max(date), date][1]
  #g <- ggplot(data = hem_series,
  #            aes(x = date, y = confirmed.ind, color = Hemisphere)) +
  #      geom_line(size = .5)
  ggplot(data = hem_series) +
    aes(x = date) +
    aes(y = confirmed.ind) +
    aes(color = Hemisphere) +
    geom_line(size = .5) +
    geom_smooth() +
    aes(xmin = first_date) +
    aes(xmax = last_date) +
    scale_x_date(date_labels = "%b %Y",
                 limit = c(as.Date("2020-01-21"),as.Date("2021-01-19")),
                 expand = c(0, 0)) +
    scale_y_log10() +
    #scale_y_continuous(name = "",
    #                 breaks = factor(seq(0, 9e+05, 1e+05)),
    #                 limits = c(0, 9e+05)) +
    theme(axis.text.x = element_text(angle=60, hjust=1)) +
    #aes(ymin = 0) +
    #aes(ymax = 7e+05) +
    ##ylim(c(0, 700000)) +
    labs(title = "Daily confirmed COVID-19 cases") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    #labs(subtitle = "Separate plots per hemisphere may hint hidden effects of seasons") +
    labs(x = "") +
    labs(y = "") +
    #labs(y = "Confirmed Cases") +
    theme_bw() +
    #labs(caption = "Visualization via ggplot2, by Christos Katsakioris")
  ggsave("hem_series_daily_cases.png")
  ggplot(data = hem_series) +
    aes(x = date) +
    aes(y = deaths.inc) +
    aes(color = Hemisphere) +
    geom_line(size = .5) +
    geom_smooth() +
    aes(xmin = first_date) +
    aes(xmax = last_date) +
    scale_x_date(date_labels = "%b %Y",
                 limit = c(as.Date("2020-01-21"),as.Date("2021-01-19")),
                 expand = c(0, 0)) +
    #scale_y_log10() +
    #theme(axis.text.x = element_text(angle=60, hjust=1)) +
    labs(title = "Daily deaths due to COVID-19") +
    labs(subtitle = "Dataset from CSSE, Johns Hopkins University") +
    #labs(subtitle = "Separate plots per hemisphere may hint hidden effects of seasons") +
    labs(x = "") +
    labs(y = "") +
    #labs(y = "Number of Deaths") +
    theme_bw() +
    #labs(caption = "Visualization via ggplot2, by Christos Katsakioris")
  ggsave("hem_series_daily_deaths.png")
}

#' Conduct an Exploratory Data Analysis given the (semi-)processed data.table.
eda <- function(dt) {
  # Assert that all subgrouped countries are indeed present in the dataset
  countries <- unique(dt$Country)
  stopifnot(EU %in% countries, NORDIC %in% countries, BRICS %in% countries,
            BALKANS %in% countries, GULF %in% countries)

  #stats_sum_dt <- calc_summary(dt)
  ##print(stats_summary[order(-total.deaths)])
  #plot_summary(stats_sum_dt)
  plot_summary(calc_summary(dt))

  #hemispheres_dt <- determine_hemispheres(dt)
  #hemispheres_dt
  plot_seasons(determine_hemispheres(dt))
}


# =============================================================================
#
# Bootstrapping
#
# =============================================================================

#' The entry point for non-interactive executions.
main <- function() {
  res <- processing()
  print_initial_processing_results(res)
  eda(res$dt)
}

#' If it is run interactively, just widen the screen (only tested with R
#' version `4.0.3 (2020-10-10)` on platform `x86_64-pc-linux-gnu`) and run
#' nothing; otherwise, run `main()`.
if (interactive()) {
  options("width" = 120)
} else {
  main()
}
