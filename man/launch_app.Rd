% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/launch_app.R
\name{launch_app}
\alias{launch_app}
\title{Explore COVID-19 Related Variables Interactively}
\source{
The app's sources of data are the \href{https://www.ecdc.europa.eu/en}{European Centre of Disease Prevention and Control}
and the \href{https://www.google.com/covid19/mobility/}{Google Community Mobility Report}.
The cumulative figures and GCMR are downloaded with the \href{https://github.com/joachim-gassen/tidycovid19}{tidycovid19} R package.
}
\usage{
launch_app()
}
\description{
This function launch a shiny based web app that allows the user to explore the data of COVID-19 related variables in Europe.
The users are allowed to customize their country of choices and the time range they want to display within the app.
}
\details{
The variables covered in this app are:
\itemize{
\item Daily and cumulative cases and deaths of COVID-19 in 51 countries in Europe.
\item Weekly testing and positivity rate of 29 EU/EEA member countries and the United Kingdom.
\item Weekly dominant source of transmission of 31 EU/EEA member countries and the United Kingdom.
\item Community mobility in six types of location of 29 EU/EEA member countries and the United Kingdom.
}
}
\examples{
\dontrun{
   launch_app()

}

}
