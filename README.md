

c19euxplorer <img src='man/figures/logo.png' align="right" height="138" />
========================================================

<!-- badges: start -->
[![R build status](https://github.com/etc5523-2020/r-package-assessment-Dewi-Amaliah/workflows/R-CMD-check/badge.svg)](https://github.com/etc5523-2020/r-package-assessment-Dewi-Amaliah/actions)
<!-- badges: end -->

<br>

The goal of **c19euxplorer** is to explore the COVID-19 related variables 
<br>
in several countries in Europe interactively via a Shiny-based web app.
<br>

The variables covered in this app are:

- Daily and cumulative cases and deaths of COVID-19 in 51 countries in Europe.
- Weekly testing and positivity rate of 29 EU/EEA member countries and the United Kingdom.
- Weekly dominant source of transmission of 31 EU/EEA member countries and the United Kingdom.
- Community mobility in six types of location of 29 EU/EEA member countries and the United Kingdom.

The app's sources of data are the [European Centre of Disease Prevention and Control](https://www.ecdc.europa.eu/en) and the [Google Community Mobility Report](https://www.google.com/covid19/mobility/). The cumulative figures and GCMR are downloaded with the [tidycovid19](https://github.com/joachim-gassen/tidycovid19) R package.




## Installation

You can install the `c19euxplorer` with:

``` r
remotes::install_github("etc5523-2020/r-package-assessment-Dewi-Amaliah")
```

## Example

### launch_app()

The main usage of this package is to launch an app to explore data of COVID-19 related variables. You can launch and use the app by run this code in R studio. 

``` r
library(c19euxplorer)
launch_app()
```
By running this function, you will have the app launched as follows:

<img src='https://datain360.com/posts/2020-10-14-shinyselfreview/shiny_gif/prev_image.png'>

<br>

You can explore each of the variables I mentioned previously by clicking each tab in the navigation bar. Inside each tab, there will be instructions on how to use the app. In general, you can compare several countries in your chosen time range. 

Beside `launch_app()`, this package also has other functions. You can refer to [this](https://etc5523-2020.github.io/r-package-assessment-Dewi-Amaliah/reference/index.html) and [this](https://etc5523-2020.github.io/r-package-assessment-Dewi-Amaliah/articles/c19euxplorer.html). 
These functions are mainly intended to simplify the code in the app, hence the usage would be very specific to the app. However, there are two functions that I think could be utilized when you build a Shiny app. They are `getref()` and `getlink()`.

<br>

### getref()

With this function, you can make a reference list in a Shiny app in simpler way. By using HTML tag, you have to write this code for each reference you use in the app.

```r
tags$li(class = "li-custom", "Winston Chang. (2018).",
        tags$a(href = "https://CRAN.R-project.org/package=shinythemes",
              "shinythemes: Themes for Shiny",
              class = "externallink"))

```

To produce this line:

- Winston Chang. (2018). [shinythemes: Themes for Shiny](https://CRAN.R-project.org/package=shinythemes)

With `getref()`, you just have to use this line:

```r
c19euxplorer::getref("Winston Chang",
       "2018",
       "https://CRAN.R-project.org/package=shinythemes",
       "shinythemes: Themes for Shiny")
```

<br>

### getlink()

Similarly, you can also use `getlink()` to create a hyperlink text. This function also incorporated HTML tag to create the hyperlink. 

```r
c19euxplorer::getlink("https://www.monash.edu/study/courses/find-a-course/2021/business-analytics-b6022?gclid=EAIaIQobChMI5bKC_Y-g7AIVzn8rCh3aQABbEAAYASAAEgK09PD_BwE&international=true#overview-1",
        "Master of Business Analytics")
```

This function will produce [Master of Business Analytics](https://www.monash.edu/study/courses/find-a-course/2021/business-analytics-b6022?gclid=EAIaIQobChMI5bKC_Y-g7AIVzn8rCh3aQABbEAAYASAAEgK09PD_BwE&international=true#overview-1).


<br>

### Data container

This package contains three data sets that could also be used outside the app. They are:

**COVID-19 weekly testing and positivity rate**

```r
library(tibble)
tibble(c19euxplorer::testing_weekly_eu)
```
<br>

**COVID-19 weekly dominant source of transmission in subnational level**

```r
tibble(c19euxplorer::transmission_subnational_level)
```
<br>

**Country centroids**

```r
tibble(c19euxplorer::country_centroids)
```
<br>
<br>

_The image source for the hex logo is from [flaticon](https://www.flaticon.com)_

