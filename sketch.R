library(jsonlite)
library(httr)
library(docopt)
library(glue)
library(cli)
library(purrr)
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)

'NWS Hourly weather graph

Usage:
  sketch.R ship new <name>...
  sketch.R ship <name> move <x> <y> [--speed=<kn>]
  sketch.R ship shoot <x> <y>
  sketch.R mine (set|remove) <x> <y> [--moored | --drifting]
  sketch.R (-h | --help)
  sketch.R --version

Options:
  -h --help     Show this screen.
  --version     Show version.
  --speed=<kn>  Speed in knots [default: 10].
  --moored      Moored (anchored) mine.
  --drifting    Drifting mine.

' -> doc

# arguments <- docopt(doc, version = 'NWS Hourly weather graph v0.1')
# print(arguments)

tribble(
  ~type,                        ~group,        ~custom, ~color,
  "heatIndex",                  "temperature", FALSE,   "brown",
  "dewpoint",                   "temperature", FALSE,   "green",
  "temperature",                "temperature", FALSE,   "red",
  "windDirection",              "wind",        TRUE,    NA,
  "windSpeed",                  "wind",        FALSE,   "purple",
  "windGust",                   "wind",        FALSE,   "navyblue",
  "relativeHumidity",           "percent",     FALSE,   "green",
  "probabilityOfPrecipitation", "percent",     FALSE,   "brown",
  "skyCover",                   "percent",     FALSE,   "blue",
  "quantitativePrecipitation",  "rain",        FALSE,   "green",
  "lightningActivityLevel",     "thunder",     FALSE,   "red"
) -> default_ts

acquireData <- function(zone = "OKX", x=65, y=66,
                        ts_of_interest = default_ts$type,
                        raw = FALSE) {

  wx.url <- glue('https://api.weather.gov/gridpoints/{zone}/{x},{y}')

  cli_alert_info("Issueing GET request to {.url {wx.url}}")

  GET(
    wx.url,
    add_headers(accept = "application/geo+json")
  ) -> resp

  resp.json <- fromJSON(content(resp, "text"))

  if (raw)
    return(resp.json)

  # The NWS api returns dates in a weird format that is a variant of ISO8601,
  # in order to get it properly parsed by lubridate:: functions, you need to
  # split out the `+00:00/` that seems to always be in the middle of the
  # string.
  split_dates <- function(ds) {
    split_into_date_duration <-
      str_split(ds, "\\+00:00/", simplify = TRUE)

    as.interval(
      duration(split_into_date_duration[,2]),
      parse_datetime(split_into_date_duration[,1])
    )
  }

  filtered_values <- resp.json$properties[ts_of_interest]

  map(filtered_values, ~tibble::as_tibble(.$values)) %>%
    bind_rows(.id = "type") %>%
    transmute(type,
              interval = split_dates(validTime),
              value)
}

int_slice <- function(interval, by = '1 hour')
  map(interval, ~seq(int_start(.), int_end(.), by = by))

split_intervals <- function(d)
  transmute(d, type, t = int_slice(interval), value) %>%
    unnest(t)

ctof <- function(temps) 32 + temps*9/5

d <- acquireData() %>%
  split_intervals %>%
  left_join(default_ts, by = "type") %>%
  mutate(
    value = ifelse(
      type %in% c("heatIndex", "dewpoint", "temperature"),
      ctof(value), value
    )
  )
  # Get rid of NA's that shouldn't exist here

p_proto <- ggplot(d, aes(t, value, color=I(color))) +
  geom_line(
    data = ~filter(., custom == FALSE),
    aes(group = type),
    size = 0.25
  ) +
  geom_point(
    data = ~filter(., custom == FALSE),
    color = 'black',
    size = 0.075
  ) +
  scale_x_datetime(
    name = NULL,
    limits = c(now(), now() + hours(48)),
    date_breaks = '3 hours',
    labels = function(brk) {
      scales::time_format("%I%p")(brk) %>% str_to_lower %>% str_remove('0')
    },
#   sec.axis = dup_axis(
#       breaks = ~seq(ceiling_date(.[1], unit = 'day'),
#                     floor_date(.[2],   unit = 'day'),
#                     by = '1 day')
#     labels = scales::time_format("%a, %b %d %Y")
#   )
  ) +
  theme_linedraw() +
  theme(
    axis.ticks = element_blank(),

    # Probably, these should not be defined in mm!
    axis.text.x = element_text(
      size = 5, margin = unit(c(t=-3, r=0, b=0, l=0), "mm")
    ),
    axis.text.y = element_text(
      size = 5, margin = unit(c(t=0, r=-4, b=0, l=0), "mm")
    )
  )

p_temps <- p_proto %+% filter(d, group == "temperature") +
  scale_y_continuous(
    name = NULL,
    breaks = scales::breaks_width(10),
    minor_breaks = NULL
  )
#   scale_y_continuous(
#     label = ~glue("{temp}", temp = .)
#   )

p_speeds <- p_proto %+% filter(d, group == "wind") +
  scale_y_continuous(
    name = NULL,

    # This is currently not working as expected! Probably needs a limits=
    # call somewhere
    breaks = function(lims) {
      scales::breaks_width(10)(c(lims[1], max(lims[2], 10)))
    },
    minor_breaks = NULL,
  )                                                       

p_percentages <- p_proto %+% filter(d, group == "percent") +
  scale_y_continuous(
    name = NULL,
    breaks = scales::breaks_width(20),
    minor_breaks = NULL,
  )                                                       

p_rain <- p_proto %+% filter(d, group == "rain") +
  scale_y_continuous(
    name = NULL
  )

p_thunder <- p_proto %+% filter(d, group == "thunder") +
  scale_y_continuous(
    name = NULL
  )

grid.arrange(
  p_temps,
  p_speeds,
  p_percentages,
  p_rain,
  p_thunder,
  ncol = 1
) -> p

  # geom_spoke(
  #   data = function(d) {
  #     inner_join(
  #       filter(d, type == "windDirection"),
  #       filter(d, type == "windSpeed"),
  #       by = "t",
  #       suffix = c(".dir", "")
  #     )
  #   },
  #   aes(x = t, y = value), # , angle = 2*pi*(value.dir)/360),
  #   angle = 3*pi/4,
  #   radius = days(1),
  #   color = 'black'
  # )

# tibble(
#   x = 1:100,
#   y = runif(100, 0, 1),
#   angle = runif(100, 0, 2*pi)
# ) %>% ggplot(aes(x,y)) +
#         geom_spoke(aes(angle=angle), radius=100)
print(p)
