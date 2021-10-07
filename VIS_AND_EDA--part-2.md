VIS\_AND\_EDA, ggplot 1
================
Xinyuan Liu
2021/10/5

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## 载入需要的程辑包：viridisLite

``` r
library(patchwork)
library(ggridges)
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)
```

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\ypmli\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:32:10 (7.617)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: C:\Users\ypmli\AppData\Local/Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:32:34 (1.701)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: C:\Users\ypmli\AppData\Local/Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:32:45 (0.913)

    ## file min/max dates: 1999-09-01 / 2021-09-30

## start with a familiar one

``` r
weather_df %>% 
  ggplot(aes(x = tmin, y =tmax, color = name)) +
  geom_point(alpha = .3) +
  labs(
    title = "Temperature at three stations",
    x = "minimum daily temp(C)",
    y = "maximum daily temp(C)",
    caption = "Data from rnaaa package with three stations"
  ) + 
  scale_x_continuous(
    breaks =c(-15, 0, 15),
    labels = c("-15C", "0", "15C")
  ) + 
  scale_y_continuous(
    trans = "sqrt", 
    position = "right"
  ) +
  scale_color_hue(
    name = "location",
    h = c(100, 300)
    ) +
  scale_color_viridis_d() +
  theme_minimal() +
  theme(legend.position = "bottom")
```

    ## Scale for 'colour' is already present. Adding another scale for 'colour',
    ## which will replace the existing scale.

    ## Warning in self$trans$transform(x): ²úÉúÁËNaNs

    ## Warning: Transformation introduced infinite values in continuous y-axis

    ## Warning: Removed 90 rows containing missing values (geom_point).

<img src="VIS_AND_EDA--part-2_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

## set option

library(tidyverse)

knitr::opts\_chunk$set( fig.width = 6, fig.asp = .6, out.width = “90%” )

theme\_set(theme\_minimal() + theme(legend.position = “bottom”))

options( ggplot2.continuous.colour = “viridis”, ggplot2.continuous.fill
= “viridis” )

scale\_colour\_discrete = scale\_colour\_viridis\_d
scale\_fill\_discrete = scale\_fill\_viridis\_d

## data in geoms

``` r
central_park = 
  weather_df %>% 
  filter(name == "CentralPark_NY")
 
waikiki = 
  weather_df %>% 
  filter(name == "Waikiki_HA")

ggplot(data = waikiki, aes(x = date, y = tmax, color = name)) +
  geom_point()+
  geom_line(data = central_park)
```

    ## Warning: Removed 3 rows containing missing values (geom_point).

<img src="VIS_AND_EDA--part-2_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

## Patchwork

``` r
ggp_tmax_tmin = 
  weather_df %>% 
  ggplot(aes(x = tmin, y = tmax, color = name)) +
  geom_point(alpha = .3)+
  theme(legend.position = "none")

ggp_prcp_dens = 
  weather_df %>% 
  filter(prcp > 0) %>%
  ggplot(aes(x = prcp, fill = name)) +
  geom_density(alpha =.3) +
  theme(legend.position = "none")

ggp_tmax_date =
  weather_df %>% 
  ggplot(aes(x = date, y = tmax, color = name)) +
  geom_point() +
  geom_smooth() +
  theme(legend.position = "bottom")

(ggp_tmax_tmin + ggp_prcp_dens) / ggp_tmax_date
```

    ## Warning: Removed 15 rows containing missing values (geom_point).

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

    ## Warning: Removed 3 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 3 rows containing missing values (geom_point).

<img src="VIS_AND_EDA--part-2_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

## data manipulation

``` r
weather_df %>% 
  mutate(
    name = fct_reorder(name, tmax)
  ) %>% 
  ggplot(aes(x = name, y = tmax)) +
  geom_boxplot()
```

    ## Warning: Removed 3 rows containing non-finite values (stat_boxplot).

<img src="VIS_AND_EDA--part-2_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

what about tmax and tmin?

``` r
weather_df %>% 
  pivot_longer(
    tmax:tmin,
    names_to = "obs",
    values_to = "temperature"
  ) %>% 
  ggplot(aes(x = temperature, fill = obs))+
  geom_density(alpha = .3) +
  facet_grid(. ~ name)
```

    ## Warning: Removed 18 rows containing non-finite values (stat_density).

<img src="VIS_AND_EDA--part-2_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
pulse_df = 
  haven::read_sas("public_pulse_data.sas7bdat") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    values_to = "bdi", 
    names_prefix = "bdi_score_"
    ) %>% 
  mutate(visit = recode(visit, "bl" = "00m"))
```

    ## Warning in FUN(X[[i]], ...): strings not representable in native encoding will
    ## be translated to UTF-8

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00C4>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00D6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00E4>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00F6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00DF>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00C6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00E6>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00D8>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00F8>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00C5>' to native encoding

    ## Warning in FUN(X[[i]], ...): unable to translate '<U+00E5>' to native encoding

``` r
pulse_df %>% 
  ggplot(aes(x = visit, y = bdi)) +
  geom_boxplot()
```

    ## Warning: Removed 879 rows containing non-finite values (stat_boxplot).

<img src="VIS_AND_EDA--part-2_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />
pulse\_df %&gt;% ggplot(aes(x = visit, y = bdi)) + geom\_point(size =
.2) + geom\_line(aes(group = id))
