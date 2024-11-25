HW6
================
Adeena Moghni
2024-11-24

## Problem 1

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## using cached file: C:\Users\adeen\AppData\Local/R/cache/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:18:38.832335 (8.668)

    ## file min/max dates: 1869-01-01 / 2024-09-30

``` r
boot_function = function(df) {
  bootstrap = sample_frac(df, replace = TRUE)
  
  model = lm(tmax ~ tmin, data = bootstrap)
  
  R_squared = 
    model %>% 
    broom::glance() %>% 
    pull(r.squared)
  
  coeff = 
    model %>% 
    broom::tidy() %>% 
    pull(estimate)
  
    log_B0_B1 = log(coeff[1] * coeff[2])
  
  tibble(R_squared = R_squared, log_B0_B1 = log_B0_B1)
  
}
```

``` r
bootstrap_df = 
  tibble(strap_number = 1:5000) %>% 
  mutate(
    strap_sample = map(strap_number, \(i) boot_function(df = weather_df))
  ) %>% 
  unnest(strap_sample)
```

``` r
R2_quantile = 
  bootstrap_df %>% 
  summarize(
    lower_ci = quantile(R_squared, .025),
    higher_ci = quantile(R_squared, .975)
  )

log_quantile = 
  bootstrap_df %>% 
  summarize(
    lower_ci = quantile(log_B0_B1, .025),
    higher_ci = quantile(log_B0_B1, .975)
  )
```

``` r
ggplot(bootstrap_df, aes(x = "", y = R_squared)) +
  geom_boxplot(outlier.shape = 4) +
  geom_point(data = R2_quantile, aes(x = "", y = lower_ci), color = "red") +
  geom_point(data = R2_quantile, aes(x = "", y = higher_ci), color = "red") +
  labs(
    x = "",
    title = "Distribution of R^2 Values",
    caption  = "2.5% and 97.5% quantile values showed in red"
  )
```

![](HW6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
  ggplot(bootstrap_df, aes(x = "", y = log_B0_B1)) +
  geom_boxplot(outlier.shape = 4) +
  geom_point(data = log_quantile, aes(x = "", y = lower_ci), color = "red") +
  geom_point(data = log_quantile, aes(x = "", y = higher_ci), color = "red") +
  labs(
    x = "",
    title = "Distribution of log(B0 * B1) Values",
    caption  = "2.5% and 97.5% quantile values showed in red"
  )
```

![](HW6_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->
