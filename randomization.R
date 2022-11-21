####  Init  ####
  {
    library(tidyverse)
  }

####  Data  ####
  {
    tbl_dist <- read_csv("dist.csv", col_types = c(thermal_band = "f"))
  }

# · Observed data ----
  {
    # · R-squared ----
      {
        tbl_dist %>%
          lm(sorensen_index ~ distance * thermal_band, data = .) %>%
          summary() %>%
          pluck("r.squared")
      }

    # · Visualization ----
      {
        tbl_dist %>%
          ggplot(aes(
            x = distance,
            y = sorensen_index,
            color = thermal_band
          )) +
          # facet_wrap(vars(thermal_band)) +
          # geom_point(size = .1, alpha = .1) +
          geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
          scale_color_brewer(palette = "RdBu", direction = -1) +
          guides(color = guide_legend(reverse = TRUE)) +
          theme_classic()
      }
  }

####  Randomization  ####
  {
    # R-squared for the two lm models with reshuffling:
    # y = beta0 + beta1
    # y = beta0 * beta1

    iter <- 2

    map_df(1:iter, ~ {
      gc()

      data <- tbl_dist %>%
        group_by(mt_group) %>%
        mutate(sorensen_index = sample(sorensen_index, size = n())) %>%
        ungroup()

      out_lm <- lm(sorensen_index ~ distance + thermal_band, data = data)
      out_lm_inter <- lm(sorensen_index ~ distance * thermal_band, data = data)

      list(
        r2 = summary(out_lm)$r.squared,
        r2_interaction = summary(out_lm_inter)$r.squared
      )
    }, .id = "iteration")
  }

####  GAM  ####
  {
    # · Model ----
      {
        library(mgcv)

        tbl_dist_log <- mutate(tbl_dist, distance = log(distance))

        out_gam <- gam(
          sorensen_index ~ thermal_band + s(distance, sp = 10, k = 6, by = thermal_band),
          data = tbl_dist_log,
          method = "REML"
        )
      }

    # · Diagnostics ----
      {
        summary(out_gam)
        gam.check(out_gam)
        concurvity(out_gam)
      }

    # · Predictions ----
      {
        pred <- tidymv::predict_gam(out_gam)

        # mean
        pred %>%
          group_by(thermal_band) %>%
          summarise(mean = mean(fit)) %>%
          arrange(mean)

        # mean lowest x
        pred %>%
          group_by(thermal_band, distance) %>%
          summarise(mean = mean(fit)) %>%
          filter(distance == min(distance)) %>%
          arrange(mean)
      }

    # · Visualization ----
      {
        pred %>%
          ggplot(aes(distance, fit, fill = thermal_band)) +
          tidymv::geom_smooth_ci(thermal_band) +
          facet_wrap(vars(thermal_band)) +
          guides(
            linetype = guide_legend(reverse = TRUE),
            color = guide_legend(reverse = TRUE),
            fill = guide_legend(reverse = TRUE)
          ) +
          theme_classic()
      }
  }
