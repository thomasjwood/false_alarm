library(plyr)
library(merTools)
library(tidyverse)
library(magrittr)
library(lme4)
library(purrrlyr)
library(emmeans)
library(broom)
library(ggstance)

d1 <- "C:/Dropbox/false alarm/replication data and code/intro/d1--fig 1 & fig 2.rds" %>% 
  readRDS

# main random effect model

lmm <- lmer(
  ans_num ~ cor*type + (cor*ideol | item), 
  data = d1, control = 
    lmerControl(
      optimizer = "bobyqa",
      optCtrl=list(maxfun=4e4)
    )
  )

summary(lmm)

# showing issue specific combination of random and fixed effects

rr1 <-REsim(lmm, n.sims = 5000, oddsRatio = F) 

# labelling group IDs

rr1$groupID %<>% 
  mapvalues(
    c("nonpol_floor_food", "nonpol_gumdigest", "nonpol_knuckles", 
      "pol_trump_taxcut", "pol_cheney_gnd", "pol_trump_california", 
      "nonpol_common_cold", "pol_aoc_ue", "pol_sanders_jobs", "nonpol_brain_power", 
      "pol_pelosi_taxcut", "nonpol_toilet_flush", "pol_trump_milpay", 
      "nonpol_microwave", "pol_omar_mcdonalds", "pol_aoc_pentagon", 
      "pol_harris_taxcut", "nonpol_antoinette", "pol_schiff_golfcart", 
      "nonpol_columbus", "pol_sanders_census", "nonpol_napoleon", "nonpol_edison", 
      "nonpol_ulcers"),
    c("Apolitical: Food must be picked up within 5 seconds",
      "Apolitical: Gum takes 5 years to digest",
      "Apolitical: Cracking knuckles causes arthritis", 
      "Political: GOP passes first tax cut since Reagan (Trump)",
      "Political: Green New Deal seeks to ban air travel (Cheney)",
      "Political: California wildfire caused by rerouting river (Trump)", 
      "Apolitical: Cold air causes common cold",
      "Political: Unemployment low because of multiple jobs (AOC)",
      "Political: Trump better on black unemployment (Sanders)",
      "Apolitical: Brain commonly uses 10% of power", 
      "Political: GOP tax cut provides 85% benefits to wealthy (Pelosi)",
      "Apolitical: Toilets flush opposite directions in hemispheres",
      "Political: GOP passes first military pay raise in decades (Trump)", 
      "Apolitical: Microwave plastic imparts chemicals",
      "Political: McDonalds workers earn $7k/year (Omar)",
      "Political: Pentagon wastes $21 trillion (AOC)", 
      "Political: GOP taxcut increases taxes on middle class (Harris)",
      "Apolitical: Antoinette said `Let them eat cake`",
      "Political: Trump rents golf carts to secret service (Schiff)", 
      "Apolitical: Flat earth belief when Columbus travels New World",
      "Political: Every Census asks citizenship since 1960 (Sanders)",
      "Apolitical: Napoleon very short",
      "Apolitical: Edison invented light bulb", 
      "Apolitical: Stress causes ulcers")
  )

rr2 <- rr1 %>% 
  filter(
    term == "corc"
  ) %>% 
  tbl_df

# adding the non random effect

rr2$median <- rr2$median %>% 
  add(
    lmm %>% 
      tidy %>%
      filter(
        term == "corc"
      ) %>% 
      use_series(estimate)
  )

# adding the interaction
rr2$median <- rr2$groupID %>% 
  str_detect("Political") %>% 
  ifelse(
    rr2$median %>% 
      add(
        lmm %>% 
          tidy %>%
          filter(
            term == "corc:typepol"
          ) %>% 
          use_series(estimate)
      ),
    rr2$median
  )

rr2 %<>% 
  mutate(
    lo = median %>% 
      subtract(sd),
    hi = median %>% 
      add(sd)
  )


rr2$type <- rr2$groupID %>% 
  str_detect("Political") %>% 
  ifelse("Political", "Apolitical")

# computing an overall effect

rr3 <- rr2  %>%  
  bind_rows(
    rr2 %>% 
      group_by(
        type
      ) %>% 
      summarize(
        median = median %>% mean,
        sd = sd %>% mean
      ) %>% 
      mutate(
        lo = median %>% 
          subtract(sd),
        hi = median %>% 
          add(sd),
        groupID = "Overall"
      )
  )

rr2$groupID %<>%
  factor(
    rr2 %>%
      arrange(median) %>%
      use_series(groupID)
  )

rr3$groupID %<>%  
  factor(
    c("Overall",
      rr2$groupID %>%
        levels)
  )


# figure 1.2

rr3 %>% 
  ggplot() +
  geom_linerangeh(
    aes(xmin = lo, xmax = hi, y = groupID)
  ) +
  geom_point(
    aes(median, groupID, 
        shape = groupID %>% equals("Overall"),
        size =  groupID %>% equals("Overall")),
    fill = "grey95"
  ) +
  geom_text(
    aes(median, groupID, label = lab),
    data = rr3 %>% 
      filter(
        groupID == "Overall"
      ) %>% 
      mutate(
        lab = median %>% 
          round(2) %>% 
          str_replace("0.", ".")
      ),
    family = "Roboto",
    size = 3
  ) +
  scale_shape_manual(
    values = c(16, 21)
  ) +
  scale_size_manual(
    values = c(3, 8)
  ) +
  facet_grid(
    type ~ ., 
    scales = "free_y"
  ) +
  scale_y_discrete(
    breaks = rr3$groupID %>% levels,
    labels = "Overall" %>% 
      c(rr2$groupID %>%
          levels %>% 
          str_sub(
            rr2$groupID %>% 
              levels %>% 
              str_locate(fixed(":")) %>% 
              extract(, 2) %>% 
              add(2)
          )
      ),
    expand = c(.075, .075)
  ) +
  labs(
    y = "",
    x =  "Correction Effect (5pt scale, negative values indicate improved accuracy)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
    )


# figure 1.1
# out of sample prediction data

nd <- crossing(
  ideol = seq(1, 7, length.out = 200),
  cor = c("nc", "c")
  )


# removing mean centering from ideology

d1$ideol %<>% 
  add(4)

# separate lm objects

d2l <- d1 %>% 
  group_by(item) %>% 
  by_slice(
    function(i){
      
      j <-  predict(
        lm(
          ans_num ~ ideol * cor, data = i
        ),
        newdata = nd,
        se.fit = T
      )
      
      data.frame(
        fit = j$fit,
        se = j$se.fit
      ) %>% 
        cbind(nd)
    },
    .collate = "rows"
  ) %>% 
  mutate(
    lo = fit %>% 
      subtract(
        se %>% multiply_by(1.96)
      ),
    hi = fit %>% 
      add(
        se %>% multiply_by(1.96)
      ),
    item = item %>%
      mapvalues(
        c("nonpol_floor_food", "nonpol_gumdigest", "nonpol_knuckles", 
          "pol_trump_taxcut", "pol_cheney_gnd", "pol_trump_california", 
          "nonpol_common_cold", "pol_aoc_ue", "pol_sanders_jobs", "nonpol_brain_power", 
          "pol_pelosi_taxcut", "nonpol_toilet_flush", "pol_trump_milpay", 
          "nonpol_microwave", "pol_omar_mcdonalds", "pol_aoc_pentagon", 
          "pol_harris_taxcut", "nonpol_antoinette", "pol_schiff_golfcart", 
          "nonpol_columbus", "pol_sanders_census", "nonpol_napoleon", "nonpol_edison", 
          "nonpol_ulcers"),
        c("Apolitical: Food clean if picked up within 5 seconds",
          "Apolitical: Gum takes 5 years to digest",
          "Apolitical: Cracking knuckles causes arthritis", 
          "Political: GOP passes first tax cut since Reagan (Trump)",
          "Political: Green New Deal to ban air travel (Cheney)",
          "Political: California fire caused by rerouted river (Trump)", 
          "Apolitical: Cold air causes common cold",
          "Political: Unemployment low because of multiple jobs (AOC)",
          "Political: Trump better on black unemployment (Sanders)",
          "Apolitical: Brain only uses 10% of power", 
          "Political: GOP provides 85% tax benefits to rich (Pelosi)",
          "Apolitical: Toilets flush opposite directions in hemispheres",
          "Political: GOP passes first military pay raise (Trump)", 
          "Apolitical: Microwave plastic imparts chemicals",
          "Political: McDonalds workers earn $7k/year (Omar)",
          "Political: Pentagon wastes $21 trillion (AOC)", 
          "Political: GOP increases tax on middle class (Harris)",
          "Apolitical: Antoinette said `Let them eat cake`",
          "Political: Trump rents golf carts to Secret Service (Schiff)", 
          "Apolitical: Flat earth belief when Columbus travels New World",
          "Political: Every Census asks citizenship since 1960 (Sanders)",
          "Apolitical: Napoleon very short",
          "Apolitical: Edison invented light bulb", 
          "Apolitical: Stress causes ulcers")
      )
  )

# arranging items by uncorrected slopes


d2l$item %<>%
  factor(
    d2l %>% 
      filter(
        cor == "nc"
      ) %>% 
      group_by(
        item
      ) %>% 
      by_slice(
        function(i)
          
          lm(fit ~ ideol, data = i) %>% 
          tidy,
        .collate = "rows"
      ) %>% 
      filter(
        term == "ideol"
      ) %>% 
      arrange(
        desc(estimate)
      ) %>% 
      use_series(item)
    )

# producing figure 1.1

d2l %>% 
  ggplot()+ 
  geom_ribbon(
    aes(
      ideol, ymin = lo, ymax =hi, fill = cor
    ),
    color = "black",
    size = .2,
    alpha = .5
  ) +
  facet_wrap(
    ~item, 
    ncol = 6,
    labeller = label_wrap_gen(width = 30)
  ) +
  geom_text(
    aes(
      x = x,
      y = y,
      label = label,
      angle = angle
    ),
    data = tribble(
      ~x, ~y,         ~label,  ~angle, ~item,
      4,  1.5,    "Corrected",     20,  d2l$item %>% levels %>% extract2(1),
      4,  3.75, "Uncorrected",     20,  d2l$item %>% levels %>% extract2(1)
    ) %>%   
      mutate(
        item = item %>%
          factor(
            d2l$item %>%
              levels
          )
      ),
    size = 3,
    fontface = "italic",
    family = "Roboto") +  
  scale_x_continuous(
    breaks = c(1.5, 4, 6.5),
    labels = c("Lib",
               "Mod",
               "Cons")
  ) +
  scale_fill_manual(
    values = c("grey60", "grey98")
  ) +
  labs(
    x = "Ideology",
    y = "Agreement with bincorrect claim (5pt scale)"
  )