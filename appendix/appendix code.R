# Replication data -- 
# FALSE ALARM--the truth about political mistruth in the Trump era

# Code is copyright Ethan Porter & Thomas J Wood
# Code written:
# Fri Aug 02 15:41:53 2019 ------------------------------




if(
  any(
    grepl(
      "pacman",
      dimnames(installed.packages())[[1]]
    )
  )
) {
  "Pacman already installed"
} else {
  install.packages("pacman")
}

pacman::p_load(
  plyr, tidyverse, magrittr,
  lubridate, RcppRoll,
  purrrlyr, broom,
  lme4, stargazer,
  emmeans
  )

# p11

t1l <- "https://github.com/thomasjwood/false_alarm/raw/master/appendix/t1.rds" %>% 
  url %>%
  gzcon %>% 
  readRDS %>% 
  mutate(
    ideol = ideol %>% 
      add(4)
  ) %>% 
  group_by(item) %>% 
  by_slice(
    ~lm(
      ans_num ~ ideol * cor, data = .
      )
    ) %>% 
  mutate(
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

t1l$intercept <- t1l$.out %>% 
  map_chr(
    function(i){
      
      # i <- t1l$.out[[1]]
      
      k <- i %>% 
        tidy %>%
        slice(1)
      
      str_c(
        k$estimate %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        c("***", "**", "*", "") %>% 
          extract(
            i %>% 
              tidy %>% 
              slice(1) %>% 
              use_series(p.value) %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          ),
        "(",
        i %>% 
          tidy %>% 
          slice(1) %>% 
          use_series(std.error) %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        ")"
      )
      
    }
  )


t1l$ideology <- t1l$.out %>% 
  map_chr(
    function(i){
      
      # i <- t1l$.out[[1]]
      
      k <- i %>% 
        tidy %>%
        slice(2)
      
      str_c(
        k$estimate %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        c("***", "**", "*", "") %>% 
          extract(
            i %>% 
              tidy %>% 
              slice(2) %>% 
              use_series(p.value) %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          ),
        "(",
        i %>% 
          tidy %>% 
          slice(2) %>% 
          use_series(std.error) %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        ")"
      )
      
    }
  )


t1l$corr <- t1l$.out %>% 
  map_chr(
    function(i){
      
      # i <- t1l$.out[[1]]
      
      k <- i %>% 
        tidy %>%
        slice(3)
      
      str_c(
        k$estimate %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        c("***", "**", "*", "") %>% 
          extract(
            i %>% 
              tidy %>% 
              slice(3) %>% 
              use_series(p.value) %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          ),
        "(",
        i %>% 
          tidy %>% 
          slice(3) %>% 
          use_series(std.error) %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        ")"
      )
      
    }
  )


t1l$inter <- t1l$.out %>% 
  map_chr(
    function(i){
      
      # i <- t1l$.out[[1]]
      
      k <- i %>% 
        tidy %>%
        slice(4)
      
      str_c(
        k$estimate %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        c("***", "**", "*", "") %>% 
          extract(
            i %>% 
              tidy %>% 
              slice(4) %>% 
              use_series(p.value) %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          ),
        "(",
        i %>% 
          tidy %>% 
          slice(4) %>% 
          use_series(std.error) %>% 
          round(2) %>% 
          str_replace(fixed("0."), "."),
        ")"
      )
      
    }
  )

t1l$rsqr <- t1l$.out %>% 
  map_chr(
    function(i)
      i %>% 
      glance %>% 
      use_series(r.squared) %>% 
      round(2) %>% 
      str_replace(fixed("0."), ".")
  )

t1l$nobs <- t1l$.out %>% 
  map_chr(
    function(i)
      i %>% 
      nobs
  )


t1l$type <- t1l$item %>% 
  str_extract("Apolitical|Political")

targs <- t1l$ideology %>% 
  str_extract(
    "\\.\\d{1,2}\\*|\\.\\d{1,2}\\("
  ) %>% 
  str_sub(, -2) 

repl <- targs %>% 
  str_pad(width = 3, side = "right", pad = "0")

t1l$ideology <- 1:24 %>% 
  map_chr(
    function(i)
      
      # i <- 7
      
      t1l$ideology[[i]] %>% 
      str_replace(
        targs[[i]],
        repl[[i]]
      )
    
  )

t1l$item <- t1l$item %>% 
  str_sub(
    t1l$item %>% 
      str_locate(fixed(": ")) %>% 
      extract(, 2) %>%  
      add(1)
  )

t1l %>% 
  select(type, item, intercept:nobs) %>% 
  mutate(
    slope = t1l$.out %>% 
      map_dbl(
        function(i)
          i %>% 
          tidy %>% 
          filter(term == "ideol") %>% 
          extract2("estimate")
      )
  ) %>% 
  arrange(
    rev(type),
    slope
  ) %>% 
  select(-slope) %>% 
  mutate(
    rsqr = rsqr %>% 
      str_pad(width = 3,side = "right", pad = "0")
  )


# p12 

lmer(
  ans_num ~ cor*type + (cor*ideol | item),
  "https://github.com/thomasjwood/false_alarm/raw/master/appendix/t1.rds" %>% 
    url %>%
    gzcon %>% 
    readRDS,
  control = 
    lmerControl(
      optimizer = "bobyqa",
      optCtrl=list(maxfun=4e4)
      )
  ) %>% 
  stargazer(
    dep.var.labels.include = F,
    dep.var.caption = "",
    type = "text",
    digits = 2, 
    initial.zero = F,
    covariate.labels = c(
      "Correction",
      "Issue Type (Political)",
      "Correction x Issue Type (Political)",
      "Intercept")
  )

# p31, p32, p33, p34, p35, p36, p37, p39
# these tables and figures can be replicated with code and syntax here:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/AGRX5U


# p40 scatterplot

t2  <- "https://github.com/thomasjwood/false_alarm/raw/master/appendix/t2.RDS" %>% 
  url %>% 
  gzcon %>% 
  readRDS

emm_options(msg.interaction = F)

ml <- 1e3 %>% 
  seq %>% 
  map_dfr(
    function(x_1)
      t2[[1]] %>% 
      sample_frac(1, T) %>% 
      group_by(
        it_join
        ) %>%
      by_slice(
        function(j1)
          emmeans(
            lm(ans ~ ideol * corr, data = j1),
            pairwise ~ corr
          ) %>%
          use_series(contrasts) %>%
          tidy,
        .collate = "rows") %>%
      select(
        it_join, estimate
      ) %>% 
      left_join(
        t2[[2]] %>% 
          sample_frac(1, T) %>% 
          group_by(it_join) %>% 
          summarize(
            accord = accordance %>% mean
          ),
        "it_join"
      )
  ) 

# ml %>% 
#   saveRDS(
#     "C:/Dropbox/false alarm/code/accordance simulations.RDS"
#   )

ml <- "C:/Dropbox/false alarm/code/accordance simulations.RDS" %>% 
  url %>%
  gzcon %>% 
  readRDS %>% 

t1 <- ml %>% 
  mutate(
    iter = 1:1000 %>% 
      rep(each = 29)
  ) %>% 
  gather(
    type, val, estimate:accord
  ) %>% 
  group_by(
    it_join, type
  ) %>% 
  summarize(
    lo =val %>% quantile(.025),
    mu = val %>% quantile(.5),
    hi = val %>% quantile(.975)
  ) %>% 
  gather(
    val, est, lo:hi
  ) %>% 
  unite(
    type2, c("type", "val"), sep = "_"
  ) %>% 
  spread(type2, est)

library(purrrlyr)

t2 <- ml %>% 
  mutate(
    iter = 1:1000 %>% 
      rep(each = 29)
  ) %>% 
  group_by(
    iter
  ) %>% 
  by_slice(
    
    # i <- t2 %>% 
    #   filter(iter == 1)
    
    function(i)
      data.frame(
        fit = lm(estimate ~ accord, data = i) %>% 
          predict(
            newdata = data.frame(
              accord = seq(40, 85, by = .1)
            )
          ) %>% 
          as.numeric,
        accord = seq(40, 85, by = .1)
      ),
    .collate = "rows"
  ) %>% 
  group_by(
    accord
  ) %>% 
  summarize(
    lo = fit %>% quantile(.005),
    mu = fit %>% quantile(.5),
    hi = fit %>% quantile(.995)
  )


t3 <- t2 %>% 
  slice(1, 451) %>% 
  mutate(
    lab = mu %>% 
      round(2)
  )

library(showtext)
font_add_google("Roboto")

l1 <- lm(estimate_mu ~ accord_mu, data = t1) %>% 
  tidy

l2 <- data.frame(
  estimate_mu = 42,
  accord_mu = -.25,
  lab = str_c(
    "y = ",
    l1$estimate[[1]] %>% 
      round(1),
    "*** + ",
    l1$estimate[[2]] %>% 
      round(3),
    " × perceived accordance\n r^2 = ",
    lm(estimate_mu ~ accord_mu, data = t1) %>% 
      glance %>% 
      use_series(r.squared) %>% 
      round(2) %>% 
      as.character %>% 
      str_sub(2)
  )
)


p1 <-  ggplot() + 
  geom_ribbon(
    aes(ymin = lo, ymax = hi, x = accord),
    fill = "grey90",
    color = "grey5",
    size = .3,
    data = t2
  ) +
  geom_line(
    aes(accord, (lo + hi)/2),
    data = t2,
    size = .3,
    linetype = "solid"
  ) +
  geom_segment(
    aes(x = accord_lo, xend = accord_hi, 
        y = estimate_mu, yend = estimate_mu),
    data = t1,
    alpha  = .5,
    linetype = "dashed"
  ) +
  geom_segment(
    aes(x = accord_mu, xend = accord_mu, 
        y = estimate_lo, yend = estimate_hi),
    data = t1,
    alpha  = .5,
    linetype = "dashed"
  ) +
  geom_point(
    aes(accord_mu, estimate_mu),
    size = 3,
    fill= 'grey90',
    shape = 21,
    data = t1
  ) +
  geom_point(
    aes(accord, mu),
    size = 10,
    shape = 21,
    fill = "grey99",
    data = t3
  ) +
  geom_text(
    aes(accord, mu, label = lab),
    data = t3,
    size = 3,
    family = "Roboto"
  ) +
  geom_text(
    aes(estimate_mu, accord_mu, label = lab),
    # parse = T,
    size = 2.5,
    family = "Roboto",
    fontface = "bold.italic",
    data = l2
  ) +
  labs(
    x = "Perceived accordance between misstatement and correction",
    y = "Average correction effect (5pt scale)"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(panel.background = 
          element_rect(color = "grey98",
                       fill = "grey98"),
        panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.margin = margin(-.6, 3, unit = "cm"),
        strip.background  =
          element_rect(color = "grey98",
                       fill = "grey98"))


# p42, p43, p48, 
# these figures and tables are replicated at the dataverse available at:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/WUQRIQ
