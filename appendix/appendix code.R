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
  lme4, stargazer
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
