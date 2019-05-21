# Replication data -- 
# FALSE ALARM--the truth about political mistruth in the Trump era

# Code is copyright Ethan Porter & Thomas J Wood
# Code written:
# Fri May 17 22:37:07 2019 ------------------------------


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
  plyr, tidyverse, lubridate, 
  magrittr, emmeans, broom, 
  purrrlyr, ggstance, plotrix
)


t1 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%203/s_1_t.RDS" %>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

lm_1 <- lm(
  polar_num ~ party_num * cond,
  data = t1
)

em1 <- emmeans(
  lm_1,
  pairwise ~ cond | party_num,
  at = list(
    party_num = seq(1, 5, length.out = 100)
  )
)

ed1 <- em1 %>% 
  summary %>% 
  extract2("emmeans") %>% 
  as_tibble %>% 
  select(1:4) %>% 
  mutate(
    rowvar = "Fitted Values\n(larger values indicate agreement with incorrect position)"
  ) %>% 
  rename(
    estimate = emmean
  ) %>% 
  bind_rows(
    em1 %>% 
      summary %>% 
      extract2("contrasts") %>% 
      as_tibble %>% 
      select(1:4) %>% 
      mutate(
        rowvar = "Subtracted Items"
      ) %>% 
      filter(
        contrast %>% str_detect(" - items")
      ) %>% 
      mutate(
        contrast = contrast %>% 
          str_replace(" - items", "")
      ) %>% 
      rename(cond = contrast),
    em1 %>% 
      summary %>% 
      extract2("contrasts") %>% 
      as_tibble %>% 
      select(1:4) %>% 
      mutate(
        rowvar = "Subtracted Misstatement"
      ) %>% 
      filter(
        contrast %>%
          str_detect("correction - items") %>% 
          not
      ) %>%
      mutate(
        estimate = contrast %>% 
          str_detect(" - items") %>% 
          ifelse(
            estimate %>% 
              multiply_by(-1),
            estimate
          ),
        contrast = contrast %>% 
          str_replace("\\s\\-\\smisstatement|misstatement\\s\\-\\s", "")
      ) %>% 
      rename(cond = contrast)
  ) %>% 
  mutate(
    low = estimate %>% 
      subtract(
        SE %>%
          multiply_by(1.96)
      ),
    hi = estimate %>% 
      add(
        SE %>%
          multiply_by(1.96)
      )
  )

ed1$cond %<>% 
  mapvalues(
    c("correction", "items", "misstatement"),
    c("Misstatement & Correction & Item", "Survey Item Only (Control)", "Misstatement & Item")
  ) %>% 
  factor(
    c("Survey Item Only (Control)",  "Misstatement & Item", "Misstatement & Correction & Item")
  )

h_l <- ed1 %>% 
  group_by(
    cond, rowvar
  ) %>% 
  slice(1) %>% 
  select(party_num) %>% 
  ungroup %>% 
  filter(
    rowvar %>% 
      str_detect("Fitted") %>% 
      not
  ) %>% 
  modify_at(
    1:2,
    factor
  )

bub_lab <- ed1 %>% 
  group_by(
    cond, rowvar
  ) %>% 
  slice(
    c(1, 50, 100)
  ) %>% 
  mutate(
    est_lab = estimate %>%
      round(1) %>% 
      as.character
  )

# produces figure 4.1

ed1 %>%
    filter(
      rowvar %>% 
        str_detect(
          "Subtracted Items", 
          T)
      ) %>% 
    ggplot() +
    geom_blank(
      aes(x = party_num, 
          ymin = low, 
          ymax = hi)
    ) +
    geom_hline(
      aes(yintercept = 0),
      linetype = "dashed",
      data = h_l %>%
        filter(
          rowvar %>% 
            str_detect(
              "Subtracted Items", 
              T)
        )
    ) +
    geom_ribbon(
      size = .25,
      fill = "grey75",
      color = "grey10",
      alpha = .5,
      aes(x = party_num, 
          ymin = low, 
          ymax = hi)
    ) +
    geom_point(
      aes(party_num, estimate),
      size = 6,
      shape = 21,
      fill = "grey98",
      data = bub_lab %>%
        filter(
          rowvar %>% 
            str_detect(
              "Subtracted Items", 
              T)
        )
    ) +
    geom_text(
      aes(party_num, estimate, label = est_lab),
      size = 2,
      color = "black",
      data = bub_lab %>%
        filter(
          rowvar %>% 
            str_detect(
              "Subtracted Items", 
              T)
        )
    ) +
    facet_grid(
      rowvar ~ cond,
      scales = "free_y",
      space = "free_y",
      labeller = label_wrap_gen(width = 15)
    ) +
    scale_x_continuous(
      expand = c(.045, .045),
      breaks = c(1.5, 3, 4.5),
      labels = c("Strong\nDem",
                 "Ind",
                 "Strong\nRep")
    ) +
    labs(
      x = "Partisanship",
      y = ""
    ) +
    scale_y_continuous(
      breaks = c(
        seq(-.9, .6, .3),
        seq(1.5, 3, .5)
      ) %>% 
        round(1),
      labels = c(
        seq(-.9, .6, .3),
        seq(1.5, 3, .5)
      ) %>% 
        round(1) %>% 
        as.character %>% 
        str_replace(fixed("0."), ".")
    ) +
    theme_minimal() +
    theme(
      panel.background  =  element_rect(
        color =  "grey95",
        fill = "grey95"
      ),
      panel.grid = element_blank(),
      strip.background = element_rect(
        color =  "grey95",
        fill = "grey95"
      ),
      strip.text.y = element_text(angle = 0),
      # axis.text.x = element_text(size = 6),
      legend.background = element_rect(color = "white",
                                       fill = "white"),
      legend.margin = margin(-.8, 0, 0, 0, "cm"),
      panel.grid.minor = element_blank(),
      # panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0),
      plot.caption = element_text(face = "italic"),
      legend.position = "none") 
  
  
# comparing turk and rdd samples

t2 <-  "https://github.com/thomasjwood/false_alarm/raw/master/chapter%203/s_2_t.RDS" %>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df


t3 <- t2 %>% 
  bind_rows(
    t2 %>% 
      mutate(
        sample = "combined"
      )
    ) %>% 
  group_by(sample) %>% 
  by_slice(
    function(i)
      lm(
        paris_num ~ party_num * cond, 
        data = i)
    )
  

nd <- expand.grid(
  party_num = seq(1, 5, length.out = 300),
  cond = t2$cond %>% 
    factor %>% 
    levels
  ) %>% 
  tbl_df

pdat <- map2_dfr(
  t3$sample,
  t3$.out,  
  function(h, i)
    nd %>% 
    mutate(
      fit = i %>%
        predict(
          newdata = nd
        ) %>% 
        as.numeric,
      se = i %>%
        predict(
          newdata = nd,
          se.fit = T
        ) %>% 
        use_series(se.fit) %>% 
        as.numeric,
      sample = h %>% 
        factor(
          c("rdd", "turk", "combined")
          )
      )
  ) %>% 
  mutate(
    cond = cond %>% 
      factor(
        c("items", "misstatement", "correction")
      ),
    sample = c(
      "rdd", "turk", "combined"
    ) %>%
      rep(each = 900) %>% 
      factor(
        c(
          "rdd", "turk", "combined"
        )
      ),
    lo = fit %>% 
      subtract(
        se %>% 
          multiply_by(1.96)
      ),
    hi = fit %>% 
      add(
        se %>% 
          multiply_by(1.96)
      )
  )

ef <- map2_dfr(
  t3$sample,
  t3$.out,
    function(h, i)
      
      # i <- t3$.out[[1]]
      
      i %>% 
      emmeans(
        ~ cond | party_num, 
        at = list(party_num = seq(1, 5, length.out = 300))
      ) %>%
      contrast(interaction = c("consec")) %>% 
      # pairs(reverse = T) %>%
      tidy %>% 
      tbl_df %>% 
      # modify_at(
      #   1:2,
      #   str_trim
      # ) %>%
      # filter(
      #   level2 == "items"
      # ) %>%
      select(
        cond_consec:std.error, p.value
      ) %>% 
    mutate(
      sample = h
      )
  ) %>% 
  # rename(level1 = level2)
  # ) %>%
  rename(
    fit = estimate,
    se = std.error
  ) %>% 
  mutate(
    lo = fit %>% 
      subtract(
        se %>% 
          multiply_by(1.96)
      ),
    hi = fit %>% 
      add(
        se %>% 
          multiply_by(1.96)
      )
  )


ef2 <- ef %>%
  mutate(
    cond_consec = cond_consec %>% 
      mapvalues(
        ef$cond_consec %>% 
          levels,
        c("Misstatement - Items",
          "Misstatement & Correction  - Misstatement")
      ) %>% 
      factor(
        c("Misstatement - Items",
          "Misstatement & Correction  - Misstatement")
      ),
    sample = ef$sample %>% 
      mapvalues(
        c("combined", "rdd", "turk"),
        c("Combined\nSamples",
          "Random\nDigit\nDialer",
          "MTurk")
        ) %>% 
      factor(
        c("Combined\nSamples",
          "Random\nDigit\nDialer",
          "MTurk") %>% 
          rev
        )
    )


labs2 <- ef2 %>%
  mutate(
    party_num = party_num %>% 
      round(1)
  ) %>% 
  group_by(
    cond_consec, sample, party_num
  ) %>% 
  slice(
    1
  ) %>% 
  group_by(
    cond_consec, sample
  ) %>% 
  filter(
    party_num %>% 
      is_in(c(1, 3, 5))
  ) %>% 
  mutate(
    fl = fit %>% 
      round(2) %>% 
      as.character %>% 
      str_replace(fixed("0."), ".") %>%
      str_c(
        c("***", "**", "*", "") %>% 
          extract(
            p.value %>% 
              findInterval(
                c(-Inf, .001, .01, .05, Inf)
              )
          )
      )
  )


ef2 %>% 
  ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  geom_ribbon(
    aes(party_num, ymin = lo, ymax = hi),
    size = .3,
    color = "grey10",
    fill = "grey90",
    alpha = .75
  ) +
  geom_point(
    aes(party_num, fit),
    shape = 21,
    size = 7,
    color = "grey10",
    fill = "white",
    data = labs2
  ) +
  geom_text(
    aes(party_num, fit, label = fl),
    data = labs2,
    size = 1.9,
    family = "Roboto"
  ) +
  geom_text(
    aes(x, y, label =  lab),
    data = data.frame(
      x = c(.7, .7),
      y = c(-.45, .45),
      lab  = c("Less accurate",
               "More accurate"),
      sample = c("MTurk", "MTurk"),
      cond_consec = c(
        "Misstatement - Items",
        "Misstatement - Items"
      )
    ),
    size = 1.7,
    angle = 90,
    fontface = "italic"
  ) +
  scale_x_continuous(
    expand = c(.05, .03),
    breaks = c(1.5, 3, 4.5),
    labels = c("Strong\nDem",
               "Ind",
               "Strong\nRep")
  ) +
  facet_grid(sample ~ cond_consec) +
  theme_minimal() +
  labs(
    # title = "Paris climate agreement correction effects, by partisanship and sample",
    x = "Partisanship",
    y = "Paris climate agreement accuracy\n(positive numbers indicate accuracy)"
  ) +
  theme(
    panel.background  =  element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    panel.grid = element_blank(),
    strip.background = element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    strip.text.y = element_text(angle = 0),
    # axis.text.x = element_text(size = 6),
    legend.background = element_rect(color = "white",
                                     fill = "white"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic"),
    legend.position = "none"
    ) 


# figure 4.3
t4 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%203/s_3_t.RDS" %>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

ml <- t4 %>%
  group_by(
    item
  ) %>% 
  by_slice(
    ~lm(ans_num ~ cond * ideol_num, data = .)
    )

lt <- pmap_dfr(
  list(
    ml$.out,
    list(
      crossing(
        cond = ml$.out[[1]]$model$cond %>% levels,
        ideol_num = seq(1, 7, length.out = 200)
      ),
      crossing(
        cond = ml$.out[[2]]$model$cond %>% levels,
        ideol_num = seq(1, 7, length.out = 200)
      )
    ),
    t4$item %>%
      levels
  ),
  function(i, j, k){
    
    j %>% 
      mutate(
        fit = predict(i, newdata = j),
        se = predict(i, newdata = j, se.fit = T) %>% 
          use_series(se.fit),
        dv = k
      )
  }
) %>% 
  mutate(
    low = fit %>% 
      subtract(
        se %>% 
          multiply_by(1.96)
      ),
    hi = fit %>% 
      add(
        se %>% 
          multiply_by(1.96)
      )
  )


lt %>% 
  mutate(
    dv = dv %>% 
      mapvalues(
        c("blue collar wages",
          "border crisis"),
        c("Trump statement: blue collar wages growing faster than white collar wages",
          "Trump statement: unprecedented crisis at the southern border")
      ),
    cond = cond %>% 
      mapvalues(
        c("confirmation", "correction", "no correction"),
        c("Confirmation of true statement",
          "Correction of false statement",
          "Original statement")
      ) %>% 
      factor(
        c("Confirmation of true statement",
          "Original statement",
          "Correction of false statement")
      )
  ) %>%  
  ggplot() +
  geom_ribbon(
    aes(ideol_num, ymin = low, ymax = hi, fill = cond),
    color = "grey10",
    size = .25,
    alpha = .5
  ) +
  geom_text(
    aes(x, y, label = label),
    data = tribble(
      ~x, ~y, ~label, ~dv, 
      4.5, 3.3, "Original (true) statement", "Trump statement: blue collar wages growing faster than white collar wages",
      4.5, 5.5, "Original statement and confirmation", "Trump statement: blue collar wages growing faster than white collar wages"
    ) %>% 
      bind_rows(
        tribble(
          ~x, ~y, ~label, ~dv, 
          4.5, 3.25, "Original statement and correction", "Trump statement: unprecedented crisis at the southern border",
          4.5, 4.65, "Original (untrue) statement",  "Trump statement: unprecedented crisis at the southern border")
      ),
    angle = c(22.5, 20, 27.5, 36.5),
    size = 2.5,
    fontface = "italic"
  ) +
  facet_wrap(
    ~dv, nrow = 1, 
    labeller = label_wrap_gen(30)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  scale_y_continuous(
    breaks = 2:6,
    labels = c(
      "Disagree",
      "Somewhat\nDisagree",
      "Neither agree\nnor disagree",
      "Somewhat\nagree",
      "Agree")
    ) +
  scale_x_continuous(
    breaks = c(1.5, 4, 6.5),
    labels = c("Extremely Liberal",
               "Moderate",
               "Extremely Conservative") %>% 
      str_replace(" ", "\n")
  ) +
  scale_fill_manual(
    values = c("grey15",
               "grey55",
               "grey95")
  ) +
  labs(
    x = "",
    y = "Agreement with Trump statement"
  ) +
  theme_minimal() +
  theme(panel.background  = 
          element_rect(color = "grey95",
                       fill = "grey95"),
        panel.grid = element_blank(),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(color = "grey95",
                                        fill = "grey95"),
        legend.background = element_rect(color = "grey98",
                                         fill = "grey98"),
        legend.margin = margin(-.1, 0, 0, 0, "cm"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_text(face = "bold", hjust = 0),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") 


# The syntax below replicates the figures 4.4 and 4.5 

t5 <-  "https://github.com/thomasjwood/false_alarm/raw/master/chapter%203/s_4_t.RDS" %>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df



lm2 <- lm(
  facts_2_num ~ cond*ideol, 
  data = t5
  ) 

em2 <- emmeans(
  lm2, 
  pairwise ~ cond | ideol,
  at = list(
    ideol = seq(1, 7, length.out = 100)
  ),
  rev = T
)

nv2 <- em2 %>% 
  summary %>% 
  extract2("contrasts") %>% 
  as.tbl %>% 
  select(1:4) %>% 
  filter(
    contrast %>% str_detect("control - ")
  ) %>% 
  filter(
    contrast %>% 
      str_detect(
        "- mcconnell no correction| - trump no correction"
      )
  ) %>% 
  mutate(
    estimate = estimate %>% 
      multiply_by(-1),
    rn = "Misinformation effect",
    contrast = contrast %>% 
      str_sub(11),
    speaker = contrast %>% 
      str_extract("mcconnell|trump")
  ) %>% 
  select(-contrast) %>% 
  bind_rows(
    em2 %>% 
      summary %>% 
      extract2("contrasts") %>% 
      as.tbl %>% 
      select(1:4) %>% 
      # filter(
      #   contrast %>% str_detect("control - ")
      # ) %>% 
      filter(
        contrast %>% 
          is_in(
            c("mcconnell correction - mcconnell no correction",
              "trump correction - trump no correction"
            )
          )
      ) %>% 
      mutate(
        rn = "Correction effect",
        contrast = contrast %>%
          str_sub(11),
        speaker = contrast %>%
          str_extract("mcconnell|trump")
      ) %>%
      select(-contrast) 
  ) %>% 
  bind_rows(
    em2 %>% 
      pairs %>% 
      extract2(2) %>% 
      summary %>% 
      tbl_df %>% 
      filter(
        contrast %>% 
          equals(
            "mcconnell correction - mcconnell no correction - trump correction - trump no correction" 
          )
      ) %>% 
      select(
        ideol:SE
      ) %>% 
      mutate(
        rn = "Difference in correction effect(McConnell effect - Trump effect)",
        speaker = "Trump"
      )
  ) %>% 
  mutate(
    speaker = speaker %>% 
      mapvalues(
        c("mcconnell",
          "trump",
          "Trump"),
        c("Mitch McConnell",
          "Donald Trump",
          "Donald Trump")
      ) %>% 
      factor(
        c("Mitch McConnell",
          "Donald Trump")
      ),
    lo = estimate %>% 
      subtract(
        SE %>% 
          multiply_by(
            1.96
          )
      ),
    hi = estimate %>% 
      add(
        SE %>% 
          multiply_by(
            1.96
          )
      ),
    rn = rn %>% 
      factor(
        c("Misinformation effect",
          "Correction effect",
          "Difference in correction effect(McConnell effect - Trump effect)")
      )
  )

nv2_line <- nv2 %>% 
  group_by(speaker, rn) %>% 
  slice(1)

p_nv2_bubs <- nv2 %>% 
  group_by(
    rn, speaker
  ) %>% 
  arrange(ideol) %>% 
  slice(c(2, 50, 99)) %>% 
  ungroup %>% 
  mutate(
    lab = estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_replace_all(fixed("0."), ".") %>% 
      str_c(
        c("", "*", "**", "***") %>% 
          extract(
            findInterval(
              abs(estimate/SE),
              c(0, 1.6, 2.5, 4, Inf)
              
            )
          )
      )
  )

nv2 %>% 
  ggplot(
    aes(ideol, 
        ymin = lo,
        ymax = hi)
  ) +
  geom_hline(
    aes(yintercept = 0),
    data = nv2_line,
    linetype = "dashed"
  ) +
  geom_ribbon(
    color = "grey8",
    size = .2,
    alpha = .3,
    fill = "grey90"
  ) +
  geom_point(
    aes(ideol, estimate),
    size = 10,
    shape = 21,
    color = "grey10",
    fill = "grey93",
    data = p_nv2_bubs
  ) +
  geom_text(
    aes(
      ideol, estimate, label = lab
    ),
    size = 2.4,
    data = p_nv2_bubs
  ) +
  facet_grid(
    rn ~ speaker,
    scales = "free_y",
    space = "free_y",
    labeller = label_wrap_gen(20)
  ) +
  scale_x_continuous(
    breaks = c(1.5, 4, 6.5),
    labels = c("Ext\nLib",
               "Mod",
               "Ext\nCons")
  ) +
  labs(
    x = "",
    y = "Differences along 5 point scale of agreement.\nPositive values indicate increased inaccuracy."
  ) +
  theme_minimal() +
  theme(
    panel.background  =  element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    panel.grid = element_blank(),
    strip.background = element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    strip.text.y = element_text(angle = 0),
    # axis.text.x = element_text(size = 6),
    legend.background = element_rect(color = "white",
                                     fill = "white"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic"),
    legend.position = "none") 

lm1 <- lm(
  facts_1_num ~ cond*ideol, 
  data = t5
  ) 

em1 <- emmeans(
  lm1, 
  pairwise ~ cond | ideol,
  at = list(
    ideol = seq(1, 7, length.out = 100)
  ),
  rev = T
  )

nv1 <- em1 %>% 
  summary %>% 
  extract2("contrasts") %>% 
  as.tbl %>% 
  select(1:4) %>% 
  filter(
    contrast %>% str_detect("control - ")
  ) %>% 
  filter(
    contrast %>% 
      str_detect(
        "- mcconnell no correction| - trump no correction"
      )
  ) %>% 
  mutate(
    estimate = estimate %>% 
      multiply_by(-1),
    rn = "Misinformation effect",
    contrast = contrast %>% 
      str_sub(11),
    speaker = contrast %>% 
      str_extract("mcconnell|trump")
  ) %>% 
  select(-contrast) %>% 
  bind_rows(
    em1 %>% 
      summary %>% 
      extract2("contrasts") %>% 
      as.tbl %>% 
      select(1:4) %>% 
      # filter(
      #   contrast %>% str_detect("control - ")
      # ) %>% 
      filter(
        contrast %>% 
          is_in(
            c("mcconnell correction - mcconnell no correction",
              "trump correction - trump no correction"
            )
          )
      ) %>% 
      mutate(
        rn = "Correction effect",
        contrast = contrast %>%
          str_sub(11),
        speaker = contrast %>%
          str_extract("mcconnell|trump")
      ) %>%
      select(-contrast) 
  ) %>% 
  bind_rows(
    em1 %>% 
      pairs %>% 
      extract2(2) %>% 
      summary %>% 
      tbl_df %>% 
      filter(
        contrast %>% 
          equals(
            "mcconnell correction - mcconnell no correction - trump correction - trump no correction" 
          )
      ) %>% 
      select(
        ideol:SE
      ) %>% 
      mutate(
        rn = "Difference in correction effect(McConnell effect - Trump effect)",
        speaker = "Trump"
      )
  ) %>% 
  mutate(
    speaker = speaker %>% 
      mapvalues(
        c("mcconnell",
          "trump",
          "Trump"),
        c("Mitch McConnell",
          "Donald Trump",
          "Donald Trump")
      ) %>% 
      factor(
        c("Mitch McConnell",
          "Donald Trump")
      ),
    lo = estimate %>% 
      subtract(
        SE %>% 
          multiply_by(
            1.96
          )
      ),
    hi = estimate %>% 
      add(
        SE %>% 
          multiply_by(
            1.96
          )
      ),
    rn = rn %>% 
      factor(
        c("Misinformation effect",
          "Correction effect",
          "Difference in correction effect(McConnell effect - Trump effect)")
      )
  )

nv_line <- nv1 %>% 
  group_by(speaker, rn) %>% 
  slice(1)

p_nv_bubs <- nv1 %>% 
  group_by(
    rn, speaker
  ) %>% 
  arrange(ideol) %>% 
  slice(c(2, 50, 99)) %>% 
  ungroup

p_nv_bubs %<>% 
  mutate(
    lab = estimate %>% 
      round(2) %>% 
      as.character %>% 
      str_replace_all(fixed("0."), ".") %>% 
      str_c(
        c("", "*", "**", "***") %>% 
          extract(
            findInterval(
              abs(p_nv_bubs$estimate/p_nv_bubs$SE),
              c(0, 1.6, 2.5, 4, Inf)
              
            )
          )
      )
  )

nv1 %>% 
  ggplot(
    aes(ideol, 
        ymin = lo,
        ymax = hi)
  ) +
  geom_hline(
    aes(yintercept = 0),
    data = nv_line,
    linetype = "dashed"
  ) +
  geom_ribbon(
    color = "grey8",
    size = .2,
    alpha = .3,
    fill = "grey90"
  ) +
  geom_point(
    aes(ideol, estimate),
    size = 10,
    shape = 21,
    color = "grey10",
    fill = "grey93",
    data = p_nv_bubs
  ) +
  geom_text(
    aes(
      ideol, estimate, label = lab
    ),
    size = 2.4,

    data = p_nv_bubs
  ) +
  facet_grid(
    rn ~ speaker,
    scales = "free_y",
    space = "free_y",
    labeller = label_wrap_gen(20)
  ) +
  scale_x_continuous(
    breaks = c(1.5, 4, 6.5),
    labels = c("Ext\nLib",
               "Mod",
               "Ext\nCons")
  ) +
  labs(
    x = "",
    y = "Differences along 5 point scale of agreement.\nPositive values indicate increased accuracy."
  ) +
  theme_minimal() +
  theme(
    panel.background  =  element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    panel.grid = element_blank(),
    strip.background = element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    strip.text.y = element_text(angle = 0),
    # axis.text.x = element_text(size = 6),
    legend.background = element_rect(color = "white",
                                     fill = "white"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic"),
    legend.position = "none") 

# 

t2 <- t5 %>% 
  select(
    facts_1_num, facts_2, cond, ideol
  ) %>% 
  mutate(
    facts_1 = facts_1 %>% 
      mapvalues(
        c("Strongly Agree",
          "Agree",
          "Neither Agree nor Disagree",
          "Disagree", 
          "Strongly disagree"),
        c("Agree",
          "Neither",
          "Disagree") %>% 
          rep(c(2, 1, 2))
      ) %>% 
      factor(
        c("Agree",
          "Neither",
          "Disagree")
      ),
    facts_2 = facts_2 %>% 
      mapvalues(
        c("Strongly Agree",
          "Agree",
          "Neither Agree nor Disagree",
          "Disagree", 
          "Strongly disagree"),
        c("Agree",
          "Neither",
          "Disagree") %>% 
          rep(c(2, 1, 2))
      ) %>% 
      factor(
        c("Agree",
          "Neither",
          "Disagree")
      ),
    speaker = cond %>% 
      str_extract("mcconnell|trump"),
    corr = cond %>% 
      str_extract(
        "no correction|correction"
      ),
    ideol = ideol %>% 
      mapvalues(
        1:7,
        c("Liberal",
          "Moderate",
          "Conservative") %>% 
          rep(c(2, 3, 2))
      ) %>%
      factor(
        c("Liberal",
          "Moderate",
          "Conservative")
      )
  ) %>% 
  na.omit %>% 
  rename(
    facts_medicare = facts_1,
    facts_venezuela = facts_2
  ) %>% 
  gather(
    topic, ans, 1:2
  )

# figure 4,6

t6 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%203/s_5_t.RDS" %>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

t7 <- t6 %>% 
  group_by(
    speaker, topic
    ) %>% 
  by_slice(
    function(i)
      table(
        i$ans, i$corr
      ) %>% 
      chisq.test %>% 
      tidy %>% 
      mutate(
        n = i %>%
          nrow
      ), 
    .collate = "rows"
  ) %>% 
  mutate(
    ideol = "All respondents"
  ) %>% 
  bind_rows(
    t6 %>% 
      group_by(
        speaker, topic, ideol
      ) %>% 
      by_slice(
        function(i)
          table(
            i$ans, i$corr
          ) %>% 
          chisq.test %>% 
          tidy %>% 
          mutate(
            n = i %>%
              nrow
          ), 
        .collate = "rows"
      )
  )

t8 <- t6 %>% 
  group_by(
    speaker, corr, topic, ans
  ) %>% 
  tally %>% 
  mutate(
    prop = n %>% 
      divide_by(
        n %>% sum
      ),
    lab = prop %>% 
      multiply_by(100) %>% 
      round,
    ideol = "All respondents"
  ) %>% 
  bind_rows(
    t6 %>% 
      group_by(
        speaker, ideol, corr, topic, ans
      ) %>% 
      tally %>% 
      mutate(
        prop = n %>% 
          divide_by(
            n %>% sum
          ),
        lab = prop %>% 
          multiply_by(100) %>% 
          round
      )
  ) %>% 
  group_by(
    speaker, topic, ideol, corr,
  ) %>% 
  mutate(
    ans = ans %>% 
      factor(
        c("Disagree",
          "Neither",
          "Agree")
      )
  ) %>% 
  arrange(
    speaker, topic, ideol, corr, ans
  ) %>% 
  mutate(
    start =  0 %>% 
      c(prop %>% 
          extract(-3) %>% 
          cumsum),
    end = prop %>% 
      extract(-3) %>% 
      cumsum %>% 
      c(1),
    mid = start %>% 
      add(end) %>% 
      divide_by(2)
  ) %>%
  ungroup %>% 
  mutate(
    ideol = ideol %>%
      factor(
        c("All respondents",
          "Liberal",
          "Moderate",
          "Conservative")
      )
  )

t8 %<>% 
  left_join(
    t8 %>% 
      filter(ans == "Neither") %>% 
      mutate(
        bump = .5 %>% 
          subtract(mid)
      ) %>% 
      select(
        speaker:topic, ideol, bump
      )
  ) %>% 
  ungroup

t8 %<>% 
  modify_at(
    t8 %>% 
      names %>% 
      is_in(c("start", "end", "mid")) %>% 
      which,
    function(i)
      i %>% 
      add(t8$bump)
  )

t8$corr %<>% 
  factor


t8 %<>%
  mutate(
    speaker2 = speaker %>% 
      equals("trump") %>% 
      ifelse(
        "President\nTrump",
        "Senator\nMcConnell")
  ) 

t7 %<>%
  mutate(
    speaker2 = speaker %>% 
      equals("trump") %>% 
      ifelse(
        "President\nTrump",
        "Senator\nMcConnell")
  ) 

t8$ans %<>% 
  mapvalues(
    c("Agree", "Neither", "Disagree"),
    c("Inaccurate",
      "Neither",
      "Accurate")
  ) %>% 
  factor(
    c("Inaccurate",
      "Neither",
      "Accurate")
  )

t8$topic %<>% 
  mapvalues(
    t8$topic %>% 
      unique,
    c("Democrats intend\nMedicare cuts",
      "Democrats favor\nVenezuelan socialism")
  )

t7 %<>%
  mutate(
    topic = topic %>% 
      mapvalues(
        c("facts_medicare", "facts_venezuela"),
        c("Democrats intend\nMedicare cuts",
          "Democrats favor\nVenezuelan socialism")
      )
  )

t8 %<>% 
  left_join(
    t8 %>% 
      group_by(
        topic, speaker, ideol
      ) %>% 
      summarize(
        e2 = end %>% max
      )
  )

t7$lab <- t7$p.value %>% 
  round(3) %>% 
  as.character %>% 
  str_sub(2)



t7$lab[4] %<>% 
  str_c(
    "p.val = ", 
    .
  )

t7$p.value %>% 
  is_less_than(.05) %>% 
  if_else(
    "*", ""
  )

t7$lab %<>% 
  str_c(
    t7$p.value %>%
      is_less_than(
        .05
      ) %>% 
      if_else("*", "")
  )

bends <- t8 %>% 
  group_by(
    topic, speaker2, ideol
  ) %>% 
  by_slice(
    function(i)
      
      tibble(
        corr_start = i$end[i$corr == "correction"] %>% 
          max,
        uncorr_start = i$end[i$corr != "correction"] %>% 
          max
      ) %>% 
      mutate(
        hi = max(corr_start, uncorr_start) %>% max
      ),
    .collate = "rows"
  )


t7 %<>% 
  left_join(
    t8 %>% 
      group_by(
        speaker2, topic, ideol
      ) %>% 
      slice(1) %>% 
      select(e2)
  )

t8$ideol
bends$ideol
t7$ideol %<>% 
  factor(
    t8$ideol %>% 
      levels
  )

t8 %>% 
  ggplot() +
  geom_rect(
    aes(xmin = corr %>% 
          as.numeric %>% 
          subtract(.27),
        xmax = corr %>% 
          as.numeric %>% 
          add(.27),
        ymin = start,
        ymax = end,
        fill = ans),
    color = "black",
    size = .2
  ) +
  geom_text(
    aes(corr %>% 
          as.numeric,
        mid, 
        label = lab),
    size = 2.05,
    data = t8 %>% 
      filter(
        prop > .11
      )
  ) +
  geom_segment(
    x = 1, xend = 1,
    aes(y = corr_start,
        yend = hi %>% add(.15)),
    data = bends,
    size = .2
  ) +
  geom_segment(
    x = 2, xend = 2,
    aes(y = uncorr_start,
        yend = hi %>% add(.15)),
    data = bends,
    size = .2
  ) +
  geom_segment(
    x = 1, xend = 2,
    aes(y = hi %>% add(.15),
        yend = hi %>% add(.15)),
    data = bends,
    size = .2
  ) +
  geom_label(
    label.size = 0,
    fill = "grey95",
    aes(label = lab,
        y = e2 %>% add(.15)),
    x = 1.5,
    size = 2.75,
    data = t7
  ) +
  facet_grid(
    topic + speaker2 ~ ideol
  ) +
  scale_fill_manual(
    values = c("grey99",
               "grey87",
               "grey75")
  ) +
  scale_x_continuous(
    breaks = 1:2, 
    labels = c("Corrected",
               "Uncorrected"),
    expand = c(.4, 0)
  ) +
  scale_y_continuous(
    breaks = NULL
  ) +
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    panel.background  =  element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    panel.grid = element_blank(),
    strip.background = element_rect(
      color =  "grey95",
      fill = "grey95"
    ),
    strip.text.y = element_text(angle = 0),
    axis.text.x = element_text(angle = 45/2,
                               hjust = 1),
    legend.background = element_rect(color = "white",
                                     fill = "white"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    # panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic"),
    legend.position = "bottom") 






