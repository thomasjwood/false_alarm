# Replication data -- 
# FALSE ALARM--the truth about political mistruth in the Trump era

# Code is copyright Ethan Porter & Thomas J Wood
# Code written:
# Thu May 16 20:21:21 2019 ------------------------------


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
  purrrlyr, ggstance, plotrix,
  furrr
  )



# loading the most important problem data set, and aggregating over issue types
t1 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/USMISC2015-MIPD_ind.rds" %>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df %>% 
  select(
    studyid, id,
    fw_end,
    weight,
    mip_singer1
  ) %>% 
  mutate(
    id = studyid %>% 
      str_c(
        "_",
        id
      )
  ) %>% 
  filter(
    fw_end %>% 
      is_greater_than(
        # hardcing for date of analysos
        as.Date("2018-10-06") %>% 
          subtract(
            years(10)
          )
      )
  )  %>% 
  rename(
    year = fw_end
  ) %>%
  mutate(
    year = year %>% 
      year %>% 
      as.numeric
  ) %>% 
  gather(
    rank, ans, starts_with("mip_"), na.rm = T
  ) %>% 
  group_by(
    ans
  ) %>% 
  tally(weight) %>% 
  complete(
    ans, fill = list(n = 0)
  ) %>% 
  mutate(
    perc = n %>% 
      divide_by(
        n %>% 
          sum(na.rm = T)
      ) %>% 
      multiply_by(100) %>% 
      round(2),
    perc = perc %>%
      is.nan %>% 
      ifelse(
        0, 
        perc
      )
  )

#  ordering issues by magnitude
ans_filt <- t1 %>% 
  group_by(
    ans
  ) %>% 
  summarize(
    mu = perc %>% 
      mean
  ) %>% 
  arrange(
    desc(
      mu
    )
  ) %>% 
  slice(1:30) %>% 
  use_series(ans)

t1 %<>% 
  filter(
    ans %>%
      is_in(ans_filt) 
  ) %>% 
  ungroup %>% 
  mutate(
    ans = ans %>% 
      fct_reorder(
        perc
      )
  )

# labelling inssues featuing a correction
t1$include <- t1$ans %>% 
  mapvalues(
    t1$ans %>% 
      levels,
    c(0, 1, 1, 
      0, 1, 1, 1, 
      0, 1, 1, 
      0, 1,0, 1, 0, 
      1, 1, 1, 
      0, 1, 0, 
      1, 1, 0, 
      1, 1, 1, 
      1, 1, 1
    )
  )

# this replicates figure 2.2
t1 %>% 
  ggplot() +
  geom_point(
    aes(perc, ans, shape = include),
    size = 2.5
  )  +
  scale_y_discrete(
    breaks = t1$ans %>% 
      levels,
    labels = t1$ans %>% 
      levels %>% 
      str_detect("\\s\\(") %>%
      ifelse(
        t1$ans %>%
          levels %>%
          str_replace_all("\\d{1,3}\\.\\s", "") %>% 
          str_sub(,
                  
                  str_locate(
                    t1$ans %>%
                      levels %>%
                      str_replace_all("\\d{1,3}\\.\\s", ""),
                    fixed(" (")
                  ) %>% 
                    extract(, 1) %>% 
                    subtract(1)
          ),
        t1$ans %>%
          levels %>%
          str_replace_all("\\d{1,3}\\.\\s", "")
      )
  ) +
  scale_shape_manual(
    values = c(21, 16),
    breaks = c(1, 0),
    labels = c("Issue not subject of an experimental correction",
               "Issue subject of an experimental correction") %>% 
      rev,
    guide = guide_legend()
  ) +
  scale_x_log10() +
  labs(
    x = "Respondents citing this an important national problem\n(Percent, Log 10 scale)",
    y = "",
    shape = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.75, .075)
  )

# raw survey data for figure 2.3
t3 <-  "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_1_mt.RDS"%>%
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df


# bootstrapping corrections--takes about 1 minute to finish on a multicore machine
t4 <- t3 %>%
  group_by(item) %>% 
  by_slice(
    function(i)
      
      1e3 %>%
      seq %>%
      furrr::future_map_dfr(
        ~i %>%
          sample_frac(1, T) %$% 
          lm(ans ~ ideol * corr) %>% 
          emmeans(
            ~ideol * corr,
            at = list(ideol = c(1, 4, 7))
          ) %>% 
          broom::tidy(),
        .progress =  T
      ), 
    .collate = "rows"
  ) %>% 
  ungroup %>% 
  mutate(
    ideol = ideol %<>%
      mapvalues(
        c(1, 4, 7),
        c("Liberals",
          "Moderates",
          "Conservatives")
      ) %>% 
      factor(
        c("Liberals",
          "Moderates",
          "Conservatives")
      ),
    corr = corr %>% 
      mapvalues(
        1:2,
        c("corrected",
          "uncorrected")
      ) %>% 
      factor
  )

# arranging factor  by ideological slope

t4$item %<>% 
  factor(
    t4 %>% 
      tbl_df %>% 
      group_by(item) %>% 
      by_slice(
        function(i)
          lm(estimate ~ as.numeric(ideol), data = i) %>% 
          tidy,
        .collate = "rows"
      ) %>% 
      filter(
        term %>% 
          str_detect(
            "ideol"
          )
      ) %>% 
      arrange(
        estimate
      ) %>% 
      use_series(item)
  )


ct <- t3 %>%
  group_by(item) %>% 
  by_slice(
    function(i)

      lm(ans ~ ideol * corr, data = i) %>% 
      emmeans(
        pairwise  ~ corr | ideol,
        at = list(ideol = c(1, 4, 7))
      ) %>%
      extract2(2) %>%
      summary %>% 
      as.data.frame,
    .collate = "rows"
  ) %>% 
  tbl_df %>% 
  mutate(
    sig = p.value %>% 
      is_less_than(.01) %>% 
      ifelse("sig", "insig"),
    ideol = ideol  %>%
      mapvalues(
        c(1, 4, 7),
        c("Liberals",
          "Moderates",
          "Conservatives")
      ) %>% 
      factor(
        c("Liberals",
          "Moderates",
          "Conservatives")
      )
    )


ct$item %<>% 
  factor(
    t4$item %>% 
      levels
  )

t4l <- t4 %>% 
  tbl_df %>% 
  group_by(
    item, ideol, corr
  ) %>% 
  summarize(
    mu = estimate %>% 
      mean
  ) %>% 
  mutate(
    lab = mu %>% 
      signif(2) %>% 
      as.character
  ) %>% 
  left_join(
    ct %>% 
      select(
        item, ideol, sig
      )
  )

# Reproduces figure 2.3

t4 %>% 
  ggplot() +
  geom_violinh(
    trim = F,
    aes(estimate, item, fill = corr),
    size = .25,
    color ="black",
    position = position_dodgev(height = 0)
  ) +
  geom_point(
    shape = 21,
    aes(mu, item, fill = corr),
    size = 5,
    data = t4l
  ) +
  geom_text(
    aes(mu, item, label = lab),
    size = 2,
    data = t4l
  ) +
  geom_text(
    aes(x, item, label = lab),
    size = 2.5,
    fontface= "italic",
    data = t4l %>% 
      filter(
        ideol == "Liberals" &
          item %>% 
          str_detect("Rubio")
      ) %>% 
      select(item, ideol) %>% 
      mutate(
        x = c(1.4, 3.45),
        lab = c("Corrected",
                "Uncorrected")
      )
  ) +
  scale_y_discrete(
    breaks = ct$item %>%
      levels,
    labels = t4$item %>%
      levels %>%
      str_sub(, -4) %>%
      str_wrap(width = 25),
    expand = c(.1, .1)
  ) +
  facet_grid(
    . ~ ideol, 
    scales = "free_x", 
    space = "free_x"
    ) +
  scale_x_continuous(
    breaks = 2:4
  ) +
  scale_fill_grey(
    start = .65,
    end = 1
  ) +
  scale_color_grey(
    start = .65,
    end = 1
  ) +
  labs(
    y = "",
    x = "Agreement with incorrect claim\n(5pt scale, higher values indcate agreement)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background  = 
      element_rect(color = "grey95",
                   fill = "grey95"),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, 
                               lineheight = .65),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(color = "grey95",
                                    fill = "grey95"),
    legend.background = element_rect(color = "grey98",
                                     fill = "grey98"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic")
  ) 



t5 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_2_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df



# bootstrapping correction effects--runs about as quickly as the one above
t6 <- t5 %>%
  group_by(
    item
    ) %>% 
  by_slice(
    function(i)
      
      1e3 %>% 
      seq %>% 
      future_map_dfr(
        ~i %>%
          sample_frac(1, T) %$% 
          lm(ans ~ ideol * corr) %>% 
          emmeans(
            ~ideol * corr,
            at = list(ideol = c(1, 4, 7))
          ) %>%
          tidy,
        .progress = T
        ),
    .collate = "rows"
  ) %>% 
  mutate(
    corr = corr %>% 
      mapvalues(
        1:2,
        c("Correction",
          "No Correction"
          )
      )
  )

# clearing up values for plotting
t6$ideol %<>% 
  mapvalues(
    c(1, 4, 7),
    c("Liberals",
      "Moderates",
      "Conservatives")
  ) %>% 
  factor(
    c("Liberals",
      "Moderates",
      "Conservatives")
  )

# separately indicating issues/ speaker dimenions

t6$item2 <- t6$item %>% 
  str_sub(
    ,
    t6$item %>% 
      str_locate(fixed(" (")) %>% 
      extract(, 1) %>% 
      subtract(1)
  ) 

t6$speaker <- case_when(
  t6$item %>%
    str_extract("\\(\\w{3,10}\\)") %>%
    str_sub(2, -2) %>% 
    is_in(
      c("Obama",  "Gutierrez",  "Longoria", "Lee") 
    ) ~ "Liberal Speaker",
  t6$item %>%
    str_extract("\\(\\w{3,10}\\)") %>%
    str_sub(2, -2) %>% 
    is_in(
      c("Obama",  "Gutierrez",  "Longoria", "Lee")
    ) %>% 
    not ~ "Conservative Speaker"
  )


# the item facets will be sorted by most to least ideological

t6$item2 %<>% 
  factor(
    t6 %>%
      filter(
        corr == "Correction"
      ) %>% 
      tbl_df %>% 
      group_by(item2) %>% 
      by_slice(
        function(i)
          i %$% 
          lm(estimate ~ as.numeric(ideol)) %>% 
          tidy, 
        .collate = "rows"
        ) %>% 
      filter(
        term %>% 
          str_detect(
            "ideol"
          )
      ) %>% 
      arrange(
        estimate
      ) %>% 
      use_series(item2)
  )

# these will label inside the violin plots
ct5 <- t5 %>%
  group_by(
    item
    ) %>% 
  by_slice(
    function(i)
      lm(
        ans ~ ideol * corr,
        data =i
        ) %>% 
      emmeans(
        pairwise  ~ corr | ideol,
        at = list(ideol = c(1, 4, 7))
      ) %>%
      extract2(2) %>%
      summary %>% 
      as.data.frame,
    .collate = "rows"
  ) %>% 
  tbl_df %>% 
  mutate(
    sig = p.value %>% 
      is_less_than(.01) %>% 
      ifelse("sig", "insig"),
    ideol = ideol %>% 
      mapvalues(
          c(1, 4, 7),
          c("Liberals",
            "Moderates",
            "Conservatives")
        ) %>% 
        factor(
          c("Liberals",
            "Moderates",
            "Conservatives")
        )
    )


t6_l <- t6 %>% 
  tbl_df %>% 
  group_by(
    item, ideol, corr
  ) %>% 
  summarize(
    mu = estimate %>% 
      mean
  ) %>% 
  mutate(
    lab = mu %>% 
      signif(2) %>% 
      as.character
  ) %>% 
  left_join(
    ct5 %>% 
      select(
        item, ideol, sig
      )
  )

t6_l_2 <- t6 %>% 
  tbl_df %>% 
  group_by(
    item2, ideol, corr, speaker
  ) %>% 
  summarize(
    mu = estimate %>% 
      mean
  ) %>% 
  mutate(
    lab = mu %>% 
      signif(2) %>% 
      as.character
  ) 



# this reproduces figure 2.4
t6 %>% 
  ggplot() +
  geom_violinh(
    trim = F,
    aes(estimate, speaker, fill = corr),
    size = .25,
    # color ="transparent",
    position = position_dodgev(height = 0)
  ) +
  geom_point(
    shape = 21,
    aes(mu, speaker, fill = corr),
    size = 5,
    # fill = "white",
    data = t6_l_2
  ) +
  geom_text(
    aes(mu, speaker, label = lab),
    size = 2,
    data = t6_l_2
    ) +
  scale_y_discrete(
    breaks = t6$speaker %>% 
      factor %>% 
      levels,
    labels = c("Cons.", "Lib.")
  ) +
  geom_text(
    aes(x, speaker, label = lab, vjust = vjust),
    size = 2.5,
    fontface= "italic",
    data = t6_l_2 %>% 
      filter(
        ideol == "Liberals" &
        item2 %>% 
          str_detect("Obama restricts ")
      ) %>% 
      select(item2, ideol) %>% 
      mutate(
        speaker = "Liberal Speaker"
      ) %>% 
      group_by(item2, corr) %>% 
      slice(1) %>% 
      ungroup %>% 
      mutate(
        vjust = c(-1.5, .5),
        lab = c("Corrected",
                "Uncorrected"),
        x = c(1.6, 3.1)
      )
  ) +
  facet_grid(
    item2 ~ ideol, 
    labeller = label_wrap_gen(width = 15),
    scales = "free_x", 
    space = "free_x"
  ) +
  scale_x_continuous(
    breaks = 2:4
  ) +
  scale_fill_grey(
    start = .65,
    end = 1
  ) +
  scale_color_grey(
    start = .65,
    end = 1
  ) +
  labs(
    y = "Speaker Ideology",
    x = "Agreement with incorrect claim\n(5pt scale, higher values indcate agreement)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background  = 
      element_rect(color = "grey95",
                   fill = "grey95"),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, 
                               lineheight = .65),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(color = "grey95",
                                    fill = "grey95"),
    legend.background = element_rect(color = "grey98",
                                     fill = "grey98"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic")
  ) 


# Table 2.1--may be (insignificantly) different to chapter because of simulation

t6_l_2 %>% 
  ungroup %>% 
  mutate(
    corr = corr %>% 
      mapvalues(
        c("Correction", "No Correction"),
        c("co", "nc")
      )
  ) %>% 
  select(-lab) %>% 
  spread(corr, mu) %>% 
  mutate(ce = co - nc) %>% 
  group_by(
    ideol, speaker
  ) %>% 
  summarize(
    mu = ce %>% 
      mean
  ) %>% 
  spread(
    speaker, mu
  )

# simulation for figure 2.5

t7 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_3_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

t8 <- t7 %>% 
  group_by(
    item
    ) %>% 
  by_slice(
    function(i)
      
      1e3 %>% 
      seq %>% 
      future_map_dfr(
        ~i %>%
          sample_frac(1, T) %$% 
          lm(ans ~ ideol * corr) %>% 
          emmeans(
            ~ideol * corr,
            at = list(ideol = c(1, 4, 7))
          ) %>% 
          tidy,
        .progress = T
      ),
    .collate = "rows"
  )


t8$ideol %<>% 
  mapvalues(
    c(1, 4, 7),
    c("Liberals",
      "Moderates",
      "Conservatives")
  ) %>% 
  factor(
    c("Liberals",
      "Moderates",
      "Conservatives")
  )

t8$corr %<>% 
  mapvalues(
  1:2,
  c("Correction",
    "No Correction")
  )

# ordering items by ideological difference
t8$item %<>% 
    factor(
      t8 %>%
        filter(
          corr == "Correction"
        ) %>% 
        group_by(
          item
        ) %>%
        by_slice(
          function(i)
            lm(
              estimate ~ as.numeric(ideol), 
              data = i
            ) %>% 
            tidy,
          .collate = "rows"
        ) %>% 
        filter(
          term %>% 
            str_detect(
              "ideol"
            )
        ) %>% 
      arrange(
        estimate
      ) %>% 
      use_series(item)
  )

ct7 <- t7 %>%
  group_by(
    item
    ) %>% 
  by_slice(
    function(i)
      lm(
        ans ~ ideol * corr,
        data = i
        ) %>% 
      emmeans(
        pairwise  ~ corr | ideol,
        at = list(ideol = c(1, 4, 7))
      ) %>%
      extract2(2) %>%
      summary %>% 
      as.data.frame,
    .collate = "rows"
  ) %>% 
  tbl_df %>% 
  mutate(
    sig = p.value %>% 
      is_less_than(.01) %>% 
      ifelse("sig", "insig")
  ) %>% 
  mutate(
    ideol = ideol %>% 
      mapvalues(
        c(1, 4, 7),
        c("Liberals",
          "Moderates",
          "Conservatives")
      ) %>% 
      factor(
        c("Liberals",
          "Moderates",
          "Conservatives")
      )
    )


t8_l <- t8 %>% 
  tbl_df %>% 
  group_by(
    item, ideol, corr
  ) %>% 
  summarize(
    mu = estimate %>% 
      mean
  ) %>% 
  mutate(
    lab = mu %>% 
      signif(2) %>% 
      as.character
  ) %>% 
  left_join(
    ct7 %>% 
      select(
        item, ideol, sig
      )
  )

# Reproduces figure 2.5

t8 %>% 
  ggplot() +
  geom_violinh(
    trim = F,
    aes(estimate, item, fill = corr),
    size = .25,
    position = position_dodgev(height = 0)
  ) +
  geom_point(
    shape = 21,
    aes(mu, item, fill = corr),
    size = 5,
    data = t8_l
  ) +
  geom_text(
    aes(mu, item, label = lab),
    size = 2,
    data = t8_l %>% 
      filter(
        corr == "Correction"
      )
    ) +
  geom_point(
    shape = 21,
    aes(mu, item),
    size = 4.25,
    color = "white",
    fill = "white",
    data = t8_l %>%
      filter(
        corr != "Correction"
      )
  ) +
  geom_text(
    aes(mu, item, label = lab),
    size = 2,
    data = t8_l %>%
      filter(
        corr != "Correction"
      )
    ) +
  geom_text(
    aes(x, item, label = lab),
    size = 2.5,
    fontface= "italic",
    data = t8_l %>%
      filter(
        ideol == "Liberals" &
          item %>%
          str_detect("Ryan")
      ) %>%
      select(item, ideol) %>%
      mutate(
        x = c(1.3, 3.5),
        lab = c("Corrected",
                "Uncorrected")
      )
  ) +
  scale_y_discrete(
    breaks = t8$item %>% 
      levels,
    labels = t8$item %>% 
      levels %>% 
      str_sub(, -4) %>% 
      str_wrap(width = 23),
    expand = c(.1, .1)
  ) +
  facet_grid(
    . ~ ideol, 
    scales = "free_x", 
    space = "free_x") +
  scale_x_continuous(
    breaks = 2:4,
    expand = c(.1, .1)
  ) +
  scale_fill_grey(
    start = .65,
    end = 1
  ) +
  scale_color_grey(
    start = .65,
    end = 1
  ) +
  labs(
    y = "",
    x = "Agreement with incorrect claim\n(5pt scale, higher values indicate agreement)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background  = 
      element_rect(color = "grey95",
                   fill = "grey95"),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, 
                               lineheight = .65),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(color = "grey95",
                                    fill = "grey95"),
    legend.background = element_rect(color = "grey98",
                                     fill = "grey98"),
    legend.margin = margin(-.8, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic")
  ) 


# The item complexity bootstrap--figure 2.6

t9 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_4_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

# the out of sample data frame
nd2 <- expand.grid(
  ideol = seq(1, 7,
              length.out = 200),
  ans_type = c("item_short",
               "item_long"),
  cor = c("C", "NC")
  ) %>% 
  tbl_df

# bootstrapped complxity data

t10 <- 1e3 %>% 
  seq %>% 
  map_dfr(
    ~t9 %>% 
      sample_frac(1, T) %$% 
      lm(res ~ ideol*cor*ans_type) %>%  
      predict(newdata = nd2) %>%
      cbind(nd2, .), 
    .id = 'iter'
    ) %>% 
  tbl_df

names(t10)[5] <- "pred"

t11 <- t10 %>% 
  spread(cor, pred) %>% 
  mutate(eff = C - NC) %>% 
  group_by(ideol, ans_type) %>% 
  summarize(lo  = eff %>% 
              quantile(.025),
            mid  = eff %>% 
              quantile(.5),
            hi  = eff %>% 
              quantile(.975))


t12 <- t11 %>% 
  group_by(ans_type) %>% 
  by_slice(
    function(i)
      
      i %>%
      ungroup %>% 
      mutate(
        lo_fit = loess(lo ~ ideol, data = i) %>%
          use_series(fitted),
        hi_fit = loess(hi ~ ideol, data = i) %>%
          use_series(fitted)),
    .collate = "rows"
    ) %>% 
  tbl_df %>% 
  mutate(ans_type = ans_type %>% 
           mapvalues(str_c("item_",
                           c("long",
                             "short")),
                     c('Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".',
                       'Survey Item: "Following the US invasion of Iraq in 2003, US forces did not find weapons of mass destruction".')) %>% 
           factor(c('Survey Item: "Immediately before the U.S. invasion, Iraq had an active weapons of mass destruction program, the ability to produce these weapons, and large stockpiles of WMD, but Saddam Hussein was able to hide or destroy these weapons right before U.S. forces arrived".',
                    'Survey Item: "Following the US invasion of Iraq in 2003, US forces did not find weapons of mass destruction".')))


t13 <- 1000 %>% 
  seq %>% 
  map_dfr(
    ~t9 %>%
      sample_frac(1, T) %>% 
      group_by(
        ans_type, ideol, cor
      ) %>% 
      summarize(
        mu = res %>% mean
      ) %>% 
      arrange(ans_type, ideol) %>% 
      spread(cor, mu) %>% 
      mutate(
        diff = C - NC
      ),
    .id = "iter"
  ) %>% 
  ungroup %>% 
  arrange(ideol, ans_type, iter) %>% 
  group_by(
    ans_type, ideol
  ) %>% 
  summarize(
    lo = diff %>% quantile(probs = .025, na.rm = T),
    mu = diff %>% quantile(probs = .5, na.rm = T),
    hi = diff %>% quantile(probs = .975, na.rm =T)
  ) %>% 
  left_join(
    t9 %>% 
      group_by(ideol, ans_type) %>% 
      tally
  )

t14 <- t12 %>%
  mutate(
    est_type = "Regression model contrast"
  ) %>% 
  bind_rows(
    t13 %>% 
      ungroup %>% 
      mutate(
        est_type = "Difference in group means",
        ans_type = ans_type %>% 
          mapvalues(
            c("item_long",
              "item_short"),
            t12$ans_type %>% 
              factor %>% 
              levels
          ) %>% 
          factor(
            t12$ans_type %>% 
              factor %>% 
              levels
          )
      )
  ) %>% 
  mutate(
    est_type = est_type %>% 
      factor(
        c("Regression model contrast",
          "Difference in group means")
      )
  )

ggplot() +
  geom_hline(yintercept = 0,
             color = "gray30",
             linetype = 2) +
  geom_ribbon(aes(x = ideol, ymin = lo_fit, ymax = hi_fit),
              alpha = .3,
              size = .25,
              color = "black",
              data = t14 %>% 
                filter(mu %>% is.na)
  ) +
  geom_text(aes(x, y, label = label),
            angle = 90,
            hjust = .5,
            size = 3.25,
            data = data.frame(x = .75, 
                              y = -.0125,
                              label = "Adoption|Backfire",
                              ans_type = t14$ans_type %>% 
                                levels %>%
                                extract2(1),
                              est_type = t14$est_type %>% 
                                levels %>% 
                                extract2(1)),
            color = "grey20")+
  geom_line(aes(x = ideol, y = mid),
            linetype = 2,
            t14 %>% 
              filter(mu %>% is.na)
            ) +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "grey50",
               data = data.frame(x = .75,
                                 xend = .75,
                                 y = -.275-.15,
                                 yend = -.4-.15,
                                 ans_type = t14$ans_type %>% 
                                   levels %>%
                                   extract2(1),
                                 est_type = t14$est_type %>% 
                                   levels %>% 
                                   extract2(1)),
               size = 1,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_segment(aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "grey50",
               data = data.frame(x = .75,
                                 xend = .75,
                                 y = .275 - .03 + .15,
                                 yend = .4 - .03 + .15,
                                 # label = "Adoption|Backfire",
                                 ans_type = t14$ans_type %>% 
                                   levels %>%
                                   extract2(1),
                                 est_type = t14$est_type %>% 
                                   levels %>% 
                                   extract2(1)
               ),
               size = 1,
               arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +
  geom_linerange(
    aes(ideol, ymin = lo, ymax = hi),
    fatten = 6,
    data = t14 %>% 
      filter(mu %>% is.na %>% not)
  ) +
  geom_point(
    aes(ideol, y = mu, size = n),
    fatten = 6,
    data = t14 %>% 
      filter(
        mu %>% 
          is.na %>%
          not
      ),
    shape = 21,
    fill = "grey98"
  ) +
  labs(
    size = "Subjects in ideology x condition group",
    y = "Marginal correction effect\n(difference on 5pt scale)"
  ) +
  scale_x_continuous("Ideology",
                     breaks = 1:7,
                     labels = str_c(c("Lib",
                                      "",
                                      "",
                                      "Mod",
                                      "",
                                      "",
                                      "Cons"))) +
  facet_grid(est_type ~ ans_type, 
             labeller = label_wrap_gen(multi_line = T, 
                                       width = 50),
             scales = "free",
             space = "free_x") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "grey97",
                                        color = "grey97"),
        strip.background = element_rect(fill = "grey97",
                                        color = "grey97"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.ticks.x = element_blank(), 
        axis.ticks.length = unit(.25, "cm"),
        legend.margin = margin(t = -.4, unit = "cm"),
        legend.position = "bottom",
        strip.text.x = element_text(size = 10.5,
                                    face = "italic"),
        strip.text.y = element_text(size = 10.5))


# complexity as point range--figure 2.7

t15 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_5_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df


# estimating separate linear models for each issue
# then having correction effects by complexity and ideology

t16 <- t15 %>% 
  group_by(
    item
    ) %>% 
  by_slice(
    function(i)
      lm(
        ans ~ corr * complexity * ideol,
        data = i
      ) %>% 
      emmeans(
        pairwise ~ corr | complexity * ideol,
        at = list(
          ideol = c(1, 4, 7)
        )
      ) %>%
      summary %>% 
      magrittr::extract("contrasts") %>% 
      as.data.frame,
    .collate = "rows"
  ) %>% 
  tbl_df %>%
  mutate(
    type = "Individual issues"
  ) %>% 
  bind_rows(
    t15 %>% 
      mutate(
        item = item %>% 
          str_detect(
            "(Sanders)|(Clinton)"
          ) %>% 
          ifelse(
            "Liberal speakers",
            "Conservative speakers"
          )
      ) %>% 
      group_by(item) %>% 
      by_slice(
        function(i)
          
          lm(
            ans ~ corr * complexity * ideol,
            data = i
          ) %>% 
          emmeans(
            pairwise ~ corr | complexity * ideol,
            at = list(
              ideol = c(1, 4, 7)
            )
          ) %>%
          summary %>% 
          magrittr::extract("contrasts") %>% 
          as.data.frame,
        .collate = "rows"
      ) %>%
      tbl_df %>% 
      mutate(
        type = "Overall"
      )
  ) %>% 
  mutate(
    contrasts.ideol = contrasts.ideol %>%
      mapvalues(
        c(1, 4, 7),
        c("Liberals",
          "Moderates",
          "Conservative")
      ) %>%
      factor(
        c("Liberals",
          "Moderates",
          "Conservative")
      )
  )

names(t16) %<>% 
  str_replace_all("contrasts.", "")

t16$lab <- t16$estimate %>%
  signif(1) %>% 
  as.character %>% 
  str_replace_all(
    fixed("0."), "."
  ) %>% 
  str_c(
    c("***",
      "**",
      "*",
      "") %>% 
      extract(
        t16$p.value %>% 
          findInterval(
            c(-Inf, .001, .01, .05, Inf)
          )
      )
  )

t16$sig <- t16$p.value %>%
  is_less_than(.05) %>% 
  ifelse(
    "significant",
    "insignificant"
  )

t16$item %<>% 
  fct_reorder(
    t16$estimate, .desc = T
  )

t16 %<>% 
  mutate(
    lo = estimate %>%
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

t16$complexity %<>% 
  mapvalues(
    3:1,
    c("Simple",
      "Moderate",
      "Complex")
  ) %>% 
  factor

t16 %>% 
  mutate(
    type = type %>% 
      str_replace(" ", "\n")
  ) %>% 
  ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_pointrangeh(
    aes(xmin = lo, xmax = hi, x = estimate, 
        y = item,
        fill = complexity,
        group = complexity),
    shape = 21,
    stat = "identity",
    color = "black",
    size = .6,
    position = position_dodgev(height =.65)
  ) +
  geom_text(
    aes(
      estimate - .3, 
      item, 
      label = complexity,
      group = complexity
    ),
    fontface = "italic",
    size = 2.75,
    hjust = 1,
    position = position_dodgev(height =.65),
    data = t16%>% 
      filter(
        ideol == "Liberals" &
          item %>% 
          str_detect("Conservative speakers")
      )
  ) +
  scale_fill_grey(
    start = .5,
    end = 1,
    guide = guide_legend(
      override.aes = list(
        size = .75
      )
    )
  ) +
  facet_grid(
    type ~ ideol, 
    scales = "free_y", space = "free_y"
  ) +
  scale_y_discrete(
    breaks = t16$item %>%
      levels,
    labels = t16$item %>%
      levels %>%
      str_detect("speakers") %>% 
      not %>% 
      ifelse(
        t16$item %>%
          levels %>% 
          str_sub(, -4) %>%
          str_wrap(width = 20),
        t16$item %>%
          levels
      ) %>% 
      str_replace(" speakers",
                  "\nspeakers")
  ) +
  labs(
    x = "Correction effect",
    fill = "Survey item complexity",
    y = ""
  ) +
  scale_x_continuous(
    breaks = seq(-1.5, .5, .5),
    labels = c("-1.5", "-1", "-.5", "0", ".5")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background  = 
      element_rect(color = "grey95",
                   fill = "grey95"),
    panel.grid = element_blank(),
    axis.text.y = element_text(angle = 0, 
                               lineheight = .75),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(color = "grey95",
                                    fill = "grey95"),
    legend.background = element_rect(color = "transparent",
                                     fill = "transparent"),
    legend.margin = margin(-.3, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic")
  ) 

# accordance and correction size

t18 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_6_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df


t18$item %<>% 
  factor(t18 %>%
           group_by(item) %>% 
           summarize(mu = accordance %>% mean) %>% 
           arrange(mu) %>% 
           use_series(item))


t18$it_join <- t18$item %>% 
  mapvalues(t18$item %>%
              levels %>%
              sort,
            c(
              "carson-spiraling_teen_pregnancy",
              "trump-us_high_taxes",
              "trump-ue_30_perc",
              "trump-undocumented_immigrants",
              "trump-us_high_taxes",
              
              "longoria-whites_imminently_minority",
              "palin-obama_passed_tarp",
              "romney-china_holds_most_debt",
              "bush-tax_cuts",
              "bush-iraq_wmd",
              
              "obama-china_holds_most_debt",
              "obama-wage_gap", 
              "obama-drug_crime",
              "obama-obama_passed_tarp",                        
              "obama-obama_drones",
              
              "obama-spiraling_abortion",                  
              "obama-chicago_homicides",
              "gutierrez-obama_accomodates_undoc",          
              "lee-spiraling_teen_pregnancy",
              "ryan-spiraling_abortion",                 
              
              "clinton-hedge_fund",
              "clinton-solar_jobs",            
              "clinton-gun_violence",
              "kerry-bush_stem_cell",            
              "cruz-obama_accomodates_undoc",          
              
              "cruz-police_killing",      
              "graham-obama_drones",
              "graham-whites_imminently_minority",
              "rubio-obama_defense_spending",
              "sanders-epa_drinking_water",
              
              "sanders-healthcare_expensive",    
              "lapierre-chicago_homicides")) %>% 
  as.character


t18$source <- t18$item %>% 
  str_detect("Kerry|Bush") %>% 
  ifelse("Nyhan & Reifler Original Corrections (2010)",
         "New Corrections") %>% 
  factor(c("Nyhan & Reifler Original Corrections (2010)",
           "New Corrections"))


t18_1 <- 1000 %>% 
  seq %>% 
  map_dfr(
    ~t18 %>%
      select(accordance, item) %>% 
      sample_frac(1, T) %>%
      group_by(item) %>% 
      summarize(mu = accordance %>%
                  mean),
    .id = "iter"
  ) %>% 
  tbl_df %>% 
  group_by(item) %>% 
  summarize(
    lo = mu %>% quantile(.025),
    mid = mu %>% quantile(.5),
    hi = mu %>% quantile(.975)
  ) %>%  
  left_join(
    t18 %>% 
      select(item, source, it_join) %>% 
      group_by(item) %>% 
      slice(1)
  )  %>% 
  mutate(
    mu_lab = mid %>% round
  )


t20 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_7_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df %>% 
  mutate(
    it_join = item %>% 
      mapvalues(c("Gun Violence at all time high (Clinton)(1)",
                  "Most prisoners serving drug sentences (Obama)(1)",
                  "Workers pay more tax than h.fund managers (Clinton)(1)", 
                  "Discrimin. causes gender wage gap (Obama)(1)",
                  "Obama cuts defense spending (Rubio)(1)",
                  "Undocumented immigrants disprop. criminal (Trump)(1)",   
                  "Surge in killings of police officers (Cruz)(1)",
                  "US taxes highest in world (Trump)(1)",
                  "Obama accomodates undoc. immigrants (Cruz)(2)",          
                  "Obama accomodates undoc. immigrants (Gutierrez)(2)",
                  "Spiraling teen pregnancy rate (Carson)(2)",
                  "Spiraling teen pregnancy rate (Lee)(2)",                 
                  "Obama passed TARP (Palin)(2)",
                  "Obama passed TARP (Obama)(2)",
                  "China holds majority of US debt (Romney)(2)",            
                  "China holds majority of US debt (Obama)(2)",
                  "Whites imminently a minority (Graham)(2)",
                  "Whites imminently a minority (Longoria)(2)",             
                  "Spiraling Chicago gun violence (LaPierre)(2)",
                  "Spiraling Chicago gun violence (Obama)(2)",
                  "Spiraling abortion rate (Ryan)(2)",                      
                  "Spiraling abortion rate (Obama)(2)",
                  "Obama restricts drones' use (Graham)(2)",
                  "Obama restricts drones' use (Obama)(2)",                 
                  "Spiraling Chicago gun violence (Obama)(3)",
                  "Spiraling Chicago gun violence (LaPierre)(3)",
                  "Undocumented immigrants disprop. criminal (Trump)(3)",   
                  "Sprialing abortion rate (Ryan)(3)",
                  "More jobs in solar than oil (Clinton)(3)",
                  "US h.care 2X expensive other countries (Sanders)(3)",    
                  "Spiraling Abortion Rate (Ryan)(4)",
                  "EPA Says Fracking Pollutes Drinking Water (Sanders)(4)",
                  "2x employed in Solar Power compared to Oil (Clinton)(4)",
                  "US Corp Taxes highest in world (Trump)(4)",
                  "True UE rate is 30%-40% (Trump)(4)",
                  "WMD found in Iraq (Bush)(4)",                    
                  "Spiraling Abortion Rate (Ryan)(5)",
                  "EPA Says Fracking Pollutes Drinking Water (Sanders)(5)",
                  "US taxes highest in world (Trump)(5)",                   
                  "Spiraling abortion rate (Obama)(5)",
                  "More jobs in solar than oil (Clinton)(5)",
                  "Surge in killings of police officers (Cruz)(5)",         
                  "Gun Violence at all time high (Clinton)(5)",
                  "True UE rate is 30%-40% (Trump)(5)",
                  "Spiraling abortion rate (Obama)(6)",                     
                  "Surge in killings of police officers (Cruz)(6)",
                  "US taxes highest in world (Trump)(6)",
                  "Gun Violence at all time high (Clinton)(6)",             
                  "EPA Says Fracking Pollutes Drinking Water (Sanders)(6)",
                  "More jobs in solar than oil (Clinton)(6)",
                  "Spiraling Abortion Rate (Ryan)(6)",              
                  "True UE rate is 30%-40% (Trump)(6)"),
                c("clinton-gun_violence",
                  "obama-drug_crime",
                  "clinton-hedge_fund", 
                  "obama-wage_gap",
                  "rubio-obama_defense_spending",
                  "trump-undocumented_immigrants",   
                  "cruz-police_killing",
                  "trump-us_high_taxes",
                  "cruz-obama_accomodates_undoc",          
                  "gutierrez-obama_accomodates_undoc",          
                  "carson-spiraling_teen_pregnancy",
                  "lee-spiraling_teen_pregnancy",                 
                  "palin-obama_passed_tarp",
                  "obama-obama_passed_tarp",
                  "romney-china_holds_most_debt",            
                  "obama-china_holds_most_debt",
                  "graham-whites_imminently_minority",
                  "longoria-whites_imminently_minority",
                  "lapierre-chicago_homicides",
                  "obama-chicago_homicides",
                  "ryan-spiraling_abortion",                      
                  "obama-spiraling_abortion",
                  "graham-obama_drones",
                  "obama-obama_drones",                 
                  "lapierre-chicago_homicides",
                  "obama-chicago_homicides",
                  "trump-undocumented_immigrants",   
                  "ryan-spiraling_abortion",                      
                  "clinton-solar_jobs",
                  "sanders-healthcare_expensive",    
                  "ryan-spiraling_abortion",
                  "sanders-epa_drinking_water",
                  "clinton-solar_jobs",
                  "trump-us_high_taxes",
                  "trump-ue_30_perc",
                  "bush-iraq_wmd",                    
                  "ryan-spiraling_abortion",
                  "sanders-epa_drinking_water",
                  "trump-us_high_taxes",                   
                  "obama-spiraling_abortion",
                  "clinton-solar_jobs",
                  "cruz-police_killing",         
                  "clinton-gun_violence",
                  "trump-ue_30_perc",
                  "obama-spiraling_abortion",                     
                  "cruz-police_killing",
                  "trump-us_high_taxes",
                  "clinton-gun_violence",             
                  "sanders-epa_drinking_water",
                  "clinton-solar_jobs",
                  "ryan-spiraling_abortion",              
                  "trump-ue_30_perc")))


library(lme4)

lmm1 <- lmer(
  ans ~ ideol * corr * it_join + (1|ResponseID),
  data = t20
)

lml <- lmm1 %>% 
  emmeans(~ corr|it_join,
          at = list(corr = t20$corr %>% 
                      unique,
                    it_join = t20$it_join %>% 
                      unique)) %>% 
  pairs %>% 
  tidy

lml_lab <- lml %>% 
  select(it_join, estimate, std.error, p.value) %>% 
  mutate(est_lab = estimate %>% 
           round(1) %>% 
           as.character %>% 
           str_c(c("***",
                   "**",
                   "*") %>% 
                   extract(p.value %>% 
                             findInterval(c(-Inf,
                                            .001,
                                            .01,
                                            .05,
                                            Inf)))) %>% 
           str_replace(fixed("-1*"),
                       fixed("-1.0*"))) %>% 
  select(-p.value) %>% 
  left_join(t18_1 %>% 
              select(item, source, it_join),
            "it_join") %>% 
  tbl_df

lt_com <- lml_lab %>%
  mutate(
    lo = estimate %>% 
      subtract(
        std.error %>% 
          multiply_by(1.96)
      ),
    hi = estimate %>% 
      add(
        std.error %>% 
          multiply_by(1.96)
      )
  ) %>% 
  tbl_df %>%
  mutate(fac = "Average Correction Effects") %>% 
  rbind.fill(
    t18_1 %>%
      mutate(fac = "Perceived Accordance") %>% 
      rename(est_lab = mu_lab,
             estimate = mid)) %>% 
  tbl_df %>% 
  mutate(fac = fac %>% 
           factor(c("Perceived Accordance",
                    "Average Correction Effects")))


ggplot() +
  geom_linerangeh(
    aes(xmin = lo, xmax = hi, y = item, color = source),
    data = lt_com,
    # fill = "grey98",
    shape = 21
  ) +
  geom_vline(
    aes(xintercept = 0),
    data = data.frame(
      fac = lt_com$fac %>% 
        levels %>% 
        extract(2)
    ),
    linetype = "dashed"
  ) +
  geom_point(
    aes(estimate, y = item, color = source),
    data = lt_com,
    size = 5,
    fill = "grey98",
    shape = 21
  ) +
  geom_text(
    aes(estimate, item, label = est_lab, color = source),
    data = lt_com,
    size = 2
  ) +
  scale_color_grey(
    start = .02, end = .45
  ) +
  labs(
    x = "",
    y = ""
  ) +
  facet_wrap(~fac, nrow = 1, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.background = 
      element_rect(color = "grey98",
                   fill = "grey98"),
    panel.grid = element_blank(),
    axis.text.y = element_text(
      angle = 0,
      face = lt_com$item %>% 
        levels %>% 
        str_detect(
          "Sec Kerry \\(D\\)|Pres Bush \\(R\\)"
        ) %>% 
        ifelse(
          "bold", "plain"
        ),
      color = lt_com$item %>% 
        levels %>% 
        str_detect(
          "Sec Kerry \\(D\\)|Pres Bush \\(R\\)"
        ) %>% 
        ifelse(
          "grey2", "grey45"
        )
    ),
    plot.title = element_text(face = "bold"),
    legend.margin = margin(-.2, 3, unit = "cm"),
    strip.background  =
      element_rect(color = "grey98",
                   fill = "grey98"))

# Correction effect by time
t22 <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_8_mt.RDS" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

t22 %<>% 
  {
    
    
    j <- "https://github.com/thomasjwood/false_alarm/raw/master/chapter%201/s_9_mt.RDS" %>% 
      url %>%
      gzcon %>%
      readRDS %>%
      tbl_df
    
    j$date[4] <- "_2014-11-05"
    
    j %<>%  mutate(
      date = date %>%
        str_sub(2) %>%
        as.Date
    )
    
    t22  %>% 
      left_join(
        j %>% 
          select(
            item, date
          )
      )
  }

t23 <- t22 %>% 
  group_by(
  study, item
  ) %>% 
  by_slice(
    possibly(
      function(i){
        
        l1 <- lm(ans ~ corr, data = i)
        
        l1 %>%
          emmeans(~corr) %>% 
          pairs(reverse = T, ) %>%
          tidy
        },
      otherwise = NULL
      ),
    .collate = "rows"
    ) %>% 
  tbl_df %>% 
  mutate(
    type = "Overall"
  ) %>% 
  bind_rows(
    t22 %>%
      group_by(
        study, item
        ) %>% 
      by_slice(
        possibly(
          function(i){
            
            
            l1 <- lm(ans ~ corr * ideol, data = i)
            
            
            l1 %>%
              emmeans(~corr | ideol,
                      at = list(ideol = c(1, 4, 7))) %>% 
              pairs(reverse = T) %>%
              tidy
          },
          otherwise = NULL
        ),
        .collate = "rows"
      ) %>% 
      mutate(
        type = "Conditional"
      )
  ) %>% 
  left_join(
    t22 %>% 
      select(study, item, date)
  )


d_r <- data.frame(
  start = c("1972-01-01",
            "2009-01-21",
            "2015-01-01",
            "2016-04-01")
)

d_r %<>% 
  mutate(
    end = c(d_r$start[-1] %>% 
              as.character, 
            today() %>% 
              as.character),
    period_lab = c("Pre Obama Administration",
                   "Obama administration",
                   "2016 cyle primaries",
                   "2016 general election")
  ) %>% 
  modify_at(
    1:2,
    ~as.Date(.)
  ) %>% 
  tbl_df

t23$period <- d_r$period_lab %>% 
  extract(
    findInterval(
      t23$date,
      d_r$start
    )
  ) %>% 
  factor(
    d_r$period_lab
  )

t23$ideol %<>% 
  mapvalues(
    c(1, 4, 7),
    c("Liberals", 
      "Moderates",
      "Conservatives")
  )

t23$type[t23$type %>% equals("Conditional")] <- t23$ideol[t23$ideol %>% is.na %>% not]

t23$type %<>% 
  factor(
    t23$type %>%
      unique
    )

t23$item %<>% 
  factor(
    t23 %>% 
      filter(
        type == "Overall"
      ) %>% 
      arrange(-estimate) %>% 
      use_series(item) %>% 
      unique
  )

library(ggstance)

t23 %<>% 
  mutate(
    xmin = estimate %>%
      subtract(
        std.error %>%
          multiply_by(1.96)
      ),
    xmax = estimate %>%
      add(
        std.error %>%
          multiply_by(1.96)
      ) 
  )

t23$signif <- t23$p.value %>% 
  is_less_than(.05) %>% 
  ifelse(
    "signif", "insignif"
  )


t23$period %<>% 
  mapvalues(
    "2016 cyle primaries",
    "2016 cycle primaries"
  )

t23$item %<>%
  mapvalues(
    c("EPA Says Fracking Pollutes Drinking Water (Sanders)", "US h.care 2X expensive other countries (Sanders)(3)", 
      "Undocumented immigrants disprop. criminal (Trump)(1)", "Undocumented immigrants disprop. criminal (Trump)(3)", 
      "EPA Says Fracking Pollutes Drinking Water (Sanders)(4)", "2x employed in Solar Power compared to Oil (Clinton)(4)", 
      "Workers pay more tax than h.fund managers (Clinton)(1)", "Surge in killings of police officers (Cruz)(1)", 
      "Most prisoners serving drug sentences (Obama)(1)",
      "Obama accomodates undoc. immigrants (Gutierrez)(2)",
      "Obama accomodates undoc. immigrants (Cruz)(2)"
    ),
    c("EPA-Fracking Pollutes Water (Sanders)",
      "US h.care 2X expensive (Sanders)(3)", 
      "Undocumented disprop. criminal (Trump)(1)",
      "Undocumented disprop. criminal (Trump)(3)", 
      "EPA-Fracking Pollutes Water (Sanders)(4)",
      "2x employed Solar compared to Oil (Clinton)(4)", 
      "Workers more taxed than bankers (Clinton)(1)",
      "Surge in killings of police (Cruz)(1)", 
      "Most prisoners serve drug sentences (Obama)(1)",
      "Obama accomodates undocumented (Gutiérrez)(2)",
      "Obama accomodates undocumented (Cruz)(2)"
    )
  )


t24 <- t23 %>%
  group_by(
    item, period,  type
  ) %>% 
  summarize(
    estimate = estimate %>% mean,
    std.error = std.error %>% mean,
    signif = signif[[1]]
  ) %>% 
  mutate(
    xmin = estimate %>% 
      subtract(
        std.error %>% 
          multiply_by(1.96)
      ),
    xmax = estimate %>% 
      add(
        std.error %>% 
          multiply_by(1.96)
      ),
    signif = (
      estimate %>%
        is_less_than(0) &
        xmax %>%
        is_less_than(0) |
        estimate %>%
        is_greater_than(0) &
        xmin %>%
        is_greater_than(0)
    ) %>% 
      ifelse(
        "signif",
        "insignif"
      )
  )

t24 %>% 
  ggplot() +
  geom_vline(
    xintercept = 0,
    linetype = 2
  ) + 
  geom_segment(
    aes(
      x = xmin,
      xend = xmax,
      y = item,
      yend = item
    )
  ) +
  geom_point(
    aes(x = estimate,
        y = item,
        shape = signif),
    fill = "grey95"
  ) +
  facet_grid(
    vars(period), 
    vars(type),
    scales = "free_y",
    space = "free_y",
    labeller = label_wrap_gen(width = 8)
  ) +
  scale_shape_manual(
    values = c(16, 21) %>% 
      rev
  ) +
  scale_y_discrete(
    breaks = t23$item %>% 
      levels,
    labels = t23$item %>% 
      levels %>% 
      str_replace_all("\\(\\d\\)", "")
  ) +
  labs(
    x = "Correction effect (difference on 5pt scale)",
    y = ""
    ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(
      hjust = 1),
    plot.background = element_rect(fill = "white",
                                   color = "white"),
    plot.title = element_text(face = "bold",
                              hjust = 0),
    panel.grid = element_blank(),
    strip.text = element_text(size = 7),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = "grey95",
                                    color = "grey95"),
    panel.background = element_rect(fill = "grey95",
                                    color = "grey95"),
    legend.margin  = margin(-.2, unit = "cm"),
    legend.position = "none"
  )



