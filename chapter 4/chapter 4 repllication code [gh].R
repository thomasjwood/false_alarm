library(plyr)
library(tidyverse)
library(magrittr)

t1 <-  "C:/Dropbox/false alarm/replication data and code/chapter 4/study 1 table.rds" %>% 
  readRDS  


t1$study  %<>% 
  factor 

t1 %<>% 
  arrange(
    estimate
  )

t1$item %<>%
  factor(t1$item %>%
           as.character)

ldf <- data.frame(
  study = t1$study %>% 
    levels %>% 
    extract(4),
  x = 1,
  y = c(.05, -.05),
  labs = c("Accuracy decrease (Backfire)",
           "Accuracy increase)")
)

t1 %>% 
  ggplot() +
  geom_hline(
    linetype = 2,
    color = "red",
    yintercept = 0
  ) +
  geom_point(
    aes(item, estimate, shape = p.value),
    size = 2
  ) +
  geom_text(
    aes(x, y, label = labs),
    hjust = 0,
    size = 3.25,
    color = "gray50",
    fontface = "italic",
    family = "Roboto",
    data = ldf
  ) +
  facet_grid(
    . ~ study,
    scales = "free_x",
    space = "free_x",
    labeller = label_wrap_gen(width = 10)
  ) +
  scale_x_discrete(
    breaks = t1$item %>% 
      levels,
    labels = t1$item %>% 
      levels %>% 
      str_replace_all("\\(\\d\\)", "")
  ) +
  scale_shape_manual(
    values = c(21, 16)
  ) +
  labs(
    x = "",
    y = "Correction effect (difference on 5pt scale)"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(panel.background  = 
          element_rect(color = "grey95",
                       fill = "grey95"),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1,
                                   vjust = 1,
                                   margin = margin(-.05, b = -.5, unit = "cm")),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(color = "grey95",
                                        fill = "grey95"),
        legend.background = element_rect(color = "white",
                                         fill = "white"),
        legend.margin = margin(-.25, 0, 0, 0, "cm"),
        panel.grid.minor = element_blank(),
        # panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0),
        plot.caption = element_text(face = "italic"),
        legend.position = "none") 


# gallup climate--taken from:
# http://news.gallup.com/file/poll/231575/180327ClimateChange.pdf

t1 <- "2019 Mar 1-10 59 4 9 16 11 1
      2018 Mar 1-8 60 4 5 16 12 3
      2017 Mar 1-5 62 4 7 16 9 3
      2016 Mar 2-6 59 5 9 17 10 1
      2015 Mar 5-8 55 3 8 17 16 1
      2014 Mar 6-9 54 3 8 16 18 2
      2013 Mar 7-10 54 3 9 15 15 3
      2012 Mar 8-11 52 4 10 15 15 2
      2011 Mar 3-6 49 4 9 17 18 3
      2010 Mar 4-7 50 3 10 16 19 2
      2009 Mar 5-8 53 5 10 15 16 2
      2008 Mar 6-9 61 4 10 13 11 1
      2007 Mar 23-25 60 4 7 15 11 3
      2007 Mar 11-14 59 3 8 19 8 3
      2006 Mar 13-16 58 5 10 15 8 3
      2005 Mar 7-10 54 5 10 19 9 3
      2004 Mar 8-11 51 5 12 18 11 3
      2003 Mar 3-5 51 6 12 17 10 4
      2002 Mar 4-7 53 5 13 17 9 3
      2001 Mar 5-7 54 4 13 18 7 4
      1997 Nov 6-9 48 3 14 19 9 7" %>% 
  str_split("\n") %>% 
  
  str_split("\n") %>% 
  unlist

s1 <- t1 %>% 
  str_locate_all(" ")

d1 <- data.frame(
  dates = t1 %>% 
    str_sub(
      ,
      s1 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_replace_all("\\d{2}\\-|\\d{1}\\-", "") %>% 
    ymd %>% 
    as.Date,
  cc_already = t1 %>% 
    str_sub(
      s1 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            add(1)
        ),
      s1 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric,
  cc_fewyears = t1 %>% 
    str_sub(
      s1 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            add(1)
        ),
      s1 %>% 
        map(
          ~extract(., 5) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric,
  cc_notlife = t1 %>% 
    str_sub(
      s1 %>% 
        map(
          ~extract(., 5) %>% 
            extract(1) %>% 
            add(1)
        ),
      s1 %>% 
        map(
          ~extract(., 6) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric,
  cc_never = t1 %>% 
    str_sub(
      s1 %>% 
        map(
          ~extract(., 6) %>% 
            extract(1) %>% 
            add(1)
        ),
      s1 %>% 
        map(
          ~extract(., 7) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric
) %>% 
  tbl_df

t4 <- "2019 Mar 1-10 65 6 26 2
2018 Mar 1-8 66 6 24 4
2017 Mar 1-5 71 5 22 2
2016 Mar 2-6 65 7 25 3
2015 Mar 5-8 62 8 27 3
2014 Mar 6-9 60 8 29 3
2013 Mar 7-10 62 6 28 4
2012 Mar 8-11 58 7 32 3
2011 Mar 3-6 55 8 33 4
2010 Mar 4-7 52 10 36 2
2008 Mar 6-9 65 7 26 3
2006 Mar 13-16 65 3 29 3
2001 Mar 5-7 61 4 30 5
1997 Nov 21-23 48 7 39 6" %>% 
  str_split("\n") %>% 
  unlist

s4 <- t4 %>% 
  str_locate_all(" ")


t5 <- data.frame(
  dates = t4 %>% 
    str_sub(
      ,
      s4 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_replace_all("\\d{2}\\-|\\d{1}\\-", "") %>% 
    ymd %>% 
    as.Date,
  cc_isoccur = t4 %>% 
    str_sub(
      s4 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            add(1)
        ),
      s4 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric,
  cc_notoccur = t4 %>% 
    str_sub(
      s4 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            add(1)
        ),
      s4 %>% 
        map(
          ~extract(., 5) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric
) %>% 
  tbl_df

t6 <- "2019 Mar 1-10 66 31 3
2018 Mar 1-8 64 33 3
2017 Mar 1-5 68 29 3
2016 Mar 2-6 65 31 4
2015 Mar 5-8 55 41 4
2014 Mar 6-9 57 40 3
2013 Mar 7-10 57 39 4
2012 Mar 8-11 53 41 6
2011 Mar 3-6 52 43 5
2010 Mar 4-7 50 46 5
2008 Mar 6-9 58 38 5
2007 Mar 11-14 61 35 5
2006 Mar 13-16 58 36 6
2003 Mar 3-5 61 33 6
2001 Mar 5-7 61 33 6" %>% 
  str_split("\n") %>% 
  unlist


s6 <- t6 %>% 
  str_locate_all(" ")

d7 <- data.frame(
  dates = t6 %>% 
    str_sub(
      ,
      s6 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_replace_all("\\d{2}\\-|\\d{1}\\-", "") %>% 
    ymd %>% 
    as.Date,
  cc_human = t6 %>% 
    str_sub(
      s6 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            add(1)
        ),
      s6 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric,
  cc_natural = t6 %>% 
    str_sub(
      s6 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            add(1)
        ),
      s6 %>% 
        map(
          ~extract(., 5) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric
) %>% 
  tbl_df

t8 <- "2019 Mar 1-10 44 21 15 20 0
  2018 Mar 1-8 43 20 18 18 0
  2017 Mar 1-5 45 21 18 16 0
  2016 Mar 2-6 37 27 17 19 0
  2015 Mar 5-8 32 23 21 24 0
  2014 Mar 6-9 34 22 19 24 0
  2013 Mar 7-10  33 25 20 23 0
  2012 Mar 8-11  30 25 22 23 1
  2011 Mar 3-6  25 26 20 28 1
  2010 Mar 4-7  28 24 19 29 0
  2009 Mar 5-8   34 26 20 20 1
  2008 Mar 6-9  37 29 16 17 1
  2007 Mar 11-14  41 24 18 16 1
  2006 Mar 13-16  36 26 21 15 1
  2004 Mar 8-11  26 25 28 19 2
  2003 Mar 3-5  28 30 23 17 2
  2002 Mar 4-7  29 29 23 17 2
  2001 Mar 5-7  33 30 22 13 2
  2000 Apr 3-9  40 32 15 12 1
  1999 Apr 13-14  34 34 18 12 2
  1999 Mar 12-14  28 31 23 16 2
  1997 Oct 27-28  24 26 29 17 4
  1991 Apr 11-14  35 27 22 12 5
  1990 Apr 5-8  30 27 20 16 6
  1989 May 4-7  35 28 18 12 7" %>% 
  str_split("\n") %>% 
  unlist %>% 
  str_trim

t8 %<>% 
  str_replace_all("\\s{2}", " ")

s8 <- t8 %>% 
  str_locate_all(" ")

T9 <- data.frame(
  dates = t8 %>% 
    str_sub(
      ,
      s4 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_replace_all("\\d{2}\\-|\\d{1}\\-", "") %>% 
    ymd %>% 
    as.Date,
  worry_gd = t4 %>% 
    str_sub(
      s4 %>% 
        map(
          ~extract(., 3) %>% 
            extract(1) %>% 
            add(1)
        ),
      s4 %>% 
        map(
          ~extract(., 4) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric,
  worry_not = t4 %>% 
    str_sub(
      s4 %>% 
        map(
          ~extract(., 6) %>% 
            extract(1) %>% 
            add(1)
        ),
      s4 %>% 
        map(
          ~extract(., 7) %>% 
            extract(1) %>% 
            subtract(1)
        )
    ) %>% 
    str_trim %>% 
    as.numeric
) %>% 
  tbl_df

d_c <- d1 %>% 
  select(dates, cc_already, cc_notlife) %>% 
  gather(
    ans, val, -dates
  ) %>% 
  mutate(
    question = "When the effects of global warming will begin to happen"
  ) %>% 
  bind_rows(
    d2 %>% 
      gather(
        ans, val, -dates
      ) %>% 
      mutate(
        question = "Most scientists believe that global warming IS or IS NOT occurring"
      ),
    d3 %>% 
      gather(
        ans, val, -dates
      ) %>% 
      mutate(
        question = "Increases in the Earth's temperature over last century due to human pollution or natural changes"
      ),
    d4 %>% 
      gather(
        ans, val, -dates
      ) %>% 
      mutate(
        question = "Do you personally worry a GREAT DEAL or NOT AT ALL about Climate Change",
        val = val %>% 
          is.na %>%
          ifelse(
            33, val
          )
      )
  )

d_c %<>% 
  left_join(
    d_c %>% 
      group_by(question, ans) %>% 
      summarize(
        mu = val %>% 
          mean) %>% 
      arrange(question, desc(mu))  %>% 
      mutate(g = 1:n() %>% 
               factor) %>% 
      ungroup %>% 
      select(ans, g)
  )

d_c$g[d_c$question %>% str_detect("worry")] %<>% 
  mapvalues(
    as.character(2:1), as.character(1:2)
  )


dcl <- d_c %>% 
  group_by(question, g) %>% 
  slice(c(1, n()))

dcl2 <- d_c %>% 
  ungroup %>% 
  filter(
    question %>% str_detect("Increases in the ")
  ) %>% 
  slice(c(5, 19)) %>% 
  mutate(
    val = val %>% 
      add(c(16, -9.5)),
    lab = c("Human Causes",
            "Natural Causes")
  ) %>% 
  bind_rows(
    d_c %>% 
      ungroup %>% 
      filter(
        question %>% str_detect("Most scientists ")
      ) %>%  
      slice(c(5, 18)) %>% 
      mutate(
        val = val %>% 
          add(c(-5, 5)),
        lab = c("IS occurring",
                "IS NOT occurring")
      ),
    d_c %>% 
      ungroup %>% 
      filter(
        question %>% str_detect("begin to happen")
      ) %>%  
      slice(c(4, 24)) %>% 
      mutate(
        val = val %>% 
          add(c(-9, 7)),
        lab = c("Already begun",
                "Not in my lifetime")
      ),
    d_c %>% 
      ungroup %>% 
      filter(
        question %>% str_detect("worry")
      ) %>%  
      slice(c(4, 29)) %>% 
      mutate(
        val = val %>% 
          add(c(+15, -7.5)),
        lab = c("Worry a GREAT DEAL",
                "Worry NOT AT ALL")
      )
  )

library(showtext)
font_add_google("Roboto")

d_c$question %<>% 
  fct_reorder(
    d_c$val, .desc = T
  )

dcl$question %<>% 
  factor(
    d_c$question %>% 
      levels
  )

dcl2$question %<>%
  factor(
    d_c$question %>% 
      levels
  )


d_c$g[d_c$question %>% str_detect("GREAT DEAL")] %<>% 
  mapvalues(
    2:1, 1:2
  )

p1 <- d_c %>% 
  ggplot() +
  geom_line(
    aes(dates, val, linetype = g)
  ) +
  geom_point(
    aes(dates, val),
    shape = 21,
    size = 5,
    fill = "grey95",
    data = dcl
  ) +
  geom_text(
    aes(dates, val, label = val),
    size = 2.5,
    data = dcl,
    family = "Roboto"
  ) +
  geom_text(
    aes(dates, val, label = lab),
    size = 2.5,
    data = dcl2,
    fontface = "italic",
    family = "Roboto"
  ) +
  facet_wrap(
    ~ question, ncol = 2,
    labeller = label_wrap_gen(width = 70)
  ) +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal(
    base_family = "Roboto"
  ) +
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
    legend.background = element_rect(color = "transparent",
                                     fill = "transparent"),
    legend.margin = margin(-.3, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic")
  ) 

