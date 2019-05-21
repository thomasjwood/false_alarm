# library(plyr)
# library(tidyverse)
# library(magrittr)

# Replication data -- 
# FALSE ALARM--the truth about political mistruth in the Trump era

# Code is copyright Ethan Porter & Thomas J Wood
# Code written:
# Sun May 19 09:21:28 2019 ------------------------------


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
  lubridate, RcppRoll
)

t1 <- "https://github.com/thomasjwood/false_alarm/raw/master/s5/s_1_t.rds" %>% 
  url %>%
  gzcon %>%
  readRDS %>%
  tbl_df

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
    factor %>% 
    levels %>% 
    extract(4),
  x = 1,
  y = c(.05, -.05),
  labs = c("Accuracy decrease (Backfire)",
           "Accuracy increase)")
)

t1$item %<>% 
  fct_reorder(
    t1$estimate
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


t2 <- "https://github.com/thomasjwood/false_alarm/raw/master/s5/s_2_t.rds" %>% 
  url %>%
  gzcon %>%
  readRDS


t2[[1]] %>% 
  ggplot() +
  geom_line(
    aes(dates, val, linetype = g)
  ) +
  geom_point(
    aes(dates, val),
    shape = 21,
    size = 5,
    fill = "grey95",
    data = t2[[2]]
  ) +
  geom_text(
    aes(dates, val, label = val),
    size = 2.5,
    data = t2[[2]],
    family = "Roboto"
  ) +
  geom_text(
    aes(dates, val, label = lab),
    size = 2.5,
    data = t2[[3]],
    fontface = "italic",
  ) +
  facet_wrap(
    ~ question, ncol = 2,
    labeller = label_wrap_gen(width = 70)
  ) +
  labs(
    x = "",
    y = ""
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
    legend.background = element_rect(color = "transparent",
                                     fill = "transparent"),
    legend.margin = margin(-.3, 0, 0, 0, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.caption = element_text(face = "italic")
  ) 

# evolution

t3 <- "https://github.com/thomasjwood/false_alarm/raw/master/s5/s_3_t.rds" %>% 
  url %>%
  gzcon %>%
  readRDS


ggplot() +
  geom_point(
    aes(date, perc, shape = ans),
    data = t3[[1]],
    size = 2
  ) +
  geom_step(
    aes(date, smu, linetype = ans),
    data = t3[[1]] %>% 
      na.omit
  ) +
  geom_point(
    aes(date, smu),
    size = 8,
    shape = 21,
    fill = "white",
    data = t3[[1]] %>% 
      na.omit %>% 
      group_by(ans) %>%
      slice(c(1, n()))
  ) +
  geom_text(
    aes(date, smu,
        label = smu %>% 
          round),
    size = 3,
    shape = 21,
    fill = "white",    
    family = "Roboto",
    data = t3[[1]] %>% 
      na.omit %>% 
      group_by(ans) %>%
      slice(c(1, n()))
  ) +
  geom_text(
    aes(
      date, y, label = labs %>% 
        str_wrap(width = 20)
      
    ),
    fontface = "italic",
    data = t3[[2]],
    lineheight = .7,
    family = "Roboto"
  ) +
  labs(x = "",
       y = "") +
  scale_shape_manual(
    values = c(21, 16, 17)
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold",
                              hjust = 0),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = .5),
    strip.background = element_rect(fill = "grey98",
                                    color = "grey98"),
    panel.background = element_rect(fill = "grey98",
                                    color = "grey98"),
    plot.caption = element_text(face = "italic")) 



t4 <- "https://github.com/thomasjwood/false_alarm/raw/master/s5/s_4_t.rds" %>% 
  url %>%
  gzcon %>%
  readRDS

ggplot() +
  geom_point(
    aes(date, est, shape = ans),
    size = 5,
    alpha = .75,
    data = t4[[1]] %>% 
      filter(
        ans != "dk"
      )
  ) +
  geom_step(
    aes(date, smu, linetype = ans),
    data = t4[[1]] %>% 
      filter(
        ans != "dk"
      )
  ) +
  geom_point(
    aes(date, smu),
    size = 8,
    shape = 21,
    fill = "white",
    data = t4[[1]] %>% 
      filter(
        ans != "dk"
      ) %>% 
      na.omit %>% 
      group_by(ans) %>%
      slice(c(1, n()))
  ) +
  geom_text(
    aes(date, smu, 
        label = smu %>% 
          round),
    size = 3,
    shape = 21,
    fill = "white",    
    data = t4[[1]] %>% 
      filter(
        ans != "dk"
      ) %>%  
      na.omit %>%
      group_by(ans) %>%
      slice(c(1, n()))
  ) +
  geom_text(
    aes(
      date, y, label = labs %>% 
        str_wrap(width = 20) 
    ),
    data = t4[[2]] %>% 
      filter(ans != "dk"),
    lineheight = .7,
    fontface = "italic"
  ) +
  scale_x_date(
    breaks = seq("1960-01-01" %>% 
                   as.Date,
                 "2010-01-01" %>% 
                   as.Date,
                 by = "10 years"),
    labels = seq("1960-01-01" %>% 
                   as.Date,
                 "2010-01-01" %>% 
                   as.Date,
                 by = "10 years") %>% 
      year
  ) +
  scale_shape_manual(values = c(1, 16)) +
  scale_linetype_manual(
    values = c(
      "dotted",
      "solid") 
  ) +
  labs(x = "",
       y = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold",
                              hjust = 0),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = .5),
    strip.background = element_rect(fill = "grey98",
                                    color = "grey98"),
    panel.background = element_rect(fill = "grey98",
                                    color = "grey98"),
    plot.caption = element_text(face = "italic")) 


