library(plyr)
library(tidyverse)
library(purrrlyr)
library(stringr)
library(magrittr)

t1 <- "C:/Dropbox/false alarm/replication data and code/chapter 2/study 1 data file.RDS" %>% 
  readRDS

t2 <- t1 %>%
  group_by(issue) %>% 
  by_slice(
    function(i)
      lm(ans_num ~ corr2,
         data = i) %>%
      tidy,   
    .collate = "rows"
    ) %>% 
  filter(term %>% 
           str_detect("corr2")) %>% 
  select(-term, -statistic) %>% 
  mutate(yax = "Overall",
         type = "Overall effects") %>% 
  rbind.fill(
    t1 %>%
      group_by(issue) %>% 
      by_slice(
        function(i)
          lm(ans_num ~ corr2*ideo, 
                 data = i) %>% 
              emmeans(~corr2 | ideo) %>% 
              pairs(rev = T) %>% 
              tidy,
        .collate = "rows") %>% 
      select(-level1, -level2, -statistic, -df) %>% 
      tbl_df %>% 
      rename("yax" = ideo) %>% 
      mutate(type = "Effects by ideology")
    ) %>% 
  # now, differences by ideology
  rbind.fill(
    t1 %>%
      group_by(issue) %>% 
      by_slice(
        function(i)
          lm(ans_num ~ corr2*ideo,
             data = i) %>%
          lsmeans(~corr2 * ideo) %>% 
          contrast(method = "revpairwise") %>% 
          contrast(method = "revpairwise", adjust = "fdr")
      ) %>%
      use_series(.out) %>% 
      map_df(
        function(i)
          i %>%
          summary %>% 
          as.data.frame() %>% 
          mutate(contrast = contrast %>% 
                   str_trim)
      ) %>%
      tbl_df %>% 
      filter(contrast %>%
               is_in(c("correction,moderate - no correction,moderate - correction,conservative - no correction,conservative",
                       "correction,liberal - no correction,liberal - correction,moderate - no correction,moderate"))
      ) %>% 
      select(-df, -t.ratio) %>% 
      rename("yax" = contrast,
             "std.error" = SE) %>% 
      mutate(type = "Difference in correction effects",
             yax = c(
               "moderate - conservative",
               "moderate - liberal"
             ) %>% 
               rep(times = 6),
             issue = ct_long$issue %>% 
               factor %>% 
               levels %>% 
               rep(each = 2)
      )
  ) %>% 
  tbl_df

# the 95% confidence interval

# Getting the contrast pointing in a consistent fashion
t2$estimate[lt4$yax == "moderate - liberal"] %<>%
  multiply_by(-1)

t2$lo <- t2$estimate %>% 
  subtract(t2$std.error %>% 
             multiply_by(1.96))

t2$hi <- t2$estimate %>% 
  add(t2$std.error %>%
        multiply_by(1.96))


t2$yax %<>%
  factor(t2$yax %>%
           unique)


# the labels for plot

t2$lab <- 
  ifelse(
    t2$estimate %>% 
      sign %>% 
      equals(-1),
    t2$estimate %>%
      round(2) %>%
      as.character %>%
      str_pad(width = 5, 
              side = "right", 
              pad = "0") %>% 
      str_replace_all(fixed("0."),
                      "."),
    t2$estimate %>%
      round(2) %>%
      as.character %>%
      str_pad(width = 4, 
              side = "right", 
              pad = "0") %>% 
      str_replace_all(fixed("0."),
                      ".")
  )

# Cleaning up a label
t2$lab[t2$lab == "00000"] <- 0

# appending significance stars
t2$lab2 <- t2$lab %>% 
  str_c(
    c("***",
      "**",
      "*",
      "") %>% 
      extract(t2$p.value %>%
                findInterval(
                  c(-Inf, .001, .01, .05, Inf)
                )
      )
  )

# mapping significance to shapes
t2$sig <- t2$p.value %>% 
  is_less_than(.05) %>% 
  ifelse("Significant p < .05",
         "Insignificant p >= .05")

# Ordering the issues
t2$issue %<>% 
  factor(
    t2 %>% 
      filter(
        type %>% 
          equals(t2$type %>% 
                   extract2(1)
          )
      ) %>% 
      arrange(estimate) %>% 
      use_series(issue)
  )

# ordering the row facets
t2$type %<>%  
  mapvalues(t2$type %>% 
              unique %>%  
              extract(3),
            c("Difference in correction effects by ideology")) %>% 
  factor(
    t2$type %>% 
      unique %>% 
      extract(-3) %>% 
      c("Difference in correction effects by ideology")
  )

t2$yax %<>% 
  mapvalues(
    3:1,
   c("Liberal",
     "Moderate",
     "Conservative") 
  ) %>% 
  factor(
    t2$yax %>% 
      levels %>% 
      mapvalues(
        3:1,
        c("Liberal",
          "Moderate",
          "Conservative")
        )
    )


t2 %>% 
  tbl_df %>% 
  ggplot() +
  geom_hline(yintercept = 0,
             linetype = 2) +
  geom_label(aes(yax, estimate, label = lab2), 
             label.padding = unit(.125, units = "lines"), 
             nudge_x = .25,
             # vjust = -1,
             family = "Roboto",
             label.size = NA,
             fill = "grey96",
             fontface = "italic",
             size = 2.25) +
  geom_linerange(
    aes(x = yax, ymin = lo, ymax = hi)) +
  geom_point(aes(yax, estimate, 
                 shape = sig),
             color = "grey5",
             fill = "grey96") +
  scale_shape_manual(values = c(16, 21) %>% 
                       rev,
                     guide = guide_legend(reverse = T,
                                          override.aes = list(size = 3))) +
  scale_x_discrete(
    breaks = t2$yax %>% 
      levels,
    labels = t2$yax %>% 
      levels %>% 
      str_to_title %>% 
      str_replace_all(" - ", " -\n")
  ) +
  facet_grid(issue ~ type, 
             space = "free_x",
             scales = "free_x",
             labeller = label_wrap_gen(width = 17)) +
  labs(y = "Correction effect(difference on 5pt agreement scale).\nNegative values indicate factual correction.",
       x = "",
       shape = "",
       caption = "***p < .001, **p < .01, *p < .05"
  ) +
  theme_minimal(base_family = "Roboto") +
  theme(legend.text = element_text(size = 11),
        plot.caption = element_text(face = "italic"),
        plot.title = element_text(face = "bold",
                                  hjust = 0), 
        panel.background = element_rect(color = "grey96", fill = "grey96"),
        strip.text.y = element_text(angle = 0),
        strip.background = element_rect(color = "grey96", fill = "grey96"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.margin = margin(t = -.35, unit = "cm")) 


# Attrition estimates

t6 <- "C:/Dropbox/false alarm/replication data and code/chapter 2/study 2 data file.RDS" %>% 
  readRDS

t7 <- t6 %>% 
  select(
    caseid, issue, treat, ans_w1, ans_w2, 
    demo_ideol_num, delay_week 
  ) %>% 
  gather(
    ans, ans_week, starts_with("ans_")
  ) %>%
  arrange(caseid %>% desc) %>% 
  mutate(
    treat2 = treat %>% 
      mapvalues(
        t6$treat %>% 
          unique,
        c("correction",
          "no correction",
          "items",
          "correction")
      ) %>% 
      factor(
        c("items",
          "no correction",
          "correction")
      ),
    delay_week2 = ans %>% 
      str_detect("_w1") %>% 
      ifelse(
        "Initial",
        delay_week %>% as.character
      ) %>% 
      factor(
        c("Initial",
          "3-7 days",
          "> 7 days")
      ),
    ans_num = ans_week %>% 
      str_to_lower %>% 
      mapvalues(
        c("strongly agree",
          "somewhat agree",
          "neither agree nor disagree",
          "somewhat disagree",
          "strongly disagree"
        ),
        1:5) %>% 
      as.numeric
  ) %>% 
  filter(
    delay_week2 %>% is.na %>% not
  )

nm <- t7 %>% 
  group_by(
    issue, delay_week2
  ) %>% 
  by_slice(
    failwith(NULL,
             function(i)
               lm(
                 # ans_num ~ ideol_cat * treat2,
                 ans_num ~ demo_ideol_num * treat2,
                 data = i)
    )
  )


t8 <- pmap_dfr(
  list(
    nm$issue,
    nm$delay_week2,
    nm$.out
  ),
  function(g, h, i)
    # i <- nm$.out[[1]]  
    
    i %>% 
    emmeans(~ treat2 | demo_ideol_num, at = list(demo_ideol_num = c(1, 4, 7))) %>%
    pairs %>% 
    tidy %>% 
    mutate(
      issue = g, delay_week2 = h
    )
) %>% 
  bind_rows(
    pmap_dfr(
      list(
        nm$issue,
        nm$delay_week2,
        nm$.out
      ),
      function(g, h, i)
        # i <- nm$.out[[1]]  
        
        i %>% 
        emmeans(~ treat2) %>%
        pairs %>% 
        tidy %>% 
        mutate(
          issue = g, delay_week2 = h, demo_ideol_num = -2
        )
    )
  ) %>% 
  mutate(
    ideol_catdelay_week2 = delay_week2 %>%
      factor(
        c("Initial",
          "3-7 days",
          "> 7 days")
      )
  )


t8 %>%
  filter(
    level1 == "no correction" &
      level2 == "correction"
  ) %>% 
  group_by(
    demo_ideol_num,
    delay_week2
  ) %>% 
  summarize(
    estimate = estimate %>% mean,
    std.error  = std.error %>% mean
  ) %>% 
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
  )  %>% 
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_pointrange(
    aes(x = demo_ideol_num, y = estimate, 
        ymin = lo, ymax = hi, group = delay_week2,
        fill = delay_week2),
    position = position_dodge(width = 1.5),
    shape = 21
  )  +
  scale_x_continuous(
    breaks = c(-2, 1, 4, 7),
    labels = c("Overall",
               "Liberals",
               "Moderates",
               "Conservatives")
  ) +
  theme(
    legend.position = "bottom"
  )


# # this produces figure 1
# t2 %>% 
#   tbl_df %>% 
#   ggplot() +
#   geom_vline(xintercept = 0,
#              linetype = 2) +
#   geom_label(aes(estimate, yax, label = lab2), 
#              label.padding = unit(.125, units = "lines"), 
#              nudge_y = .25,
#              # vjust = -1,
#              label.size = NA,
#              fill = "grey96",
#              fontface = "italic",
#              size = 2.25) +
#   geom_linerangeh(
#     aes(y = yax, xmin = lo, xmax = hi)) +
#   geom_point(aes(estimate, yax,
#                  shape = sig),
#              color = "grey5",
#              fill = "grey96") +
#   scale_shape_manual(values = c(16, 21) %>% 
#                        rev,
#                      guide = guide_legend(reverse = T,
#                                           override.aes = list(size = 3))) +
#   scale_y_discrete(
#     breaks = t2$yax %>% 
#       levels,
#     labels = t2$yax %>% 
#       levels %>% 
#       str_to_title
#   ) +
#   facet_grid(type ~ issue, 
#              space = "free_y",
#              scales = "free_y",
#              labeller = label_wrap_gen(width = 17)) +
#   labs(x = "Correction effect(difference on 5pt agreement scale).\nNegative values indicate factual correction.",
#        y = "",
#        shape = "",
#        caption = "***p < .001, **p < .01, *p < .05"
#   ) +
#   theme_minimal() +
#   theme(legend.text = element_text(size = 11),
#         plot.caption = element_text(face = "italic"),
#         plot.title = element_text(face = "bold",
#                                   hjust = 0), 
#         panel.background = element_rect(color = "grey96", fill = "grey96"),
#         strip.text.y = element_text(angle = 0),
#         strip.background = element_rect(color = "grey96", fill = "grey96"),
#         panel.grid = element_blank(),
#         legend.position = "bottom",
#         legend.margin = margin(t = -.35, unit = "cm")) 
# # 
# 
# # reverse facets for CUP
# 
# library(showtext)
# font_add_google("Roboto")



# 
# showtext.auto()
# ggsave(
#   plot = p2,
#   "C:/Dropbox/false alarm/R and R 2 (April 2019)/fig 3.1 remade as vertical (to distinguish jeps).pdf",
#   width = 6.5,
#   height = 7
#   )

# 



t3 <- "C:/Dropbox/CHESSINFORMANT/study 3 (attrition)/data/CHESSINFORMANT s3 combined table.RDS" %>% 
  readRDS %>% 
  mutate(
    
  )

library(lubridate)

t3$date_w1 %>% 
  ymd_hms(tz = "America/New_York") %>% 
  str_sub(12) %>% 
  hms


t3$delay <- t3$date_w2 %>% 
  subtract(d1$date_w1) %>% 
  as.numeric 


d1$demo_ideol_num <- d1$demo_ideol %>%
  str_sub(, 1) %>% 
  as.numeric

d1$treat2 <- d1$treat %>% 
  mapvalues(
    c("correction",
      "no correction",
      "items",
      "correction_fna award"),
    c("correction",
      "no correction",
      "items",
      "correction")
  ) %>% 
  factor(
    c("items",
      "no correction",
      "correction")
  )


d1$delay_week <- d1$delay %>% 
  cut(
    c(0, 8, Inf),
    c("3-7 days",
      "> 7 days")
  )  %>% 
  factor(
    c("Initial",
      "3-7 days",
      "> 7 days")
  )


t4 <- d1 %>% 
  select(
    workerid, issue, treat, ans_w1, ans_w2, 
    demo_ideol_num, delay_week 
  ) %>% 
  gather(
    ans, ans_week, starts_with("ans_")
  ) %>%
  arrange(workerid %>% desc) %>% 
  mutate(
    treat2 = treat %>% 
      mapvalues(
        d1$treat %>% 
          unique,
        c("correction",
          "no correction",
          "items",
          "correction")
      ) %>% 
      factor(
        c("items",
          "no correction",
          "correction")
      ),
    delay_week2 = ans %>% 
      str_detect("_w1") %>% 
      ifelse(
        "Initial",
        delay_week %>% as.character
      ) %>% 
      factor(
        c("Initial",
          "3-7 days",
          "> 7 days")
      ),
    ans_num = ans_week %>% 
      str_to_lower %>% 
      mapvalues(
        c("strongly agree",
          "somewhat agree",
          "neither agree nor disagree",
          "somewhat disagree",
          "strongly disagree"
        ),
        5:1) %>% 
      as.numeric
  ) %>% 
  filter(
    delay_week2 %>% is.na %>% not
  )

nm <- t4 %>% 
  group_by(
    issue, delay_week2
  ) %>% 
  by_slice(
    failwith(NULL,
             function(i)
               lm(
                 # ans_num ~ ideol_cat * treat2,
                 ans_num ~ demo_ideol_num * treat2,
                 data = i)
    )
  )


