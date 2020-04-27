if(grepl("level_balancing", Sys.getenv("REPORTS"))){
library(stringr)
source("src/get_all_versions.R")
sink("level_balancing.Rmd")
data_folder <- paste0("data/",Sys.getenv("PROJECT"))

cat("--- 
title: \"", str_to_upper(Sys.getenv("PROJECT"))," ~ Levels. Data from ",
Sys.getenv("START_DATE")," to ",Sys.getenv("END_DATE"),"\"
output: 
  flexdashboard::flex_dashboard:
    orientation: columns
    vertical_layout: fill
    favicon: image/generaLogo.png
    logo: image/",Sys.getenv("PROJECT"),"/appLogo.jpg
    css: css/",Sys.getenv("PROJECT"),"/style.css
    theme: simplex
---
  
```{r setup, include=FALSE}
library(flexdashboard)
library(readr)
library(dplyr)
library(shiny)
library(rhandsontable)
library(plotly)
library(tibble)
library(tidyr)
library(purrr)

data_level <- read_csv(\"", data_folder, "/table_levels.csv\", col_types = 
  cols(level = col_integer()))


data_churn <- read_csv(\"", data_folder, "/table_churn.csv\", col_types = 
  cols(level = col_integer()))

data_pu <- read_csv(\"", data_folder, "/table_pu.csv\", col_types = 
  cols(level = col_integer()))

data_retry <- read_csv(\"", data_folder, "/table_retries.csv\", col_types = 
  cols(level = col_integer()))

source(\"src/get_parameters_version.R\")
source(\"src/get_mechanic.R\")
source(\"src/render_table.R\")

table_level <- data_level %>% 
  left_join(data_churn) %>% 
  left_join(table_versions) %>% 
  left_join(data_retry) %>% 
  left_join(table_mechanics) %>% 
  mutate(level = as.integer(level))

glimpse(table_level)


table_level <- table_level  %>% 
  mutate(
    abs_level = case_when(
      context == 'summer_map' ~ level + 285L,
      (context == 'endless_map' & level < 31 ) ~ level + 585L,
      (context == 'endless_map' & (level >= 31 & level < 61  )) ~ level + 586L,
      (context == 'endless_map' & (level >= 61 & level < 91  )) ~ level + 587L,
      (context == 'endless_map' & (level >= 91 & level < 121  )) ~ level + 588L,
      (context == 'endless_map' & (level >= 121 & level < 151  )) ~ level + 589L,
      (context == 'endless_map' & (level >= 151 & level < 181  )) ~ level + 590L,
      (context == 'endless_map' & (level >= 181 & level < 211  )) ~ level + 591L,
      (context == 'endless_map' & (level >= 211 & level < 241  )) ~ level + 592L,
      (context == 'endless_map' & (level >= 241 & level < 271  )) ~ level + 593L,
      (context == 'endless_map' & (level >= 271 & level < 301  )) ~ level + 594L,
      (context == 'endless_map' & (level >= 301 & level < 331  )) ~ level + 595L,
      (context == 'endless_map' & (level >= 331 & level < 361  )) ~ level + 596L,
      (context == 'endless_map' & (level >= 361 & level < 391  )) ~ level + 597L,
      (context == 'endless_map' & (level >= 391 & level < 421  )) ~ level + 598L,
      (context == 'endless_map' & (level >= 421 & level < 451  )) ~ level + 599L,
      (context == 'endless_map' & (level >= 451 & level < 481  )) ~ level + 600L,
      (context == 'endless_map' & (level >= 481 & level < 511  )) ~ level + 601L,
      (context == 'endless_map' & (level >= 511 & level < 541  )) ~ level + 602L,
      (context == 'endless_map' & (level >= 541 )) ~ level + 603L,
      context == 'Book 2' ~ level + 285L,
      TRUE ~ level)
  )

message(\"Pasa level abs\")

table_level <- table_level %>% 
  mutate_all(funs(replace(., which(is.na(.)), 0))) 

message(\"Pasa primer mutate_all\")

table_level <- table_level %>% 
  transmute(
    map = context,
    level = as.integer(level),
    abs_level,
    version = version,
    start_users = as.integer(total_start_users),
    avg_retry,
    median_retry,
    sd_retry,
    difficulty = round(total_fail/total_success,2),
    difficulty_nopu = round(total_fails_wo_pu / total_success_wo_pu,2),
    difficulty_pu = round(total_fails_w_pu / total_success_w_pu,2),
    ARPU = round(total_pu_used/total_start_users,2),
    success_rate = round(total_success/total_starts,4),
    perc_payer = round(total_pu_users/total_start_users,4),
    perc_1_star = round(total_1_star_users/total_success,4),
    perc_2_star = round(total_2_star_users/total_success,4),
    perc_3_star = round(total_3_star_users/total_success,4),
    avg_moves_remaining,
    median_reshuffles,
    median_trigger_tiles,
    median_free_falls,
    median_time_elapsed,
    fail_churn_vs_ARPU = round((c7_l/total_start_users) / ARPU, 4),
    churn_d7 = round(c7/total_start_users,4),
    # start_churn_d7 = round(c7_s/total_start_users,4),
    # success_churn_d7 = round(c7_w/total_start_users,4),
    fail_churn_d7 = round(c7_l/total_start_users,4),
    success_churn_d7_vs_churn_d7 =
    round((c7_s/total_start_users)/(c7/total_start_users),4),
    mechanic,
    winmultiple1,
    winmultiple2
  ) 

message(\"Pasa transmute\")

table_level <- table_level %>% 
  mutate_all(funs(replace(., which(is.na(.)), 0)))


message(\"Pasa segundo mutate_all\")

table_version <- table_level %>% 
  filter(version != '-') %>% 
  mutate_all(funs(replace(., which(is.na(.) | is.infinite(.)), 0))) %>% 
  group_by(version) %>% 
  summarise(avg_difficulty = mean(difficulty),
            avg_difficulty_nopu = mean(na.omit(difficulty_nopu)),
            avg_difficulty_pu = mean(difficulty_pu),
            max_difficulty = max(difficulty),
            # max_difficulty_nopu = max(na.omit(difficulty_nopu)),
            # max_difficulty_pu = max(difficulty_pu),
            avg_churn_d7 = mean(churn_d7),
            # avg_start_churn_d7 = mean(start_churn_d7),
            # avg_success_churn_d7 = mean(success_churn_d7),
            avg_fail_churn_d7 = mean(fail_churn_d7),
            # max_start_churn_d7 = max(start_churn_d7),
            # max_success_churn_d7 = max(success_churn_d7),
            max_fail_churn_d7 = max(fail_churn_d7),
            avg_ARPU = mean(ARPU)
  )

message(\"Pasa table version\")


if(Sys.getenv('PROJECT') == 'fff')
{
    table_level <- table_level %>% 
    mutate(map = case_when(
    map == \"winter_map\" ~ 'Winter',
    map == \"summer_map\" ~ 'Summer',
    map == \"endless_map\" ~ 'Endless',
    TRUE ~  map))
}

get_prop <- function(tabla){
  return(mutate(tabla, prop = total / sum(total)))}

table_pu <- data_pu %>% 
  filter(level > 0 && !(context == 'winter_map' & level > 285)) %>% 
  group_by(context, level) %>% 
  nest() %>% 
  modify_at(\"data\", ~map(., get_prop)) %>% 
  unnest() %>% 
  select(-total) %>% 
  spread(tier2, prop) %>%
  map_if(is.numeric, ~round(replace(., is.na(.), 0),4)) %>% 
  as.tibble() %>% 
  rename(map = context, Level = level)


message(\"Pasa table_pu\")

```

", sep = "")

if(Sys.getenv("PROJECT") == "mff")
{
  list_maps <- c("Book 1", "Book 2")
}
if(Sys.getenv("PROJECT") == "fff")
{
  list_maps <-  c("winter_map", "summer_map", "endless_map")
}

for (current_map in list_maps){
cat(
current_map, " {data-navmenu=\"Maps\"}", 
"
=====================================  
  
Column {data-width=650}  {.tabset .tabset-fade}
-----------------------------------------------------------------------
  
### Summary Level
  
```{r summary ", current_map ,"}

table_level_map <- table_level %>% 
  filter(map == \"", current_map, "\") %>% 
  select(-version)

table_pu_map <- table_pu %>% 
  filter(map == \"", current_map, "\")

render_table(table_level_map)

```

", sep = "")
  
}

cat("Summary {data-navmenu=\"Version\"}
===================================== 
  
Column  {data-width=700} {.tabset .tabset-fade}
-----------------------------------------------------------------------
  
### Data
  
```{r table_by_version}

render_table(table_version, extraFormat = FALSE)

```

### Difficulty

```{r plot_diff_by_version}

plot_ly(table_version, x = ~version, y = ~avg_difficulty_nopu, 
        type = 'bar', name = 'Non Powerup') %>% 
  add_trace(y = ~avg_difficulty_pu, name = 'Powerup') %>% 
  add_trace(y = ~avg_difficulty, name = 'General',
            type = 'scatter', mode = 'lines+markers')

```

### ARPU Vs Churn

```{r plot_apu_vs_churn}

plot_ly(table_version, x = ~version, y = ~avg_ARPU, name = 'ARPU',
        type = 'scatter', mode = 'lines+markers') %>%
  add_trace(y = ~avg_churn_d7, name = 'Churn D7', 
            mode = 'lines+markers', yaxis = 'y2') %>%
  layout(yaxis2 = list(overlaying = 'y',side = 'right'))

```

Difficulty {data-navmenu=\"Version\"}
=====================================  

Column  {data-width=700} {.tabset .tabset-fade}
-----------------------------------------------------------------------
"
)

for(current_version in vector_versions$version){
  cat(
"
### ", current_version,"
  
```{r ", current_version, "_table_difficulty}
a <- filter(table_level, version == \"", current_version,"\") %>% 
  select(-map,-version)

render_table(a) 
```

"
  ,sep = "")
}

cat("Column  {data-width=700} {.tabset .tabset-fade}
-----------------------------------------------------------------------
")

for(current_version in vector_versions$version){
cat("
### ", current_version,"
  
```{r ", current_version, "_plots}
a <- filter(table_level, version == '",current_version,"')

if(nrow(a) > 0){
xaxisdiff <- list(
  title = 'Level'
)
yaxisdiff <- list(
  title = 'Difficulty',
  range = c(0,50)
)
yaxisdiff2 <- list(
  title = 'Difficulty'
)
p1 <- plot_ly(a, x = ~level, y = ~difficulty, type = 'bar', name = 'Non Powerup') %>% 
  add_trace(y = ~difficulty, name = 'Powerup') %>% 
  add_trace(x = ~level, y = ~difficulty, type = 'scatter', mode = 'lines+markers', name = 'General') %>% 
  layout(xaxis = xaxisdiff, yaxis = yaxisdiff)

p2 <- plot_ly(a, x = ~level, y = ~ARPU, type = 'scatter', mode = 'lines+markers', name = 'ARPU') %>% 
  add_trace(y = ~churn_d7, name = 'Churn', yaxis = 'y2') %>% 
  layout(xaxis = xaxisdiff, yaxis = yaxisdiff2, yaxis2 = list(overlaying = 'y2',side = 'right'))

subplot(p1,p2,nrows = 2)
} else {\"We dont have data for this specific set of levels\"}

```

"
,sep = "")
}

cat("
```{r write_csv, include = FALSE, message = FALSE, warning = FALSE}
write.csv2(table_level, file = 'level_balancing.csv', dec = ',', sep = ';', row.names = FALSE)
```
")

sink()
}