# Libraries ---------------------------------------------------------------
library(cfbfastR)
library(tidyverse)
library(magrittr)
library(gt)
library(gtExtras)
library(gtUtils)

gt_theme_f5 <- function(gt_object, ...) {
  
  gt_object %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(
        font = google_font("Roboto"),
        weight = 700
      )
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(
        font = google_font("Roboto"),
        color = "gray65",
        weight = 400
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "top", color = "black", weight = px(0)
        ),
        cell_text(
          font = google_font("Roboto"),
          #transform = "uppercase",
          v_align = "bottom",
          size = px(14),
          weight = 'bold'
        )
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_stubhead()
      )
    ) %>%
    tab_options(
      column_labels.background.color = "floralwhite",
      data_row.padding = px(7.5),
      heading.border.bottom.style = "none",
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold", 
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "floralwhite",
      stub.border.color = "floralwhite",
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 16,
      heading.align = "left",
      table.background.color = "floralwhite",
      table_body.hlines.color = 'gray90',
      ...
    )
}

# Wranglin ----------------------------------------------------------------

#all <- map_dfr(1:14, ~ cfbd_pbp_data(year = 2025, epa_wpa = T, week = .x))




plays <- map_dfr(1:14, ~ cfbd_pbp_data(year = 2025, team = "Boise State", epa_wpa = T, week = .x))

info <- cfbd_team_info()

off <- plays %>%
  filter(pos_team == "Boise State")



off %<>%
  mutate(passer = case_when(str_detect(passer_player_name, "Madsen") ~ "Madsen",
                            str_detect(passer_player_name, "Cutforth") ~ "Cutforth",
                            TRUE ~ NA)) %>%
  left_join(info %>% select(school, logo), by = c("def_pos_team" = "school")) %>%
  mutate(logo = case_when(def_pos_team == "Eastern Washington" ~ "https://a.espncdn.com/i/teamlogos/ncaa/500/331.png",
                          TRUE ~ logo),
         garbage = case_when(period = 2 & abs(pos_score_diff) > 38 ~ 1,
                             period = 3 & abs(pos_score_diff) > 28 ~ 1,
                             period = 4 & abs(pos_score_diff) > 22 ~ 1,
                             TRUE ~ 0))

# off %>%
#   select(period, garbage, pos_score_diff) %>%
#   view()


qbs <- off %>%
  filter(!is.na(passer)) %>%
  filter(play_type != "Fumble Recovery (Opponent)") %>%
  filter(garbage == 0)





epa <- qbs %>%
  group_by(wk, passer, logo) %>%
  summarize(mean = mean(EPA),
            sum = sum(EPA),
            n = n()) %>%
  filter(n > 10) %>%
  as.data.frame() %>%
  mutate(opp_epa = c(-.01, 0, .39, .15, -.08, .1, -.02, 0, -.13, -.27, .13, 0),
         adj_mean = mean - opp_epa) %>%
  select(wk, passer, logo, adj_mean, mean, sum, n)



  
epa %>%
  gt() %>%
  gt_img_rows(
    columns = logo,
    height = 30
  ) %>%
  gt_theme_f5() %>%
  cols_label(wk = "Week",
             passer = "QB",
             logo = "Opponent",
             adj_mean = "Adjusted EPA/DB",
             mean = "EPA/DB",
             sum = "Pass EPA",
             n = "Plays") %>%
  fmt_number(columns = adj_mean,
             decimals = 3) %>%
  fmt_number(columns = mean,
             decimals = 3) %>%
  fmt_number(columns = sum,
             decimals = 1) %>%
  cols_align(columns = everything(),
             align = "center") %>%
  tab_header(title = "Boise State QBs in 2025",
             subtitle = "Non-Garbage Time Plays") %>%
  tab_options(heading.align = "center") %>%
  data_color(adj_mean, palette = "rcartocolor::Tropic", domain = c(-.5, .61), reverse = T) %>%
  data_color(mean, palette = "rcartocolor::Tropic", domain = c(-.65, .75), reverse = T) %>%
  data_color(sum, palette = "rcartocolor::Tropic", domain = c(-20, 17), reverse = T) %>%
  data_color(n, palette = "rcartocolor::Tropic", domain = c(15, 49), reverse = T) %>% 
  tab_options(data_row.padding = '0px') %>%
  tab_source_note(
    source_note = html("Table by @UnterHonson using <i>The F5</i> Theme, Data by CFBfastR")) %>% 
  gt_save_crop(file = "BSUQBs25_2.png", whitespace = 20, bg = "floralwhite")


  
  

off %>%
  filter(!is.na(passer)) %>%
  filter(def_pos_team == "Colorado State") %>%
  select(period, play_text, EPA) %>%
  view()
