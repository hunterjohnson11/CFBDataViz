library(cfbfastR)
library(tidyverse)
library(gt)
library(gtExtras)

update.packages("cfbfastR")

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

test <- load_cfb_pbp()
info <- cfbd_team_info()

fbs_conferences <- c(
  "ACC",
  "American Athletic",
  "Big 12",
  "Big Ten",
  "Conference USA",
  "Mid-American",
  "Mountain West",
  "Pac-12",
  "SEC",
  "Sun Belt",
  "FBS Independents"
)


qb_leaders <- test %>%
  filter(offense_conference %in% fbs_conferences & defense_conference %in% fbs_conferences) %>%
  group_by(passer_player_name, pos_team) %>%
  summarize(plays = n(), 
            epa.play = mean(EPA),
            epa = sum(EPA)) %>%
  ungroup() %>%
  filter(plays > 100, !is.na(epa)) %>%
  arrange(-epa) %>%
  mutate(rank = row_number())
  


qb_leaders %>%
  left_join(info %>% select(school, logo), by = c("pos_team" = "school")) %>%
  select(rank, logo, passer_player_name, epa.play, epa, plays) %>%
  gt() %>%
  gt_theme_f5() %>%
  gt_img_rows(
    columns = logo,
    height = 30
  ) %>%
  fmt_number(
    columns = c(epa),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(epa.play),
    decimals = 2
  ) %>%
  tab_header(
    title = "QB EPA Leaders",
    subtitle = "Data from CFBfastR"
  ) %>%
  tab_options(heading.align = "center") %>%
  cols_label(
    rank = "Rank",
    logo = "",
    passer_player_name = "Player",
    epa = md("Total<br>EPA"),
    epa.play = md("EPA/<br>Dropback")
  ) 
  
  



