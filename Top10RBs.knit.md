---
title: "Top 10 MWC RBs"
author: "Hunter Johnson"
date: "2025-06-05"
output: github_document
---




``` r
library(cfbfastR)
library(gt)
library(gtExtras)
library(data.table)
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.2     ✔ tibble    3.2.1
## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
## ✔ purrr     1.0.4     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::between()     masks data.table::between()
## ✖ dplyr::filter()      masks stats::filter()
## ✖ dplyr::first()       masks data.table::first()
## ✖ lubridate::hour()    masks data.table::hour()
## ✖ lubridate::isoweek() masks data.table::isoweek()
## ✖ dplyr::lag()         masks stats::lag()
## ✖ dplyr::last()        masks data.table::last()
## ✖ lubridate::mday()    masks data.table::mday()
## ✖ lubridate::minute()  masks data.table::minute()
## ✖ lubridate::month()   masks data.table::month()
## ✖ lubridate::quarter() masks data.table::quarter()
## ✖ lubridate::second()  masks data.table::second()
## ✖ purrr::transpose()   masks data.table::transpose()
## ✖ lubridate::wday()    masks data.table::wday()
## ✖ lubridate::week()    masks data.table::week()
## ✖ lubridate::yday()    masks data.table::yday()
## ✖ lubridate::year()    masks data.table::year()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(magrittr)
```

```
## 
## Attaching package: 'magrittr'
## 
## The following object is masked from 'package:purrr':
## 
##     set_names
## 
## The following object is masked from 'package:tidyr':
## 
##     extract
```


``` r
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
```


``` r
stats <- cfbfastR::cfbd_stats_season_player(2024,
                                   season_type = "both",
                                   conference = "MWC") 


rush <- stats |>
  filter(rushing_yds > 500) |>
  mutate(passing_completions = ifelse(is.na(passing_completions), 0, passing_completions)) |>
  filter(passing_completions < 5) |>
  select(team, athlete_id, player, rushing_car, rushing_yds, rushing_td, rushing_long, rushing_ypc, fumbles_lost, fumbles_fum, receiving_rec, receiving_yds, receiving_td, receiving_long)
  

usage <- cfbd_player_usage(2024,
                            conference = "MWC")

rb_usage <- usage |>
  filter(position == "RB") |>
  select(athlete_id, usg_overall, usg_rush, usg_pass)


info <- cfbd_team_info()
```

```
## New names:
## 2025-06-05 09:35:33.485505:Invalid arguments or no team data available!
## • `id` -> `id...1`
## • `id` -> `id...14`
```

``` r
rush %<>%
  left_join(info %>% select(school, logo), by = c("team" = "school"))
```


``` r
rush %>%
  select(logo, player, rushing_car, rushing_yds, rushing_ypc, rushing_td, receiving_rec, receiving_yds, receiving_td) %>%
  arrange(-rushing_yds) %>%
  gt() %>%
  gt_theme_f5() %>%
  gt_img_rows(
    columns = logo,
    height = 30
  ) %>%
  tab_header(
    title = "Mountain West Running Backs",
    subtitle = "Top 10 RBs by Rushing Yards"
  ) %>%
  cols_label(
    logo = "",
    player = "Player",
    rushing_car = "Carries",
    rushing_yds = "Rush Yds",
    rushing_ypc = "YPC",
    rushing_td = "Rush TDs",
    receiving_rec = "Rec",
    receiving_yds = "Rec Yds",
    receiving_td = "Rec TDs"
  ) %>%
  data_color(rushing_ypc, palette = "rcartocolor::Tropic", domain = c(4, 8), reverse = T) %>% 
  tab_options(data_row.padding = '0px')
```

```{=html}
<div id="ckwotkgand" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>@import url("https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
@import url("https://fonts.googleapis.com/css2?family=Roboto:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap");
#ckwotkgand table {
  font-family: Roboto, system-ui, 'Segoe UI', Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#ckwotkgand thead, #ckwotkgand tbody, #ckwotkgand tfoot, #ckwotkgand tr, #ckwotkgand td, #ckwotkgand th {
  border-style: none;
}

#ckwotkgand p {
  margin: 0;
  padding: 0;
}

#ckwotkgand .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: 400;
  font-style: normal;
  background-color: #FFFAF0;
  width: auto;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#ckwotkgand .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#ckwotkgand .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFAF0;
  border-bottom-width: 0;
}

#ckwotkgand .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFAF0;
  border-top-width: 0;
}

#ckwotkgand .gt_heading {
  background-color: #FFFAF0;
  text-align: left;
  border-bottom-color: #FFFAF0;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ckwotkgand .gt_bottom_border {
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ckwotkgand .gt_col_headings {
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#ckwotkgand .gt_col_heading {
  color: #333333;
  background-color: #FFFAF0;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#ckwotkgand .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFAF0;
  font-size: 100%;
  font-weight: bold;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#ckwotkgand .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ckwotkgand .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ckwotkgand .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #000000;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#ckwotkgand .gt_spanner_row {
  border-bottom-style: hidden;
}

#ckwotkgand .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFAF0;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFAF0;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#ckwotkgand .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFAF0;
  font-size: 100%;
  font-weight: initial;
  border-top-style: none;
  border-top-width: 2px;
  border-top-color: #000000;
  border-bottom-style: solid;
  border-bottom-width: 1px;
  border-bottom-color: #FFFAF0;
  vertical-align: middle;
}

#ckwotkgand .gt_from_md > :first-child {
  margin-top: 0;
}

#ckwotkgand .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ckwotkgand .gt_row {
  padding-top: 0px;
  padding-bottom: 0px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #E5E5E5;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#ckwotkgand .gt_stub {
  color: #333333;
  background-color: #FFFAF0;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 0px;
  border-right-color: #FFFAF0;
  padding-left: 5px;
  padding-right: 5px;
}

#ckwotkgand .gt_stub_row_group {
  color: #333333;
  background-color: #FFFAF0;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#ckwotkgand .gt_row_group_first td {
  border-top-width: 2px;
}

#ckwotkgand .gt_row_group_first th {
  border-top-width: 2px;
}

#ckwotkgand .gt_summary_row {
  color: #333333;
  background-color: #FFFAF0;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ckwotkgand .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#ckwotkgand .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#ckwotkgand .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ckwotkgand .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFAF0;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ckwotkgand .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ckwotkgand .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#ckwotkgand .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ckwotkgand .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ckwotkgand .gt_footnotes {
  color: #333333;
  background-color: #FFFAF0;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ckwotkgand .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ckwotkgand .gt_sourcenotes {
  color: #333333;
  background-color: #FFFAF0;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#ckwotkgand .gt_sourcenote {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#ckwotkgand .gt_left {
  text-align: left;
}

#ckwotkgand .gt_center {
  text-align: center;
}

#ckwotkgand .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ckwotkgand .gt_font_normal {
  font-weight: normal;
}

#ckwotkgand .gt_font_bold {
  font-weight: bold;
}

#ckwotkgand .gt_font_italic {
  font-style: italic;
}

#ckwotkgand .gt_super {
  font-size: 65%;
}

#ckwotkgand .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#ckwotkgand .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#ckwotkgand .gt_indent_1 {
  text-indent: 5px;
}

#ckwotkgand .gt_indent_2 {
  text-indent: 10px;
}

#ckwotkgand .gt_indent_3 {
  text-indent: 15px;
}

#ckwotkgand .gt_indent_4 {
  text-indent: 20px;
}

#ckwotkgand .gt_indent_5 {
  text-indent: 25px;
}

#ckwotkgand .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#ckwotkgand div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_title gt_font_normal" style="font-family: Roboto; font-weight: 700;">Mountain West Running Backs</td>
    </tr>
    <tr class="gt_heading">
      <td colspan="9" class="gt_heading gt_subtitle gt_font_normal gt_bottom_border" style="color: #A6A6A6; font-family: Roboto; font-weight: 400;">Top 10 RBs by Rushing Yards</td>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="logo"></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="player">Player</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="rushing_car">Carries</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="rushing_yds">Rush Yds</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="rushing_ypc">YPC</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="rushing_td">Rush TDs</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="receiving_rec">Rec</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="receiving_yds">Rec Yds</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" style="border-top-width: 0px; border-top-style: solid; border-top-color: black; font-family: Roboto; font-size: 14px; vertical-align: bottom; font-weight: bold;" scope="col" id="receiving_td">Rec TDs</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/68.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Ashton Jeanty</td>
<td headers="rushing_car" class="gt_row gt_right">374</td>
<td headers="rushing_yds" class="gt_row gt_right">2601</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #7BC5C6; color: #000000;">7.0</td>
<td headers="rushing_td" class="gt_row gt_right">29</td>
<td headers="receiving_rec" class="gt_row gt_right">23</td>
<td headers="receiving_yds" class="gt_row gt_right">138</td>
<td headers="receiving_td" class="gt_row gt_right">1</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/21.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Marquez Cooper</td>
<td headers="rushing_car" class="gt_row gt_right">292</td>
<td headers="rushing_yds" class="gt_row gt_right">1274</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #D07DB8; color: #FFFFFF;">4.4</td>
<td headers="rushing_td" class="gt_row gt_right">12</td>
<td headers="receiving_rec" class="gt_row gt_right">20</td>
<td headers="receiving_yds" class="gt_row gt_right">137</td>
<td headers="receiving_td" class="gt_row gt_right">2</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/328.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Rahsul Faison</td>
<td headers="rushing_car" class="gt_row gt_right">198</td>
<td headers="rushing_yds" class="gt_row gt_right">1109</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #EAD4E3; color: #000000;">5.6</td>
<td headers="rushing_td" class="gt_row gt_right">8</td>
<td headers="receiving_rec" class="gt_row gt_right">22</td>
<td headers="receiving_yds" class="gt_row gt_right">99</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/167.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Eli Sanders</td>
<td headers="rushing_car" class="gt_row gt_right">147</td>
<td headers="rushing_yds" class="gt_row gt_right">1064</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #5CBDBE; color: #000000;">7.2</td>
<td headers="rushing_td" class="gt_row gt_right">9</td>
<td headers="receiving_rec" class="gt_row gt_right">15</td>
<td headers="receiving_yds" class="gt_row gt_right">134</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/36.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Avery Morrow</td>
<td headers="rushing_car" class="gt_row gt_right">184</td>
<td headers="rushing_yds" class="gt_row gt_right">1006</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #E7CDDF; color: #000000;">5.5</td>
<td headers="rushing_td" class="gt_row gt_right">9</td>
<td headers="receiving_rec" class="gt_row gt_right">16</td>
<td headers="receiving_yds" class="gt_row gt_right">102</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/2439.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Jai'Den Thomas</td>
<td headers="rushing_car" class="gt_row gt_right">161</td>
<td headers="rushing_yds" class="gt_row gt_right">915</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #EBDBE6; color: #000000;">5.7</td>
<td headers="rushing_td" class="gt_row gt_right">7</td>
<td headers="receiving_rec" class="gt_row gt_right">11</td>
<td headers="receiving_yds" class="gt_row gt_right">91</td>
<td headers="receiving_td" class="gt_row gt_right">1</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/36.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Justin Marshall</td>
<td headers="rushing_car" class="gt_row gt_right">156</td>
<td headers="rushing_yds" class="gt_row gt_right">767</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #DBA2C9; color: #000000;">4.9</td>
<td headers="rushing_td" class="gt_row gt_right">4</td>
<td headers="receiving_rec" class="gt_row gt_right">8</td>
<td headers="receiving_yds" class="gt_row gt_right">62</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/23.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Floyd Chalk IV</td>
<td headers="rushing_car" class="gt_row gt_right">154</td>
<td headers="rushing_yds" class="gt_row gt_right">721</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #D793C2; color: #000000;">4.7</td>
<td headers="rushing_td" class="gt_row gt_right">10</td>
<td headers="receiving_rec" class="gt_row gt_right">6</td>
<td headers="receiving_yds" class="gt_row gt_right">58</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/2440.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Savion Red</td>
<td headers="rushing_car" class="gt_row gt_right">119</td>
<td headers="rushing_yds" class="gt_row gt_right">687</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #EDE3EA; color: #000000;">5.8</td>
<td headers="rushing_td" class="gt_row gt_right">8</td>
<td headers="receiving_rec" class="gt_row gt_right">16</td>
<td headers="receiving_yds" class="gt_row gt_right">74</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/2439.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Kylin James</td>
<td headers="rushing_car" class="gt_row gt_right">89</td>
<td headers="rushing_yds" class="gt_row gt_right">653</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #49B8BA; color: #000000;">7.3</td>
<td headers="rushing_td" class="gt_row gt_right">5</td>
<td headers="receiving_rec" class="gt_row gt_right">1</td>
<td headers="receiving_yds" class="gt_row gt_right">1</td>
<td headers="receiving_td" class="gt_row gt_right">0</td></tr>
    <tr><td headers="logo" class="gt_row gt_left"><img src="http://a.espncdn.com/i/teamlogos/ncaa/500/2005.png" style="height:30px;"></td>
<td headers="player" class="gt_row gt_left">Dylan Carson</td>
<td headers="rushing_car" class="gt_row gt_right">138</td>
<td headers="rushing_yds" class="gt_row gt_right">600</td>
<td headers="rushing_ypc" class="gt_row gt_right" style="background-color: #CE75B5; color: #FFFFFF;">4.3</td>
<td headers="rushing_td" class="gt_row gt_right">5</td>
<td headers="receiving_rec" class="gt_row gt_right">NA</td>
<td headers="receiving_yds" class="gt_row gt_right">NA</td>
<td headers="receiving_td" class="gt_row gt_right">NA</td></tr>
  </tbody>
  
  
</table>
</div>
```

