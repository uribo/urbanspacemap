# 1/3 パッケージの読み込み -----------------------------------------------------------
library(osmdata)
library(tidyverse)
library(sf)
library(zipangu)
data("jpnprefs", package = "zipangu")
library(kuniezu)
# uribo/kuniumi

route_circple_crop <- function(pref_no, col1 = "#2E3440", col2 = "#3B4542") {
  df_pref <- df_prefecture %>%
    slice(pref_no)
  cropped_roads <-
    st_intersection(sf_osm_highway[[pref_no]], sf_station_2kmbuffer[pref_no, ])
  cropped_roads2 <-
    st_intersection(sf_osm_railway[[pref_no]], sf_station_2kmbuffer[pref_no, ])
  bb <-
    sf_station_2kmbuffer[pref_no, ] %>%
    st_bbox()
  scale_line <-
    c(st_point(c(bb[1], bb[2])),
      st_point(c(bb[1] + (bb[3] - bb[1]) / 4, bb[2]))) %>%
    st_cast("LINESTRING") %>%
    st_sfc(crs = 4326)
  df_scale <-
    st_centroid(scale_line) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    mutate(Y = Y + 0.0005,
           scale = "500m")
  # plot
  ggplot() +
    geom_sf(data = cropped_roads, color = col1, size = 0.35) +
    geom_sf(data = cropped_roads2, color = col2, size = 0.5) +
    geom_sf(data = scale_line) +
    geom_text(data = df_scale,
              aes(X, Y, label = scale),
              family = "TsukuARdGothic-Regular") +
    geom_sf(data = sf_station_2kmbuffer[pref_no, ],
            fill = "transparent") +
    coord_sf(datum = NA) +
    theme_bw(base_family = "TsukuARdGothic-Regular") +
    theme(rect = element_blank(),
          axis.title = element_blank()) +
    labs(title = df_pref %>% pull(description),
         subtitle = "都道府県の県庁所在駅から2km圏の道路および鉄道",
         caption = "@uribo\nSource: \u00a9 国土数値情報 鉄道データ 第2.3版 平成30年度 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N02-v2_3.html,\n\u00a9 OpenStreetMap contributors"
         )
}

# 2/3 データの用意 ----------------------------------------------------------
# a. 駅データ (座標として利用)
# Source: 国土数値情報
# 埼玉、東京、福岡、鹿児島は県庁所在地名とは異なる駅名
stations <-
  c("札幌", "青森", "盛岡", "仙台", "秋田",
    "山形", "福島", "水戸", "宇都宮", "前橋",
    "浦和", "千葉", "新宿", "横浜", "新潟",
    "富山", "金沢", "福井", "甲府", "長野",
    "岐阜", "静岡", "名古屋", "津", "大津",
    "京都", "大阪", "神戸", "奈良", "和歌山",
    "鳥取", "松江", "岡山", "広島", "山口",
    "徳島", "高松", "松山", "高知", "博多",
    "佐賀", "長崎", "熊本", "大分", "宮崎",
    "鹿児島中央")
sf_station <-
  kuniumi::read_ksj_n02("~/Documents/resources/国土数値情報/N02/N02-18_GML/N02-18_Station.shp") %>%
  sf::st_transform(crs = 6668) %>%
  sf::st_transform(crs = 4326) %>%
  filter(railwayType == "11",
         serviceProviderType == "2")

sf_station <-
  sf_station %>%
  filter(stringr::str_detect(operationCompany, "旅客鉄道$"),
         stringr::str_detect(railwayLineName, "大阪環状線$", negate = TRUE),
         stationName %in% stations) %>%
  group_by(stationName) %>%
  slice(1L) %>%
  assertr::verify(nrow(.) == 46L) %>%
  ungroup()

sf_station <-
  sf_station %>% 
  mutate(stationName = as_factor(stationName) %>% 
           fct_relevel(stations),
         geometry = st_centroid(geometry),
         srid = st_detect_jgd2011(geometry)) %>% 
  arrange(stationName) %>%
  select(names(.)[!names(.) %in% attr(., "sf_column")])

# バッファ
sf_station_2kmbuffer <-
  sf_station %>%
  group_by(stationName) %>%
  mutate(geometry = geometry %>%
           st_transform(crs = srid) %>%
           st_buffer(dist = units::set_units(2, km)) %>%
           st_transform(crs = 4326) %>%
           st_geometry()) %>%
  ungroup()

df_prefecture <-
  jpnprefs %>%
  select(jis_code, prefecture = prefecture_kanji, region) %>%
  # 沖縄県は除外
  slice(seq.int(46L)) %>%
  bind_cols(sf_station_2kmbuffer %>%
              st_drop_geometry()) %>%
  mutate(description = glue::glue("{jis_code} {prefecture} ({stationName}駅)"))

# b. 道路データ、鉄道データ
# Source: OSM
if (file.exists(here::here("data/urban_space_osm_highway.rds")) == FALSE) {
  sf_osm_highway <-
    st_geometry(sf_station_2kmbuffer) %>%
    map(~ st_bbox(.x) %>%
                 opq() %>%
                 add_osm_feature(key = "highway") %>%
                 osmdata_sf() %>%
                 pluck("osm_lines") %>%
                 select(osm_id))
  write_rds(sf_osm_highway, here::here("data/urban_space_osm_highway.rds"))
} else {
  sf_osm_highway <-
    read_rds(here::here("data/urban_space_osm_highway.rds"))
}
if (file.exists(here::here("data/urban_space_osm_railway.rds")) == FALSE) {
  sf_osm_railway <-
    st_geometry(sf_station_2kmbuffer) %>%
    map(~ st_bbox(.x) %>%
                 opq() %>%
                 add_osm_feature(key = "railway") %>%
                 osmdata_sf() %>%
                 pluck("osm_lines") %>%
                 select(osm_id))
  write_rds(sf_osm_railway, here::here("data/urban_space_osm_railway.rds"))
} else {
  sf_osm_railway <-
    read_rds(here::here("data/urban_space_osm_railway.rds"))
}

# 3/3 マッピング ---------------------------------------------------------------
# route_circple_crop(33)
if (length(fs::dir_ls(here::here("figures"), regexp = ".png$")) != 49L) {
  seq.int(46) %>%
    walk(~ ggsave(here::here("figures", str_c(
      "urban_space_2km_",
      df_prefecture %>%
        slice(.x) %>%
        pull(description) %>%
        str_replace(" ", "_"),
      ".png"
    )), route_circple_crop(.x), dpi = 300, width = 11.5, height = 9.94))
  
  # renv::install("thomasp85/patchwork")
  library(patchwork)
  library(nord)
  
  route_circple_crop_blank <- function(pref_no, col1 = "#2E3440", col2 = "#3B4542") {
    df_pref <- df_prefecture %>%
      slice(pref_no)
    cropped_roads <-
      st_intersection(sf_osm_highway[[pref_no]], sf_station_2kmbuffer[pref_no, ])
    cropped_roads2 <-
      st_intersection(sf_osm_railway[[pref_no]], sf_station_2kmbuffer[pref_no, ])
    bb <-
      sf_station_2kmbuffer[pref_no, ] %>%
      st_bbox()
    scale_line <-
      c(st_point(c(bb[1], bb[2])),
        st_point(c(bb[1] + (bb[3] - bb[1]) / 4, bb[2]))) %>%
      st_cast("LINESTRING") %>%
      st_sfc(crs = 4326)
    df_scale <-
      st_centroid(scale_line) %>%
      st_coordinates() %>%
      as.data.frame() %>%
      mutate(Y = Y + 0.0005,
             scale = "500m")
    # plot
    ggplot() +
      geom_sf(data = cropped_roads, color = col1, size = 0.35) +
      geom_sf(data = cropped_roads2, color = col2, size = 0.5) +
      geom_sf(data = sf_station_2kmbuffer[pref_no, ],
              fill = "transparent") +
      coord_sf(datum = NA) +
      theme_bw(base_family = "TsukuARdGothic-Regular",
               base_size = 8) +
      theme(rect = element_blank(),
            axis.title = element_blank()) +
      labs(title = df_pref %>% pull(description)
      )
  }
  tile_output <- function(prefs, col1 = nord("frost")[1], col2 = nord("frost")[4]) {
    prefs %>% 
      purrr::map(
        ~ route_circple_crop_blank(.x,
                                   col1 = col1,
                                   col2 = col2)) %>%
      patchwork::wrap_plots() +
      patchwork::plot_layout(ncol = 5) +
      patchwork::plot_annotation(theme = theme(text = element_text(family = "TsukuARdGothic-Regular")),
                                 subtitle = "都道府県の県庁所在駅から2km圏の道路および鉄道",
                                 caption = "@uribo\nSource: \u00a9 国土数値情報 鉄道データ 第2.3版 平成30年度 http://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N02-v2_3.html,\n\u00a9 OpenStreetMap contributors")
  }
  
  p1east_japan <- 
    tile_output(seq_len(23), nord("frost")[1], nord("frost")[4])
  ggsave(here::here("figures/p1_1-23.png"), p1east_japan, width = 8, height = 10, dpi = 300)
  
  p2west_japan <- 
    tile_output(seq.int(24, 46), nord("aurora")[1], nord("aurora")[4])
  ggsave(here::here("figures/p2_24-46.png"), p2west_japan, width = 8, height = 10, dpi = 300)
}
