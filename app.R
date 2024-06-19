library(tidyverse)
library(shiny)
library(patchwork)
library(reactable)
library(grid)
library(ggridges)
library(sf)
library(sp)
library(rsconnect)
library(scales)
library(ggpubr)
library(bslib)
library(shinythemes)
library(htmltools)
library(cowplot)
library(ggforce)
library(gt)
library(gtExtras)
library(plotly)


load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
load(file = paste0(getwd(),"/data/functions_1.rda"))
load(file = paste0(getwd(),"/data/aesthetics.rda"))
load(file = paste0(getwd(),"/data/app_setup.rda"))
load(file = paste0(getwd(),"/data/plot_hitters.rda"))
load(file = paste0(getwd(),"/data/plotly_hitters.rda"))

# mutate_data <- function(input) {
#         
#         nu_df <- input
#         
#         nu_df <- nu_df %>% 
#                 filter(!is.na(RelSpeed)) %>% 
#                 mutate(
#                         PlateLocSide = 12*PlateLocSide,
#                         PlateLocHeight = 12*PlateLocHeight
#                 )
#         
#         nu_df <- nu_df %>% 
#                 separate(Pitcher, c("Pitcher_Last", "Pitcher_First"))
#         
#         nu_df <- nu_df %>% 
#                 separate(Batter, c("Batter_Last", "Batter_First"))
#         
#         nu_df <- nu_df %>% mutate(
#                 PlayResult = case_when(
#                         KorBB == "Strikeout" ~ "Strikeout",
#                         KorBB == "Walk" ~ "Walk", 
#                         PlayResult == "Undefined" ~ NA,
#                         TRUE ~ PlayResult),
#                 
#                 PlayResult = factor(PlayResult, levels = c("Single", "Double", "Triple", "HomeRun",
#                                                            "Walk", "Strikeout", "Out", "Error", "Sacrifice",
#                                                            "FieldersChoice")),
#                 
#                 AutoHitType = factor(AutoHitType, levels = c("GroundBall", "LineDrive", "FlyBall", "Popup")),
#                 
#                 AutoPitchType = case_when(
#                         AutoPitchType %in% c("Four-Seam", "Sinker", "Cutter") ~ "Fastball",
#                         AutoPitchType == "Splitter" ~ "Changeup",
#                         TRUE ~ AutoPitchType),
#                 
#                 AutoPitchType = factor(AutoPitchType, levels = c("Fastball", "Changeup", "Curveball", "Slider")),
#                 
#                 PitchCall = factor(PitchCall, levels = c("BallCalled", "BallinDirt", "BallIntentional", "InPlay",
#                                                          "StrikeCalled", "FoulBall", "StrikeSwinging", "HitByPitch")),
#                 
#                 ynhit = case_when(
#                         PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
#                         PlayResult %in% c("Strikeout", "Out", "Error", "FieldersChoice") ~ 0,
#                         PlayResult == "Undefined" ~ NA
#                 ),
#                 
#                 launch_speed_angle = case_when(
#                         ExitSpeed * 1.5 - Angle >= 117 &
#                                 ExitSpeed + Angle >= 124 &
#                                 ExitSpeed >= 98 &
#                                 Angle >= 4 & Angle <= 50 ~ "Barrel",
#                         
#                         ExitSpeed * 1.5 - Angle >= 111 &
#                                 ExitSpeed + Angle >= 119 &
#                                 ExitSpeed >= 95 &
#                                 Angle >= 0 & Angle <= 52 ~ "Solid_Contact",
#                         
#                         ExitSpeed * 2 - Angle >= 87 &
#                                 Angle <= 41 & 
#                                 ExitSpeed * 2 + Angle <= 175 &
#                                 ExitSpeed + Angle * 1.3 >= 89 &
#                                 ExitSpeed >= 59 & Angle <= 72 ~ "Flare_or_Burner",
#                         
#                         ExitSpeed + Angle * 1.3 <= 112 &
#                                 ExitSpeed + Angle * 1.55 >= 92 &
#                                 ExitSpeed >= 72 & Angle <= 86 ~ "Flare_or_Burner",
#                         
#                         Angle <= 20 &
#                                 ExitSpeed + Angle * 2.4 >= 98 &
#                                 ExitSpeed >= 86 & ExitSpeed <= 95 ~ "Flare_or_Burner",
#                         
#                         ExitSpeed - Angle >= 76 &
#                                 ExitSpeed + Angle * 2.4 >= 98 &
#                                 ExitSpeed >= 95 &
#                                 Angle <= 30 ~ "Flare_or_Burner",
#                         
#                         ExitSpeed + Angle * 2 >= 116 ~  "Poorly_Under",
#                         
#                         ExitSpeed + Angle * 2 <= 116 ~  "Poorly_Topped",
#                         
#                         ExitSpeed <= 59 ~ "Poorly_Weak"
#                 ),
#                 
#                 launch_speed_angle = factor(launch_speed_angle, levels = c(
#                         "Barrel", "Solid_Contact", "Flare_or_Burner", "Poorly_Under", "Poorly_Topped", "Poorly_Weak"
#                 ))
#         )
#         
#         nu_df <- nu_df %>% 
#                 mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% 
#                 mutate(
#                         Date = format(Date, "%b-%d")
#                 )
#         
#         nu_df <- nu_df %>% 
#                 mutate(
#                         swing = ifelse(
#                                 PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 1, 0),
#                         whiff = ifelse(
#                                 PitchCall %in% c("StrikeSwinging"), 1, 0)
#                 )
#         
#         save(nu_df, file = paste0(getwd(),"/data/nu_df.rda"))
# }

#### Pitch Types
pitch_types <- function(Batter, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Batter_Last == Batter),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoPitchType),
                           size = 5, shape = 21, position = "jitter") +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
        
}

#### Pitch Result
pitch_result <- function(Batter, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Batter_Last == Batter & !is.na(PlayResult)),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
                           size = 5, shape = 21) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_manual(values = PlayResultColors) +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Pitch Desc.
pitch_description <- function(Batter, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Batter_Last == Batter),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall),
                           size = 5, shape = 21) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
                scale_fill_manual(values = PitchCallColors) +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Batted Ball Type
batted_ball_type <- function(Batter, df) {
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Batter_Last == Batter & !is.na(AutoHitType)),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType),
                           size = 5, shape = 21) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_manual(values = HitTypeColors) +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Contact Type
contact_type <- function(Batter, df) {
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Batter_Last == Batter & !is.na(launch_speed_angle)),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = launch_speed_angle),
                           size = 5, shape = 21) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

# Heatmaps ----

#### Pitch Heatmap
pitch_heatmap <- function(Batter, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot(df %>% filter(Batter_Last == Batter),
               mapping = aes(PlateLocSide, PlateLocHeight)) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_distiller(palette = "RdBu") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Swing Heatmap
swing_heatmap <- function(Batter, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        df <- df %>% 
                filter(PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"))
        
        ggplot(df %>% filter(Batter_Last == Batter),
               mapping = aes(PlateLocSide, PlateLocHeight)) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_distiller(palette = "RdBu") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

# Polys ----

zone_function_setup <- function(Batter, input){
        
        require(sp)
        require(sf)
        require(tidyverse)
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Batter_Last == Batter) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Pitches = n(),
                        EV = mean(ExitSpeed, na.rm = TRUE),
                        LA = mean(Angle, na.rm = TRUE), 
                        Hits = mean(ynhit, na.rm = TRUE),
                        Swings = mean(swing, na.rm = TRUE),
                        Whiffs = mean(whiff, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
                        Freq_NumPitches = Pitches,
                        Freq_EV = EV,
                        Freq_LA = LA,
                        Freq_Hits = Hits * 100,
                        Freq_Swings = Swings * 100,
                        Freq_Whiffs = Whiffs * 100
                )
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV,
                       Freq_LA, Freq_Hits, Freq_Swings, Freq_Whiffs) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Save data ----
        
        save(df_pitch_polys, label_coords, file = paste0(getwd(),"/data/zone_setup.rda"))
        
}

#### Zone - Pitch Percent
zone_pitch_perc <- function(Batter, df_input, label_input) {
        
        #### Load App Setup
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_PitchPercent))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_PitchPercent))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_PitchPercent),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_PitchPercent) + 5),
                                     na.value = "lightgrey", guide = "legend") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
                          size = 4, fontface = "bold", family = "mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}

#### Zone - Pitch Percent
zone_total_pitches <- function(Batter, df_input, label_input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_NumPitches))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_NumPitches))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_NumPitches),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_input$Freq_NumPitches)),
                                     na.value = "lightgrey", guide = "legend") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = Freq_NumPitches),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}

#### Zone - Exit Velocity
zone_exit_velo <- function(Batter, df_input, label_input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_EV))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_EV))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_EV),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdBu",
                                     limits = c(25, max(df_input$Freq_EV)),
                                     na.value = "lightgrey", guide = "legend") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}

#### Zone - Launch Angle
zone_launch_angle <- function(Batter, df_input, label_input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_LA))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_LA))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_LA),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                scale_fill_gradientn(colours = c("#2166ac",'white', '#b2182b', "#b2182b", '#b2182b', 'white', "#2166ac"),
                                     limits = c(0, 45), na.value = "#2166ac") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_LA, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}

#### Zone - Hit Percentage
zone_hit_perc <- function(Batter, df_input, label_input) {
        
        #### Load App Setup
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_Hits))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_Hits))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Hits),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Hits)),
                                     na.value = "lightgrey") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Hits, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}

#### Zone - Swing Percentage
zone_swing_perc <- function(Batter, df_input, label_input) {
        
        #### Load App Setup
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_Swings))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_Swings))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Swings),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Swings)),
                                     na.value = "lightgrey") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Swings, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}

#### Zone - Swing Percentage
zone_whiff_perc <- function(Batter, df_input, label_input) {
        
        #### Load App Setup
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        #### Fix NA ----
        df_input <- df_input %>%
                filter(!is.na(Freq_Whiffs))
        
        label_input <- label_input %>% 
                filter(!is.na(Freq_Whiffs))
        
        #### Create Plot ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_input, mapping = aes(x, z, group = pitch_zone, fill = Freq_Whiffs),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-20, 20), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_input$Freq_Whiffs)),
                                     na.value = "lightgrey") +
                geom_text(label_input, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Whiffs, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                theme(
                        axis.title = element_blank(),
                        axis.ticks = element_blank(),
                        axis.text.x = element_blank(),
                        axis.text.y = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(), 
                        legend.text = element_text(family = "sans", size = 12),
                        legend.title = element_blank(),
                        legend.position = "right"
                )
}


# Save Data ----
# save(mutate_data, zone_function_setup,
#      
#      batted_ball_type, pitch_result, pitch_types, pitch_description,
#      pitch_heatmap, contact_type, swing_heatmap,
#      
#      zone_swing_perc, zone_hit_perc, zone_launch_angle, zone_exit_velo,
#      zone_total_pitches, zone_pitch_perc, zone_swing_perc, zone_whiff_perc,
#      
#      file = paste0(getwd(),"/data/functions.rda"))



# ----

# library(tidyverse)
# 
# load(paste0(getwd(),"/data/functions.rda"))
# load(paste0(getwd(),"/data/custom_aes.rda"))
# season <- read.csv(paste0(getwd(),"/data/23_season.csv")
# 
# mutate_data(season)
# load(paste0(getwd(),"/data/nu_df.rda"))
# 
# pitch_types("Calarco", nu_df)



# test1 <- function(batter, pitcher, df) {
# 
#   if (is.null(pitcher)) {
#     df <- df %>%
#       filter(Batter == batter & !PlayResult %in% c("Undefined"))
#   }
# 
#   if (is.null(batter)) {
#     df <- df %>%
#       filter(Pitcher == pitcher & !PlayResult %in% c("Undefined"))
#   }
# 
#   ggplot() +
#     coord_equal(xlim = c(-2, 2), ylim = c(0, 4)) +
#     geom_point(df,
#                mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
#                size = 5, shape = 21, position = "jitter") +
#     geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
#     geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
#     geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
#     geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
#     scale_fill_manual(values = PlayResultColors) +
#     theme_minimal() +
#     TH +
#     theme(
#       plot.margin = unit(c(1,1,1,1), "cm"),
#       legend.position = "bottom",
#       legend.background = element_rect(color = "#582c83")
#     )
# }
# 
# test1("Calarco, Alex", NULL, season)
# 
# test1(NULL,"Sund, Ethan",season)




library(tidyverse)
library(sp)
library(sf)

mutate_data_pitcher <- function(input) {
        
        nu_df <- input
        
        nu_df <- nu_df %>% 
                filter(!is.na(RelSpeed)) %>% 
                mutate(
                        PlateLocSide = 12*PlateLocSide,
                        PlateLocHeight = 12*PlateLocHeight
                )
        
        nu_df <- nu_df %>% 
                separate(Pitcher, c("Pitcher_Last", "Pitcher_First"))
        
        nu_df <- nu_df %>% 
                separate(Batter, c("Batter_Last", "Batter_First"))
        
        nu_df <- nu_df %>% mutate(
                PlayResult = case_when(
                        KorBB == "Strikeout" ~ "Strikeout",
                        KorBB == "Walk" ~ "Walk", 
                        PlayResult == "Undefined" ~ NA,
                        TRUE ~ PlayResult),
                
                PlayResult = factor(PlayResult, levels = c("Single", "Double", "Triple", "HomeRun",
                                                           "Walk", "Strikeout", "Out", "Error", "Sacrifice",
                                                           "FieldersChoice")),
                
                AutoHitType = factor(AutoHitType, levels = c("GroundBall", "LineDrive", "FlyBall", "Popup")),
                
                AutoPitchType = case_when(
                        AutoPitchType %in% c("Four-Seam", "Sinker", "Cutter") ~ "Fastball",
                        AutoPitchType == "Splitter" ~ "Changeup",
                        TRUE ~ AutoPitchType),
                
                AutoPitchType = factor(AutoPitchType, levels = c("Fastball", "Changeup", "Curveball", "Slider")),
                
                PitchCall = factor(PitchCall, levels = c("BallCalled", "BallinDirt", "BallIntentional", "InPlay",
                                                         "StrikeCalled", "FoulBall", "StrikeSwinging", "HitByPitch")),
                
                ynhit = case_when(
                        PlayResult %in% c("Single", "Double", "Triple", "HomeRun") ~ 1,
                        PlayResult %in% c("Strikeout", "Out", "Error", "FieldersChoice") ~ 0,
                        PlayResult == "Undefined" ~ NA
                ),
                launch_speed_angle = case_when(
                        ExitSpeed * 1.5 - Angle >= 117 &
                                ExitSpeed + Angle >= 124 &
                                ExitSpeed >= 98 &
                                Angle >= 4 & Angle <= 50 ~ "Barrel",
                        
                        ExitSpeed * 1.5 - Angle >= 111 &
                                ExitSpeed + Angle >= 119 &
                                ExitSpeed >= 95 &
                                Angle >= 0 & Angle <= 52 ~ "Solid_Contact",
                        
                        ExitSpeed * 2 - Angle >= 87 &
                                Angle <= 41 & 
                                ExitSpeed * 2 + Angle <= 175 &
                                ExitSpeed + Angle * 1.3 >= 89 &
                                ExitSpeed >= 59 & Angle <= 72 ~ "Flare_or_Burner",
                        
                        ExitSpeed + Angle * 1.3 <= 112 &
                                ExitSpeed + Angle * 1.55 >= 92 &
                                ExitSpeed >= 72 & Angle <= 86 ~ "Flare_or_Burner",
                        
                        Angle <= 20 &
                                ExitSpeed + Angle * 2.4 >= 98 &
                                ExitSpeed >= 86 & ExitSpeed <= 95 ~ "Flare_or_Burner",
                        
                        ExitSpeed - Angle >= 76 &
                                ExitSpeed + Angle * 2.4 >= 98 &
                                ExitSpeed >= 95 &
                                Angle <= 30 ~ "Flare_or_Burner",
                        
                        ExitSpeed + Angle * 2 >= 116 ~  "Poorly_Under",
                        
                        ExitSpeed + Angle * 2 <= 116 ~  "Poorly_Topped",
                        
                        ExitSpeed <= 59 ~ "Poorly_Weak"
                ),
                
                launch_speed_angle = factor(launch_speed_angle, levels = c(
                        "Barrel", "Solid_Contact", "Flare_or_Burner", "Poorly_Under", "Poorly_Topped", "Poorly_Weak"
                ))
        )
        
        save(nu_df, file = paste0(getwd(),"/data/nu_df.rda"))
}

#### Batted Ball Type
batted_ball_type_pitcher <- function(Pitcher, df) {
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                geom_point(df %>% filter(Pitcher_Last == Pitcher & !is.na(AutoHitType)),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoHitType),
                           size = 5, shape = 21 ) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_manual(values = HitTypeColors) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Pitch Result
pitch_result_pitcher <- function(Pitcher, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                geom_point(df %>% filter(Pitcher_Last == Pitcher & !is.na(PlayResult)),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PlayResult),
                           size = 5, shape = 21 ) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_manual(values = PlayResultColors) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Pitch Types
pitch_types_pitcher <- function(Pitcher, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Pitcher_Last == Pitcher),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoPitchType),
                           size = 5, shape = 21) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
        
}

#### Pitch Desc.
pitch_description_pitcher <- function(Pitcher, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot() +
                geom_point(df %>% filter(Pitcher_Last == Pitcher),
                           mapping = aes(x = PlateLocSide, y = PlateLocHeight, fill = PitchCall),
                           size = 5, shape = 21) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") + 
                scale_fill_manual(values = PitchCallColors) +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}

#### Pitch Heatmap
pitch_heatmap_pitcher <- function(Pitcher, df) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        ggplot(df %>% filter(Pitcher_Last == Pitcher),
               mapping = aes(PlateLocSide, PlateLocHeight)) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) +
                geom_path(sz, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_2, mapping = aes(x, z), lty = 1, color = "black") +
                geom_path(sz_3, mapping = aes(x, z), lty = 1, color = "black") +
                scale_fill_distiller(palette = "RdBu") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                theme_minimal() +
                TH +
                theme(
                        plot.margin = unit(c(1,1,1,1), "cm"),
                        legend.position = "bottom",
                        legend.background = element_rect(color = "#582c83")
                )
}


## Polys ----

#### Zone - Pitch Percent
zone_pitch_perc_pitcher <- function(Pitcher, input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        require(sp)
        require(sf)
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Pitcher_Last == Pitcher) %>%
                dplyr::select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Pitches = n(),
                        EV = mean(ExitSpeed, na.rm = TRUE),
                        LA = mean(Angle, na.rm = TRUE),
                        Hits = mean(ynhit, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
                        Freq_NumPitches = Pitches,
                        Freq_EV = EV,
                        Freq_LA = LA,
                        Freq_Hits = Hits * 100
                )
        
        # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        load(paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Fix NA ----
        df_pitch_polys <- df_pitch_polys %>%
                filter(!is.na(Freq_PitchPercent))
        
        label_coords <- label_coords %>% 
                filter(!is.na(Freq_PitchPercent))
        
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_PitchPercent),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_pitch_polys$Freq_PitchPercent) + 5),
                                     breaks = c(0, max(df_pitch_polys$Freq_PitchPercent) + 5, 4),
                                     na.value = "lightgrey", guide = "legend") +
                geom_text(label_coords, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_PitchPercent, digits = 1))),
                          size = 4, fontface = "bold", family = "mono") +
                theme_minimal() +
                TH
        
        # save(df_pitch_polys, label_coords, file = paste0(getwd(),"/data/sz_poly_results.rda"))
}

#### Zone - Pitch Percent
zone_total_pitches_pitcher <- function(Pitcher, input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        #### Setup ----
        require(sp)
        require(sf)
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Pitcher_Last == Pitcher) %>%
                dplyr::select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Pitches = n(),
                        EV = mean(ExitSpeed, na.rm = TRUE),
                        LA = mean(Angle, na.rm = TRUE),
                        Hits = mean(ynhit, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
                        Freq_NumPitches = Pitches,
                        Freq_EV = EV,
                        Freq_LA = LA,
                        Freq_Hits = Hits * 100
                )
        
        # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        load(paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Fix NA ----
        df_pitch_polys <- df_pitch_polys %>%
                filter(!is.na(Freq_NumPitches))
        
        label_coords <- label_coords %>% 
                filter(!is.na(Freq_NumPitches))
        
        #### GGPLOT ----
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_NumPitches),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdBu", limits = c(0, max(df_pitch_polys$Freq_NumPitches)),
                                     na.value = "lightgrey", guide = "legend") +
                geom_text(label_coords, 
                          mapping = aes(x, z, label = Freq_NumPitches),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                labs(
                        caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
                ) +
                TH
}

#### Zone - Exit Velocity
zone_exit_velo_pitcher <- function(Pitcher, input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        #### Setup ----
        require(sp)
        require(sf)
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Pitcher_Last == Pitcher) %>%
                dplyr::select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Pitches = n(),
                        EV = mean(ExitSpeed, na.rm = TRUE),
                        LA = mean(Angle, na.rm = TRUE),
                        Hits = mean(ynhit, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
                        Freq_NumPitches = Pitches,
                        Freq_EV = EV,
                        Freq_LA = LA,
                        Freq_Hits = Hits * 100
                )
        
        # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        load(paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Fix NA ----
        df_pitch_polys <- df_pitch_polys %>%
                filter(!is.na(Freq_EV))
        
        label_coords <- label_coords %>% 
                filter(!is.na(Freq_EV))
        
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_EV),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdBu",
                                     limits = c(25, max(df_pitch_polys$Freq_EV)),
                                     na.value = "lightgrey", guide = "legend") +
                geom_text(label_coords, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_EV, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                labs(
                        caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
                ) +
                TH
}

#### Zone - Launch Angle
zone_launch_angle_pitcher <- function(Pitcher, input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        #### Setup ----
        require(sp)
        require(sf)
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Pitcher_Last == Pitcher) %>%
                dplyr::select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Pitches = n(),
                        EV = mean(ExitSpeed, na.rm = TRUE),
                        LA = mean(Angle, na.rm = TRUE),
                        Hits = mean(ynhit, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_PitchPercent = (Pitches/sum(Pitches)) * 100,
                        Freq_NumPitches = Pitches,
                        Freq_EV = EV,
                        Freq_LA = LA,
                        Freq_Hits = Hits * 100
                )
        
        # ggplot(df_pitches, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        load(paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_PitchPercent, Freq_NumPitches, Freq_EV, Freq_LA, Freq_Hits) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Fix NA ----
        df_pitch_polys <- df_pitch_polys %>%
                filter(!is.na(Freq_LA))
        
        label_coords <- label_coords %>% 
                filter(!is.na(Freq_LA))
        
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_LA),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_gradientn(colours = c("#2166ac",'white', '#b2182b', "#b2182b", '#b2182b', 'white', "#2166ac"),
                                     limits = c(0, 45), na.value = "#2166ac") +
                geom_text(label_coords, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_LA, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                labs(
                        caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
                ) +
                TH
}

#### Zone - Hit Percentage
zone_hit_perc_pitcher <- function(Pitcher, input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        #### Setup ----
        require(sp)
        require(sf)
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        # ggplot(df, aes(PlateLocSide, PlateLocHeight, color = pitch_zone)) +
        #   geom_point()
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Pitcher_Last == Pitcher) %>%
                dplyr::select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, ynhit, pitch_zone) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Hits = mean(ynhit, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_Hits = Hits * 100
                )
        
        
        load(paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_Hits) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Fix NA ----
        df_pitch_polys <- df_pitch_polys %>%
                filter(!is.na(Freq_Hits))
        
        label_coords <- label_coords %>% 
                filter(!is.na(Freq_Hits))
        
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_Hits),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_pitch_polys$Freq_Hits)),
                                     na.value = "lightgrey") +
                geom_text(label_coords, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_Hits, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                labs(
                        caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
                ) +
                TH
}

#### Zone - Swing Percentage
zone_swing_perc_pitcher <- function(Pitcher, input) {
        
        load(file = paste0(getwd(),"/data/app_setup.rda"))
        
        load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        load(file = paste0(getwd(),"/data/custom_aes.rda"))
        
        #### Setup ----
        require(sp)
        require(sf)
        
        df <- input %>% 
                filter(!is.na(c(PlateLocHeight)))
        
        K_Zone_Points <- SpatialPoints(coords = cbind(df$PlateLocSide, df$PlateLocHeight))
        
        df <- df %>% 
                mutate(
                        pitch_zone = case_when(
                                over(K_Zone_Points, SZ_SpatialPoly_01, returnList = TRUE) == 1 ~ "KZONE_01",
                                over(K_Zone_Points, SZ_SpatialPoly_02, returnList = TRUE) == 1 ~ "KZONE_02",
                                over(K_Zone_Points, SZ_SpatialPoly_03, returnList = TRUE) == 1 ~ "KZONE_03",
                                over(K_Zone_Points, SZ_SpatialPoly_04, returnList = TRUE) == 1 ~ "KZONE_04",
                                over(K_Zone_Points, SZ_SpatialPoly_05, returnList = TRUE) == 1 ~ "KZONE_05",
                                over(K_Zone_Points, SZ_SpatialPoly_06, returnList = TRUE) == 1 ~ "KZONE_06",
                                over(K_Zone_Points, SZ_SpatialPoly_07, returnList = TRUE) == 1 ~ "KZONE_07",
                                over(K_Zone_Points, SZ_SpatialPoly_08, returnList = TRUE) == 1 ~ "KZONE_08",
                                over(K_Zone_Points, SZ_SpatialPoly_09, returnList = TRUE) == 1 ~ "KZONE_09",
                                over(K_Zone_Points, SZ_SpatialPoly_11, returnList = TRUE) == 1 ~ "KZONE_11",
                                over(K_Zone_Points, SZ_SpatialPoly_12, returnList = TRUE) == 1 ~ "KZONE_12",
                                over(K_Zone_Points, SZ_SpatialPoly_13, returnList = TRUE) == 1 ~ "KZONE_13",
                                over(K_Zone_Points, SZ_SpatialPoly_14, returnList = TRUE) == 1 ~ "KZONE_14",
                                
                                PlateLocSide < -10 & PlateLocHeight > 30 |
                                        PlateLocSide < 0 & PlateLocHeight > 42 ~ 'KZONE_11',
                                
                                PlateLocSide > 10 & PlateLocHeight > 30 |
                                        PlateLocSide > 0 & PlateLocHeight > 42 ~ 'KZONE_12',
                                
                                PlateLocSide < -10 & PlateLocHeight < 30 |
                                        PlateLocSide < 0 & PlateLocHeight < 18 ~ 'KZONE_13',
                                
                                PlateLocSide > 10 & PlateLocHeight < 30 |
                                        PlateLocSide > 0 & PlateLocHeight < 18 ~ 'KZONE_14'
                        )
                )
        
        df <- df %>% 
                mutate(
                        swing = ifelse(
                                PitchCall %in% c("StrikeSwinging", "FoulBall", "InPlay"), 1, 0)
                )
        
        #### Data Frame ----
        df_pitches <- df %>%
                filter(Pitcher_Last == Pitcher) %>%
                dplyr::select(Pitcher_Last, PlayResult, AutoPitchType, Angle, ExitSpeed, RelSpeed, swing, pitch_zone) %>%
                group_by(pitch_zone) %>%
                summarise(
                        Swings = mean(swing, na.rm = TRUE)
                ) %>%
                mutate(
                        Freq_SwingPercent = Swings * 100
                )
        
        load(paste0(getwd(),"/data/Strikezone_Polys.rda"))
        
        #### Fortify Polys ----
        df_sz_polys = fortify(Strikezone_Polys, region = "id")
        
        #### Fix Column Names ----
        colnames(df_sz_polys)[1] <- "x"
        colnames(df_sz_polys)[2] <- "z"
        colnames(df_sz_polys)[6] <- "pitch_zone"
        
        #### Join Data and Polys ----
        df_pitch_polys <- full_join(x = df_pitches, y = df_sz_polys, by = "pitch_zone")
        
        #### Labels ----
        labels <- df_pitch_polys %>%
                dplyr::select(pitch_zone, Freq_SwingPercent) %>%
                distinct() %>%
                arrange(pitch_zone)
        
        #### Label Coordinates ----
        label_info <- data.frame(
                pitch_zone = c("KZONE_01", "KZONE_02", "KZONE_03",
                               "KZONE_04", "KZONE_05", "KZONE_06",
                               "KZONE_07", "KZONE_08", "KZONE_09",
                               "KZONE_11", "KZONE_12", "KZONE_13", "KZONE_14"),
                x = c(-20/3, 0, 20/3, -20/3, 0, 20/3, -20/3, 0, 20/3, -10, 10, -10, 10),
                z = c(38, 38, 38, 30, 30, 30, 22, 22, 22, 44, 44, 16, 16)
        )
        
        #### Join Label Coords ----
        label_coords <- full_join(x = labels, y = label_info, by = "pitch_zone")
        
        #### Fix NA ----
        df_pitch_polys <- df_pitch_polys %>%
                filter(!is.na(Freq_SwingPercent))
        
        label_coords <- label_coords %>% 
                filter(!is.na(Freq_SwingPercent))
        
        ggplot() +
                geom_polygon(sz, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_path(sz_2, mapping = aes(x, z), color = "black") +
                geom_path(sz_3, mapping = aes(x, z), color = "black") +
                geom_polygon(kzone_11, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_12, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_13, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(kzone_14, mapping = aes(x, z), fill = "#ededed", color = "black") +
                geom_polygon(df_pitch_polys, mapping = aes(x, z, group = pitch_zone, fill = Freq_SwingPercent),
                             color = "black", show.legend = FALSE) +
                geom_polygon(home_plate, mapping = aes(x, z), fill = "#ededed", color = "lightgrey") +
                coord_equal(xlim = c(-24, 24), ylim = c(0, 54)) +
                scale_fill_distiller(palette = "RdYlBu", limits = c(0, max(df_pitch_polys$Freq_SwingPercent)),
                                     na.value = "lightgrey") +
                geom_text(label_coords, 
                          mapping = aes(x, z, label = sprintf("%0.1f", round(Freq_SwingPercent, digits = 1))),
                          size = 4, fontface = "bold", family="mono") +
                theme_minimal() +
                labs(
                        caption = paste("Date Range: ", min(input$Date), "to", max(input$Date))
                ) +
                TH
}


save(mutate_data_pitcher, batted_ball_type_pitcher, pitch_result_pitcher, pitch_types_pitcher,
     pitch_description_pitcher, pitch_heatmap_pitcher, zone_swing_perc_pitcher, zone_hit_perc_pitcher,
     zone_launch_angle_pitcher, zone_exit_velo_pitcher, zone_total_pitches_pitcher,
     zone_pitch_perc_pitcher, file = paste0(getwd(),"/data/functions_pitcher.rda"))





# library(tidyverse)
# 
# load(paste0(getwd(),"/data/functions.rda"))
# load(paste0(getwd(),"/data/custom_aes.rda"))
# season <- read.csv(paste0(getwd(),"/data/23_season.csv")
# 
# mutate_data(season)
# load(paste0(getwd(),"/data/nu_df.rda"))
# 
# pitch_types("Calarco", nu_df)


















# Define UI for application that draws a histogram
ui <- navbarPage(
        title = "DS8 Analytics",
        bg = "#582c83", inverse = T, underline = TRUE, fillable = TRUE,
        theme = bs_theme(version = 5, bootswatch = "pulse"),
        
        tabPanel("Hitters",
                 tabsetPanel(
                         tabPanel("Illustrator",
                                  sidebarLayout(
                                          sidebarPanel(
                                                  width = 3,
                                                  fileInput("upload_1", "Upload TrackMan .csv File", accept = c(".csv"), multiple = TRUE),
                                                  textInput("hitter_choice", "Enter Player Name", 
                                                            value = "", placeholder = "Last, First"),
                                                  selectInput("chart_choice", "Select Chart",
                                                              choices = chart_types, selected = NULL),
                                                  selectInput("pitcher_throws", "Pitcher Throws:",
                                                              choices = c("All", "Right", "Left")),
                                                  selectInput("pitch_tag", "Pitch Tag Type:",
                                                              choices = c("TaggedPitchType", "AutoPitchType"),
                                                              selected = "TaggedPitchType"),
                                                  actionButton("goButton_1", "Update Page")),
                                          
                                          mainPanel(
                                                  layout_column_wrap(
                                                          width = NULL, height = 600, fill = FALSE,
                                                          style = css(grid_template_columns = "4fr 1fr"),
                                                          plotOutput("hitter_plots"),
                                                          plotOutput("legend_plot")
                                                  )
                                          )
                                  )
                         ),
                         tabPanel("Interactive",
                                  sidebarLayout(
                                          sidebarPanel(
                                                  width = 3,
                                                  fileInput("upload_2", "Upload TrackMan .csv File", accept = c(".csv"), multiple = TRUE),
                                                  textInput("hitter_choice2", "Enter Player Name", 
                                                            value = "", placeholder = "Last, First"),
                                                  selectInput("chart_choice2", "Select Chart",
                                                              choices = c("Pitch Types", 
                                                                          "Pitch Description",
                                                                          "Pitch Result", 
                                                                          "Batted Ball Type",
                                                                          "Contact Type"), 
                                                              selected = NULL),
                                                  selectInput("pitcher_throws2", "Pitcher Throws:",
                                                              choices = c("All", "Right", "Left")),
                                                  selectInput("pitch_tag2", "Pitch Tag Type:",
                                                              choices = c("TaggedPitchType", "AutoPitchType"),
                                                              selected = "TaggedPitchType"),
                                                  actionButton("goButton_2", "Update Page")),
                                          mainPanel(
                                                  layout_column_wrap(
                                                          width = NULL, height = 720, fill = FALSE,
                                                          style = css(grid_template_columns = "3fr 1fr"),
                                                          plotlyOutput("hitter_plotly"),
                                                          plotOutput("legend_plotly"))
                                          )
                                  )
                         )
                 )
        )
)

# Define server logic required to draw a histogram
server <-  function(input, output, session) {
        
        options(shiny.maxRequestSize = 10 * 1024^2)
        
        # Hitters ----
        df_1 <- eventReactive(input$upload_1, {
                combined <- lapply(input$upload_1$datapath, read_csv)
                do.call(rbind, combined)
        })
        df_2 <- eventReactive(input$upload_2, {
                combined <- lapply(input$upload_2$datapath, read_csv)
                do.call(rbind, combined)
        })
        
        data_1 <- eventReactive(input$goButton_1, {
                # Load Files 
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                
                #### Mutate data_1 
                tm_df <- mutate_data(df_1())
                
                # Pitcher Handness 
                if (input$pitcher_throws == "Right") {tm_df <- tm_df %>% filter(PitcherThrows == "Right")}
                else if (input$pitcher_throws == "Left") {tm_df <- tm_df %>% filter(PitcherThrows == "Left")}
                
                tm_df <- tm_df %>% 
                        mutate(
                                TaggedPitchType = case_when(
                                        TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
                                        TRUE ~ TaggedPitchType
                                ))
                
                # Tag Type
                if (input$pitch_tag == "TaggedPitchType") {tm_df$pitch_tag <- tm_df$TaggedPitchType}
                else if (input$pitch_tag == "AutoPitchType") {tm_df$pitch_tag <- tm_df$AutoPitchType}
                
                tm_df <- tm_df %>% 
                        mutate(pitch_tag = fct_infreq(pitch_tag))
                
                return(tm_df)})
        data_2 <- eventReactive(input$goButton_2, {
                # Load Files 
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                
                #### Mutate data_1 
                tm_df <- mutate_data(df_2())
                
                # Pitcher Handness 
                if (input$pitcher_throws2 == "Right") {tm_df <- tm_df %>% filter(PitcherThrows == "Right")}
                else if (input$pitcher_throws2 == "Left") {tm_df <- tm_df %>% filter(PitcherThrows == "Left")}
                
                tm_df <- tm_df %>% 
                        mutate(
                                TaggedPitchType = case_when(
                                        TaggedPitchType == "FourSeamFastBall" ~ "Fastball",
                                        TRUE ~ TaggedPitchType
                                ))
                
                # Tag Type
                if (input$pitch_tag2 == "TaggedPitchType") {tm_df$pitch_tag <- tm_df$TaggedPitchType}
                else if (input$pitch_tag2 == "AutoPitchType") {tm_df$pitch_tag <- tm_df$AutoPitchType}
                
                tm_df <- tm_df %>% 
                        mutate(pitch_tag = fct_infreq(pitch_tag))
                
                return(tm_df)})
        
        poly_df_1 <- eventReactive(input$goButton_1, {
                # Load Files 
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                tm_poly <- zone_poly_setup(data_1(), input$hitter_choice)
                return(tm_poly)
        })
        poly_df_2 <- eventReactive(input$goButton_2, {
                # Load Files 
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                tm_poly <- zone_poly_setup(data_2(), input$hitter_choice2)
                return(tm_poly)
        })
        
        label_df_1 <- eventReactive(input$goButton_1, {
                # Load Files 
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                tm_label <- zone_label_setup(poly_df_1())
                return(tm_label)
        })
        label_df_2 <- eventReactive(input$goButton_2, {
                # Load Files 
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                tm_label <- zone_label_setup(poly_df_2())
                return(tm_label)
        })
        
        hitterPlot <- eventReactive(input$goButton_1, {
                if (input$chart_choice == "Zone - Total Pitches") {
                        plot_zone_total_pitches(input$hitter_choice, poly_df_1(), label_df_1())
                }
                else if (input$chart_choice == "Zone - Pitch Percentage") {
                        plot_zone_pitch_perc(input$hitter_choice, poly_df_1(), label_df_1())
                }
                else if (input$chart_choice == "Zone - Exit Velocity") {
                        plot_zone_exit_velo(input$hitter_choice, poly_df_1(), label_df_1())
                }
                else if (input$chart_choice == "Zone - Launch Angle") {
                        plot_zone_launch_angle(input$hitter_choice, poly_df_1(), label_df_1())
                }
                else if (input$chart_choice == "Zone - Batting Avg.") {
                        plot_zone_hit_perc(input$hitter_choice, poly_df_1(), label_df_1())
                }
                else if (input$chart_choice == "Zone - Swing Percentage") {
                        plot_zone_swing_perc(input$hitter_choice, poly_df_1(), label_df_1())
                }
                else if (input$chart_choice == "Zone - Whiff Percentage") {
                        plot_zone_whiff_perc(input$hitter_choice, poly_df_1(), label_df_1())
                }
                # Scatter Plots ----
                else if (input$chart_choice == "Pitch Types") {
                        plot_pitch_types(input$hitter_choice, data_1())
                }
                else if (input$chart_choice == "Pitch Description") {
                        plot_pitch_description(input$hitter_choice, data_1())
                }
                else if (input$chart_choice == "Pitch Result") {
                        plot_pitch_result(input$hitter_choice, data_1())
                }
                else if (input$chart_choice == "Batted Ball Type") {
                        plot_batted_ball_type(input$hitter_choice, data_1())
                }
                else if (input$chart_choice == "Contact Type") {
                        plot_contact_type(input$hitter_choice, data_1())
                }
                # Heatmaps ----
                else if (input$chart_choice == "Pitch Heatmap") {
                        plot_pitch_heatmap(input$hitter_choice, data_1())
                }
                else if (input$chart_choice == "Swing Heatmap") {
                        plot_swing_heatmap(input$hitter_choice, data_1())
                }
                else if (input$chart_choice == "Hard-Hit Heatmap") {
                        plot_hardhit_heatmap(input$hitter_choice, data_1())
                }
                
        })
        
        hitterPlotly <- eventReactive(input$goButton_2, {
                if (input$chart_choice2 == "Pitch Types") {
                        plotly_pitch_types(input$hitter_choice2, data_2())
                }
                else if (input$chart_choice2 == "Pitch Description") {
                        plotly_pitch_description(input$hitter_choice2, data_2())
                }
                else if (input$chart_choice2 == "Pitch Result") {
                        plotly_pitch_result(input$hitter_choice2, data_2())
                }
                else if (input$chart_choice2 == "Batted Ball Type") {
                        plotly_batted_ball_type(input$hitter_choice2, data_2())
                }
                else if (input$chart_choice2 == "Contact Type") {
                        plotly_contact_type(input$hitter_choice2, data_2())
                }
                
        })
        
        legends_1 <- eventReactive(input$goButton_1, {
                # Load Files
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                
                # Legend Options ----
                type_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = pitch_tag), shape = 21, size = 6) +
                        scale_fill_manual(values = pitch_colors)
                
                result_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = PlayResult), shape = 21, size = 6) +
                        scale_fill_manual(values = result_colors)
                
                call_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = PitchCall), shape = 21, size = 6) +
                        scale_fill_manual(values = call_colors)
                
                hit_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = AutoHitType), shape = 21, size = 6) +
                        scale_fill_manual(values = hit_colors)
                
                contact_legend <- ggplot(data_1() %>% filter(Batter == input$hitter_choice, !is.na(launch_speed_angle))) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = launch_speed_angle), shape = 21, size = 6)
                
                # Plot Legends ----
                plot_legend <- function(legend_choice) {
                        
                        legend <- get_legend(
                                legend_choice +
                                        theme_minimal() +
                                        guides(colour = guide_legend(ncol = 1)) +
                                        theme(
                                                legend.key.size = unit(rel(1.2), 'cm'),
                                                legend.text = element_text(size = rel(1.2)),
                                                legend.title = element_blank(),
                                                legend.box.background = element_blank(),
                                                legend.background = element_blank(),
                                                legend.box = element_blank(),
                                                legend.box.margin = margin(0, -1, 0, -1),
                                                legend.position = "right",
                                                legend.box.just = "center",
                                                legend.direction = "vertical"
                                        ))
                        
                        plot_grid(legend, ncol = 1)
                }
                
                if (input$chart_choice == "Pitch Types") {plot_legend(type_legend)}
                else if (input$chart_choice == "Pitch Description") {plot_legend(call_legend)}
                else if (input$chart_choice == "Pitch Result") {plot_legend(result_legend)}
                else if (input$chart_choice == "Batted Ball Type") {plot_legend(hit_legend)}
                else if (input$chart_choice == "Contact Type") {plot_legend(contact_legend)}
        })
        legends_2 <- eventReactive(input$goButton_2, {
                # Load Files
                load(file = paste0(getwd(),"/data/Strikezone_Polys.rda"))
                load(file = paste0(getwd(),"/data/functions.rda"))
                load(file = paste0(getwd(),"/data/aesthetics.rda"))
                
                # Legend Options ----
                type_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = pitch_tag), shape = 21, size = 6) +
                        scale_fill_manual(values = pitch_colors)
                
                result_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = PlayResult), shape = 21, size = 6) +
                        scale_fill_manual(values = result_colors)
                
                call_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = PitchCall), shape = 21, size = 6) +
                        scale_fill_manual(values = call_colors)
                
                hit_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2)) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = AutoHitType), shape = 21, size = 6) +
                        scale_fill_manual(values = hit_colors)
                
                contact_legend <- ggplot(data_2() %>% filter(Batter == input$hitter_choice2, !is.na(launch_speed_angle))) +
                        geom_point(aes(PlateLocSide, PlateLocHeight, fill = launch_speed_angle), shape = 21, size = 6)
                
                # Plot Legends ----
                plot_legend <- function(legend_choice) {
                        
                        legend <- get_legend(
                                legend_choice +
                                        theme_minimal() +
                                        guides(colour = guide_legend(ncol = 1)) +
                                        theme(
                                                legend.key.size = unit(rel(1.2), 'cm'),
                                                legend.text = element_text(size = rel(1.2)),
                                                legend.title = element_blank(),
                                                legend.box.background = element_blank(),
                                                legend.background = element_blank(),
                                                legend.box = element_blank(),
                                                legend.box.margin = margin(0, -1, 0, -1),
                                                legend.position = "right",
                                                legend.box.just = "center",
                                                legend.direction = "vertical"
                                        ))
                        
                        plot_grid(legend, ncol = 1)
                }
                
                if (input$chart_choice2 == "Pitch Types") {plot_legend(type_legend)}
                else if (input$chart_choice2 == "Pitch Description") {plot_legend(call_legend)}
                else if (input$chart_choice2 == "Pitch Result") {plot_legend(result_legend)}
                else if (input$chart_choice2 == "Batted Ball Type") {plot_legend(hit_legend)}
                else if (input$chart_choice2 == "Contact Type") {plot_legend(contact_legend)}
        })
        
        output$hitter_plots <- renderPlot({hitterPlot()})
        output$legend_plot <- renderPlot({legends_1()})
        
        output$hitter_plotly <- renderPlotly({hitterPlotly()})
        output$legend_plotly <- renderPlot({legends_2()})
}


shinyApp(ui, server)
