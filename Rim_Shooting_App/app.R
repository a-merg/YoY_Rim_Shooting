### --- YoY Rim Shooting --- ###

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(gridExtra)

#setwd("/Users/alexandermerg/Data Science/Basketball/Projects/Rim Shooting/YoY_Rim_Shooting2")
rim_data <- read.csv("rim_data.csv")

# Set up data subset
player_filter <- rim_data %>%
    group_by(player) %>%
    summarize(seasons = n_distinct(season),
              last_season = max(season)) %>%
    filter(last_season == 2020 | seasons > 2)

player_list <- as.list(player_filter$player)

### Load theme for graphics

theme_personal <- function (x) { 
    theme_minimal(base_size=12, base_family="Avenir") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            #panel.grid.major.x = element_blank(),
            panel.grid.major = element_line(color = 'gray91', size = .5),
            #plot.background = element_rect(fill = 'ghostwhite', color = 'ghostwhite'),
            panel.border = element_rect(fill = NA, color = 'gray91', size = 1),
            axis.title.x = element_text(vjust = -1.5, size = 14),
            axis.title.y = element_text(vjust = 5, angle = 90, size = 14),
            plot.margin = margin(25, 15, 12.5, 15),
            plot.caption = element_text(vjust = -1.7, hjust = 1, size = 9, color = 'gray50'),
            plot.title = element_text(size = 20, hjust = 0, vjust = 4.3),
            plot.subtitle = element_text(size = 12, hjust = 0.0, vjust = 5)
        )
}

# Create player list



### Shiny code

# UI code
ui <- dashboardPage(
    dashboardHeader(title = "Y-o-Y Rim Shooting"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Source code", icon = icon("file-code-o"), 
                     href = "https://github.com/rstudio/shinydashboard/")
            )
    ),
    dashboardBody(
        fluidRow(
            box(width = 3,
                selectInput(inputId = "player",
                            label = "Select/Type player name:",
                            choices = as.list(player_list),
                            selectize = TRUE,
                            selected = "Kevin Durant")
                   )
            ),
        fluidRow(
            tabBox(id = "tabset",
                   width = 8,
                   tabPanel("All Attempts", plotOutput("plot1", height = 600)),
                   tabPanel("Non-dunks", plotOutput("plot2", height = 600)))
                   )
        )
    )




# Server logic
server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        
        input$player
        input$tabset
        
        # 
        p1 <- rim_data %>%
            rename(dunk_pct = rim_dunk_pct) %>%
            mutate(dunk_pct_rank = percent_rank(dunk_pct)) %>%
            mutate(ysd = (case_when(ysd == 0 ~ 1, ysd == 1 ~ 2, ysd == 2 ~ 3, ysd == 3 ~ 4,
                                    ysd == 4 ~ 5, ysd == 5 ~ 6, ysd == 6 ~ 7, ysd == 7 ~ 8, ysd == 8 ~ 9,
                                    ysd == 9 ~ 10, ysd == 10 ~ 11, ysd == 11 ~ 12, ysd == 12 ~ 13,
                                    ysd == 13 ~ 14, ysd == 14 ~ 15, ysd == 15 ~ 16, ysd == 16 ~ 17,
                                    ysd == 17 ~ 18))) %>%
            mutate(team = str_replace(team, "NOK", "NOP"),
                   team = str_replace(team, "NOH", "NOP")) %>%
            filter(
                player == input$player,
                non_dunk_rim_fga > 24) %>%
            select(2:9,
                   rim_accuracy_dunk_avg, rim_fga, rim_accuracy, rim_accuracy_avg) %>%
            arrange(ysd) %>%
            mutate(new_team = ifelse(lead(team) == team, 0, 1),
                   team_label = lead(team)) %>%
            replace_na(list(new_team = 0))
        
        vlines <- p1$ysd[p1$new_team == 1]
        
        
        p2 <- ggplot(data = p1, aes(x = ysd)) +
            geom_vline(
                xintercept = vlines + if(max(p1$ysd) <= 7){.06} 
                else if(max(p1$ysd) <= 9){.07}
                else if(max(p1$ysd) <= 12){.085}
                else {.13}, 
                color = "gray85", 
                linetype = "dashed", 
                size = .6) +
            geom_text(mapping = aes(label = ifelse(new_team == 1, `team_label`, ""), 
                                    y = if(max(`rim_accuracy`) > .77){.9}
                                    else if(max(`rim_accuracy`) > .67){.8}
                                    else{.7}),
                      nudge_x = if(max(p1$ysd) <= 7){.195}
                      else if(max(p1$ysd) <= 9){.24}
                      else if(max(p1$ysd) <= 12){.32}
                      else if(max(p1$ysd) <= 15){.4}
                      else {.47},
                      nudge_y = -.015,
                      color = "gray65",
                      size = 4.6) +
            geom_text(mapping = aes(label = ifelse(ysd == min(ysd) & p1[1, 13] != 1, `team`, ""), 
                                    y = if(max(`rim_accuracy`) > .77){.9}
                                    else if(max(`rim_accuracy`) > .67){.8}
                                    else{.7}),
                      nudge_x = if(max(p1$ysd) <= 7){.05} 
                      else if(max(p1$ysd) <= 9){.07}
                      else if(max(p1$ysd) <= 12){.07}
                      else if(max(p1$ysd) <= 15){.08} 
                      else {.1},
                      nudge_y = -.015,
                      family = "Avenir",
                      color = "gray65",
                      size = 4.6) +
            geom_line(aes(y = rim_accuracy_dunk_avg, color = "gray52"), size = .8) +
            geom_line(aes(y = rim_accuracy_avg, color = "steelblue1"), size = .8) +
            geom_line(aes(y = rim_accuracy, color = "dodgerblue3"), size = 1) +
            geom_point(aes(y = rim_accuracy, color = "dodgerblue3", size = pmin(rim_fga, 300))) +
            scale_color_manual(values = c("dodgerblue3", "steelblue1", "gray52"),
                               labels = c("Rim FG%", "Dunk group avg**", "League avg")) +
            scale_size_continuous(
                name = "Rim attempts",
                range = c(1, 8),
                limits = c(25, 700),
                breaks = c(25, 100, 200, 300),
                labels = c("25", "100", "200", "300+")
            ) +
            theme_personal() +
            theme(panel.grid.major.x = element_blank(),
                  legend.position = "top",
                  panel.border = element_blank(),
                  plot.margin = margin(30, 35, 12.5, 38.5),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14),
                  plot.title = element_text(size = 26),
                  legend.box = "horizontal",
                  plot.subtitle = element_text(size = 15, color = "gray45")) +
            scale_y_continuous(
                labels = function(x) paste0(round(as.numeric(x*100)), "%"), 
                breaks = seq(
                    if(min(p1$rim_accuracy) <= .31){.2}
                    else if(min(p1$rim_accuracy) <= .41){.3}
                    else if(min(p1$rim_accuracy) <= .59){.4}
                    else{.5}, 
                    if(max(p1$rim_accuracy) > .77){.9}
                    else if(max(p1$rim_accuracy) > .67){.8}
                    else{.7}, .1), 
                limits = c(
                    if(min(p1$rim_accuracy) <= .31){.2}
                    else if(min(p1$rim_accuracy) <= .41){.3}
                    else if(min(p1$rim_accuracy) <= .59){.4}
                    else{.5},
                    if(max(p1$rim_accuracy) > .77){.9}
                    else if(max(p1$rim_accuracy) > .67){.8}
                    else{.7}),
                expand = c(.01, .01)) +
            scale_x_continuous(
                limits = c(min(p1$ysd), ifelse(max(p1$ysd) <= 6, 6, max(p1$ysd))), 
                breaks = seq(min(p1$ysd), ifelse(max(p1$ysd) <= 6, 6, max(p1$ysd)), 1),
                expand = c(.01, .01)) + 
            labs(y = NULL,
                 x = NULL,
                 title = input$player,
                 subtitle = "Rim accuracy progression compared to peer benchmarks (top) and context metrics (bottom)") +
            guides(
                size = guide_legend(
                    nrow = 1,
                    title.position = 'top',
                    title.hjust = .5,
                    title.vjust = -.7),
                color = guide_legend(
                    title = NULL,
                    ncol = 1,
                    order = 1)
            )
        
        
        p3 <- rim_data %>%
            mutate(dunk_pct_rank = percent_rank(rim_dunk_pct),
                   blocked_pct_rank = percent_rank(rim_pct_blocked),
                   self_created_pct_rank = percent_rank(self_created_rim_pct)) %>%
            mutate(ysd = (case_when(ysd == 0 ~ 1, ysd == 1 ~ 2, ysd == 2 ~ 3, ysd == 3 ~ 4, 
                                    ysd == 4 ~ 5, ysd == 5 ~ 6, ysd == 6 ~ 7, ysd == 7 ~ 8, 
                                    ysd == 8 ~ 9, ysd == 9 ~ 10, ysd == 10 ~ 11, ysd == 11 ~ 12, 
                                    ysd == 12 ~ 13, ysd == 13 ~ 14, ysd == 14 ~ 15, ysd == 15 ~ 16, 
                                    ysd == 16 ~ 17, ysd == 17 ~ 18
            ))) %>%
            mutate(team = str_replace(team, "NOK", "NOP"),
                   team = str_replace(team, "NOH", "NOP")) %>%
            filter(player == input$player,
                   non_dunk_rim_fga > 24) %>%
            select(2:9, 
                   rim_dunk_pct, rim_pct_blocked, self_created_rim_pct)  %>%
            pivot_longer(
                cols = contains("rim"),
                names_to = "metric",
                values_to = "value"
            )
        
        
        p4 <- ggplot(data = p3, aes(x = ysd)) +
            geom_vline(
                xintercept = vlines + if(max(p3$ysd) <= 7){.06} 
                else if(max(p3$ysd) <= 9){.07}
                else if(max(p3$ysd) <= 12){.085}
                else {.13}, 
                color = "gray85", 
                linetype = "dashed", 
                size = .6) +
            geom_line(aes(y = value, color = metric), size = .8) +
            scale_color_manual(values = c("steelblue3", "darkseagreen4", "indianred3"),
                               labels = c("Dunk %", "Blocked %", "Self created %")) +
            theme_personal() +
            scale_x_continuous(
                limits = c(min(p1$ysd), ifelse(max(p3$ysd) <= 6, 6, max(p3$ysd))), 
                breaks = seq(min(p1$ysd), ifelse(max(p3$ysd) <= 6, 6, max(p3$ysd)), 1),
                expand = c(.01, .01)) +
            scale_y_continuous(
                labels = function(x) paste0(round(as.numeric(x*100)), "%"), 
                breaks = seq(0, if(max(p3$value) > .89){1} 
                             else if(max(p3$value) > .79){.9}
                             else{.8}, .2), 
                limits = c(0, if(max(p3$value) > .89){1} 
                           else if(max(p3$value) > .79){.9}
                           else{.8}),
                expand = c(.01, .01)) +
            labs(x = "Year in NBA",
                 y = NULL,
                 caption = "**League-wide player dunk rates were grouped into quantiles of 6 for each season. 
    Plot displays the rim accuracy for the player's dunk group for a given season.") +
            theme(legend.position = "bottom",
                  panel.grid.major.x = element_blank(),
                  panel.border = element_blank(),
                  plot.margin = margin(25, 35, 12.5, 38.5),
                  axis.title.x = element_text(size = 17, vjust = -2.5),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.box.spacing = unit(.6, 'cm'),
                  plot.caption = element_text(size = 11, hjust = 0)) +
            guides(
                color = guide_legend(
                    title = NULL,
                    nrow = 1
                ))
        
        
        grid.arrange(p2, p4, ncol = 1, heights = c(4.5,3))
        
    })
    
    
    
    output$plot2 <- renderPlot({
        
        input$player
        
        p5 <- rim_data %>%
            rename(dunk_pct = rim_dunk_pct) %>%
            mutate(dunk_pct_rank = percent_rank(dunk_pct)) %>%
            mutate(ysd = (case_when(ysd == 0 ~ 1, ysd == 1 ~ 2, ysd == 2 ~ 3, ysd == 3 ~ 4,
                                    ysd == 4 ~ 5, ysd == 5 ~ 6, ysd == 6 ~ 7, ysd == 7 ~ 8, ysd == 8 ~ 9,
                                    ysd == 9 ~ 10, ysd == 10 ~ 11, ysd == 11 ~ 12, ysd == 12 ~ 13,
                                    ysd == 13 ~ 14, ysd == 14 ~ 15, ysd == 15 ~ 16, ysd == 16 ~ 17,
                                    ysd == 17 ~ 18))) %>%
            mutate(team = str_replace(team, "NOK", "NOP"),
                   team = str_replace(team, "NOH", "NOP")) %>%
            filter(
                player == input$player,
                non_dunk_rim_fga > 24) %>%
            select(2:9,
                   non_dunk_rim_accuracy_dunk_avg, non_dunk_rim_fga, 
                   non_dunk_rim_accuracy, non_dunk_rim_accuracy_avg) %>%
            arrange(ysd) %>%
            mutate(new_team = ifelse(lead(team) == team, 0, 1),
                   team_label = lead(team)) %>%
            replace_na(list(new_team = 0))
        
        vlines <- p5$ysd[p5$new_team == 1]
        
        
        p6 <- ggplot(data = p5, aes(x = ysd)) +
            geom_vline(
                xintercept = vlines + if(max(p5$ysd) <= 7){.06} 
                else if(max(p5$ysd) <= 9){.07}
                else if(max(p5$ysd) <= 12){.085}
                else {.13}, 
                color = "gray85", 
                linetype = "dashed", 
                size = .6) +
            geom_text(mapping = aes(label = ifelse(new_team == 1, `team_label`, ""), 
                                    y = if(max(`non_dunk_rim_accuracy`) > .77){.9}
                                    else if(max(`non_dunk_rim_accuracy`) > .67){.8}
                                    else{.7}),
                      nudge_x = if(max(p5$ysd) <= 7){.195}
                      else if(max(p5$ysd) <= 9){.24}
                      else if(max(p5$ysd) <= 12){.32}
                      else if(max(p5$ysd) <= 15){.4}
                      else {.47},
                      nudge_y = -.015,
                      color = "gray65",
                      size = 4.6) +
            geom_text(mapping = aes(label = ifelse(ysd == min(ysd) & p5[1, 13] != 1, `team`, ""), 
                                    y = if(max(`non_dunk_rim_accuracy`) > .77){.9}
                                    else if(max(`non_dunk_rim_accuracy`) > .67){.8}
                                    else{.7}),
                      nudge_x = if(max(p5$ysd) <= 7){.05} 
                      else if(max(p5$ysd) <= 9){.07}
                      else if(max(p5$ysd) <= 12){.07}
                      else if(max(p5$ysd) <= 15){.08} 
                      else {.1},
                      nudge_y = -.015,
                      color = "gray65",
                      size = 4.6) +
            geom_line(aes(y = non_dunk_rim_accuracy_dunk_avg, color = "gray52"), size = .8) +
            geom_line(aes(y = non_dunk_rim_accuracy_avg, color = "steelblue1"), size = .8) +
            geom_line(aes(y = non_dunk_rim_accuracy, color = "dodgerblue3"), size = 1) +
            geom_point(aes(y = non_dunk_rim_accuracy, color = "dodgerblue3", size = pmin(non_dunk_rim_fga, 300))) +
            scale_size_continuous(
                name = "Non-dunk rim attempts",
                range = c(1, 8),
                limits = c(25, 700),
                breaks = c(25, 100, 200, 300),
                labels = c("25", "100", "200", "300+")
            ) +
            scale_color_manual(values = c("dodgerblue3","steelblue1", "gray52"),
                               labels = c("Rim FG% - Non-dunks", "Dunk group avg**", "League avg")) +
            theme_personal() +
            theme(panel.grid.major.x = element_blank(),
                  legend.position = "top",
                  panel.border = element_blank(),
                  plot.margin = margin(30, 35, 12.5, 38.5),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14),
                  plot.title = element_text(size = 26),
                  legend.box = "horizontal",
                  plot.subtitle = element_text(size = 15, color = "gray45")) +
            scale_y_continuous(
                labels = function(x) paste0(round(as.numeric(x*100)), "%"), 
                breaks = seq(
                    if(min(p5$non_dunk_rim_accuracy) <= .31){.2}
                    else if(min(p5$non_dunk_rim_accuracy) <= .41){.3}
                    else{.4},
                    if(max(p5$non_dunk_rim_accuracy) > .77){.9}
                    else if(max(p5$non_dunk_rim_accuracy) > .67){.8}
                    else{.7}, .1), 
                limits = c(
                    if(min(p5$non_dunk_rim_accuracy) <= .31){.2}
                    else if(min(p5$non_dunk_rim_accuracy) <= .41){.3}
                    else{.4}, 
                    if(max(p5$non_dunk_rim_accuracy) > .77){.9}
                    else if(max(p5$non_dunk_rim_accuracy) > .67){.8}
                    else{.7}),
                expand = c(.01, .01)) +
            scale_x_continuous(
                limits = c(min(p5$ysd), ifelse(max(p5$ysd) <= 6, 6, max(p5$ysd))), 
                breaks = seq(min(p5$ysd), ifelse(max(p5$ysd) <= 6, 6, max(p5$ysd)), 1),
                expand = c(.01, .01)) + 
            labs(y = NULL,
                 x = NULL,
                 title = input$player,
                 subtitle = "Rim accuracy progression compared to peer benchmarks (top) and context metrics (bottom)") +
            guides(
                size = guide_legend(
                    nrow = 1,
                    title.position = 'top',
                    title.hjust = .5,
                    title.vjust = -.7),
                color = guide_legend(
                    title = NULL,
                    ncol = 1,
                    order = 1)
            )
        
        p9 <- rim_data %>%
            mutate(dunk_pct_rank = percent_rank(rim_dunk_pct),
                   blocked_pct_rank = percent_rank(rim_pct_blocked),
                   self_created_pct_rank = percent_rank(self_created_rim_pct)) %>%
            mutate(ysd = (case_when(ysd == 0 ~ 1, ysd == 1 ~ 2, ysd == 2 ~ 3, ysd == 3 ~ 4, 
                                    ysd == 4 ~ 5, ysd == 5 ~ 6, ysd == 6 ~ 7, ysd == 7 ~ 8, 
                                    ysd == 8 ~ 9, ysd == 9 ~ 10, ysd == 10 ~ 11, ysd == 11 ~ 12, 
                                    ysd == 12 ~ 13, ysd == 13 ~ 14, ysd == 14 ~ 15, ysd == 15 ~ 16, 
                                    ysd == 16 ~ 17, ysd == 17 ~ 18
            ))) %>%
            mutate(team = str_replace(team, "NOK", "NOP"),
                   team = str_replace(team, "NOH", "NOP")) %>%
            filter(player == input$player,
                   non_dunk_rim_fga > 24) %>%
            select(2:9, 
                   rim_dunk_pct, rim_pct_blocked, self_created_rim_pct)  %>%
            pivot_longer(
                cols = contains("rim"),
                names_to = "metric",
                values_to = "value"
            )
        
        
        p10 <- ggplot(data = p9, aes(x = ysd)) +
            geom_vline(
                xintercept = vlines + if(max(p9$ysd) <= 7){.06} 
                else if(max(p9$ysd) <= 9){.07}
                else if(max(p9$ysd) <= 12){.085}
                else {.13}, 
                color = "gray85", 
                linetype = "dashed", 
                size = .6) +
            geom_line(aes(y = value, color = metric), size = .8) +
            scale_color_manual(values = c("steelblue3", "darkseagreen4", "indianred3"),
                               labels = c("Dunk %", "Blocked %", "Self created %")) +
            theme_personal() +
            scale_x_continuous(
                limits = c(min(p5$ysd), ifelse(max(p9$ysd) <= 6, 6, max(p9$ysd))), 
                breaks = seq(min(p5$ysd), ifelse(max(p9$ysd) <= 6, 6, max(p9$ysd)), 1),
                expand = c(.01, .01)) +
            scale_y_continuous(
                labels = function(x) paste0(round(as.numeric(x*100)), "%"), 
                breaks = seq(0, if(max(p9$value) > .89){1} 
                             else if(max(p9$value) > .79){.9}
                             else{.8}, .2), 
                limits = c(0, if(max(p9$value) > .89){1} 
                           else if(max(p9$value) > .79){.9}
                           else{.8}),
                expand = c(.01, .01)) +
            labs(x = "Year in NBA",
                 y = NULL,
                 caption = "**League-wide player dunk rates were grouped into quantiles of 6 for each season. Plot
    displays the rim accuracy on non-dunks for the player's dunk group for a given season.") +
            theme(legend.position = "bottom",
                  panel.grid.major.x = element_blank(),
                  panel.border = element_blank(),
                  plot.margin = margin(25, 35, 12.5, 38.5),
                  axis.title.x = element_text(size = 17, vjust = -2.5),
                  axis.text = element_text(size = 12),
                  legend.text = element_text(size = 12),
                  legend.box.spacing = unit(.6, 'cm'),
                  plot.caption = element_text(size = 11, hjust = 0)) +
            guides(
                color = guide_legend(
                    title = NULL,
                    nrow = 1
                ))
        
        
        grid.arrange(p6, p10, ncol = 1, heights = c(4.5,3))
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
