##########################################################################################################
# PLOT ROUND STATS WITH JUDGING DECISION AND PREDICTED SCORE PROBABILITIES

Plot_Round_Judging_Stats <- function(round_df, stats_dict, paired_colors, use_dark_theme, 
                                     bar_plot, single_plot, include_twitter, 
                                     single_plot_theme, multi_plot_theme, dark_theme){
  # This function plots each fighters stats for a given round
  # round_df = data.table of a single round of a fight
  # stats_dict = data.table mapping stat column names to labels
  # paired_colors = vector of 4 line colors (order: dark red, light red, dark blue, light blue)
  # use_dark_theme = boolean to determine whether to display the figure with a dark theme
  # bar_plot = booalean to determine whether to display bars or lines
  # single_plot = boolean to determine whether plot is pa standalone figure
  # include_twitter = boolean to determine whether to include Twitter handle
  # single_plot_theme = theme object for single plots
  # multi_plot_theme = theme object for multiple plots on a single figure
  # dark theme = theme object using a dark theme
  
  round_df <- copy(round_df)
  # define measure variables for melt
  if(single_plot == T){
    measure_vars <- c('F1_Rev', 'F2_Rev', 'F1_SUBa', 'F2_SUBa', 'F1_CtrlMin', 'F2_CtrlMin', 'F1_TDl', 'F2_TDl', 
                      'F1_SSl_Legs', 'F2_SSl_Legs', 'F1_SSl_Body', 'F2_SSl_Body', 'F1_SSl_Head', 'F2_SSl_Head', 
                      'F1_KD', 'F2_KD', 'F1_SSl', 'F2_SSl', 'F1_TSl', 'F2_TSl')
  } else {
    measure_vars <- c('F1_SSl_Legs', 'F2_SSl_Legs', 'F1_SSl_Body', 'F2_SSl_Body', 'F1_SSl_Head', 'F2_SSl_Head', 
                      'F1_TDl', 'F2_TDl', 'F1_CtrlMin', 'F2_CtrlMin', 'F1_KD', 'F2_KD', 'F1_TSl', 'F2_TSl', 'F1_SSl', 'F2_SSl')
  }
  # convert data types
  round_df[, (measure_vars) := lapply(.SD, as.numeric), .SDcols = measure_vars]
  round_df[, `:=`(F1_CtrlMin = round(F1_CtrlMin, 1), F2_CtrlMin = round(F2_CtrlMin, 1))]
  # define ID variables for melt
  id_vars <- c('EventDate', 'Round', 'F1_Name', 'F2_Name', 'Judges_F1_Round_Win', 
               'Judge1', 'Judge1_Score', 'Judge2', 'Judge2_Score', 'Judge3', 'Judge3_Score', 'model_round_score', 
               'probs_10_8', 'probs_10_9', 'probs_9_10', 'probs_8_10')
  # melt round-level data.table
  plot_df <- melt(round_df[, c(id_vars, measure_vars), with = F], 
                  id.vars = id_vars, 
                  measure.vars = measure_vars, 
                  variable.name = 'Stat_Name', 
                  value.name = 'Stat_Value')
  plot_df[, Stat_Name := factor(Stat_Name, levels = measure_vars)]
  # create columns for plot
  plot_df[, Stat_Abbrev := gsub('F1_|F2_', '', Stat_Name)]
  plot_df[, Stat_Abbrev := factor(Stat_Abbrev, levels = unique(gsub('F1_|F2_', '', measure_vars)))]
  plot_df[, greater_than_opp := max(Stat_Value), by = Stat_Abbrev][, greater_than_opp := Stat_Value == greater_than_opp]
  plot_df[, Stat_Label := Stat_Value]
  plot_df[grepl('F1', Stat_Name), Stat_Value := -Stat_Value]
  plot_df[grepl('F1', Stat_Name), Fighter_Name := unique(F1_Name)]
  plot_df[grepl('F2', Stat_Name), Fighter_Name := unique(F2_Name)]
  plot_df[grepl('F1_', Stat_Name), Stat_Label_hjust_line := 2]
  plot_df[grepl('F2_', Stat_Name), Stat_Label_hjust_line := -1]
  plot_df[grepl('F1_', Stat_Name), Stat_Label_hjust_bar := 1.4]
  plot_df[grepl('F2_', Stat_Name), Stat_Label_hjust_bar := -.5]
  plot_df[grepl('F1_', Stat_Name) & greater_than_opp == T, `:=`(Line_Color = paired_colors[1], Fighter_Label = paste0(Fighter_Name, ' '))]
  plot_df[grepl('F1_', Stat_Name) & greater_than_opp == F, `:=`(Line_Color = paired_colors[2], Fighter_Label = Fighter_Name)]
  plot_df[grepl('F2_', Stat_Name) & greater_than_opp == T, `:=`(Line_Color = paired_colors[3], Fighter_Label = paste0(Fighter_Name, ' '))]
  plot_df[grepl('F2_', Stat_Name) & greater_than_opp == F, `:=`(Line_Color = paired_colors[4], Fighter_Label = Fighter_Name)]
  plot_df[, Fighter_Label := factor(Fighter_Label, levels = c(paste0(unique(F1_Name), ' '), 
                                                              unique(F1_Name), 
                                                              paste0(unique(F2_Name), ' '), 
                                                              unique(F2_Name)))]
  # merge stat labels onto plot_df
  setkey(stats_dict, Column)
  setkey(plot_df, Stat_Abbrev)
  plot_df[stats_dict, Stat_Display_Name := i.Label]
  # create y-axis breaks
  y_breaks <- Create_Axis_Breaks(plot_df[, abs(Stat_Value)])
  # create custom themes for plot
  if(single_plot == T){
    custom_theme <- single_plot_theme
    label_size <- 2.5
  } else if (single_plot == F){
    custom_theme <- multi_plot_theme
    label_size <- 4
  }
  
  if(use_dark_theme == F){
    dark_theme <- theme()
    plot_theme <- theme_bw()
    label_color <- 'black'
  } else if (use_dark_theme == T){
    dark_theme <- dark_theme
    plot_theme <- dark_theme_gray()
    label_color <- 'white'
  }
  # create custom labels
  if(single_plot == T){
    custom_title <- plot_df[, paste0('Judging Overview for Round ', unique(Round), ': ', unique(F1_Name), ' vs ', unique(F2_Name))]
    custom_subtitle <- plot_df[, paste0('Official Scores: ', 
                                        unique(Judge1), ' ', unique(Judge1_Score), ', ', 
                                        unique(Judge2), ' ', unique(Judge2_Score), ', ', 
                                        unique(Judge3), ' ', unique(Judge3_Score), 
                                        ' \n',
                                        'JudgeAI Score: ', unique(model_round_score),
                                        ' \n', 
                                        'JudgeAI Probabilities: ', round(100 * unique(probs_10_8)), '% 10-8', 
                                        ' || ', round(100 * unique(probs_10_9)), '% 10-9', 
                                        ' || ', round(100 * unique(probs_9_10)), '% 9-10', 
                                        ' || ', round(100 * unique(probs_8_10)), '% 8-10')]
  } else if (single_plot == F){
    custom_title <- plot_df[, paste0('Round ', unique(Round), ' Official Scores: ', 
                                     unique(Judge1), ' ', unique(Judge1_Score), ', ', 
                                     unique(Judge2), ' ', unique(Judge2_Score), ', ', 
                                     unique(Judge3), ' ', unique(Judge3_Score), 
                                     ' \n',
                                     'JudgeAI Score: ', unique(model_round_score),
                                     ' \n', 
                                     'JudgeAI Probabilities: ', round(100 * unique(probs_10_8)), '% 10-8', 
                                     ' || ', round(100 * unique(probs_10_9)), '% 10-9', 
                                     ' || ', round(100 * unique(probs_9_10)), '% 9-10', 
                                     ' || ', round(100 * unique(probs_8_10)), '% 8-10')]
    custom_subtitle <- ''
    
  }
  # include Twitter, if specified
  if(include_twitter == T){
    custom_tag <- 'Twitter: @NateLatshaw'
  } else {
    custom_tag <- ''
  }
  # create plot
  p <- ggplot(plot_df, aes(x = Stat_Abbrev, y = Stat_Value)) + 
    geom_hline(yintercept = 0, linetype = 'dashed', color = label_color) + 
    scale_y_continuous(breaks = unique(c(-y_breaks, y_breaks)), 
                       labels = abs(unique(c(-y_breaks, y_breaks))), 
                       limits = c(min(-y_breaks), max(y_breaks))) + 
    scale_x_discrete(labels = plot_df[order(Stat_Name)][, unique(Stat_Display_Name)]) + 
    labs(title = custom_title, 
         subtitle = custom_subtitle, 
         x = '', 
         y = '', 
         color = paste0('Greater performance in category: ', '\nLesser performance in category:'), 
         fill = paste0('Greater performance in category: ', '\nLesser performance in category:'), 
         tag = custom_tag) + 
    coord_flip() + 
    plot_theme + 
    custom_theme + 
    dark_theme
  # toggle between bars and lines
  if(bar_plot == T){
    p <- p + geom_bar(width = .8, stat = 'identity', aes(fill = Fighter_Label)) +  
      geom_text(aes(x = Stat_Abbrev, y = Stat_Value, label = Stat_Label, hjust = Stat_Label_hjust_bar), color = label_color, vjust = .5, fontface = 'bold', size = label_size) + 
      scale_fill_manual(values = paired_colors, drop = F) + 
      guides(fill = guide_legend(nrow = 2, title.position = 'left', title.vjust = .7, override.aes = list(size = 1)))
  } else if (bar_plot == F){
    p <- p + geom_segment(aes(x = Stat_Abbrev, xend = Stat_Abbrev, y = 0, yend = Stat_Value, color = Fighter_Label), size = 5) + 
      geom_text(aes(x = Stat_Abbrev, y = Stat_Value, label = Stat_Label, hjust = Stat_Label_hjust_line), color = label_color, vjust = .5, fontface = 'bold', size = label_size) + 
      scale_color_manual(values = paired_colors, drop = F) + 
      guides(color = guide_legend(nrow = 2, title.position = 'left', title.vjust = .7))
  }
  # return plot
  return(p)
}

##########################################################################################################
# PLOT FIGHT SCORE PROBABILITES

Plot_Fight_Score_Probabilties <- function(fight_df, n_samples, use_dark_theme, single_plot, include_twitter, 
                                          single_plot_theme, multi_plot_theme, dark_theme){
  # This function plots final score probabilities on a heat map
  # fight_df = round-level data.table for a single fight
  # n_samples = number of times matches to simulate to recover score probabilities
  # use_dark_theme = boolean to determine whether to display the figure with a dark theme
  # single_plot = boolean to determine whether plot is pa standalone figure
  # include_twitter = boolean to determine whether to include Twitter handle
  # single_plot_theme = theme object for single plots
  # multi_plot_theme = theme object for multiple plots on a single figure
  # dark theme = theme object using a dark theme
  
  set.seed(123)
  # identify number of rounds
  num_rounds <- fight_df[, max(Round)]
  # create data.table for heat map plot
  heat_map_df <- copy(fight_df[, .(EventDate, F1_Name, F2_Name, Round, 
                                   probs_10_8, probs_10_9, probs_9_10, probs_8_10, 
                                   Judge1, Judge1_Score, Judge2, Judge2_Score, 
                                   Judge3, Judge3_Score, 
                                   model_round_score)][order(Round)])
  # create final score probabilities for each fighter
  if(num_rounds == 3){
    heat_map_probs <- data.table(Round1_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[1, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]), 
                                 Round2_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[2, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]), 
                                 Round3_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[3, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]))
    
    heat_map_probs[, F1_Scores := as.numeric(gsub('-.*', '', Round1_Scores)) + 
                     as.numeric(gsub('-.*', '', Round2_Scores)) + 
                     as.numeric(gsub('-.*', '', Round3_Scores))]
    heat_map_probs[, F2_Scores := as.numeric(gsub('.*-', '', Round1_Scores)) + 
                     as.numeric(gsub('.*-', '', Round2_Scores)) + 
                     as.numeric(gsub('.*-', '', Round3_Scores))]
    heat_map_probs[, Total_Scores := paste0(F1_Scores, '-', F2_Scores)]
  } else if (num_rounds == 5){
    heat_map_probs <- data.table(Round1_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[1, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]), 
                                 Round2_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[2, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]), 
                                 Round3_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[3, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]), 
                                 Round4_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[4, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]), 
                                 Round5_Scores = sample(x = c('10-8', '10-9', '9-10', '8-10'), 
                                                        size = n_samples, 
                                                        replace = T, 
                                                        prob = heat_map_df[5, c(probs_10_8, probs_10_9, probs_9_10, probs_8_10)]))
    
    heat_map_probs[, F1_Scores := as.numeric(gsub('-.*', '', Round1_Scores)) + 
                     as.numeric(gsub('-.*', '', Round2_Scores)) + 
                     as.numeric(gsub('-.*', '', Round3_Scores)) + 
                     as.numeric(gsub('-.*', '', Round4_Scores)) + 
                     as.numeric(gsub('-.*', '', Round5_Scores))]
    heat_map_probs[, F2_Scores := as.numeric(gsub('.*-', '', Round1_Scores)) + 
                     as.numeric(gsub('.*-', '', Round2_Scores)) + 
                     as.numeric(gsub('.*-', '', Round3_Scores)) + 
                     as.numeric(gsub('.*-', '', Round4_Scores)) + 
                     as.numeric(gsub('.*-', '', Round5_Scores))]
    heat_map_probs[, Total_Scores := paste0(F1_Scores, '-', F2_Scores)]
  } else {
    stop('Check number of rounds in the fight.')
  }
  # create plot df
  full_plot_df <- heat_map_probs[, .N, .(F1_Scores, F2_Scores, Total_Scores)]
  full_plot_df[, score_probability := N / sum(N)]
  full_plot_df <- cbind(full_plot_df, unique(heat_map_df[, .(EventDate, F1_Name, F2_Name, Max_Round = max(Round), 
                                                             Judge1, Judge2, Judge3, 
                                                             model_score = paste0(sum(as.numeric(gsub('-.*', '', model_round_score))), 
                                                                                  '-', 
                                                                                  sum(as.numeric(gsub('.*-', '', model_round_score)))), 
                                                             Judge1_Score = paste0(sum(as.numeric(gsub('-.*', '', Judge1_Score))), 
                                                                                   '-', 
                                                                                   sum(as.numeric(gsub('.*-', '', Judge1_Score)))), 
                                                             Judge2_Score = paste0(sum(as.numeric(gsub('-.*', '', Judge2_Score))), 
                                                                                   '-', 
                                                                                   sum(as.numeric(gsub('.*-', '', Judge2_Score)))), 
                                                             Judge3_Score = paste0(sum(as.numeric(gsub('-.*', '', Judge3_Score))), 
                                                                                   '-', 
                                                                                   sum(as.numeric(gsub('.*-', '', Judge3_Score)))))]))
  if(num_rounds == 3){
    plot_df <- full_plot_df[F1_Scores >= full_plot_df[score_probability >= .01, min(min(F1_Scores), 27)] & F2_Scores >= full_plot_df[score_probability >= .01, min(min(F2_Scores), 27)]]
  } else if (num_rounds == 5){
    plot_df <- full_plot_df[F1_Scores >= full_plot_df[score_probability >= .01, min(min(F1_Scores), 45)] & F2_Scores >= full_plot_df[score_probability >= .01, min(min(F2_Scores), 45)]]
  }
  # create custom themes for plot
  if(single_plot == T){
    custom_theme <- single_plot_theme
    label_size <- 2.5
  } else if (single_plot == F){
    custom_theme <- multi_plot_theme
    label_size <- 4
  }
  if(use_dark_theme == F){
    dark_theme <- theme()
    plot_theme <- theme_bw()
    label_color <- 'black'
  } else if (use_dark_theme == T){
    dark_theme <- dark_theme
    plot_theme <- dark_theme_gray()
    label_color <- 'white'
  }
  # include Twitter, if specified
  if(include_twitter == T){
    custom_tag <- 'Twitter: @NateLatshaw'
  } else {
    custom_tag <- ''
  }
  # create plot variables
  axis_breaks <- plot_df[, min(c(F1_Scores, F2_Scores))] : plot_df[, max(c(F1_Scores, F2_Scores))]
  
  if(single_plot == T){
    custom_title <- plot_df[, paste0('Final Scorecard Overview for ', unique(F1_Name), ' vs ', unique(F2_Name))]
    custom_subtitle <- plot_df[, paste0('Official Final Scorecards: ', unique(Judge1), ' ', unique(Judge1_Score), 
                                        ', ', unique(Judge2), ' ', unique(Judge2_Score), ', ', 
                                        unique(Judge3), ' ', unique(Judge3_Score), 
                                        '\n', 
                                        'JudgeAI Final Scorecard: ', unique(model_score), 
                                        '\n', 
                                        'JudgeAI Win Probabilities: ', unique(F1_Name), ' ', full_plot_df[F1_Scores > F2_Scores, paste0(format(round(100 * sum(score_probability), 1), nsmall = 1), '%')], ', ', unique(F2_Name), ' ', full_plot_df[F1_Scores < F2_Scores, paste0(format(round(100 * sum(score_probability), 1), nsmall = 1), '%')])]
    rectangles_F1 <- plot_df[F1_Scores > F2_Scores, .(ymin = F1_Scores - .5, 
                                                      ymax = F1_Scores + .5, 
                                                      xmin = F2_Scores - .5, 
                                                      xmax = F2_Scores + .5, 
                                                      color = paired_colors4[1])]
    rectangles_F2 <- plot_df[F1_Scores < F2_Scores, .(ymin = F1_Scores - .5, 
                                                      ymax = F1_Scores + .5, 
                                                      xmin = F2_Scores - .5, 
                                                      xmax = F2_Scores + .5, 
                                                      color = paired_colors4[3])]
  } else if (single_plot == F){
    custom_title <- plot_df[, paste0('Official Final Scorecards: ', unique(Judge1), ' ', unique(Judge1_Score), 
                                     ', ', unique(Judge2), ' ', unique(Judge2_Score), ', ', 
                                     unique(Judge3), ' ', unique(Judge3_Score), 
                                     '\n', 
                                     'JudgeAI Final Scorecard: ', unique(model_score), 
                                     '\n', 
                                     'JudgeAI Win Probabilities: ', unique(F1_Name), ' ', full_plot_df[F1_Scores > F2_Scores, paste0(format(round(100 * sum(score_probability), 1), nsmall = 1), '%')], ', ', unique(F2_Name), ' ', full_plot_df[F1_Scores < F2_Scores, paste0(format(round(100 * sum(score_probability), 1), nsmall = 1), '%')])]
    rectangles_F1 <- plot_df[F1_Scores > F2_Scores, .(ymin = F1_Scores - .5, 
                                                      ymax = F1_Scores + .5, 
                                                      xmin = F2_Scores - .5, 
                                                      xmax = F2_Scores + .5, 
                                                      color = paired_colors4[1])]
    rectangles_F2 <- plot_df[F1_Scores < F2_Scores, .(ymin = F1_Scores - .5, 
                                                      ymax = F1_Scores + .5, 
                                                      xmin = F2_Scores - .5, 
                                                      xmax = F2_Scores + .5, 
                                                      color = paired_colors4[3])]
    custom_subtitle <- ''
  }
  
  # color scale
  if(use_dark_theme){
    low_color <- 'gray10'
    high_color <- 'gray50'
  } else {
    low_color <- 'white'
    high_color <- 'gray50'
  }
  
  # create plot
  p <- ggplot() + 
    geom_tile(data = plot_df, aes(x = F2_Scores, y = F1_Scores, fill = score_probability), size = 2, color = label_color) + 
    scale_x_continuous(breaks = axis_breaks) + 
    scale_y_continuous(breaks = axis_breaks) + 
    labs(title = custom_title, 
         subtitle = custom_subtitle, 
         x = plot_df[, paste0('\n', unique(F2_Name), ' score')], 
         y = plot_df[, paste0(unique(F1_Name), ' score\n')], 
         fill = 'JudgeAI probability', 
         color = 'Winner', 
         tag = custom_tag) + 
    geom_text(data = plot_df, aes(x = F2_Scores, y = F1_Scores, label = paste0(format(round(100 * score_probability, 1), nsmall = 1), '%')), fontface = 'bold', 
              size = label_size, color = label_color) + 
    scale_fill_gradient(low = low_color, high = high_color, labels = percent_format(accuracy = 1)) + 
    plot_theme + 
    custom_theme + 
    dark_theme + 
    theme(legend.position = 'right') + 
    geom_rect(data = rectangles_F1, size = 2, fill = NA, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = 'F1'), show.legend = T) + 
    geom_rect(data = rectangles_F2, size = 2, fill = NA, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = 'F2'), show.legend = T) + 
    scale_color_manual(name = 'Winner', values = c('F1' = paired_colors4[1], 'F2' = paired_colors4[3]), 
                       labels = plot_df[, c(unique(F1_Name), unique(F2_Name))]) + 
    guides(fill = guide_colorbar(order = 0), 
           color = guide_legend(order = 1)) + 
    theme(plot.tag.position = c(.07, .015))
  # return plot
  return(p)
}

##########################################################################################################
# MAP STAT COLUMN NAMES TO DESCRIPTIONS

stats_map <- data.table(Column = c('SSl', 
                                   'SSa', 
                                   'SSp', 
                                   'KD', 
                                   'CtrlMin', 
                                   'TDl', 
                                   'TDa', 
                                   'TDp', 
                                   'SUBa', 
                                   'Rev', 
                                   'SSl_Head', 
                                   'SSa_Head', 
                                   'SSp_Head', 
                                   'SSl_Body', 
                                   'SSa_Body', 
                                   'SSp_Body', 
                                   'SSl_Legs', 
                                   'SSa_Legs', 
                                   'SSp_Legs', 
                                   'SSl_Dist', 
                                   'SSa_Dist', 
                                   'SSp_Dist', 
                                   'SSl_Clnch', 
                                   'SSa_Clnch', 
                                   'SSp_Clnch', 
                                   'SSl_Grnd', 
                                   'SSa_Grnd', 
                                   'SSp_Grnd', 
                                   'TSl', 
                                   'TSa', 
                                   'TSp'), 
                        Label = c('Significant strikes landed', 
                                  'Significant strikes attempted', 
                                  'Significant strike accuracy', 
                                  'Knockdowns', 
                                  'Control time (minutes)', 
                                  'Takedowns', 
                                  'Takedowns attempted', 
                                  'Takedown percent', 
                                  'Submission attempts', 
                                  'Reversals', 
                                  'Significant strikes landed to the head', 
                                  'Significant strikes attempted to the head', 
                                  'Significant strike accuracy to the head', 
                                  'Significant strikes landed to the body', 
                                  'Significant strikes attempted to the body', 
                                  'Significant strike accuracy to the body', 
                                  'Significant strikes landed to the legs', 
                                  'Significant strikes attempted to the legs', 
                                  'Significant strike accuracy to the legs', 
                                  'Significant strikes landed at distance', 
                                  'Significant strikes attempted at distance', 
                                  'Significant strike accuracy at distance', 
                                  'Significant strikes landed in the clinch', 
                                  'Significant strikes attempted in the clinch', 
                                  'Significant strike accuracy in the clinch', 
                                  'Significant strikes landed on the ground', 
                                  'Significant strikes attempted on the ground', 
                                  'Significant strike accuracy on the ground', 
                                  'Total strikes landed', 
                                  'Total strikes attempted', 
                                  'Total strike accuracy'))

#######################################################################################################################
# function that creates axis breaks given a vector of numbers
library(plyr)

Create_Axis_Breaks <- function(vec, include_negatives = F){
  # vec = numeric vector (or the max value of the vector)
  # returns sequence of numbers to be used as axis breaks on a plot
  absolute_val <- max(abs(vec))
  val <- max(vec)
  min_val <- min(c(vec, 0))
  if(absolute_val <= .1){
    val_round <- round_any(x = val, accuracy = .1, f = ceiling)
    val_breaks <- seq(0, val_round, by = .01)
    val_round_positive <- round_any(x = val, accuracy = .1, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = .1, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = .02))))
  } else if(absolute_val > .1 & absolute_val <= .75){
    val_round <- round_any(x = val, accuracy = .1, f = ceiling)
    val_breaks <- seq(0, val_round, by = .1)
    val_round_positive <- round_any(x = val, accuracy = .1, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = .1, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = .1))))
  } else if(absolute_val > .75 & absolute_val <= 2){
    val_round <- round_any(x = val, accuracy = .25, f = ceiling)
    val_breaks <- seq(0, val_round, by = .25)
    val_round_positive <- round_any(x = val, accuracy = .5, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = .5, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = .5))))
  } else if (absolute_val > 2 & absolute_val <= 10){
    val_round <- round_any(x = val, accuracy = 1, f = ceiling)
    val_breaks <- seq(0, val_round, by = 1)
    val_round_positive <- round_any(x = val, accuracy = 2, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 2, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 2))))
  } else if (absolute_val > 10 & absolute_val <= 20){
    val_round <- round_any(x = val, accuracy = 2, f = ceiling)
    val_breaks <- seq(0, val_round, by = 2)
    val_round_positive <- round_any(x = val, accuracy = 5, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 5, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 5))))
  } else if (absolute_val > 20 & absolute_val <= 40){
    val_round <- round_any(x = val, accuracy = 5, f = ceiling)
    val_breaks <- seq(0, val_round, by = 5)
    val_round_positive <- round_any(x = val, accuracy = 10, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 10, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 10))))
  }  else if (absolute_val > 40 & absolute_val <= 60){
    val_round <- round_any(x = val, accuracy = 10, f = ceiling)
    val_breaks <- seq(0, val_round, by = 10)
    val_round_positive <- round_any(x = val, accuracy = 10, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 10, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 10))))
  } else if (absolute_val > 60 & absolute_val <= 100){
    val_round <- round_any(x = val, accuracy = 20, f = ceiling)
    val_breaks <- seq(0, val_round, by = 20)
    val_round_positive <- round_any(x = val, accuracy = 20, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 20, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 20))))
  } else if (absolute_val > 100 & absolute_val <= 200){
    val_round <- round_any(x = val, accuracy = 25, f = ceiling)
    val_breaks <- seq(0, val_round, by = 25)
    val_round_positive <- round_any(x = val, accuracy = 50, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 50, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 50))))
  } else if (absolute_val > 200 & absolute_val <= 400){
    val_round <- round_any(x = val, accuracy = 50, f = ceiling)
    val_breaks <- seq(0, val_round, by = 50)
    val_round_positive <- round_any(x = val, accuracy = 100, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 100, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 100))))
  } else if (absolute_val > 400 & absolute_val <= 600){
    val_round <- round_any(x = val, accuracy = 100, f = ceiling)
    val_breaks <- seq(0, val_round, by = 100)
    val_round_positive <- round_any(x = val, accuracy = 200, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 200, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 200))))
  } else if (absolute_val > 600){
    val_round <- round_any(x = val, accuracy = 200, f = ceiling)
    val_breaks <- seq(0, val_round, by = 200)
    val_round_positive <- round_any(x = val, accuracy = 200, f = ceiling)
    val_round_negative <- round_any(x = min_val, accuracy = 200, f = floor)
    val_breaks_negative <- unique(sort(c(0, seq(val_round_negative, val_round_positive, by = 200))))
  }
  if(include_negatives == F){
    return(val_breaks)
  } else if (include_negatives == T) {
    return(val_breaks_negative)
  } else {
    stop('Error in Create_Axis_Breaks. Check include_negatives input value.')
  }
  
}

