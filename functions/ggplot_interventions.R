#######################################################################
# Global Trade Alert: Task 1
# ggplot Function
#
# This file contains a function plotting interventions over time
# according to their gta.evaluation.
#
# J. Leopold, St. Gallen, 2019
#######################################################################

ggplot_interventions <- 
  function(data, plot_title = "Number of Interventions from 2009 - 2018"){
    ggplot(data = data, aes(x = year.announced, y = count,
                                              group = gta.evaluation,
                                              color = gta.evaluation,
                                              label = labels)) + 
      geom_line(size = 0.75) +
      geom_point(shape = 21, size = 3) +
      geom_point(size = 1.5) + 
      geom_text_repel(mapping = aes(label = labels), 
                      point.padding = 0.5,
                      size = 2.7,
                      nudge_x = 0.05,
                      nudge_y = max(data$count)/10,
                      show.legend = FALSE) +
      scale_x_discrete(name = "Year") +
      scale_y_continuous(name = "Number of Interventions",
                         limits = c(0,max(data$count) + max(data$count)/10)) +
                         #breaks = seq(0, 2500, by = 250)) +
      scale_color_manual(name = "Evaluation by GTA", 
                         values = c("#329141",
                                    "#F39B24",
                                    "#D22F5A",
                                    "#000000"))+
      labs(caption="Global Trade Alert, 2019") +
      ggtitle(plot_title) +
      theme_minimal() +
      theme(panel.border = element_blank(),
            panel.grid = element_blank(),
            panel.grid.major.y = element_line(color = "gray"),
            text = element_text(color = "gray20"),
            axis.title.x = element_text(size = 12, vjust = -0.2),
            axis.title.y = element_text(size = 12, vjust = 0.5),
            axis.line = element_line(size = 0.25),
            axis.ticks.x.bottom = element_line(size = 0.25),
            axis.ticks.y.left = element_line(size = 0.25),
            axis.ticks.length = unit(3, "pt"),
            axis.text.x = element_text(vjust = -0.2, face="italic"), 
            axis.text.y = element_text(face="italic"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.justification = "left",
            legend.text = element_text(size = 12),
            plot.caption = element_text(hjust=0),
            plot.title = element_text(size = 14, face = "bold", color = "#3B88C8"))
  }
