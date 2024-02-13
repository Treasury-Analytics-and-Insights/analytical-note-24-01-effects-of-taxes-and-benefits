library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure17_GiniCoefficientsComparisonPrevious.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Gini coefficient 1987/88", "Gini coefficient 1997/98",
                   "Gini coefficient 2006/07", "Gini coefficient 2009/10",
                   "Gini coefficient 2018/19"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

giniprev_plot_data <-
  data_subset[, .(year = gsub(".*\\s(\\d{2})\\d{2}\\/(\\d{2})", "\\1\\2", Statistic),
                  income_type = `Group or category`,
                  gini_coefficient = Estimate,
                  gini_coefficient_ase = `Sampling error`)]

giniprev_plot_data[, year := as.integer(year)]

giniprev_plot_data[, income_type := factor(income_type, levels = unique(income_type))]

p <-
  ggplot(giniprev_plot_data,
         aes(x = year, y = gini_coefficient, colour = income_type, shape = income_type)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = gini_coefficient - gini_coefficient_ase,
                    ymax = gini_coefficient + gini_coefficient_ase),
                width = 0.5, show.legend = FALSE) +
  scale_x_continuous(breaks = giniprev_plot_data[, unique(year)],
                     labels = c(expression(1987/1988^a), expression(1997/1998^a),
                                expression(2006/2007^b), expression(2009/2010^b),
                                expression(2018/2019))) +
  labs(x = "Tax year", y = "Gini coefficient") +
  scale_colour_manual(values = fi_palette) +
  scale_shape_manual(values = c("circle", "triangle", "square")) +
  theme_bw() +
  theme(text = element_text(size = 23),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  annotate(geom = "text", x = 1988.75, y = 26.75,
           label = expression(paste(phantom(0)^a, Crawford~and~Johnston, "'s (2004) calculations")),
           colour = "grey39", size = 6, hjust = 0) +
  annotate(geom = "text", x = 1988.75, y = 25.00,
           label = expression(paste(phantom(0)^b, Calculated~from~Aziz, italic(" et al"),
                                    ".'s (2012) microdata")),
           colour = "grey39", size = 6, hjust = 0) +
  scale_y_continuous(breaks = seq(25, 50, 5), limits = c(25, 50))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
