library(data.table)
library(ggplot2)

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure14_GiniCoefficientsRetiredNonretired.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Gini coefficient - retired households",
                   "Gini coefficient - non-retired households"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

giniret_plot_data <-
  data_subset[, .(group = gsub(".*\\s-\\s(.*)\\s.*", "\\1", Statistic),
                  income_type = `Group or category`,
                  gini_coefficient = Estimate,
                  gini_coefficient_ase = `Sampling error`)]

giniret_plot_data[, group := gsub("(\\w)(.*)", "\\U\\1\\E\\2", group, perl = TRUE)]

giniret_plot_data[income_type %like% "income$",
                  income_type := gsub("(.*)\\s.*", "\\1", income_type)]

giniret_plot_data[, group := factor(group, levels = unique(group))]

giniret_plot_data[, income_type := factor(income_type, levels = unique(income_type))]
giniret_plot_data[, x := as.numeric(as.integer(income_type))]

giniret_plot_data[group == "Retired", x := x - 0.125]
giniret_plot_data[group == "Non-retired", x := x + 0.125]

p <-
  ggplot(giniret_plot_data, aes(x = x, y = gini_coefficient, colour = group, shape = group)) +
  geom_point(size = 5) +
  scale_colour_manual(values = c("black", "#0071B2")) +
  scale_shape_manual(values = c("circle", "square")) +
  geom_errorbar(aes(ymin = gini_coefficient - gini_coefficient_ase,
                    ymax = gini_coefficient + gini_coefficient_ase),
                width = 0.1, show.legend = FALSE) +
  labs(x = "Income definition", y = "Gini coefficient") +
  scale_x_continuous(breaks = 1:giniret_plot_data[, uniqueN(income_type)],
                     labels = giniret_plot_data[, levels(income_type)]) +
  scale_y_continuous(breaks = seq(20, 70, 10), limits = c(20, 70)) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 23),
        title = element_text("Household group"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank())

ggsave(p, filename = figure_filename,
       width = 15, height = 8, units = "in", dpi = 600, type = "cairo")
