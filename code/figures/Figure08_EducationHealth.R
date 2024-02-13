library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure08_EducationHealth.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Average Education spending", "Average Health spending"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

ik_plot_data <-
  data_subset[, .(spending_type = gsub(".*\\s(.*)\\s.*", "\\1", Statistic),
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  spending = Estimate,
                  spending_ase = `Sampling error`)]

ik_plot_data[income_group == "All", income_group := "Average"]

ik_plot_data[, income_group := factor(income_group, levels = unique(income_group))]
ik_plot_data[, spending_type := factor(spending_type, levels = unique(spending_type))]

p <-
  ggplot(ik_plot_data, aes(x = income_group, y = spending, fill = spending_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = spending - spending_ase, ymax = spending + spending_ase),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile",
       y = "Average in-kind spending ($2019)") +
  scale_fill_manual(name = "", values = fi_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank())

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
