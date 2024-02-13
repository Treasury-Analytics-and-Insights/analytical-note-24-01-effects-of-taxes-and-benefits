library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure03_IncomeSupport.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic == "Average Income support",
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

is_plot_data <-
  data_subset[, .(quantity = Statistic,
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  income_support = Estimate,
                  income_support_ase = `Sampling error`)]

is_plot_data[income_group == "All", income_group := "Average"]

is_plot_data[, income_group := factor(income_group, levels = unique(income_group))]

p <-
  ggplot(is_plot_data, aes(x = income_group, y = income_support, fill = quantity)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  geom_errorbar(aes(ymin = income_support - income_support_ase,
                    ymax = income_support + income_support_ase),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile",
       y = "Average income support ($2019)") +
  scale_fill_manual(name = "", values = fi_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
