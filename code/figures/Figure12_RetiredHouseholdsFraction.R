library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure12_RetiredHouseholdsFraction.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic == "Proportion of retired households",
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

retfrac_plot_data <-
  data_subset[, .(quantity = Statistic,
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  retired_fraction = Estimate,
                  retired_fraction_ase = `Sampling error`)]

retfrac_plot_data[income_group == "All", income_group := "Average"]

retfrac_plot_data[, income_group := factor(income_group, levels = unique(income_group))]

p <-
  ggplot(retfrac_plot_data, aes(x = income_group, y = retired_fraction, fill = quantity)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  geom_errorbar(aes(ymin = retired_fraction - retired_fraction_ase,
                    ymax = retired_fraction + retired_fraction_ase),
                width = 0.3, position = position_dodge(0.9)) +
  labs(x = "Household equivalised disposable income decile",
       y = "Proportion of households retired (%)") +
  scale_fill_manual(name = "", values = fi_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
