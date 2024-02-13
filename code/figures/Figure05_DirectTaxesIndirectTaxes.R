library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure05_DirectTaxesIndirectTaxes.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Average Direct taxes", "Average Indirect taxes"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

dtaxitax_plot_data <-
  data_subset[, .(tax_type = gsub("\\w+\\s(.*)", "\\1", Statistic),
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  tax = Estimate,
                  tax_ase = `Sampling error`)]

dtaxitax_plot_data[income_group == "All", income_group := "Average"]

dtaxitax_plot_data[, income_group := factor(income_group, levels = unique(income_group))]
dtaxitax_plot_data[, tax_type := factor(tax_type, levels = unique(tax_type))]

p <-
  ggplot(dtaxitax_plot_data, aes(x = income_group, y = tax, fill = tax_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = tax - tax_ase, ymax = tax + tax_ase),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile", y = "Average tax ($2019)") +
  scale_fill_manual(name = "", values = fi_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.title = element_blank())

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
