library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure06_DirectTaxesComponents.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Average Personal income tax", "Average ACC earners' levy"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

dtc_plot_data <-
  data_subset[, .(tax_type = gsub("\\w+\\s(.*)", "\\1", Statistic),
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  tax = Estimate,
                  tax_ase = `Sampling error`)]

dtc_plot_data[income_group == "All", income_group := "Average"]

dtc_plot_data[, income_group := factor(income_group, levels = unique(income_group))]
dtc_plot_data[, tax_type := factor(tax_type, levels = unique(tax_type))]

p <-
  ggplot(dtc_plot_data, aes(x = income_group, y = tax, fill = tax_type)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = tax - tax_ase, ymax = tax + tax_ase),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile", y = "Average tax or levy ($2019)") +
  scale_fill_manual(name = "", values = fi_palette) +
  facet_wrap(~ tax_type, nrow = 2L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white", linetype = "solid"))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
