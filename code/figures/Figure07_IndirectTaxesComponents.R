library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure07_IndirectTaxesComponents.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Average GST", "Average Alcohol excise",
                   "Average Petrol excise", "Average Tobacco excise"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

itc_plot_data <-
  data_subset[, .(tax_type = gsub("\\w+\\s(.*)", "\\1", Statistic),
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  tax = Estimate,
                  tax_ase = `Sampling error`)]

itc_plot_data[income_group == "All", income_group := "Average"]

itc_plot_data[, income_group := factor(income_group, levels = unique(income_group))]
itc_plot_data[, tax_type := factor(tax_type, levels = unique(tax_type))]

itc_plot_data[, panel := "Excise"]
itc_plot_data[tax_type == "GST", panel := "GST"]
itc_plot_data[, panel := factor(panel, levels = c("GST", "Excise"))]

itc_plot_data[is.na(tax), label := "S"]
itc_plot_labels <- itc_plot_data[!is.na(label)]
itc_plot_labels[, tax := 0L]
itc_plot_data[, label := NULL]

itc_plot_labels_tob <- itc_plot_labels[tax_type == "Tobacco excise"]

itc_plot_labels_sup <- copy(itc_plot_labels_tob[1])
itc_plot_labels_sup[, label := "S - Suppressed due to large\n      coefficient of variation"]

p <-
  ggplot(itc_plot_data, aes(x = income_group, y = tax, fill = tax_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = tax - tax_ase, ymax = tax + tax_ase),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile", y = "Average tax ($2019)") +
  scale_fill_manual(name = "", values = fi_palette) +
  geom_text(data = itc_plot_labels_tob, nudge_x = 0.25, nudge_y = 35,
            label = itc_plot_labels_tob$label, colour = fi_palette[4], size = 6) +
  geom_text(data = itc_plot_labels_sup, nudge_x = -1, nudge_y = 600,
            label = itc_plot_labels_sup$label, colour = "black", size = 5) +
  facet_wrap(~ panel, nrow = 2L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white", linetype = "solid"))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
