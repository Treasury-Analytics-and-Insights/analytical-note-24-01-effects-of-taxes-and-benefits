library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure04_IncomeSupportComponents.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Average NZ Super and Vets", "Average Working-age support",
                   "Average Housing support", "Average Working for Families",
                   "Average Other income support"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

isc_plot_data <-
  data_subset[, .(support_type = Statistic,
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  support = Estimate,
                  support_ase = `Sampling error`)]

isc_plot_data[, support_type := gsub("\\w+\\s(.*)", "\\1", support_type)]
isc_plot_data[support_type %like% "^Housing", support_type := "Housing"]
isc_plot_data[support_type %like% "^Other", support_type := "Other"]

isc_plot_data[income_group == "All", income_group := "Average"]

isc_plot_data[, support_type := factor(support_type, levels = unique(support_type))]
isc_plot_data[, income_group := factor(income_group, levels = unique(income_group))]

isc_plot_data[is.na(support), label := "S"]
isc_plot_labels <- isc_plot_data[!is.na(label)]
isc_plot_labels[, support := 0L]
isc_plot_data[, label := NULL]

isc_plot_labels_was <- isc_plot_labels[support_type == "Working-age support"]
isc_plot_labels_hou <- isc_plot_labels[support_type == "Housing"]
isc_plot_labels_wff <- isc_plot_labels[support_type == "Working for Families"]

isc_plot_labels_sup <- copy(isc_plot_labels_was[1])
isc_plot_labels_sup[, label := "S - Suppressed due to large\n      coefficient of variation"]

p <-
  ggplot(isc_plot_data, aes(x = income_group, y = support, fill = support_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = support - support_ase,
                    ymax = support + support_ase),
                width = 0.3, position = position_dodge(0.9)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile",
       y = "Average income support ($2019)") +
  scale_fill_manual(name = "", values = fi_palette) +
  geom_text(data = isc_plot_labels_was, nudge_x = -0.185, nudge_y = 300,
            label = isc_plot_labels_was$label, colour = fi_palette[2], size = 5) +
  geom_text(data = isc_plot_labels_hou, nudge_x = 0, nudge_y = 300,
            label = isc_plot_labels_was$label, colour = fi_palette[3], size = 5) +
  geom_text(data = isc_plot_labels_wff, nudge_x = 0.185, nudge_y = 300,
            label = isc_plot_labels_wff$label, colour = fi_palette[4], size = 5) +
  geom_text(data = isc_plot_labels_sup, nudge_x = 1, nudge_y = 18500,
            label = isc_plot_labels_sup$label, colour="black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white", linetype = "solid"))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
