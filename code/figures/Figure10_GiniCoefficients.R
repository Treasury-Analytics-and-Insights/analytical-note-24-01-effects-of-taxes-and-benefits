library(data.table)
library(ggplot2)
library(ggh4x)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure10_GiniCoefficients.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic == "Gini coefficient",
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

gini_plot_data <-
  data_subset[, .(income_type = `Group or category`,
                  gini_coefficient = Estimate,
                  gini_coefficient_ase = `Sampling error`)]

gini_plot_data[income_type %like% "income$",
               income_type := gsub("(.*)\\s.*", "\\1", income_type)]
gini_plot_data[income_type == "HES", income_type := "HES*"]
gini_plot_data[income_type %like% "^TAWA",
               income_type := gsub(".*\\(.*\\s(.*)\\)", "TAWA\\\n(HES \\1)", income_type)]

gini_plot_data[!(income_type %like% "HES"), panel := "Income definition"]
gini_plot_data[income_type %like% "HES", panel := "Disposable income source"]

gini_plot_data[, income_type := factor(income_type, levels = unique(income_type))]
gini_plot_data[, panel := factor(panel, levels = unique(panel))]

gini_panel_labels <- gini_plot_data[income_type %in% c("Disposable", "TAWA\n(HES Inc)")]
gini_panel_labels[, `:=`(gini_coefficient = 50, gini_coefficient_ase = NA_real_)]
gini_panel_labels[income_type == "Disposable", label := "Fiscal incidence study"]
gini_panel_labels[income_type == "TAWA\n(HES Inc)", label := "Reference disposable incomes"]

gini_plot_label_hes <- gini_plot_data[income_type == "HES*"]
gini_plot_label_hes[, `:=`(gini_coefficient = 25,
                           gini_coefficient_ase = NA_real_,
                           label = "* year ended June 2019")]

p <-
  ggplot(gini_plot_data, aes(x = income_type, y = gini_coefficient)) +
  geom_point(size = 5, colour = fi_palette[1]) +
  geom_errorbar(aes(ymin = gini_coefficient - gini_coefficient_ase,
                    ymax = gini_coefficient + gini_coefficient_ase),
                width = 0.3, colour = fi_palette[1], show.legend = FALSE) +
  labs(x = element_blank(), y = "Gini coefficient") +
  scale_y_continuous(breaks = seq(25, 50, 5), limits = c(25, 50)) +
  facet_wrap(~ panel,
             ncol = 2L,
             scales = "free_x",
             strip.position = "bottom") +
  force_panelsizes(cols = c(1.9, 1.1)) +
  geom_text(data = gini_panel_labels, nudge_x = 0, nudge_y = 0,
            label = gini_panel_labels$label, colour = "grey39", size = 8) +
  geom_text(data = gini_plot_label_hes, nudge_x = 1.75, nudge_y = 0,
            label = gini_plot_label_hes$label, colour = "grey39", size = 6) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 23),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 23))

ggsave(p, filename = figure_filename,
       width = 15, height = 8, units = "in", dpi = 600, type = "cairo")
