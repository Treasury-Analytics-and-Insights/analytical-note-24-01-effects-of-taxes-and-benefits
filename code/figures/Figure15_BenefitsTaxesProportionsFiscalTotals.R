library(data.table)
library(ggplot2)
library(ggtext)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
fiscals_data_filename <- "../../data/fiscal_totals.csv"
figure_filename <- "../../figures/Figure15_BenefitsTaxesProportionsFiscalTotals.png"

figures_data <- fread(figures_data_filename)
fiscals_data <- fread(fiscals_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Unscaled total - TAWA (HES Expenditure)",
                   "Unscaled total - TAWA (HES Income)"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

estimates <-
  data_subset[, .(survey = gsub(".*\\(.*\\s(.*)\\)", "\\1", Statistic),
                  variable = `Group or category`,
                  estimate_total = Estimate,
                  estimate_total_ase = `Sampling error`)]

fiscal_totals <- fiscals_data[, .(variable = Quantity, fiscal_total = TY2019)]

fiscalprop_plot_data <- estimates[fiscal_totals, on = .(variable)]

fiscalprop_plot_data <-
  fiscalprop_plot_data[!(variable %in% c("Minimum family tax credit", "Best Start tax credit"))]

fiscalprop_plot_data[variable == "Health",
                     fiscal_total :=
                       fiscalprop_plot_data[variable == "PBFF model budget total", fiscal_total]]
fiscalprop_plot_data <- fiscalprop_plot_data[variable != "PBFF model budget total"]

setorder(fiscalprop_plot_data, -variable)
fiscalprop_plot_data[, variable := factor(variable, levels = unique(variable))]

fiscalprop_plot_data[, `:=`(proportion = estimate_total / fiscal_total,
                            proportion_ase = estimate_total_ase / fiscal_total,
                            estimate_total = NULL, estimate_total_ase = NULL, fiscal_total = NULL)]

fiscalprop_plot_data[, survey := factor(survey, levels = c("Income", "Expenditure"))]

axis_labels <- fiscalprop_plot_data[, sort(as.character(unique(variable)), decreasing = TRUE)]
axis_labels <- gsub("Supported Living Payment", "Supported Living Paym.", axis_labels)
axis_labels <- gsub("Youth/Young Parent Payment", "Youth/Young Parent Paym.", axis_labels)
axis_labels <- gsub("Accommodation Supplement", "Accommodation Supp.", axis_labels)
axis_labels <- gsub("Income-Related Rent Subsidy", "Income-Related Rent Sub.", axis_labels)
axis_labels <- gsub("(.*)\\sexcise", "\\1 excise<sup>a</sup>", axis_labels)
axis_labels <- gsub("Student loan", "Student loan<sup>a</sup>", axis_labels)
axis_labels <- gsub("Health", "Health<sup>b</sup>", axis_labels)

fiscalprop_labels <-
  fiscalprop_plot_data[variable == "Youth/Young Parent Payment" & survey == "Expenditure"]
fiscalprop_labels[, `:=`(proportion = 1.01, label = "S")]

p <-
  ggplot(fiscalprop_plot_data, aes(x = variable, y = proportion, colour = survey)) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  scale_colour_manual(values = fi_palette[c(2, 1)],
                      guide = guide_legend(reverse = TRUE)) +
  geom_errorbar(aes(ymin = proportion - proportion_ase,
                    ymax = pmin(proportion + proportion_ase, 1.50)),
                position = position_dodge(width = 0.5),
                linewidth = 1, width = 0, show.legend = FALSE) +
  geom_hline(yintercept = 1) +
  xlab("Benefit or tax") +
  ylab("Proportion of fiscal total") +
  labs(colour = "HES sample") +
  geom_text(data = fiscalprop_labels, nudge_x = 0.1, nudge_y = 0,
            label = fiscalprop_labels$label, colour = fi_palette[1], size = 4) +
  scale_x_discrete(labels = axis_labels) +
  scale_y_continuous(limits = c(0.5, 1.5), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_markdown(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  coord_flip() +
  annotate(geom = "text", x = 23.5, y = 1.20,
           label = expression(phantom(0)^a), size = 5, hjust = 0) +
  annotate(geom = "text", x = 22.5, y = 1.23,
           label = "Fiscal values\nattributed proportionally\nto impacted samples",
           size = 5, hjust = 0) +
  annotate(geom="text", x = 20.0, y = 1.20,
           label = expression(phantom(0)^b), size = 5, hjust = 0) +
  annotate(geom = "text", x = 19.0, y = 1.23,
           label = "Proportion of PBFF\nmodel service budgets\ntotal", size = 5, hjust = 0) +
  annotate(geom = "text", x = 2.0, y = 1.20,
           label = "S - Suppressed due to\n      small sample count", size = 5, hjust = 0)

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
