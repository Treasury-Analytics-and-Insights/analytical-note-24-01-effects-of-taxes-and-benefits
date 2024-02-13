library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure09_NetFiscalImpact.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Average Net Fiscal Impact", "Average Income support",
                   "Average Health spending", "Average Education spending",
                   "Average Direct taxes", "Average Indirect taxes"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

nfi_data_long <-
  data_subset[, .(quantity = Statistic,
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`)]

nfi_data_long[quantity %like% "^Average",
              quantity := gsub("Average", "Mean", quantity)]
nfi_data_long[quantity %like% "Net",
              quantity := gsub("Net\\s", "", quantity)]

nfi_data_long[, quantity := tolower(gsub("\\s", "_", quantity))]

nfi_data_long <- rbind(nfi_data_long[, .(quantity, income_group, amount)],
                       nfi_data_long[quantity == "mean_fiscal_impact",
                                     .(quantity = "mean_fiscal_impact_ase",
                                       income_group,
                                       amount = amount_ase)])

nfi_data_long[income_group == "All", income_group := "Average"]
nfi_data_long[, income_group := factor(income_group, levels = unique(income_group))]

nfi_data_wide <- dcast(nfi_data_long, income_group ~ quantity)

nfi_plot_data <-
  melt(nfi_data_wide[, .(income_group,
                         income_support = mean_income_support,
                         inkind_benefits = mean_health_spending + mean_education_spending,
                         direct_tax = -mean_direct_taxes,
                         indirect_tax = -mean_indirect_taxes,
                         fiscal_impact = mean_fiscal_impact,
                         fiscal_impact_ase = mean_fiscal_impact_ase)],
       id.vars = c("income_group", "fiscal_impact", "fiscal_impact_ase"))

nfi_plot_data[variable != "income_support",
              `:=`(fiscal_impact = NA_real_, fiscal_impact_ase = NA_real_)]

nfi_plot_data[, variable := factor(variable,
                                   levels = c("inkind_benefits", "income_support",
                                              "indirect_tax", "direct_tax"))]

nfi_plot_data[variable == "income_support" & is.na(fiscal_impact), label := "S"]
nfi_plot_labels <- nfi_plot_data[!is.na(label)]
nfi_plot_labels[, value := 0L]
nfi_plot_data[, label := NULL]

nfi_plot_labels_sup <- copy(nfi_plot_labels[1])
nfi_plot_labels_sup[, label := "S - Suppressed due to large\n      coefficient of variation"]

p <-
  ggplot(nfi_plot_data, aes(x = income_group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_point(aes(x = income_group, y = fiscal_impact, colour = "black"), size = 2) +
  geom_errorbar(aes(ymin = fiscal_impact - fiscal_impact_ase,
                    ymax = fiscal_impact + fiscal_impact_ase),
                width = 0.15) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile",
       y = "Average benefit or tax ($2019)") +
  guides(fill = guide_legend(override.aes = list(shape = NA)), colour = guide_legend("")) +
  scale_fill_manual(name = "",
                    values = fi_palette,
                    breaks = c("inkind_benefits", "income_support",
                               "direct_tax", "indirect_tax"),
                    labels = c("In-kind benefits", "Income support",
                               "Direct taxes", "Indirect taxes")) +
  scale_colour_manual(values = "black", labels = "Net fiscal impact") +
  geom_text(data = nfi_plot_labels, nudge_x = 0, nudge_y = 0,
            label = nfi_plot_labels$label, colour = "black", size = 5) +
  geom_text(data = nfi_plot_labels_sup, nudge_x = -5, nudge_y = -9.0e4, hjust = 0,
            label = nfi_plot_labels_sup$label, colour = "black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
