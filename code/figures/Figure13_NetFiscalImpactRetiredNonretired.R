library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/figures_data.csv"
figure_filename <- "../../figures/Figure13_NetFiscalImpactRetiredNonretired.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in%
                 c("Average Income support - retired households",
                   "Average Direct taxes - retired households",
                   "Average Indirect taxes - retired households",
                   "Average In-kind spending - retired households",
                   "Average Net Fiscal Impact - retired households",
                   "Average Income support - non-retired households",
                   "Average Direct taxes - non-retired households",
                   "Average Indirect taxes - non-retired households",
                   "Average In-kind spending - non-retired households",
                   "Average Net Fiscal Impact - non-retired households"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

nfi_data_long <-
  data_subset[, .(group = gsub(".*\\s-\\s(.*)\\s.*", "\\1", Statistic),
                  quantity = gsub("(.*)\\s-\\s.*", "\\1", Statistic),
                  income_group = gsub("(.*\\s)?(.*)", "\\2", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`)]

nfi_data_long[, group := gsub("(\\w)(.*)", "\\U\\1\\E\\2", group, perl = TRUE)]

nfi_data_long[quantity %like% "^Average",
              quantity := gsub("Average", "Mean", quantity)]
nfi_data_long[quantity %like% "Net",
              quantity := gsub("Net\\s", "", quantity)]

nfi_data_long[, quantity := tolower(gsub("(\\s|-)", "_", quantity))]

nfi_data_long <- rbind(nfi_data_long[, .(group, quantity, income_group, amount)],
                       nfi_data_long[quantity == "mean_fiscal_impact",
                                     .(group,
                                       quantity = "mean_fiscal_impact_ase",
                                       income_group,
                                       amount = amount_ase)])

nfi_data_long[income_group == "All", income_group := "Average"]
nfi_data_long[, income_group := factor(income_group, levels = unique(income_group))]

nfi_data_wide <- dcast(nfi_data_long, group + income_group ~ quantity)

nfiret_plot_data <-
  melt(nfi_data_wide[, .(group,
                         income_group,
                         income_support = mean_income_support,
                         inkind_benefits = mean_in_kind_spending,
                         direct_tax = -mean_direct_taxes,
                         indirect_tax = -mean_indirect_taxes,
                         fiscal_impact = mean_fiscal_impact,
                         fiscal_impact_ase = mean_fiscal_impact_ase)],
       id.vars = c("group", "income_group", "fiscal_impact", "fiscal_impact_ase"))

nfiret_plot_data[variable != "income_support",
                 `:=`(fiscal_impact = NA_real_, fiscal_impact_ase = NA_real_)]

nfiret_plot_data[, group := factor(group, levels = c("Retired", "Non-retired"))]

nfiret_plot_data[, variable := factor(variable,
                                      levels = c("inkind_benefits", "income_support",
                                                 "indirect_tax", "direct_tax"))]

nfiret_plot_data[variable == "income_support" & is.na(fiscal_impact), label := "S"]
nfiret_plot_labels <- nfiret_plot_data[!is.na(label)]
nfiret_plot_labels[, value := 0L]
nfiret_plot_data[, label := NULL]
 
nfiret_plot_labels_sup <- copy(nfiret_plot_labels[1])
nfiret_plot_labels_sup[, label := "S - Suppressed due to large\n      coefficient of variation"]

p <-
  ggplot(nfiret_plot_data, aes(x = income_group, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", width = 0.9) +
  geom_point(aes(x = income_group, y = fiscal_impact, colour = "black"), size = 2) +
  geom_errorbar(aes(ymin = fiscal_impact - fiscal_impact_ase,
                    ymax = fiscal_impact + fiscal_impact_ase),
                width = 0.15) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Household equivalised disposable income decile",
       y = "Average benefit or tax ($2019)") +
  guides(fill = guide_legend(override.aes = list(shape = NA)), colour = guide_legend("")) +
  scale_fill_manual(name = "", values = fi_palette,
                    breaks = c("inkind_benefits", "income_support",
                               "direct_tax", "indirect_tax"),
                    labels = c("In-kind benefits", "Income support",
                               "Direct taxes", "Indirect taxes")) +
  scale_colour_manual(values = "black", labels = "Net fiscal impact") +
  geom_text(data = nfiret_plot_labels, nudge_x = 0, nudge_y = 0,
            label = nfiret_plot_labels$label, colour = "black", size = 5) +
  geom_text(data = nfiret_plot_labels_sup, nudge_x = -4, nudge_y = -8.0e4, hjust = 0,
            label = nfiret_plot_labels_sup$label, colour = "black", size = 5) +
  facet_wrap(~ group, nrow = 2L) +
  theme_bw() +
  theme(legend.position = "bottom",
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_rect(colour = "black", fill = "white", linetype = "solid"))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
