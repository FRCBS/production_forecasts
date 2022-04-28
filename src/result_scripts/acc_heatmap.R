library(pheatmap)
library(grid)
# Load data
data <- read.csv("/home/esa/prodfore_publ/results/acc_FI_NL_reord.csv", header = TRUE, row.names = 1)
rownames(data) <- c(" MIN", " SNAIVE", " 5-MA", " 7-MA", " 9-MA", " 12-MA", " ETS", " STL",
                    " TBATS", " NNAR", " ARIMAX", " DYNREG", " STLF", "MLP", "ELM", " AVG", " W.AVG",
                    " AUTO-1", " AUTO-3", " AUTO-6", " AUTO-12", " AUTO-18", " AUTO-24")
# data_ro <- data[c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 2, 16, 17, 18, 19, 20, 21),
#                 c(6, 5, 4, 3, 2, 1, 12, 11, 10, 9, 8, 7)]
# colnames(data_ro) <- c("1", "3", "6", "12", "18", "24", "1", "3", "6", "12", "18", "24")

# Rotate cos image() is stupid
rotdat <- apply(as.matrix(data), 2, rev)

# Separate, scale each, glue back
sep_scale_data <- cbind(scale(data[, 1:6]), scale(data[, 7:12]))

# Draw both countries together

# postscript("acc_heatmap_weekly.eps", width = 9, height = 12, horizontal = FALSE,
#            onefile = FALSE, paper = "special", colormodel = "cmyk",
#            family = "Helvetica")

# p <- pheatmap(data, cluster_rows = F, cluster_cols = F, show_colnames = F, show_rownames = T, gaps_col = 6, gaps_row = 16,
#          display_numbers = T, main = " ", fontsize = 20, fontsize_row = 12, fontsize_number = 12, number_color = "black", legend = F)
#
# grid.text(c("Finland", "Netherlands"), x = c(0.23, 0.68), y = c(0.976, 0.976), gp = gpar(fontsize = 12, fontface = "bold"))
# grid.text(c(1, 3, 6, 12, 18, 24, 1, 3, 6, 12, 18, 24),
#           x = c(0.045, 0.12, 0.195, 0.27, 0.345, 0.42, 0.7, 0.8, 0.9, 1, 1.1, 1.2),
#           y = c(0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973),
#           gp = gpar(fontsize = 10))
#
# p

# dev.off()

##################### Draw both countries separately

postscript("/home/esa/prodfore_publ/results/acc_heatmap_weekly_FI_wo_min_reord.eps", width = 4, height = 12, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

pf <- pheatmap(data[-1, 1:6], cluster_rows = F, cluster_cols = F, show_colnames = F, show_rownames = F, gaps_row = 15,
         display_numbers = T, main = " ", fontsize = 20, fontsize_row = 12, fontsize_number = 12, number_color = "black", legend = F)

# grid.text(c("Finland", "Netherlands"), x = c(0.23, 0.68), y = c(0.976, 0.976), gp = gpar(fontsize = 12, fontface = "bold"))
# grid.text(c(1, 3, 6, 12, 18, 24, 1, 3, 6, 12, 18, 24),
#           x = c(0.045, 0.12, 0.195, 0.27, 0.345, 0.42, 0.7, 0.8, 0.9, 1, 1.1, 1.2),
#           y = c(0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973),
#           gp = gpar(fontsize = 10))

pf

dev.off()

postscript("acc_heatmap_weekly_NL_wo_min.eps", width = 4, height = 12, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

pnl <- pheatmap(data[-1, 7:12], cluster_rows = F, cluster_cols = F, show_colnames = F, show_rownames = F, gaps_row = 13,
         display_numbers = T, main = " ", fontsize = 20, fontsize_row = 12, fontsize_number = 12, number_color = "black", legend = F)

# grid.text(c("Finland", "Netherlands"), x = c(0.23, 0.68), y = c(0.976, 0.976), gp = gpar(fontsize = 12, fontface = "bold"))
# grid.text(c(1, 3, 6, 12, 18, 24, 1, 3, 6, 12, 18, 24),
#           x = c(0.045, 0.12, 0.195, 0.27, 0.345, 0.42, 0.7, 0.8, 0.9, 1, 1.1, 1.2),
#           y = c(0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973),
#           gp = gpar(fontsize = 10))

pnl

dev.off()

# Just minimums
postscript("minrow_FI.eps", width = 4, height = 1, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

minf <- pheatmap(data[1, 1:6], color = c("#ffffff"), cluster_rows = F, cluster_cols = F, show_colnames = F, show_rownames = F, display_numbers = T, main = " ", fontsize = 20, fontsize_row = 12, fontsize_number = 12, number_color = "black", legend = F)

# grid.text(c("Finland", "Netherlands"), x = c(0.23, 0.68), y = c(0.976, 0.976), gp = gpar(fontsize = 12, fontface = "bold"))
# grid.text(c(1, 3, 6, 12, 18, 24, 1, 3, 6, 12, 18, 24),
#           x = c(0.045, 0.12, 0.195, 0.27, 0.345, 0.42, 0.7, 0.8, 0.9, 1, 1.1, 1.2),
#           y = c(0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973),
#           gp = gpar(fontsize = 10))

minf

dev.off()

postscript("minrow_NL.eps", width = 4, height = 1, horizontal = FALSE,
           onefile = FALSE, paper = "special", colormodel = "cmyk",
           family = "Helvetica")

minf <- pheatmap(data[1, 7:12], color = c("#ffffff"), cluster_rows = F, cluster_cols = F, show_colnames = F, show_rownames = F, display_numbers = T, main = " ", fontsize = 20, fontsize_row = 12, fontsize_number = 12, number_color = "black", legend = F)

# grid.text(c("Finland", "Netherlands"), x = c(0.23, 0.68), y = c(0.976, 0.976), gp = gpar(fontsize = 12, fontface = "bold"))
# grid.text(c(1, 3, 6, 12, 18, 24, 1, 3, 6, 12, 18, 24),
#           x = c(0.045, 0.12, 0.195, 0.27, 0.345, 0.42, 0.7, 0.8, 0.9, 1, 1.1, 1.2),
#           y = c(0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973, 0.973),
#           gp = gpar(fontsize = 10))

minf

dev.off()
