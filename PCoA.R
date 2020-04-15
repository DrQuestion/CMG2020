library('plotly')
library('ape')
dist_mat <- read.csv("mash_distance.csv", header = TRUE, row.names = 1)
dist_mat <- as.matrix(dist_mat)
meta_mat <- read.csv("Samples_metadata_Group1.tsv", sep='\t', stringsAsFactors = F)
idxs <- match(paste(meta_mat$Genome_name, '.fa', sep = ''), row.names(dist_mat))
meta_mat <- meta_mat[idxs,]
row.names(dist_mat) <- NULL
pcoa_results <- pcoa(dist_mat)

# draw PCoA plot

#Color by Age
color_by_age=palette()[1:length(unique(meta_mat$Age.Category))][as.factor(meta_mat$Age.Category)]
plot(pcoa_results$vectors[,1], pcoa_results$vectors[,2], xlab="Axis1", ylab="Axis2", main="PCoA of genomes distance", type="p", pch=10, col=color_by_age)
legend('topright', legend = levels(as.factor(meta_mat$Age.Category)), col = 1:4, cex = 0.8, pch = 10)

fig <- plot_ly(x=pcoa_results$vectors[,1], y=pcoa_results$vectors[,2], z=pcoa_results$vectors[,3], type ="scatter3d", mode="markers", color = as.factor(meta_mat$Age.Category))
fig <- fig %>% layout(
  title = "PCoA analysis of Mash distances",
  scene = list(
    xaxis = list(title = "Axis1"),
    yaxis = list(title = "Axis2"),
    zaxis = list(title = "Axis3")
    ))
fig


#Color by Country
Europe=c('AUT', 'DEU', 'DNK', 'ESP', 'FIN', 'FRA', 'GBR', 'SWE')
in_eu=meta_mat$Country %in% Europe
meta_mat$Country[in_eu]='Europe'
color_by_country=rainbow(length(unique(meta_mat$Country)))[as.factor(meta_mat$Country)]
plot(pcoa_results$vectors[,1], pcoa_results$vectors[,2], xlab="Axis1", ylab="Axis2", main="PCoA of genomes distance", type="p", pch=10, col=color_by_country)
legend('bottomright', legend = levels(as.factor(meta_mat$Country)), col = rainbow(length(unique(meta_mat$Country))), cex = 0.8, pch = 10)

fig <- plot_ly(x=pcoa_results$vectors[,1], y=pcoa_results$vectors[,2], z=pcoa_results$vectors[,3], type ="scatter3d", mode="markers", color = as.factor(meta_mat$Country))
fig <- fig %>% layout(
  title = "PCoA analysis of Mash distances",
  scene = list(
    xaxis = list(title = "Axis1"),
    yaxis = list(title = "Axis2"),
    zaxis = list(title = "Axis3")
  ))
fig
