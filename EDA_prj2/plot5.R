library(ggplot2)

# Load in the data
NEI <- readRDS("FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("FNEI_data/Source_Classification_Code.rds")

# For some reason merging takes forever, so using a workaround:
# 1) Search SCC for all MV-related SCC IDs (onroad category)
# 2) Filter out NEI by SCC ID from #1 and Baltimore's FIPS

mv_scc <- as.character(SCC$SCC[grep("Vehicle",SCC$SCC.Level.Two)])
mv_bal_nei <- NEI[NEI$SCC %in% mv_scc & NEI$fips=="24510",]

# Aggregate the pollution by year and type
agg_emiss <- aggregate(Emissions~year,mv_bal_nei,sum)

# Plot
png(filename="plot5.png", height=480, width=480)
ggplot(data=agg_emiss, aes(x=year, y=Emissions))+
  geom_line(colour='darkred')+
  ggtitle("Total Emissions From Vehicles in Baltimore")+
  ylab("Total Emissions (tons)")+
  xlab("Year")
dev.off()