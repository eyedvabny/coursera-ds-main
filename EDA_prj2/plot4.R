library(ggplot2)

# Load in the data
NEI <- readRDS("FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("FNEI_data/Source_Classification_Code.rds")

# For some reason merging takes forever, so using a workaround:
# 1) Search SCC for all coal-related SCC IDs
# 2) Filter out NEI by SCC ID from #1
coal_scc <- as.character(SCC$SCC[grep("Coal|coal",SCC$EI.Sector)])
coal_nei <- NEI[NEI$SCC %in% coal_scc,]

# Aggregate the pollution by year
agg_emiss <- aggregate(Emissions~year,coal_nei,sum)

# Plot
png(filename="plot4.png", height=480, width=480)
ggplot(data=agg_emiss, aes(x=year, y=Emissions))+
  geom_line(colour="darkred")+
  ggtitle("Total Emissions From Coal")+
  ylab("Total Emissions (tons)")+
  xlab("Year")
dev.off()