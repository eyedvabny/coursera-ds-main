library(ggplot2)

# Load in the data
NEI <- readRDS("FNEI_data/summarySCC_PM25.rds")

# Aggregate the pollution by year and type
agg_emiss <- aggregate(Emissions~year+type,NEI[NEI$fips=="24510",],sum)

# Type needs to be a factor
agg_emiss$type <- factor(agg_emiss$type)

# Plot
png(filename="plot3.png", height=480, width=480)
ggplot(data=agg_emiss, aes(x=year, y=Emissions, colour=type))+
  geom_line()+
  ggtitle("Total Emissions in Baltimore")+
  ylab("Total Emissions (tons)")+
  xlab("Year")+
  theme(legend.position="bottom",legend.title=element_blank())
dev.off()