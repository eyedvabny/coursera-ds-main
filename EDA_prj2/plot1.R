# Load in the data
NEI <- readRDS("FNEI_data/summarySCC_PM25.rds")

# Aggregate the pollution by year
agg_emiss <- aggregate(Emissions~year,NEI,sum)

with(agg_emiss,{
  png(filename="plot1.png", height=480, width=480)
  plot(year,Emissions,
       type='l',
       main='Total Emissions',
       ylab='Emissions (tons)',
       xlab='Year')
  dev.off()
})