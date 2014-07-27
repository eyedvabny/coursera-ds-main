library(ggplot2)
library(gridExtra)

# Load in the data
NEI <- readRDS("FNEI_data/summarySCC_PM25.rds")
SCC <- readRDS("FNEI_data/Source_Classification_Code.rds")

# For some reason merging takes forever, so using a workaround:
# 1) Search SCC for all MV-related SCC IDs (onroad category)
# 2) Filter out NEI by SCC ID from #1 and the two city's FIPS
mv_scc <- as.character(SCC$SCC[grep("Vehicle",SCC$SCC.Level.Two)])
mv_bal_nei <- NEI[NEI$SCC %in% mv_scc & NEI$fips=="24510",]
mv_la_nei <- NEI[NEI$SCC %in% mv_scc & NEI$fips=="06037",]

mv_bal_nei$city <- "Baltimore"
mv_la_nei$city <- "Los Angeles"
mv_nei <- rbind(mv_bal_nei,mv_la_nei)
mv_nei$city <- factor(mv_nei$city)

# Name the two cities and aggregate pollution by year
mv_nei <- aggregate(Emissions~year+city,mv_nei,sum)

# Additionally track the relative % change by year
bal_emiss <- mv_nei$Emissions[mv_nei$city=="Baltimore"]
la_emiss <- mv_nei$Emissions[mv_nei$city=="Los Angeles"]

bal_emiss <- (bal_emiss[2:4]-bal_emiss[1:3])/bal_emiss[1:3]*100
la_emiss <- (la_emiss[2:4]-la_emiss[1:3])/la_emiss[1:3]*100

# Combine and reformat the data to make one data frame
mv_nei_pc <- rbind(
  cbind("Baltimore",bal_emiss,c(1,2,3)),
  cbind("Los Angeles",la_emiss,c(1,2,3)))

mv_nei_pc <- data.frame(
  city = factor(mv_nei_pc[,1]),
  per_chn = as.numeric(mv_nei_pc[,2]),
  yr_lab = factor(mv_nei_pc[,3],labels=c("99-02","02-05","05-08")))

# Plot
png(filename="plot6.png", height=480, width=960)
p1 <- ggplot(data=mv_nei, aes(x=year, y=Emissions, colour=city))+
      geom_line()+
      ggtitle("Total Emissions From Vehicles")+
      ylab("Total Emissions (tons)")+
      xlab("Year")+
      theme(legend.position="bottom",legend.title=element_blank())

p2 <- ggplot(data=mv_nei_pc, aes(x=yr_lab, y=per_chn, group=city, colour=city))+
      geom_line()+
      ggtitle("Change in Emissions From Vehicles")+
      ylab("Relative Percent Change")+
      xlab("Year")+
      theme(legend.position="bottom",legend.title=element_blank())

grid.arrange(p1, p2, ncol=2)
dev.off()