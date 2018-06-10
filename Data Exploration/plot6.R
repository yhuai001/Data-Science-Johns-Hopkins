##Q6:Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
subNEI4 <- subset(NEI, fips == "06037" & type == "ON-ROAD")
baltimore_by_mv <- with(subNEI3, aggregate(Emissions,by=list(year,fips),sum))
LA_by_mv <- with(subNEI4, aggregate(Emissions,by=list(year,fips),sum))
names(baltimore_by_mv) <- c("year","fips","Emissions")
names(LA_by_mv) <- c("year","fips","Emissions")
baltimore_by_mv$name <- "Baltimore"
LA_by_mv$name <- "Los Angeles"
compare_mv <- rbind(baltimore_by_mv,LA_by_mv)

png("plot6.png", width = 600, height = 600)
g <- ggplot(compare_mv, aes(factor(year),y=Emissions))
g <- g + geom_bar(stat="identity",group="fips",fill="orange")
g <- g + facet_grid(.~ fips)
g <- g + geom_line(stat="identity",colour="blue",group="fips")
g <- g + geom_point()
g <- g + labs(x="Years",y="PM2.5 Emissions", title="Motor Vehicle total PM2.5 Emissions in Baltimore(06037) v.s LA(24510)")
g


dev.off()
