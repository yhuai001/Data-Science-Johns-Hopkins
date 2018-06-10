##Q5:How have emissions from motor vehicle sources changed from 1999¨C2008 in Baltimore City?
subNEI3 <- subset(NEI, fips == "24510" & type == "ON-ROAD")
baltimore_by_mv <- with(subNEI3, aggregate(Emissions,by=list(year),sum))
names(baltimore_by_mv) <- c("year","Emissions")

png("plot5.png", width = 600, height = 600)
g <- ggplot(baltimore_by_mv, aes(factor(year),y=Emissions))
g <- g + geom_line(stat="identity",group="year")
g <- g + geom_point()
g <- g + labs(x="Years",y="PM2.5 Emissions", title="Baltimore total PM2.5 Emissions from motor vehicle from 1999 to 2008")
g

dev.off()