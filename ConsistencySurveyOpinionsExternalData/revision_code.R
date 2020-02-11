require(HH)
require(colorspace)
require(RColorBrewer)
require(classInt)
require(mapStats)
require(foreign)
require(rgdal)
require(rgeos)
require(xtable)

par.orig<-c(5, 4, 4, 2) + 0.2
par.map<-rep(0,4)
omd.orig<-par()$omd

#get data and convert from SPSS format
y2008 <- read.spss("knightfoundation2008sotcdata.por", to.data.frame=TRUE)
y2009 <- read.spss("knightfoundation2009sotcdata.por", to.data.frame=TRUE)
y2010 <- read.spss("knightfoundation2010sotcdata.por", to.data.frame=TRUE)
colnames(y2008)<-tolower(colnames(y2008))
colnames(y2009)<-tolower(colnames(y2009))
colnames(y2010)<-tolower(colnames(y2010))

#subset the data from Long Beach
lb2008<-y2008[ y2008$qsb == "Long Beach, CA", ]
lb2009<-y2009[ y2009$qsb == "Long Beach, CA", ]
lb2010<-y2010[ y2010$qsb == "Long Beach, CA", ]

#recode races so years match
lb2008$race[ lb2008$qd111=="Some other race (list)" ]<-1
lb2008$race[ lb2008$qd111=="(DK)" ]<-2
lb2008$race[ lb2008$qd111=="(Refused)" ]<-3
lb2008$race[ lb2008$qd111=="White" ]<-6
lb2008$race[ lb2008$qd111=="(Hispanic)" ]<-11
lb2008$race[ lb2008$qd111=="(More than one)" ]<-12
lb2008$race[ lb2008$qd111=="Native Hawaiian or other Pacific Islander" ]<- 10
lb2008$race[ lb2008$qd111=="American Indian or Alaskan Native" ]<- 9
lb2008$race[ lb2008$qd111=="Asian (If necessary, read:) includes Chinese, Filipino..." ]<- 8
lb2008$race[ lb2008$qd111=="Black or African-American" ]<- 7
lb2008$race[ lb2008$qd111=="None" ]<-3

lb2009$race<-lb2009$qd111
lb2010$race<-lb2010$qd111

#collect all relevant variables across years into one dataset
lb3yr<-rbind(lb2008[, c("qs3a","q18r","q19r","q7ar","q10r","q15abr","q7fr","q7lr","qd9","race","qd10","weight","qd4","qd5a","qd5b")], 
		   lb2009[, c("qs3a","q18r","q19r","q7ar","q10r","q15abr","q7fr","q7lr","qd9","race","qd10","weight","qd4","qd5a","qd5b")],
             lb2010[, c("qs3a","q18r","q19r","q7ar","q10r","q15abr","q7fr","q7lr","qd9","race","qd10","weight","qd4","qd5a","qd5b")])

lb3yr$race_grp[ lb3yr$race==6 ]<-"white"
lb3yr$race_grp[ lb3yr$race==7 ]<-"black"
lb3yr$race_grp[ lb3yr$race==11 ]<-"hispanic"
lb3yr$race_grp[ lb3yr$race %in% c(1,2,3) ]<-NA
lb3yr$race_grp[ lb3yr$race %in% c(8,9,10,12) ]<-"other"

#read in all shapefiles and align coordinates
#zip codes that are in Long Beach
lb.zips<-c( 90802:90810, 90813:90815, 90822, 90831:90835)
CA <-readShapeSpatial("calif/tl_2010_06_zcta510.shp")

#select zip codes from CA dataset just of Long Beach
LB <-CA[ CA$ZCTA5CE10 %in% lb.zips,]
LB@data$ZCTA5CE10<- factor(LB@data$ZCTA5CE10)

#boundary file
LBboundary<- readShapeSpatial("CityBoundary/CityBoundary.shp", proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
proj4string(LBboundary)=CRS("+init=epsg:2229")
LBboundary<-spTransform(LBboundary, CRS("+proj=longlat +datum=WGS84"))

#parks
LBparks<- readShapeSpatial("Parks/Parks.shp", proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
proj4string(LBparks)=CRS("+init=epsg:2229")
LBparks<-spTransform(LBparks, CRS("+proj=longlat +datum=WGS84"))

#water areas
LBwater<- readShapeSpatial("water/Waterways.shp", proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
proj4string(LBwater)=CRS("+init=epsg:2229")
LBwater<-spTransform(LBwater, CRS("+proj=longlat +datum=WGS84"))

#city schools
LBschools<-readShapeSpatial("schools/Schools.shp", proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
proj4string(LBschools)=CRS("+init=epsg:2229")
LBschools<-spTransform(LBschools, CRS("+proj=longlat +datum=WGS84"))


LB.copy<-LB

#capture the original areas of each zip code so we can adjust areas now
area_orig <- c()
for (k in 1:13) {

area_orig[k] <- LB@polygons[[k]]@Polygons[[1]]@area

}
LB@data$area_orig <- area_orig



#there are a few zipcodes that are partially outside of the city boundary
#replace these shapes with intersections.  90802 and 90803 are the water-
#bordering zip codes so we just want the part on land. Only 90810 really is
#divided between two cities.

LB@data$area_modified <- LB@data$area_orig

zips.out <- which( LB$ZCTA5CE10 %in% c(90802, 90803, 90810))
for (k in zips.out) {

x<-gIntersection(LBboundary, LB[ k, ])
LB@polygons[[k]]@Polygons[[1]]@coords<-x@polygons[[1]]@Polygons[[1]]@coords
LB@polygons[[k]]@Polygons[[1]]@area<-x@polygons[[1]]@Polygons[[1]]@area

LB@data$area_modified[k] <- LB@polygons[[k]]@Polygons[[1]]@area

}

#this is necessary because only 90810 is REALLY outside the city boundary
#the other shapefiles for some reason 
LB@data$area_fraction <- LB@data$area_modified/LB@data$area_orig
LB@data$area_fraction[ LB@data$ZCTA5CE10 !=90810] <- 1

############### table of population comparisons


#table of weighted respondent counts by zip code
lbcounts<-na.omit(lb3yr[, c("qs3a","weight") ])
zip.table.wt<- as.data.frame(tapply(lbcounts$weight, lbcounts$qs3a, sum))
zip.table.wt<-cbind(zip.table.wt, as.character(rownames(zip.table.wt)))
names(zip.table.wt)<-c("weight", "zip")

#table of raw respondent counts by zip code
zip.table.raw<-as.data.frame(table(lbcounts$qs3a))
names(zip.table.raw)<-c("zip","frequency")

#merge the two tables and drop missing zip codes
zip.table<-merge(zip.table.raw, zip.table.wt, by="zip")
zip.table<-zip.table[ zip.table$zip %in% c(90802:90808, 90810, 90813:90822),]
totals<-colSums(zip.table[,c("frequency","weight")])
zip.table$raw.pct<-100*zip.table$frequency/totals[1]
zip.table$weight<-100*zip.table$weight/totals[2]

#combine with actual population counts from Census
census.pop<- read.csv("LBpop.2010.csv", header=TRUE)
colnames(census.pop)[1:2] <- c("zip","census_pop")

adj <- LB@data[ LB@data$ZCTA5CE10 %in% census.pop$zip ,c("ZCTA5CE10","area_fraction")]
adj <- adj[order(adj$ZCTA5CE10),]

#adjust by fraction actually inside the city
census.pop$GE16_65 <- census.pop$GE16 - census.pop$GE65
census.pop$GE16 <- round(census.pop$census_pop*adj$area_fraction)
census.pop$GE16_65 <- round(census.pop$GE16_65*adj$area_fraction) 
zip.table <- merge(zip.table, census.pop[,c(1,3)], by="zip")



zip.table$census.pct <- 100*zip.table$GE16/sum(zip.table$GE16)

toLatex(xtable(zip.table[,c(1,2,4,3,6)]))


############### map of Long Beach and surrounding areas
LB.area<-CA[ CA$ZCTA5CE10 %in% c(lb.zips, 90744, 90745, 90746, 90220, 90059, 90262, 90723, 90706, 90703, 90630, 90720, 90740, 90221),]

#there are a few tiny zipcodes in LB map that have nobody in them. exclude these
zips.out <- which( LB.area$ZCTA5CE10 %in% c(90802, 90803))
for (k in zips.out) {

x<-gIntersection(LBboundary, LB.area[ k, ])
LB.area@polygons[[k]]@Polygons[[1]]@coords<-x@polygons[[1]]@Polygons[[1]]@coords
LB.area@polygons[[k]]@Polygons[[1]]@area<-x@polygons[[1]]@Polygons[[1]]@area

}


#map of Long Beach and surrounding areas

LBboundary.temp<-LBboundary

new.bbox<-matrix(cbind(c(-118.25, 33.712),c(-118.045,33.945)), ncol=2)
colnames(new.bbox)<-c("min","max")
rownames(new.bbox)<-c("x","y")

attributes(LB.area)$bbox<-new.bbox
attributes(LBboundary.temp)$bbox<-attributes(LB.area)$bbox
attributes(LBwater)$bbox<-attributes(LB.area)$bbox

pdf("area_map.pdf")
par(mar=c(0,0,2,0))
plot(LBboundary.temp, border=NA,  col="bisque")
plot(LB.area, add=TRUE, border="grey")
plot(LBwater, add=TRUE, col="cyan3", border=NA)
plot(LBzones[ LBzones$OBJECTID %in% c(1678,1356, 1805), ], border=NA, col="grey", add=TRUE)
plot(LBboundary.temp, add=TRUE, border="red", lwd=2)

title("City of Long Beach, CA, and surrounding area",  cex.main=1.6)
inLB<-LB.area$ZCTA5CE10 %in% lb.zips
text(coordinates(LB.area)[inLB ,1], coordinates(LB.area)[inLB,2],
   LB.area$ZCTA5CE10[ inLB ], cex=1, col="red")
text(x=c(-118.21, -118.15, -118.22, -118.16, -118.165, -118.15, -118.13, 
         -118.11, -118.26, -118.045, -118.08, -118.13, -118.26), 
     y=c(33.73, 33.715, 33.9, 33.815, 33.8, 33.93, 33.745, 
          33.785, 33.83, 33.82, 33.73, 33.855, 33.75), 
     labels=c("city port", "Pacific Ocean","Compton","airport","Signal Hill",
     "Los Angeles", "marina", "university", "Carson","Cypress","Seal Beach","Lakewood", "naval\ncomplex"))
arrows(x0=-118.15, x1=-118.155, y0=33.935, y1=33.948)
dev.off()

##################################### crime rate data

#read in police district data
LBdist<- readShapeSpatial("PoliceReportingDistricts/PoliceReportingDistricts.shp", proj4string=CRS("+proj=utm +zone=33 +datum=WGS84"))
proj4string(LBdist)=CRS("+init=epsg:2229")
LBdist<-spTransform(LBdist, CRS("+proj=longlat +datum=WGS84"))

#crime totals by reporting district
#do the second way, not the first way
crime.rd<-read.csv("LBcrime.2010.csv", header=TRUE)
crime.rd<-na.omit(crime.rd)
crime.rd$PO_RD_NO<-as.character(crime.rd$PO_RD_NO)
crime.rd$PO_RD_NO[ nchar(crime.rd$PO_RD_NO)==2 ] <- paste("0",crime.rd$PO_RD_NO[ nchar(crime.rd$PO_RD_NO)==2 ],sep="")
crime.rd<- crime.rd[ crime.rd$PO_RD_NO %in% LBdist$PO_RD_NO , ]


#now apportion crime counts by zip code
good.zips<-(! (LB@data$ZCTA5CE10 %in% c(90822, 90831)))

zips1<-LB@data$ZCTA5CE10[ good.zips ]
zipmat<-matrix(nrow=length(zips1), ncol=length(crime.rd$PO_RD_NO))
rownames(zipmat)<-zips1
colnames(zipmat)<-c(crime.rd$PO_RD_NO)


for (zip in zips1 ) {
   #zip code area
   zip.poly<-LB[ LB@data$ZCTA5CE10==zip, ]
   
   for (rd in crime.rd$PO_RD_NO) {
      rd.poly<-LBdist[ LBdist$PO_RD_NO==rd, ]
      inter<-gIntersection(zip.poly, rd.poly) 
      inter.area<-0
  
      if (!( zip==90802 & rd=="031")) { 

      if (! is.null(inter)) { 
          if (nrow(inter@polygons[[1]]@Polygons[[1]]@coords )>1) {   
        # plot(inter)
                inter.area<-inter@polygons[[1]]@Polygons[[1]]@area 
             }
           }
        }

      zipmat[as.character(zip), as.character(rd)]<-inter.area
   }

}


#find fraction of each RD in each zip code

colsum<-as.matrix(colSums(zipmat[, crime.rd$PO_RD_NO]))
for (k in 1:nrow(zipmat)) {
 zipmat[k,] <- zipmat[k, ]/t(colsum)
}

zipmat.person<-zipmat
zipmat.property<-zipmat

for (k in 1:nrow(zipmat)) {
  zipmat.person[k,  ] <- zipmat.person[k, ]*t(crime.rd$person)
  zipmat.property[k,  ] <- zipmat.property[k, ]*t(crime.rd$property)
}

#contain totals for each zip code
zip <- as.numeric(rownames(zipmat.person))

zipmat.person<-cbind(zipmat.person, person=rowSums( zipmat.person, na.rm=TRUE ), zip=zip)
zipmat.property<-cbind(zipmat.property, property=rowSums( zipmat.property, na.rm=TRUE), zip=zip)

zipmat.person <- zipmat.person[ order(zip),]
zipmat.property <- zipmat.property[ order(zip),]

#calculate person and property crime rates per 1000
crime.zip <- merge(zipmat.person[, c("person","zip")], zipmat.property[, c("property","zip")], by="zip")
crime.zip <- merge(crime.zip, census.pop[,c("zip","GE16_65")], by="zip")
crime.zip$person_rate <- 1000*crime.zip$person/crime.zip$GE16_65
crime.zip$property_rate <- 1000*crime.zip$property/crime.zip$GE16_65

#recode safety and crime variables
lb3yr$feel.unsafe <- 100*(lb3yr$q18r=="Low")
lb3yr$crime.bad <- 100*(lb3yr$q19r=="Low")

feel.unsafe <- calcStats(d=lb3yr, var="feel.unsafe", stat="mean", wt.var="weight",
          d.geo.var="qs3a")[[1]]$Mean
crime.bad <- calcStats(d=lb3yr, var="crime.bad", stat="mean", wt.var="weight",
          d.geo.var="qs3a")[[1]]$Mean
d <- na.omit(merge(feel.unsafe, crime.bad, by="qs3a"))
colnames(d) <- c("zip","feel.unsafe","crime.bad")
crime.zip <- merge(crime.zip, d, by="zip")


#show association between survey responses and external data
par(mar=c(5,8.3,4,2)+.1)
par(omd=c(.04,1,0,1))

reg1<-lm(crime.bad~  person_rate, data=crime.zip)
reg2<-lm(feel.unsafe ~ person_rate , data=crime.zip)

pdf("crime_person_byzip2.pdf")
xyplot( crime.bad ~ person_rate, data=crime.zip, 
        xlab=list("Crimes against persons per 1,000 in zip code (age 16-64)",cex=1.5),
        ylab=list("Percent saying crime is bad",cex=1.5), 
        main=list("Accuracy of Crime Perceptions", cex=1.5),
        pch=2, cex=1.5, col=2, type=c("p","r"), xlim=c(25,80), ylim=c(15,85), lwd=1,
        panel=function(x,y,...){
        panel.xyplot(x,y,...) 
         panel.text(c(crime.zip$person_rate, 65),
                    c(crime.zip$crime.bad, 20), cex=1.2,
                    labels=c(crime.zip$zip, "Not significant"),
                    font=c(rep(1,11),2), 
                    pos=ifelse(crime.zip$zip %in% c("90804","90813"),4,1))})
dev.off()


pdf("safety_person_byzip2.pdf")
xyplot( feel.unsafe ~ person_rate, data=crime.zip, 
        xlab=list("Crimes against persons per 1,000 in zip code (age 16-64)", cex=1.5),
        ylab=list("Percent saying they feel unsafe", cex=1.5), 
        main=list("Accuracy of Safety Perceptions", cex=1.5),
        pch=15, cex=1.5, col=1, type=c("p","r"), xlim=c(25,80),ylim=c(15,85), lwd=1,
        panel=function(x,y,...){
        panel.xyplot(x,y,...) 
         panel.text(c(crime.zip$person_rate, 65),
                    c(crime.zip$feel.unsafe, 20), cex=1.2,
                    labels=c(crime.zip$zip, "Slope=0.6917, p=0.0528"),
                    font=c(rep(1,11),2), pos=ifelse(crime.zip$zip=="90806",4,1))})
dev.off()



##################################### Likert plot for incomes

#percent nonwhite
lb3yr$nonwhite <- ifelse(lb3yr$race_grp == "white", 0, 100)

#Likert plot for income distribution
lb3yr.qd9<-na.omit(lb3yr[,c("qs3a","qd9","weight")])
wtsum<- as.data.frame(tapply(lb3yr.qd9$weight, lb3yr.qd9[c("qs3a","qd9")], sum))
wtsum[ is.na(wtsum)] <-0
tot<-rowSums(wtsum)

wtsum<-cbind(wtsum,tot)
wtsum<-wtsum[ wtsum$tot>0 ,]
wtsum$ZCTA5CE10<-rownames(wtsum)
wtsum<-wtsum[ wtsum$ZCTA5CE10 !=90822, ]
colnames(wtsum)[1:8]<-c("<15","15-25","25-35","35-45","45-55","55-75","75-100",">=100")
#sort in order of which ones have highest two percentage of rich people
ord.hi<- rev( order(rowSums(wtsum[, c("75-100",">=100")])/rowSums(wtsum[, 1:8])))

wtsum$ZCTA5CE10<-factor(wtsum$ZCTA5CE10)

income.dist<-wtsum

race.side <- na.omit(calcStats(d=lb3yr, var="nonwhite", stat="mean", wt.var="weight",
                     d.geo.var="qs3a")[[1]]$Mean)[-12,]
colnames(race.side)[2] <- "nonwhite"
inc.order<-rev(order((income.dist[,c("75-100")]+income.dist[,c(">=100")])/income.dist$tot))
race_colors <- findColours(classIntervals(var=race.side$nonwhite, n=5, style="jenks"), brewer.pal(5, "Reds"))
race_colors<-race_colors[ rev(inc.order) ]
rownames(income.dist)[inc.order]
income.dist <- merge(income.dist, race.side, by.x="ZCTA5CE10", by.y="qs3a")


lik.plot <- likert( ZCTA5CE10 ~ "<15" +"15-25"+ "25-35"+"35-45"+"45-55"+"55-75"+"75-100"+">=100", 
     data=income.dist, as.percent=TRUE, positive.order=TRUE,par.settings.in=list(mai=rep(5,4)),
     xlab=list("Percent in income category (in thousands of dollars)", cex=1.3),
     ylab=list(expression("Zip code ordered by share with income  " >= "$75,000"), cex=1.3), 
     ylab.right=list("Racial Minority Status as in Figure 2", cex=1.3),
     main=list("Income Distribution by Zip Code", cex=2), 
     horizontal=TRUE,
     transposeAxisLabels=FALSE, ReferenceZero=6.5, col=rev(sequential_hcl(14)[c(1:3,5,7,9,12,14 )]),
     scales.in=list(cex=1.3),
     auto.key=list(cex=1.3), rightAxisLabels=rep("",11),
     resize.height=income.dist$nonwhite[inc.order]
     )
lik.plot$panel.args.common$border <- "#B6B9D0"
lik.plot$trellis.settings$clip$panel <- FALSE
lik.plot$par.settings$clip$panel <- FALSE

lik.plot.panel <- lik.plot$panel
lik.plot$panel <- function(...) {
  lik.plot.panel(...)
  panel.rect(x=83, y=1:11, width=8, height=.6, col=race_colors)
}
lik.plot
pdf("income_zipcode2.pdf", width=9, height=7)
lik.plot
dev.off()

#trellis.focus()
#current.panel.limits()

dump("income.dist","")
dump("race_colors","")


##################################### mapStats plots


#set bounds for image
bounds <- attributes(LB)$bbox
ranges <- abs(bounds[,2]-bounds[,1])
new_bounds <- bounds
new_bounds[,1] <- new_bounds[,1] - c(0.03, -.1)*ranges
new_bounds[,2] <- new_bounds[,2] + 0.03*ranges
xbox <- new_bounds[1,]
ybox <- new_bounds[2,]

#percent nonwhite

pdf("race_map.pdf")
mapStats(d=lb3yr[ ,c("qs3a","nonwhite","weight")], var="nonwhite", wt.var="weight",
         stat="mean", wt.label=FALSE, d.geo.var="qs3a", map.file=LB, map.geo.var="ZCTA5CE10",
         ngroups=5, col.pal="Reds", titles="Percent Hispanic or non-white by zip code", style="jenks", 
         cell.min=3, cex.label=1.2, xlim=xbox, ylim=ybox, cex.title=1.5)
dev.off()
 
black_square <- list("sp.points", matrix(c(-118.1, 33.87), ncol=2), 
                      pch=15, col="black", cex=4, lwd=5)

pdf("unsafe_map.pdf")
mapStats(d=na.omit(lb3yr[ ,c("qs3a","feel.unsafe","weight")]), var="feel.unsafe", wt.var="weight",
         stat="mean", wt.label=FALSE, d.geo.var="qs3a", map.file=LB, map.geo.var="ZCTA5CE10",
         ngroups=5, col.pal="Reds", titles="Percent feeling unsafe at night near home by zip code", style="jenks", 
         cell.min=3, cex.label=1.2, , xlim=xbox, ylim=ybox, cex.title=1.5,
         sp_layout.pars=list(black_square))         
dev.off()
 
lb3yr$crime.bad <- ifelse(lb3yr$q19r=="Low", 100, 0)

red_triangle <- list("sp.points", matrix(c(-118.1, 33.87), ncol=2), 
                      pch=2, col="red", cex=4, lwd=5)

pdf("crime_map.pdf")
mapStats(d=na.omit(lb3yr[ ,c("qs3a","crime.bad","weight")]), var="crime.bad", wt.var="weight",
         stat="mean", wt.label=FALSE, d.geo.var="qs3a", map.file=LB, map.geo.var="ZCTA5CE10",
         ngroups=5, col.pal="Reds", titles="Percent saying crime level is high by zip code", style="jenks", 
         cell.min=3, cex.label=1.2,xlim=xbox, ylim=ybox, cex.title=1.5,
         sp_layout.pars=list(red_triangle))
dev.off()

lb3yr$parks <- ifelse(lb3yr$q7ar=="High", 100, 0)


park1 <- list("sp.polygons", LBparks, fill=ifelse(LBparks$PK_TYPE=="GC", "DarkGreen", "LightGreen"))
water <- list("sp.polygons", LBwater, fill="cyan3", col="transparent")


pdf("park_map.pdf")
mapStats(d=na.omit(lb3yr[ ,c("qs3a","parks","weight")]), var="parks",  wt.var="weight", map.label=TRUE,
         stat="mean", wt.label=FALSE, d.geo.var="qs3a", map.file=LB, map.geo.var="ZCTA5CE10",
         ngroups=5, col.pal="Reds", titles="Percent saying park availability is high, by zip code", style="jenks", 
         cell.min=3, cex.label=1.2 ,xlim=xbox, ylim=ybox, sp_layout.pars=list(park1), cex.title=1.5)
dev.off()

lb3yr$schools.good <- ifelse(lb3yr$q7fr=="High", 100, 0)

pdf("schools_map.pdf")

mapStats(d=na.omit(lb3yr[ ,c("qs3a","schools.good","weight")]), var="schools.good", wt.var="weight",
         stat="mean", wt.label=FALSE, d.geo.var="qs3a", map.file=LB, map.geo.var="ZCTA5CE10",
         ngroups=5, col.pal="Reds", titles="Percent saying public schools are good by zip code", style="jenks", 
         cell.min=3, cex.label=1.2, xlim=xbox, ylim=ybox, cex.title=1.5)
dev.off()

##################################### school quality plot

#get API data to match cds with school names and check which ones are public, etc.

API<-read.csv("api10gdb.csv",header=TRUE)
colnames(API)<-tolower(colnames(API))
LBschools.api<-API[ API$dname=="Long Beach Unified" , c("sname","sch_wide","api10","rtype","stype","api09","cds","median10","median09","tested","enroll")]
LBschools.api$cds<-as.character(as.numeric(LBschools.api$cds))
LBschools.api$sname<-as.vector(toupper(LBschools.api$sname))

#make some edits to the data in the shapefile
school.public<-(LBschools$TYPE=="PUBLIC" & LBschools$LEVEL_ %in% c("ELEM","ELEM-HIGH","HIGH","MIDDLE","MIDDLE-HIGH"))

#missing values and LB school names
LBschools$NAME<-as.character(toupper(LBschools$NAME))
subset<-(school.public & (! is.na(LBschools$FULLNAME) & is.na(LBschools$NAME)) )
LBschools$NAME[ subset ]<- LBschools$FULLNAME[ subset ]
LBschools$NAME[ school.public ]<- sub(pattern="^(\\w+)(\\s.+|)$", replacement="\\1", perl=TRUE, x=LBschools$NAME[ school.public ])

#keep only public schools
LBschools<-LBschools[ school.public,]

#some quick edits to api names
LBschools.api$sname <- sub(pattern="EDUCATIONAL PARTNERSHIP HIGH SCHOOL", replacement="EPHS",  perl=TRUE, x=LBschools.api$sname )
LBschools.api$sname <- sub(pattern="CALIFORNIA ACADEMY OF MATHEMATICS AND SC", replacement="CAMS",  perl=TRUE, x=LBschools.api$sname )
LBschools.api$sname <- sub(pattern="^POLYTECHNIC", replacement="POLY",  perl=TRUE, x=LBschools.api$sname )
LBschools.api$sname <- sub(pattern="^(\\w+)(\\s.+|)$", replacement="\\1", perl=TRUE, x=LBschools.api$sname)


#rename column
colnames(LBschools@data)[ colnames(LBschools@data)=="ZIP"] <-"ZCTA5CE10"
colnames(LBschools.api)[ colnames(LBschools.api)=="sname" ]<-"NAME"
LBschools.api<-LBschools.api[ LBschools.api$NAME !="",]


#make EPHS one: Anaheim
LBschools@data$FULLNAME[ LBschools@data$NAME=="EPHS" ]<-"EPHS - ANAHEIM SITE"
LBschools@data$ZCTA5CE10[ LBschools@data$NAME=="EPHS" ] <-90804
LBschools <- LBschools[ LBschools$FULLNAME !="DAVID STARR JORDAN HIGH SCHOOL", ]


#need unique ones
u<-rownames(unique( LBschools@data[ ,c("NAME","FULLNAME","ZCTA5CE10")]))

LBschools<-LBschools[ rownames(LBschools@data) %in% u, ]

#only keep the ones we have school data for.
#R doesn't maintain the original order when 
#x has values that don't match y, these are moved to the bottom.

LBschools<-LBschools[ LBschools$NAME %in% LBschools.api$NAME, ]

#merge the data
LBschools@data<-merge( LBschools@data , LBschools.api[ ,c("NAME","sch_wide","api10","cds","median10")], by="NAME", all.x=TRUE, sort=FALSE)

boxplot(api10 ~ ZCTA5CE10, data=LBschools@data)

#difference from group median
#LBschools@data$api10.diff<- 100*(LBschools@data$api10 - LBschools@data$median10)/LBschools@data$median10
LBschools@data$api10.diff<- LBschools@data$api10 - LBschools@data$median10

lb3yr$schools.good <- 100*(lb3yr$q7fr=="High")

#calculate color scheme for survey school ratings
par(mar=par.map, omd=omd.orig)
school.ratings <- na.omit(calcStats(d=lb3yr, var="schools.good", stat="mean", wt.var="weight",
                            d.geo.var="qs3a")$schools.good$Mean)[-12,]
colnames(school.ratings) <- c("zip","schools.good")

#rescale the api
LBschools$api10.rescale<-LBschools$api10/mean(LBschools$api10)

#Median and mean API by zip code
apizip <- calcStats(d=LBschools@data, var="api10", d.geo.var="ZCTA5CE10", 
                     stat=c("mean","quantile"), quantiles=.5)$api10[1:2]
apizip <- merge(x=apizip$Mean, y=apizip$Median, by="ZCTA5CE10", sorted=FALSE)

names(apizip)[2:3]<-c("api.mean", "api.median")
apizip <- merge(x=apizip, y=school.ratings, by.x="ZCTA5CE10", by.y="zip", sorted=FALSE)

par(omd=c(.1,1,0,1))

apizip<-merge(apizip, income.dist[,c("75-100",">=100","tot","ZCTA5CE10")], 
              by="ZCTA5CE10",sorted=FALSE)



#median API by zip code
#par(mar=(par.orig+c(1,1,0,0) ))


apizip$income.high<-100*(apizip[,"75-100"]+ apizip[,">=100"])/apizip$tot

class.inc<-classIntervals(var=apizip$income.high, n=5, style="jenks")
apizip$colors.inc<-findColours(class.inc, brewer.pal(5, "Blues"))
class.schools<-classIntervals(var=school.ratings$schools.good, n=5, style="jenks")
apizip$colors.schools<-findColours(class.schools, brewer.pal(5, "Reds"))

#par(mar=rep(0,4))
par(mar=c(5,8.3,4,8)+.1)
par(omd=c(.04,1,0,1))

xoff <- rep(0,11)
xoff[ apizip$ZCTA5CE10 %in% c("90810","90804") ] <- 2
xoff[ apizip$ZCTA5CE10 =="90815" ] <- -2
yoff <- rep(0,11)
yoff[ apizip$ZCTA5CE10 =="90813" ] <- 3


summary(lm(schools.good ~ api.median, data=apizip))
summary(lm(schools.good ~ api.mean, data=apizip))

schplot <- xyplot( schools.good ~ api.median, data=apizip, ylim=c(12, 40),  
        main=list("Perception of public school quality", cex=1.5),
        xlab=list("Median 2010 public school Annual Performance Index (API)",cex=1.2),
        ylab=list("Perception of public school quality", cex=1.2),las=1,
        pch=15, cex=2.35, type=c("p","r"), col=apizip$colors.inc,
        panel=function(x,y,...){
        panel.xyplot(x,y,...) 
         panel.text(c(apizip$api.median + xoff , 790),
                    c(apizip$schools.good +yoff,15) , cex=1.2,
                    labels=c(as.character(apizip$ZCTA5CE10), "slope= - 0.1242, p=0.0049"),
                    font=c(rep(1,11),2), 
                    pos=c(ifelse(apizip$ZCTA5CE10 %in% c("90810","90804"), 4,
                          ifelse(apizip$ZCTA5CE10 =="90815",2,1)),1))})


pdf("school_perception2.pdf")
schplot
schplot$panel.args.common$border <- "#B6B9D0"
schplot$trellis.settings$clip$panel <- FALSE
schplot$par.settings$clip$panel <- FALSE

schplot.panel <- schplot$panel
schplot$panel <- function(...) {
  schplot.panel(...) 
  panel.rect(x=897.5, y=apizip$schools.good, width=6, height=1.2, col=apizip$colors.schools)
  panel.rect(x=855, y=seq(35.5,28, length.out=5), width=6, height=1.2, col=attributes(apizip$colors.inc)$palette)
  panel.rect(x=apizip$api.median, y=apizip$schools.good, width=6, height=1.2)
  panel.text(x=c(865, rep(871,5)), y=c(38, seq(35.5,28, length.out=5)), cex=1.2, 
             labels=c("Percent with income\n>=$75,000",
                       paste(sprintf("%4.1f", class.inc$brks[1:(length(class.inc$brks)-1)]),"% ", c(" ", rep("<",4)), sep="")) )
  #rect(xleft=840, xright=900, ytop=40, ybottom=23)
}

schplot
apizip$schools.good

ifelse(crime.zip$zip=="90813",4,1))})
)


        panel=function(x,y,...){
        panel.xyplot(x,y,...) 
         panel.text(c(crime.zip$person_rate, 70),
                    c(crime.zip$crime.bad, 20),
                    labels=c(crime.zip$zip, "Not significant"),
                    font=c(rep(1,11),2), 
                    pos=ifelse(crime.zip$zip=="90813",4,1))})






reg.sch<-lm( schools.good ~ api.median , data=apizip)

m<-plot(schools.good ~ api.median , data=apizip, xlim=c(730, 905), ylim=c(15,40), 
     ylab="",
     xlab="Median 2010 public school Annual Performance Index (API)", 
     main="Perception of public school quality", cex.main=1.5,
     cex=2.5, pch=22, cex.axis=1.2, cex.lab=1.2, las=1, bg=apizip$colors.inc)
abline(reg.sch, col=1)
pos.vec<-rep(4,11)
pos.vec[apizip$ZCTA5CE10 %in% c(90805,90807)]<-1

#orig label cex=1.2
text(apizip$api.median, apizip$schools.good , apizip$ZCTA5CE10, pos=pos.vec, cex=1.15)
text(680, 28, "Percent rating\nlocal public\nschools highly", cex=1.2,  crt=0, xpd=NA, pos=3)
legend("bottomleft",legend="p=0.0033, slope=-0.1116", cex=1.2)
leg<-legend(x=911, y=40,
   legend=paste(sprintf("%4.1f", class.inc$brks[1:(length(class.inc$brks)-1)]),"% ", c(" ", rep("<",4)), sep=""),
   fill=attributes(apizip$colors.inc)$palette, cex=1.1, title="Percent with\nincome >=$75,000", 
   bty="n", xjust=1)

rect(xleft=leg$rect[["left"]], xright=(leg$rect[["w"]] + leg$rect[["left"]])+5,
     ytop=(leg$rect[["top"]]+1), ybottom=(leg$rect[["top"]] - leg$rect[["h"]]))

#plot red shades from school quality
points(x=rep(920,11), y=apizip$schools.good, col="black", pch=22, 
     cex=2.5, cex.axis=1.2, cex.lab=1.1, bg=apizip$colors.school, xpd=NA)

dev.off()

