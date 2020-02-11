################### read in the SOTC data sets ################################################

d.08<- read.csv('http://streaming.stat.iastate.edu/dataexpo/2013/data/sotc-08.csv', header=T)
d.09<- read.csv('http://streaming.stat.iastate.edu/dataexpo/2013/data/sotc-09.csv', header=T)
d.10<- read.csv('http://streaming.stat.iastate.edu/dataexpo/2013/data/sotc-10.csv', header=T)

################################################################################################

###### subset the index variables for each year #################
d.08.sub<- d.08[,c(2,85,136,138,140,143:154)]
d.09.sub<- d.09[,c(2,98,151,153,155,158:164,166:170)]
d.10.sub<- d.10[,c(3,127,184,186,188,191:202)]

#################################################################

###### create matching column names across the years ############
colnames(d.08.sub)[1]<- "City"
colnames(d.09.sub)[1]<- "City"
colnames(d.10.sub)[1]<- "City"
colnames(d.08.sub)[2]<- "Urbanicity"
colnames(d.09.sub)[2]<- "Urbanicity"
colnames(d.10.sub)[2]<- "Urbanicity"
colnames(d.08.sub)[5]<- "CA"
colnames(d.10.sub)[5]<- "CA"

##################################################################

###### remove missing values from the data subsets ##############
d.08.sub2<- d.08.sub[complete.cases(d.08.sub),]
d.09.sub2<- d.09.sub[complete.cases(d.09.sub),]
d.10.sub2<- d.10.sub[complete.cases(d.10.sub),]

#################################################################


###### calculate the means for each city ############################################################
means.LOYALTY.08<- tapply(d.08.sub2$LOYALTY, d.08.sub2$City, mean)
means.PASSION.08<- tapply(d.08.sub2$PASSION, d.08.sub2$City, mean)
means.CA.08<- tapply(d.08.sub2$CA, d.08.sub2$City, mean)
means.BASIC_SE.08<- tapply(d.08.sub2$BASIC_SE, d.08.sub2$City, mean)
means.LEADERSH.08<- tapply(d.08.sub2$LEADERSH, d.08.sub2$City, mean)
means.EDUCATIO.08<- tapply(d.08.sub2$EDUCATIO, d.08.sub2$City, mean)
means.SAFETY.08<- tapply(d.08.sub2$SAFETY, d.08.sub2$City, mean)
means.AESTHETI.08<- tapply(d.08.sub2$AESTHETI, d.08.sub2$City, mean)
means.ECONOMY.08<- tapply(d.08.sub2$ECONOMY, d.08.sub2$City, mean)
means.SOCIAL_O.08<- tapply(d.08.sub2$SOCIAL_O, d.08.sub2$City, mean)
means.COMMUNIT.08<- tapply(d.08.sub2$COMMUNIT, d.08.sub2$City, mean)
means.INVOLVEM.08<- tapply(d.08.sub2$INVOLVEM, d.08.sub2$City, mean)
means.OPENNESS.08<- tapply(d.08.sub2$OPENNESS, d.08.sub2$City, mean)
means.SOCIAL_C.08<- tapply(d.08.sub2$SOCIAL_C, d.08.sub2$City, mean)
means.DOMAINS.08<- tapply(d.08.sub2$DOMAINS, d.08.sub2$City, mean)

d.08.means<- data.frame(means.LOYALTY.08,means.PASSION.08,means.CA.08,means.BASIC_SE.08,
                              means.LEADERSH.08, means.EDUCATIO.08,means.SAFETY.08,
                              means.AESTHETI.08,means.ECONOMY.08,means.SOCIAL_O.08,
                              means.COMMUNIT.08,means.INVOLVEM.08,means.OPENNESS.08,
                              means.SOCIAL_C.08,means.DOMAINS.08)

means.LOYALTY.09<- tapply(d.09.sub2$LOYALTY, d.09.sub2$City, mean)
means.PASSION.09<- tapply(d.09.sub2$PASSION, d.09.sub2$City, mean)
means.CA.09<- tapply(d.09.sub2$CA, d.09.sub2$City, mean)
means.BASIC_SE.09<- tapply(d.09.sub2$BASIC_SE, d.09.sub2$City, mean)
means.LEADERSH.09<- tapply(d.09.sub2$LEADERSH, d.09.sub2$City, mean)
means.EDUCATIO.09<- tapply(d.09.sub2$EDUCATIO, d.09.sub2$City, mean)
means.SAFETY.09<- tapply(d.09.sub2$SAFETY, d.09.sub2$City, mean)
means.AESTHETI.09<- tapply(d.09.sub2$AESTHETI, d.09.sub2$City, mean)
means.ECONOMY.09<- tapply(d.09.sub2$ECONOMY, d.09.sub2$City, mean)
means.SOCIAL_O.09<- tapply(d.09.sub2$SOCIAL_O, d.09.sub2$City, mean)
means.COMMUNIT.09<- tapply(d.09.sub2$COMMUNIT, d.09.sub2$City, mean)
means.INVOLVEM.09<- tapply(d.09.sub2$INVOLVEM, d.09.sub2$City, mean)
means.OPENNESS.09<- tapply(d.09.sub2$OPENNESS, d.09.sub2$City, mean)
means.SOCIAL_C.09<- tapply(d.09.sub2$SOCIAL_C, d.09.sub2$City, mean)
means.DOMAINS.09<- tapply(d.09.sub2$DOMAINS, d.09.sub2$City, mean)

d.09.means<- data.frame(means.LOYALTY.09,means.PASSION.09,means.CA.09,means.BASIC_SE.09,
                        means.LEADERSH.09, means.EDUCATIO.09,means.SAFETY.09,
                        means.AESTHETI.09,means.ECONOMY.09,means.SOCIAL_O.09,
                        means.COMMUNIT.09,means.INVOLVEM.09,means.OPENNESS.09,
                        means.SOCIAL_C.09,means.DOMAINS.09)

means.LOYALTY.10<- tapply(d.10.sub2$LOYALTY, d.10.sub2$City, mean)
means.PASSION.10<- tapply(d.10.sub2$PASSION, d.10.sub2$City, mean)
means.CA.10<- tapply(d.10.sub2$CA, d.10.sub2$City, mean)
means.BASIC_SE.10<- tapply(d.10.sub2$BASIC_SE, d.10.sub2$City, mean)
means.LEADERSH.10<- tapply(d.10.sub2$LEADERSH, d.10.sub2$City, mean)
means.EDUCATIO.10<- tapply(d.10.sub2$EDUCATIO, d.10.sub2$City, mean)
means.SAFETY.10<- tapply(d.10.sub2$SAFETY, d.10.sub2$City, mean)
means.AESTHETI.10<- tapply(d.10.sub2$AESTHETI, d.10.sub2$City, mean)
means.ECONOMY.10<- tapply(d.10.sub2$ECONOMY, d.10.sub2$City, mean)
means.SOCIAL_O.10<- tapply(d.10.sub2$SOCIAL_O, d.10.sub2$City, mean)
means.COMMUNIT.10<- tapply(d.10.sub2$COMMUNIT, d.10.sub2$City, mean)
means.INVOLVEM.10<- tapply(d.10.sub2$INVOLVEM, d.10.sub2$City, mean)
means.OPENNESS.10<- tapply(d.10.sub2$OPENNESS, d.10.sub2$City, mean)
means.SOCIAL_C.10<- tapply(d.10.sub2$SOCIAL_C, d.10.sub2$City, mean)
means.DOMAINS.10<- tapply(d.10.sub2$DOMAINS, d.10.sub2$City, mean)

d.10.means<- data.frame(means.LOYALTY.10,means.PASSION.10,means.CA.10,means.BASIC_SE.10,
                        means.LEADERSH.10, means.EDUCATIO.10,means.SAFETY.10,
                        means.AESTHETI.10,means.ECONOMY.10,means.SOCIAL_O.10,
                        means.COMMUNIT.10,means.INVOLVEM.10,means.OPENNESS.10,
                        means.SOCIAL_C.10,means.DOMAINS.10)


#####################################################################################################

######## Cluster Analysis on means ###################
plot(hclust(dist(d.08.means),method="average"),cex=0.8,main='2008')
groups <- cutree(hclust(dist(d.08.means)), h=.8)
rect.hclust(hclust(dist(d.08.means)), h=.8, 
            border=palette())

plot(hclust(dist(d.09.means),method="average"),cex=0.8,main='2009')
groups <- cutree(hclust(dist(d.09.means)), h=.8)
rect.hclust(hclust(dist(d.09.means)), h=.8, 
            border=palette())

plot(hclust(dist(d.10.means),method="average"),cex=0.8,main='2010')
groups <- cutree(hclust(dist(d.10.means)), h=.8)
rect.hclust(hclust(dist(d.10.means)), h=.8, 
            border=palette())

###################################################################

######### append the years in a column to each data set ######
year08<- rep(2008,26)
year09<- rep(2009,26)
year10<- rep(2010,26)

d.08.means$Year<- year08
d.09.means$Year<- year09
d.10.means$Year<- year10

################################################################

### change first column from row names into a column in the data frame ###
d.08.means$City = rownames(d.08.means)
rownames(d.08.means) = NULL
d.09.means$City = rownames(d.09.means)
rownames(d.09.means) = NULL
d.10.means$City = rownames(d.10.means)
rownames(d.10.means) = NULL

#########################################################################


####### calculate z-scores ####################################
loyalty08<- mean(d.08.means$means.LOYALTY.08)
passion08<- mean(d.08.means$means.PASSION.08)
ca08<- mean(d.08.means$means.CA.08)
basic_se08<- mean(d.08.means$means.BASIC_SE.08)
leadersh08<- mean(d.08.means$means.LEADERSH.08)
educatio08<- mean(d.08.means$means.EDUCATIO.08)
safety08<- mean(d.08.means$means.SAFETY.08)
aestheti08<- mean(d.08.means$means.AESTHETI.08)
economy08<- mean(d.08.means$means.ECONOMY.08)
social_o08<- mean(d.08.means$means.SOCIAL_O.08)
communit08<- mean(d.08.means$means.COMMUNIT.08)
involvem08<- mean(d.08.means$means.INVOLVEM.08)
openness08<- mean(d.08.means$means.OPENNESS.08)
social_c08<- mean(d.08.means$means.SOCIAL_C.08)
domains08<- mean(d.08.means$means.DOMAINS.08)

loyalty09<- mean(d.09.means$means.LOYALTY.09)
passion09<- mean(d.09.means$means.PASSION.09)
ca09<- mean(d.09.means$means.CA.09)
basic_se09<- mean(d.09.means$means.BASIC_SE.09)
leadersh09<- mean(d.09.means$means.LEADERSH.09)
educatio09<- mean(d.09.means$means.EDUCATIO.09)
safety09<- mean(d.09.means$means.SAFETY.09)
aestheti09<- mean(d.09.means$means.AESTHETI.09)
economy09<- mean(d.09.means$means.ECONOMY.09)
social_o09<- mean(d.09.means$means.SOCIAL_O.09)
communit09<- mean(d.09.means$means.COMMUNIT.09)
involvem09<- mean(d.09.means$means.INVOLVEM.09)
openness09<- mean(d.09.means$means.OPENNESS.09)
social_c09<- mean(d.09.means$means.SOCIAL_C.09)
domains09<- mean(d.09.means$means.DOMAINS.09)

loyalty10<- mean(d.10.means$means.LOYALTY.10)
passion10<- mean(d.10.means$means.PASSION.10)
ca10<- mean(d.10.means$means.CA.10)
basic_se10<- mean(d.10.means$means.BASIC_SE.10)
leadersh10<- mean(d.10.means$means.LEADERSH.10)
educatio10<- mean(d.10.means$means.EDUCATIO.10)
safety10<- mean(d.10.means$means.SAFETY.10)
aestheti10<- mean(d.10.means$means.AESTHETI.10)
economy10<- mean(d.10.means$means.ECONOMY.10)
social_o10<- mean(d.10.means$means.SOCIAL_O.10)
communit10<- mean(d.10.means$means.COMMUNIT.10)
involvem10<- mean(d.10.means$means.INVOLVEM.10)
openness10<- mean(d.10.means$means.OPENNESS.10)
social_c10<- mean(d.10.means$means.SOCIAL_C.10)
domains10<- mean(d.10.means$means.DOMAINS.10)


data08.index.means<- data.frame(loyalty08, passion08, ca08, basic_se08,
                                leadersh08,educatio08,safety08,aestheti08,
                                economy08,social_o08,communit08,involvem08,
                                openness08,social_c08,domains08)
data09.index.means<- data.frame(loyalty09, passion09, ca09, basic_se09,
                                leadersh09,educatio09,safety09,aestheti09,
                                economy09,social_o09,communit09,involvem09,
                                openness09,social_c09,domains09)
data10.index.means<- data.frame(loyalty10, passion10, ca10, basic_se10,
                                leadersh10,educatio10,safety10,aestheti10,
                                economy10,social_o10,communit10,involvem10,
                                openness10,social_c10,domains10)

sd.LOYALTY.08<- tapply(d.08.sub2$LOYALTY, d.08.sub2$City, sd)
sd.PASSION.08<- tapply(d.08.sub2$PASSION, d.08.sub2$City, sd)
sd.CA.08<- tapply(d.08.sub2$CA, d.08.sub2$City, sd)
sd.BASIC_SE.08<- tapply(d.08.sub2$BASIC_SE, d.08.sub2$City, sd)
sd.LEADERSH.08<- tapply(d.08.sub2$LEADERSH, d.08.sub2$City, sd)
sd.EDUCATIO.08<- tapply(d.08.sub2$EDUCATIO, d.08.sub2$City, sd)
sd.SAFETY.08<- tapply(d.08.sub2$SAFETY, d.08.sub2$City, sd)
sd.AESTHETI.08<- tapply(d.08.sub2$AESTHETI, d.08.sub2$City, sd)
sd.ECONOMY.08<- tapply(d.08.sub2$ECONOMY, d.08.sub2$City, sd)
sd.SOCIAL_O.08<- tapply(d.08.sub2$SOCIAL_O, d.08.sub2$City, sd)
sd.COMMUNIT.08<- tapply(d.08.sub2$COMMUNIT, d.08.sub2$City, sd)
sd.INVOLVEM.08<- tapply(d.08.sub2$INVOLVEM, d.08.sub2$City, sd)
sd.OPENNESS.08<- tapply(d.08.sub2$OPENNESS, d.08.sub2$City, sd)
sd.SOCIAL_C.08<- tapply(d.08.sub2$SOCIAL_C, d.08.sub2$City, sd)
sd.DOMAINS.08<- tapply(d.08.sub2$DOMAINS, d.08.sub2$City, sd)

d.08.sd<- data.frame(sd.LOYALTY.08,sd.PASSION.08,sd.CA.08,sd.BASIC_SE.08,
                     sd.LEADERSH.08, sd.EDUCATIO.08,sd.SAFETY.08,
                     sd.AESTHETI.08,sd.ECONOMY.08,sd.SOCIAL_O.08,
                     sd.COMMUNIT.08,sd.INVOLVEM.08,sd.OPENNESS.08,
                     sd.SOCIAL_C.08,sd.DOMAINS.08)

sd.LOYALTY.09<- tapply(d.09.sub2$LOYALTY, d.09.sub2$City, sd)
sd.PASSION.09<- tapply(d.09.sub2$PASSION, d.09.sub2$City, sd)
sd.CA.09<- tapply(d.09.sub2$CA, d.09.sub2$City, sd)
sd.BASIC_SE.09<- tapply(d.09.sub2$BASIC_SE, d.09.sub2$City, sd)
sd.LEADERSH.09<- tapply(d.09.sub2$LEADERSH, d.09.sub2$City, sd)
sd.EDUCATIO.09<- tapply(d.09.sub2$EDUCATIO, d.09.sub2$City, sd)
sd.SAFETY.09<- tapply(d.09.sub2$SAFETY, d.09.sub2$City, sd)
sd.AESTHETI.09<- tapply(d.09.sub2$AESTHETI, d.09.sub2$City, sd)
sd.ECONOMY.09<- tapply(d.09.sub2$ECONOMY, d.09.sub2$City, sd)
sd.SOCIAL_O.09<- tapply(d.09.sub2$SOCIAL_O, d.09.sub2$City, sd)
sd.COMMUNIT.09<- tapply(d.09.sub2$COMMUNIT, d.09.sub2$City, sd)
sd.INVOLVEM.09<- tapply(d.09.sub2$INVOLVEM, d.09.sub2$City, sd)
sd.OPENNESS.09<- tapply(d.09.sub2$OPENNESS, d.09.sub2$City, sd)
sd.SOCIAL_C.09<- tapply(d.09.sub2$SOCIAL_C, d.09.sub2$City, sd)
sd.DOMAINS.09<- tapply(d.09.sub2$DOMAINS, d.09.sub2$City, sd)

d.09.sd<- data.frame(sd.LOYALTY.09,sd.PASSION.09,sd.CA.09,sd.BASIC_SE.09,
                     sd.LEADERSH.09, sd.EDUCATIO.09,sd.SAFETY.09,
                     sd.AESTHETI.09,sd.ECONOMY.09,sd.SOCIAL_O.09,
                     sd.COMMUNIT.09,sd.INVOLVEM.09,sd.OPENNESS.09,
                     sd.SOCIAL_C.09,sd.DOMAINS.09)

sd.LOYALTY.10<- tapply(d.10.sub2$LOYALTY, d.10.sub2$City, sd)
sd.PASSION.10<- tapply(d.10.sub2$PASSION, d.10.sub2$City, sd)
sd.CA.10<- tapply(d.10.sub2$CA, d.10.sub2$City, sd)
sd.BASIC_SE.10<- tapply(d.10.sub2$BASIC_SE, d.10.sub2$City, sd)
sd.LEADERSH.10<- tapply(d.10.sub2$LEADERSH, d.10.sub2$City, sd)
sd.EDUCATIO.10<- tapply(d.10.sub2$EDUCATIO, d.10.sub2$City, sd)
sd.SAFETY.10<- tapply(d.10.sub2$SAFETY, d.10.sub2$City, sd)
sd.AESTHETI.10<- tapply(d.10.sub2$AESTHETI, d.10.sub2$City, sd)
sd.ECONOMY.10<- tapply(d.10.sub2$ECONOMY, d.10.sub2$City, sd)
sd.SOCIAL_O.10<- tapply(d.10.sub2$SOCIAL_O, d.10.sub2$City, sd)
sd.COMMUNIT.10<- tapply(d.10.sub2$COMMUNIT, d.10.sub2$City, sd)
sd.INVOLVEM.10<- tapply(d.10.sub2$INVOLVEM, d.10.sub2$City, sd)
sd.OPENNESS.10<- tapply(d.10.sub2$OPENNESS, d.10.sub2$City, sd)
sd.SOCIAL_C.10<- tapply(d.10.sub2$SOCIAL_C, d.10.sub2$City, sd)
sd.DOMAINS.10<- tapply(d.10.sub2$DOMAINS, d.10.sub2$City, sd)

d.10.sd<- data.frame(sd.LOYALTY.10,sd.PASSION.10,sd.CA.10,sd.BASIC_SE.10,
                     sd.LEADERSH.10, sd.EDUCATIO.10,sd.SAFETY.10,
                     sd.AESTHETI.10,sd.ECONOMY.10,sd.SOCIAL_O.10,
                     sd.COMMUNIT.10,sd.INVOLVEM.10,sd.OPENNESS.10,
                     sd.SOCIAL_C.10,sd.DOMAINS.10)

loyalty.sd08<- sd(d.08.sd$sd.LOYALTY.08)
passion.sd08<- sd(d.08.sd$sd.PASSION.08)
ca.sd08<- sd(d.08.sd$sd.CA.08)
basic_se.sd08<- sd(d.08.sd$sd.BASIC_SE.08)
leadersh.sd08<- sd(d.08.sd$sd.LEADERSH.08)
educatio.sd08<- sd(d.08.sd$sd.EDUCATIO.08)
safety.sd08<- sd(d.08.sd$sd.SAFETY.08)
aestheti.sd08<- sd(d.08.sd$sd.AESTHETI.08)
economy.sd08<- sd(d.08.sd$sd.ECONOMY.08)
social_o.sd08<- sd(d.08.sd$sd.SOCIAL_O.08)
communit.sd08<- sd(d.08.sd$sd.COMMUNIT.08)
involvem.sd08<- sd(d.08.sd$sd.INVOLVEM.08)
openness.sd08<- sd(d.08.sd$sd.OPENNESS.08)
social_c.sd08<- sd(d.08.sd$sd.SOCIAL_C.08)
domains.sd08<- sd(d.08.sd$sd.DOMAINS.08)

loyalty.sd09<- sd(d.09.sd$sd.LOYALTY.09)
passion.sd09<- sd(d.09.sd$sd.PASSION.09)
ca.sd09<- sd(d.09.sd$sd.CA.09)
basic_se.sd09<- sd(d.09.sd$sd.BASIC_SE.09)
leadersh.sd09<- sd(d.09.sd$sd.LEADERSH.09)
educatio.sd09<- sd(d.09.sd$sd.EDUCATIO.09)
safety.sd09<- sd(d.09.sd$sd.SAFETY.09)
aestheti.sd09<- sd(d.09.sd$sd.AESTHETI.09)
economy.sd09<- sd(d.09.sd$sd.ECONOMY.09)
social_o.sd09<- sd(d.09.sd$sd.SOCIAL_O.09)
communit.sd09<- sd(d.09.sd$sd.COMMUNIT.09)
involvem.sd09<- sd(d.09.sd$sd.INVOLVEM.09)
openness.sd09<- sd(d.09.sd$sd.OPENNESS.09)
social_c.sd09<- sd(d.09.sd$sd.SOCIAL_C.09)
domains.sd09<- sd(d.09.sd$sd.DOMAINS.09)

loyalty.sd10<- sd(d.10.sd$sd.LOYALTY.10)
passion.sd10<- sd(d.10.sd$sd.PASSION.10)
ca.sd10<- sd(d.10.sd$sd.CA.10)
basic_se.sd10<- sd(d.10.sd$sd.BASIC_SE.10)
leadersh.sd10<- sd(d.10.sd$sd.LEADERSH.10)
educatio.sd10<- sd(d.10.sd$sd.EDUCATIO.10)
safety.sd10<- sd(d.10.sd$sd.SAFETY.10)
aestheti.sd10<- sd(d.10.sd$sd.AESTHETI.10)
economy.sd10<- sd(d.10.sd$sd.ECONOMY.10)
social_o.sd10<- sd(d.10.sd$sd.SOCIAL_O.10)
communit.sd10<- sd(d.10.sd$sd.COMMUNIT.10)
involvem.sd10<- sd(d.10.sd$sd.INVOLVEM.10)
openness.sd10<- sd(d.10.sd$sd.OPENNESS.10)
social_c.sd10<- sd(d.10.sd$sd.SOCIAL_C.10)
domains.sd10<- sd(d.10.sd$sd.DOMAINS.10)

data08.index.sd<- data.frame(loyalty.sd08, passion.sd08, ca.sd08, basic_se.sd08,
                             leadersh.sd08,educatio.sd08,safety.sd08,aestheti.sd08,
                             economy.sd08,social_o.sd08,communit.sd08,involvem.sd08,
                             openness.sd08,social_c.sd08,domains.sd08)
data09.index.sd<- data.frame(loyalty.sd09, passion.sd09, ca.sd09, basic_se.sd09,
                             leadersh.sd09,educatio.sd09,safety.sd09,aestheti.sd09,
                             economy.sd09,social_o.sd09,communit.sd09,involvem.sd09,
                             openness.sd09,social_c.sd09,domains.sd09)
data10.index.sd<- data.frame(loyalty.sd10, passion.sd10, ca.sd10, basic_se.sd10,
                             leadersh.sd10,educatio.sd10,safety.sd10,aestheti.sd10,
                             economy.sd10,social_o.sd10,communit.sd10,involvem.sd10,
                             openness.sd10,social_c.sd10,domains.sd10)

aberdeen.z.loyalty08<- (d.08.means[1,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
akron.z.loyalty08<- (d.08.means[2,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
biloxi.z.loyalty08<- (d.08.means[3,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
boulder.z.loyalty08<- (d.08.means[4,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
bradenton.z.loyalty08<- (d.08.means[5,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
charlotte.z.loyalty08<- (d.08.means[6,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
columbia.z.loyalty08<- (d.08.means[7,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
columbus.z.loyalty08<- (d.08.means[8,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
detroit.z.loyalty08<- (d.08.means[9,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
duluth.z.loyalty08<- (d.08.means[10,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
fort.wayne.z.loyalty08<- (d.08.means[11,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
gary.z.loyalty08<- (d.08.means[12,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
grand.forks.z.loyalty08<- (d.08.means[13,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
lexington.z.loyalty08<- (d.08.means[14,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
long.beach.z.loyalty08<- (d.08.means[15,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
macon.z.loyalty08<- (d.08.means[16,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
miami.z.loyalty08<- (d.08.means[17,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
milledgeville.z.loyalty08<- (d.08.means[18,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
myrtle.beach.z.loyalty08<- (d.08.means[19,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
palm.beach.z.loyalty08<- (d.08.means[20,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
philadelphia.z.loyalty08<- (d.08.means[21,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
san.jose.z.loyalty08<- (d.08.means[22,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
st.paul.z.loyalty08<- (d.08.means[23,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
state.college.z.loyalty08<- (d.08.means[24,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
tallahassee.z.loyalty08<- (d.08.means[25,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08
wichita.z.loyalty08<- (d.08.means[26,1]-data08.index.means$loyalty08)/data08.index.sd$loyalty.sd08

aberdeen.z.passion08<- (d.08.means[1,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
akron.z.passion08<- (d.08.means[2,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
biloxi.z.passion08<- (d.08.means[3,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
boulder.z.passion08<- (d.08.means[4,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
bradenton.z.passion08<- (d.08.means[5,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
charlotte.z.passion08<- (d.08.means[6,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
columbia.z.passion08<- (d.08.means[7,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
columbus.z.passion08<- (d.08.means[8,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
detroit.z.passion08<- (d.08.means[9,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
duluth.z.passion08<- (d.08.means[10,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
fort.wayne.z.passion08<- (d.08.means[11,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
gary.z.passion08<- (d.08.means[12,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
grand.forks.z.passion08<- (d.08.means[13,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
lexington.z.passion08<- (d.08.means[14,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
long.beach.z.passion08<- (d.08.means[15,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
macon.z.passion08<- (d.08.means[16,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
miami.z.passion08<- (d.08.means[17,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
milledgeville.z.passion08<- (d.08.means[18,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
myrtle.beach.z.passion08<- (d.08.means[19,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
palm.beach.z.passion08<- (d.08.means[20,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
philadelphia.z.passion08<- (d.08.means[21,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
san.jose.z.passion08<- (d.08.means[22,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
st.paul.z.passion08<- (d.08.means[23,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
state.college.z.passion08<- (d.08.means[24,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
tallahassee.z.passion08<- (d.08.means[25,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08
wichita.z.passion08<- (d.08.means[26,2]-data08.index.means$passion08)/data08.index.sd$passion.sd08

aberdeen.z.ca08<- (d.08.means[1,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
akron.z.ca08<- (d.08.means[2,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
biloxi.z.ca08<- (d.08.means[3,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
boulder.z.ca08<- (d.08.means[4,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
bradenton.z.ca08<- (d.08.means[5,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
charlotte.z.ca08<- (d.08.means[6,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
columbia.z.ca08<- (d.08.means[7,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
columbus.z.ca08<- (d.08.means[8,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
detroit.z.ca08<- (d.08.means[9,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
duluth.z.ca08<- (d.08.means[10,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
fort.wayne.z.ca08<- (d.08.means[11,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
gary.z.ca08<- (d.08.means[12,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
grand.forks.z.ca08<- (d.08.means[13,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
lexington.z.ca08<- (d.08.means[14,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
long.beach.z.ca08<- (d.08.means[15,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
macon.z.ca08<- (d.08.means[16,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
miami.z.ca08<- (d.08.means[17,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
milledgeville.z.ca08<- (d.08.means[18,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
myrtle.beach.z.ca08<- (d.08.means[19,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
palm.beach.z.ca08<- (d.08.means[20,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
philadelphia.z.ca08<- (d.08.means[21,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
san.jose.z.ca08<- (d.08.means[22,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
st.paul.z.ca08<- (d.08.means[23,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
state.college.z.ca08<- (d.08.means[24,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
tallahassee.z.ca08<- (d.08.means[25,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08
wichita.z.ca08<- (d.08.means[26,3]-data08.index.means$ca08)/data08.index.sd$ca.sd08

aberdeen.z.basic_se08<- (d.08.means[1,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
akron.z.basic_se08<- (d.08.means[2,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
biloxi.z.basic_se08<- (d.08.means[3,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
boulder.z.basic_se08<- (d.08.means[4,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
bradenton.z.basic_se08<- (d.08.means[5,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
charlotte.z.basic_se08<- (d.08.means[6,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
columbia.z.basic_se08<- (d.08.means[7,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
columbus.z.basic_se08<- (d.08.means[8,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
detroit.z.basic_se08<- (d.08.means[9,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
duluth.z.basic_se08<- (d.08.means[10,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
fort.wayne.z.basic_se08<- (d.08.means[11,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
gary.z.basic_se08<- (d.08.means[12,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
grand.forks.z.basic_se08<- (d.08.means[13,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
lexington.z.basic_se08<- (d.08.means[14,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
long.beach.z.basic_se08<- (d.08.means[15,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
macon.z.basic_se08<- (d.08.means[16,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
miami.z.basic_se08<- (d.08.means[17,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
milledgeville.z.basic_se08<- (d.08.means[18,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
myrtle.beach.z.basic_se08<- (d.08.means[19,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
palm.beach.z.basic_se08<- (d.08.means[20,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
philadelphia.z.basic_se08<- (d.08.means[21,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
san.jose.z.basic_se08<- (d.08.means[22,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
st.paul.z.basic_se08<- (d.08.means[23,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
state.college.z.basic_se08<- (d.08.means[24,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
tallahassee.z.basic_se08<- (d.08.means[25,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08
wichita.z.basic_se08<- (d.08.means[26,4]-data08.index.means$basic_se08)/data08.index.sd$basic_se.sd08

aberdeen.z.leadersh08<- (d.08.means[1,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
akron.z.leadersh08<- (d.08.means[2,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
biloxi.z.leadersh08<- (d.08.means[3,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
boulder.z.leadersh08<- (d.08.means[4,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
bradenton.z.leadersh08<- (d.08.means[5,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
charlotte.z.leadersh08<- (d.08.means[6,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
columbia.z.leadersh08<- (d.08.means[7,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
columbus.z.leadersh08<- (d.08.means[8,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
detroit.z.leadersh08<- (d.08.means[9,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
duluth.z.leadersh08<- (d.08.means[10,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
fort.wayne.z.leadersh08<- (d.08.means[11,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
gary.z.leadersh08<- (d.08.means[12,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
grand.forks.z.leadersh08<- (d.08.means[13,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
lexington.z.leadersh08<- (d.08.means[14,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
long.beach.z.leadersh08<- (d.08.means[15,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
macon.z.leadersh08<- (d.08.means[16,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
miami.z.leadersh08<- (d.08.means[17,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
milledgeville.z.leadersh08<- (d.08.means[18,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
myrtle.beach.z.leadersh08<- (d.08.means[19,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
palm.beach.z.leadersh08<- (d.08.means[20,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
philadelphia.z.leadersh08<- (d.08.means[21,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
san.jose.z.leadersh08<- (d.08.means[22,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
st.paul.z.leadersh08<- (d.08.means[23,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
state.college.z.leadersh08<- (d.08.means[24,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
tallahassee.z.leadersh08<- (d.08.means[25,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08
wichita.z.leadersh08<- (d.08.means[26,5]-data08.index.means$leadersh08)/data08.index.sd$leadersh.sd08

aberdeen.z.educatio08<- (d.08.means[1,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
akron.z.educatio08<- (d.08.means[2,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
biloxi.z.educatio08<- (d.08.means[3,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
boulder.z.educatio08<- (d.08.means[4,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
bradenton.z.educatio08<- (d.08.means[5,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
charlotte.z.educatio08<- (d.08.means[6,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
columbia.z.educatio08<- (d.08.means[7,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
columbus.z.educatio08<- (d.08.means[8,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
detroit.z.educatio08<- (d.08.means[9,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
duluth.z.educatio08<- (d.08.means[10,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
fort.wayne.z.educatio08<- (d.08.means[11,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
gary.z.educatio08<- (d.08.means[12,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
grand.forks.z.educatio08<- (d.08.means[13,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
lexington.z.educatio08<- (d.08.means[14,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
long.beach.z.educatio08<- (d.08.means[15,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
macon.z.educatio08<- (d.08.means[16,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
miami.z.educatio08<- (d.08.means[17,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
milledgeville.z.educatio08<- (d.08.means[18,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
myrtle.beach.z.educatio08<- (d.08.means[19,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
palm.beach.z.educatio08<- (d.08.means[20,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
philadelphia.z.educatio08<- (d.08.means[21,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
san.jose.z.educatio08<- (d.08.means[22,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
st.paul.z.educatio08<- (d.08.means[23,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
state.college.z.educatio08<- (d.08.means[24,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
tallahassee.z.educatio08<- (d.08.means[25,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08
wichita.z.educatio08<- (d.08.means[26,6]-data08.index.means$educatio08)/data08.index.sd$educatio.sd08

aberdeen.z.safety08<- (d.08.means[1,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
akron.z.safety08<- (d.08.means[2,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
biloxi.z.safety08<- (d.08.means[3,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
boulder.z.safety08<- (d.08.means[4,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
bradenton.z.safety08<- (d.08.means[5,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
charlotte.z.safety08<- (d.08.means[6,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
columbia.z.safety08<- (d.08.means[7,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
columbus.z.safety08<- (d.08.means[8,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
detroit.z.safety08<- (d.08.means[9,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
duluth.z.safety08<- (d.08.means[10,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
fort.wayne.z.safety08<- (d.08.means[11,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
gary.z.safety08<- (d.08.means[12,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
grand.forks.z.safety08<- (d.08.means[13,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
lexington.z.safety08<- (d.08.means[14,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
long.beach.z.safety08<- (d.08.means[15,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
macon.z.safety08<- (d.08.means[16,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
miami.z.safety08<- (d.08.means[17,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
milledgeville.z.safety08<- (d.08.means[18,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
myrtle.beach.z.safety08<- (d.08.means[19,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
palm.beach.z.safety08<- (d.08.means[20,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
philadelphia.z.safety08<- (d.08.means[21,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
san.jose.z.safety08<- (d.08.means[22,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
st.paul.z.safety08<- (d.08.means[23,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
state.college.z.safety08<- (d.08.means[24,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
tallahassee.z.safety08<- (d.08.means[25,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08
wichita.z.safety08<- (d.08.means[26,7]-data08.index.means$safety08)/data08.index.sd$safety.sd08

aberdeen.z.aestheti08<- (d.08.means[1,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
akron.z.aestheti08<- (d.08.means[2,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
biloxi.z.aestheti08<- (d.08.means[3,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
boulder.z.aestheti08<- (d.08.means[4,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
bradenton.z.aestheti08<- (d.08.means[5,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
charlotte.z.aestheti08<- (d.08.means[6,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
columbia.z.aestheti08<- (d.08.means[7,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
columbus.z.aestheti08<- (d.08.means[8,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
detroit.z.aestheti08<- (d.08.means[9,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
duluth.z.aestheti08<- (d.08.means[10,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
fort.wayne.z.aestheti08<- (d.08.means[11,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
gary.z.aestheti08<- (d.08.means[12,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
grand.forks.z.aestheti08<- (d.08.means[13,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
lexington.z.aestheti08<- (d.08.means[14,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
long.beach.z.aestheti08<- (d.08.means[15,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
macon.z.aestheti08<- (d.08.means[16,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
miami.z.aestheti08<- (d.08.means[17,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
milledgeville.z.aestheti08<- (d.08.means[18,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
myrtle.beach.z.aestheti08<- (d.08.means[19,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
palm.beach.z.aestheti08<- (d.08.means[20,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
philadelphia.z.aestheti08<- (d.08.means[21,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
san.jose.z.aestheti08<- (d.08.means[22,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
st.paul.z.aestheti08<- (d.08.means[23,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
state.college.z.aestheti08<- (d.08.means[24,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
tallahassee.z.aestheti08<- (d.08.means[25,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08
wichita.z.aestheti08<- (d.08.means[26,8]-data08.index.means$aestheti08)/data08.index.sd$aestheti.sd08

aberdeen.z.economy08<- (d.08.means[1,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
akron.z.economy08<- (d.08.means[2,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
biloxi.z.economy08<- (d.08.means[3,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
boulder.z.economy08<- (d.08.means[4,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
bradenton.z.economy08<- (d.08.means[5,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
charlotte.z.economy08<- (d.08.means[6,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
columbia.z.economy08<- (d.08.means[7,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
columbus.z.economy08<- (d.08.means[8,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
detroit.z.economy08<- (d.08.means[9,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
duluth.z.economy08<- (d.08.means[10,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
fort.wayne.z.economy08<- (d.08.means[11,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
gary.z.economy08<- (d.08.means[12,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
grand.forks.z.economy08<- (d.08.means[13,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
lexington.z.economy08<- (d.08.means[14,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
long.beach.z.economy08<- (d.08.means[15,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
macon.z.economy08<- (d.08.means[16,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
miami.z.economy08<- (d.08.means[17,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
milledgeville.z.economy08<- (d.08.means[18,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
myrtle.beach.z.economy08<- (d.08.means[19,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
palm.beach.z.economy08<- (d.08.means[20,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
philadelphia.z.economy08<- (d.08.means[21,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
san.jose.z.economy08<- (d.08.means[22,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
st.paul.z.economy08<- (d.08.means[23,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
state.college.z.economy08<- (d.08.means[24,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
tallahassee.z.economy08<- (d.08.means[25,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08
wichita.z.economy08<- (d.08.means[26,9]-data08.index.means$economy08)/data08.index.sd$economy.sd08

aberdeen.z.social_o08<- (d.08.means[1,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
akron.z.social_o08<- (d.08.means[2,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
biloxi.z.social_o08<- (d.08.means[3,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
boulder.z.social_o08<- (d.08.means[4,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
bradenton.z.social_o08<- (d.08.means[5,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
charlotte.z.social_o08<- (d.08.means[6,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
columbia.z.social_o08<- (d.08.means[7,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
columbus.z.social_o08<- (d.08.means[8,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
detroit.z.social_o08<- (d.08.means[9,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
duluth.z.social_o08<- (d.08.means[10,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
fort.wayne.z.social_o08<- (d.08.means[11,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
gary.z.social_o08<- (d.08.means[12,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
grand.forks.z.social_o08<- (d.08.means[13,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
lexington.z.social_o08<- (d.08.means[14,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
long.beach.z.social_o08<- (d.08.means[15,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
macon.z.social_o08<- (d.08.means[16,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
miami.z.social_o08<- (d.08.means[17,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
milledgeville.z.social_o08<- (d.08.means[18,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
myrtle.beach.z.social_o08<- (d.08.means[19,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
palm.beach.z.social_o08<- (d.08.means[20,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
philadelphia.z.social_o08<- (d.08.means[21,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
san.jose.z.social_o08<- (d.08.means[22,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
st.paul.z.social_o08<- (d.08.means[23,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
state.college.z.social_o08<- (d.08.means[24,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
tallahassee.z.social_o08<- (d.08.means[25,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08
wichita.z.social_o08<- (d.08.means[26,10]-data08.index.means$social_o08)/data08.index.sd$social_o.sd08

aberdeen.z.communit08<- (d.08.means[1,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
akron.z.communit08<- (d.08.means[2,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
biloxi.z.communit08<- (d.08.means[3,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
boulder.z.communit08<- (d.08.means[4,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
bradenton.z.communit08<- (d.08.means[5,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
charlotte.z.communit08<- (d.08.means[6,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
columbia.z.communit08<- (d.08.means[7,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
columbus.z.communit08<- (d.08.means[8,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
detroit.z.communit08<- (d.08.means[9,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
duluth.z.communit08<- (d.08.means[10,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
fort.wayne.z.communit08<- (d.08.means[11,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
gary.z.communit08<- (d.08.means[12,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
grand.forks.z.communit08<- (d.08.means[13,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
lexington.z.communit08<- (d.08.means[14,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
long.beach.z.communit08<- (d.08.means[15,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
macon.z.communit08<- (d.08.means[16,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
miami.z.communit08<- (d.08.means[17,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
milledgeville.z.communit08<- (d.08.means[18,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
myrtle.beach.z.communit08<- (d.08.means[19,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
palm.beach.z.communit08<- (d.08.means[20,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
philadelphia.z.communit08<- (d.08.means[21,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
san.jose.z.communit08<- (d.08.means[22,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
st.paul.z.communit08<- (d.08.means[23,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
state.college.z.communit08<- (d.08.means[24,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
tallahassee.z.communit08<- (d.08.means[25,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08
wichita.z.communit08<- (d.08.means[26,11]-data08.index.means$communit08)/data08.index.sd$communit.sd08

aberdeen.z.involvem08<- (d.08.means[1,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
akron.z.involvem08<- (d.08.means[2,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
biloxi.z.involvem08<- (d.08.means[3,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
boulder.z.involvem08<- (d.08.means[4,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
bradenton.z.involvem08<- (d.08.means[5,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
charlotte.z.involvem08<- (d.08.means[6,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
columbia.z.involvem08<- (d.08.means[7,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
columbus.z.involvem08<- (d.08.means[8,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
detroit.z.involvem08<- (d.08.means[9,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
duluth.z.involvem08<- (d.08.means[10,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
fort.wayne.z.involvem08<- (d.08.means[11,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
gary.z.involvem08<- (d.08.means[12,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
grand.forks.z.involvem08<- (d.08.means[13,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
lexington.z.involvem08<- (d.08.means[14,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
long.beach.z.involvem08<- (d.08.means[15,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
macon.z.involvem08<- (d.08.means[16,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
miami.z.involvem08<- (d.08.means[17,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
milledgeville.z.involvem08<- (d.08.means[18,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
myrtle.beach.z.involvem08<- (d.08.means[19,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
palm.beach.z.involvem08<- (d.08.means[20,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
philadelphia.z.involvem08<- (d.08.means[21,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
san.jose.z.involvem08<- (d.08.means[22,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
st.paul.z.involvem08<- (d.08.means[23,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
state.college.z.involvem08<- (d.08.means[24,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
tallahassee.z.involvem08<- (d.08.means[25,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08
wichita.z.involvem08<- (d.08.means[26,12]-data08.index.means$involvem08)/data08.index.sd$involvem.sd08

aberdeen.z.openness08<- (d.08.means[1,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
akron.z.openness08<- (d.08.means[2,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
biloxi.z.openness08<- (d.08.means[3,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
boulder.z.openness08<- (d.08.means[4,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
bradenton.z.openness08<- (d.08.means[5,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
charlotte.z.openness08<- (d.08.means[6,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
columbia.z.openness08<- (d.08.means[7,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
columbus.z.openness08<- (d.08.means[8,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
detroit.z.openness08<- (d.08.means[9,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
duluth.z.openness08<- (d.08.means[10,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
fort.wayne.z.openness08<- (d.08.means[11,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
gary.z.openness08<- (d.08.means[12,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
grand.forks.z.openness08<- (d.08.means[13,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
lexington.z.openness08<- (d.08.means[14,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
long.beach.z.openness08<- (d.08.means[15,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
macon.z.openness08<- (d.08.means[16,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
miami.z.openness08<- (d.08.means[17,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
milledgeville.z.openness08<- (d.08.means[18,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
myrtle.beach.z.openness08<- (d.08.means[19,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
palm.beach.z.openness08<- (d.08.means[20,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
philadelphia.z.openness08<- (d.08.means[21,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
san.jose.z.openness08<- (d.08.means[22,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
st.paul.z.openness08<- (d.08.means[23,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
state.college.z.openness08<- (d.08.means[24,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
tallahassee.z.openness08<- (d.08.means[25,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08
wichita.z.openness08<- (d.08.means[26,13]-data08.index.means$openness08)/data08.index.sd$openness.sd08

aberdeen.z.social_c08<- (d.08.means[1,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
akron.z.social_c08<- (d.08.means[2,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
biloxi.z.social_c08<- (d.08.means[3,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
boulder.z.social_c08<- (d.08.means[4,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
bradenton.z.social_c08<- (d.08.means[5,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
charlotte.z.social_c08<- (d.08.means[6,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
columbia.z.social_c08<- (d.08.means[7,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
columbus.z.social_c08<- (d.08.means[8,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
detroit.z.social_c08<- (d.08.means[9,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
duluth.z.social_c08<- (d.08.means[10,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
fort.wayne.z.social_c08<- (d.08.means[11,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
gary.z.social_c08<- (d.08.means[12,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
grand.forks.z.social_c08<- (d.08.means[13,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
lexington.z.social_c08<- (d.08.means[14,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
long.beach.z.social_c08<- (d.08.means[15,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
macon.z.social_c08<- (d.08.means[16,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
miami.z.social_c08<- (d.08.means[17,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
milledgeville.z.social_c08<- (d.08.means[18,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
myrtle.beach.z.social_c08<- (d.08.means[19,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
palm.beach.z.social_c08<- (d.08.means[20,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
philadelphia.z.social_c08<- (d.08.means[21,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
san.jose.z.social_c08<- (d.08.means[22,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
st.paul.z.social_c08<- (d.08.means[23,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
state.college.z.social_c08<- (d.08.means[24,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
tallahassee.z.social_c08<- (d.08.means[25,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08
wichita.z.social_c08<- (d.08.means[26,14]-data08.index.means$social_c08)/data08.index.sd$social_c.sd08

aberdeen.z.domains08<- (d.08.means[1,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
akron.z.domains08<- (d.08.means[2,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
biloxi.z.domains08<- (d.08.means[3,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
boulder.z.domains08<- (d.08.means[4,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
bradenton.z.domains08<- (d.08.means[5,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
charlotte.z.domains08<- (d.08.means[6,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
columbia.z.domains08<- (d.08.means[7,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
columbus.z.domains08<- (d.08.means[8,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
detroit.z.domains08<- (d.08.means[9,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
duluth.z.domains08<- (d.08.means[10,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
fort.wayne.z.domains08<- (d.08.means[11,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
gary.z.domains08<- (d.08.means[12,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
grand.forks.z.domains08<- (d.08.means[13,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
lexington.z.domains08<- (d.08.means[14,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
long.beach.z.domains08<- (d.08.means[15,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
macon.z.domains08<- (d.08.means[16,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
miami.z.domains08<- (d.08.means[17,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
milledgeville.z.domains08<- (d.08.means[18,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
myrtle.beach.z.domains08<- (d.08.means[19,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
palm.beach.z.domains08<- (d.08.means[20,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
philadelphia.z.domains08<- (d.08.means[21,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
san.jose.z.domains08<- (d.08.means[22,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
st.paul.z.domains08<- (d.08.means[23,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
state.college.z.domains08<- (d.08.means[24,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
tallahassee.z.domains08<- (d.08.means[25,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08
wichita.z.domains08<- (d.08.means[26,15]-data08.index.means$domains08)/data08.index.sd$domains.sd08

################################
z08<- rbind(aberdeen.z.loyalty08,akron.z.loyalty08,biloxi.z.loyalty08,boulder.z.loyalty08,bradenton.z.loyalty08,
          charlotte.z.loyalty08, columbia.z.loyalty08,columbus.z.loyalty08,detroit.z.loyalty08,duluth.z.loyalty08,fort.wayne.z.loyalty08,
          gary.z.loyalty08,grand.forks.z.loyalty08,lexington.z.loyalty08,long.beach.z.loyalty08,macon.z.loyalty08,miami.z.loyalty08,
          milledgeville.z.loyalty08,myrtle.beach.z.loyalty08,palm.beach.z.loyalty08,philadelphia.z.loyalty08,
          san.jose.z.loyalty08,st.paul.z.loyalty08,state.college.z.loyalty08,tallahassee.z.loyalty08,wichita.z.loyalty08,
          aberdeen.z.passion08,akron.z.passion08,biloxi.z.passion08,boulder.z.passion08,bradenton.z.passion08,
          charlotte.z.passion08, columbia.z.passion08,columbus.z.passion08,detroit.z.passion08,duluth.z.passion08,fort.wayne.z.passion08,
          gary.z.passion08,grand.forks.z.passion08,lexington.z.passion08,long.beach.z.passion08,macon.z.passion08,miami.z.passion08,
          milledgeville.z.passion08,myrtle.beach.z.passion08,palm.beach.z.passion08,philadelphia.z.passion08,
          san.jose.z.passion08,st.paul.z.passion08,state.college.z.passion08,tallahassee.z.passion08,wichita.z.passion08,
          
          aberdeen.z.ca08,akron.z.ca08,biloxi.z.ca08,boulder.z.ca08,bradenton.z.ca08,
          charlotte.z.ca08, columbia.z.ca08,columbus.z.ca08,detroit.z.ca08,duluth.z.ca08,fort.wayne.z.ca08,
          gary.z.ca08,grand.forks.z.ca08,lexington.z.ca08,long.beach.z.ca08,macon.z.ca08,miami.z.ca08,
          milledgeville.z.ca08,myrtle.beach.z.ca08,palm.beach.z.ca08,philadelphia.z.ca08,
          san.jose.z.ca08,st.paul.z.ca08,state.college.z.ca08,tallahassee.z.ca08,wichita.z.ca08,
          aberdeen.z.basic_se08,akron.z.basic_se08,biloxi.z.basic_se08,boulder.z.basic_se08,bradenton.z.basic_se08,
          charlotte.z.basic_se08, columbia.z.basic_se08,columbus.z.basic_se08,detroit.z.basic_se08,duluth.z.basic_se08,fort.wayne.z.basic_se08,
          gary.z.basic_se08,grand.forks.z.basic_se08,lexington.z.basic_se08,long.beach.z.basic_se08,macon.z.basic_se08,miami.z.basic_se08,
          milledgeville.z.basic_se08,myrtle.beach.z.basic_se08,palm.beach.z.basic_se08,philadelphia.z.basic_se08,
          san.jose.z.basic_se08,st.paul.z.basic_se08,state.college.z.basic_se08,tallahassee.z.basic_se08,wichita.z.basic_se08,
          
          aberdeen.z.leadersh08,akron.z.leadersh08,biloxi.z.leadersh08,boulder.z.leadersh08,bradenton.z.leadersh08,
          charlotte.z.leadersh08, columbia.z.leadersh08,columbus.z.leadersh08,detroit.z.leadersh08,duluth.z.leadersh08,fort.wayne.z.leadersh08,
          gary.z.leadersh08,grand.forks.z.leadersh08,lexington.z.leadersh08,long.beach.z.leadersh08,macon.z.leadersh08,miami.z.leadersh08,
          milledgeville.z.leadersh08,myrtle.beach.z.leadersh08,palm.beach.z.leadersh08,philadelphia.z.leadersh08,
          san.jose.z.leadersh08,st.paul.z.leadersh08,state.college.z.leadersh08,tallahassee.z.leadersh08,wichita.z.leadersh08,
          
          aberdeen.z.educatio08,akron.z.educatio08,biloxi.z.educatio08,boulder.z.educatio08,bradenton.z.educatio08,
          charlotte.z.educatio08, columbia.z.educatio08,columbus.z.educatio08,detroit.z.educatio08,duluth.z.educatio08,fort.wayne.z.educatio08,
          gary.z.educatio08,grand.forks.z.educatio08,lexington.z.educatio08,long.beach.z.educatio08,macon.z.educatio08,miami.z.educatio08,
          milledgeville.z.educatio08,myrtle.beach.z.educatio08,palm.beach.z.educatio08,philadelphia.z.educatio08,
          san.jose.z.educatio08,st.paul.z.educatio08,state.college.z.educatio08,tallahassee.z.educatio08,wichita.z.educatio08,
          
          aberdeen.z.safety08,akron.z.safety08,biloxi.z.safety08,boulder.z.safety08,bradenton.z.safety08,
          charlotte.z.safety08, columbia.z.safety08,columbus.z.safety08,detroit.z.safety08,duluth.z.safety08,fort.wayne.z.safety08,
          gary.z.safety08,grand.forks.z.safety08,lexington.z.safety08,long.beach.z.safety08,macon.z.safety08,miami.z.safety08,
          milledgeville.z.safety08,myrtle.beach.z.safety08,palm.beach.z.safety08,philadelphia.z.safety08,
          san.jose.z.safety08,st.paul.z.safety08,state.college.z.safety08,tallahassee.z.safety08,wichita.z.safety08,
          
          aberdeen.z.aestheti08,akron.z.aestheti08,biloxi.z.aestheti08,boulder.z.aestheti08,bradenton.z.aestheti08,
          charlotte.z.aestheti08, columbia.z.aestheti08,columbus.z.aestheti08,detroit.z.aestheti08,duluth.z.aestheti08,fort.wayne.z.aestheti08,
          gary.z.aestheti08,grand.forks.z.aestheti08,lexington.z.aestheti08,long.beach.z.aestheti08,macon.z.aestheti08,miami.z.aestheti08,
          milledgeville.z.aestheti08,myrtle.beach.z.aestheti08,palm.beach.z.aestheti08,philadelphia.z.aestheti08,
          san.jose.z.aestheti08,st.paul.z.aestheti08,state.college.z.aestheti08,tallahassee.z.aestheti08,wichita.z.aestheti08,
          
          aberdeen.z.economy08,akron.z.economy08,biloxi.z.economy08,boulder.z.economy08,bradenton.z.economy08,
          charlotte.z.economy08, columbia.z.economy08,columbus.z.economy08,detroit.z.economy08,duluth.z.economy08,fort.wayne.z.economy08,
          gary.z.economy08,grand.forks.z.economy08,lexington.z.economy08,long.beach.z.economy08,macon.z.economy08,miami.z.economy08,
          milledgeville.z.economy08,myrtle.beach.z.economy08,palm.beach.z.economy08,philadelphia.z.economy08,
          san.jose.z.economy08,st.paul.z.economy08,state.college.z.economy08,tallahassee.z.economy08,wichita.z.economy08,
          
          aberdeen.z.social_o08,akron.z.social_o08,biloxi.z.social_o08,boulder.z.social_o08,bradenton.z.social_o08,
          charlotte.z.social_o08, columbia.z.social_o08,columbus.z.social_o08,detroit.z.social_o08,duluth.z.social_o08,fort.wayne.z.social_o08,
          gary.z.social_o08,grand.forks.z.social_o08,lexington.z.social_o08,long.beach.z.social_o08,macon.z.social_o08,miami.z.social_o08,
          milledgeville.z.social_o08,myrtle.beach.z.social_o08,palm.beach.z.social_o08,philadelphia.z.social_o08,
          san.jose.z.social_o08,st.paul.z.social_o08,state.college.z.social_o08,tallahassee.z.social_o08,wichita.z.social_o08,
          
          aberdeen.z.communit08,akron.z.communit08,biloxi.z.communit08,boulder.z.communit08,bradenton.z.communit08,
          charlotte.z.communit08, columbia.z.communit08,columbus.z.communit08,detroit.z.communit08,duluth.z.communit08,fort.wayne.z.communit08,
          gary.z.communit08,grand.forks.z.communit08,lexington.z.communit08,long.beach.z.communit08,macon.z.communit08,miami.z.communit08,
          milledgeville.z.communit08,myrtle.beach.z.communit08,palm.beach.z.communit08,philadelphia.z.communit08,
          san.jose.z.communit08,st.paul.z.communit08,state.college.z.communit08,tallahassee.z.communit08,wichita.z.communit08,
          
          aberdeen.z.involvem08,akron.z.involvem08,biloxi.z.involvem08,boulder.z.involvem08,bradenton.z.involvem08,
          charlotte.z.involvem08, columbia.z.involvem08,columbus.z.involvem08,detroit.z.involvem08,duluth.z.involvem08,fort.wayne.z.involvem08,
          gary.z.involvem08,grand.forks.z.involvem08,lexington.z.involvem08,long.beach.z.involvem08,macon.z.involvem08,miami.z.involvem08,
          milledgeville.z.involvem08,myrtle.beach.z.involvem08,palm.beach.z.involvem08,philadelphia.z.involvem08,
          san.jose.z.involvem08,st.paul.z.involvem08,state.college.z.involvem08,tallahassee.z.involvem08,wichita.z.involvem08,
          
          aberdeen.z.openness08,akron.z.openness08,biloxi.z.openness08,boulder.z.openness08,bradenton.z.openness08,
          charlotte.z.openness08, columbia.z.openness08,columbus.z.openness08,detroit.z.openness08,duluth.z.openness08,fort.wayne.z.openness08,
          gary.z.openness08,grand.forks.z.openness08,lexington.z.openness08,long.beach.z.openness08,macon.z.openness08,miami.z.openness08,
          milledgeville.z.openness08,myrtle.beach.z.openness08,palm.beach.z.openness08,philadelphia.z.openness08,
          san.jose.z.openness08,st.paul.z.openness08,state.college.z.openness08,tallahassee.z.openness08,wichita.z.openness08,
          
          
          aberdeen.z.social_c08,akron.z.social_c08,biloxi.z.social_c08,boulder.z.social_c08,bradenton.z.social_c08,
          charlotte.z.social_c08, columbia.z.social_c08,columbus.z.social_c08,detroit.z.social_c08,duluth.z.social_c08,fort.wayne.z.social_c08,
          gary.z.social_c08,grand.forks.z.social_c08,lexington.z.social_c08,long.beach.z.social_c08,macon.z.social_c08,miami.z.social_c08,
          milledgeville.z.social_c08,myrtle.beach.z.social_c08,palm.beach.z.social_c08,philadelphia.z.social_c08,
          san.jose.z.social_c08,st.paul.z.social_c08,state.college.z.social_c08,tallahassee.z.social_c08,wichita.z.social_c08,
          
          aberdeen.z.domains08,akron.z.domains08,biloxi.z.domains08,boulder.z.domains08,bradenton.z.domains08,
          charlotte.z.domains08, columbia.z.domains08,columbus.z.domains08,detroit.z.domains08,duluth.z.domains08,fort.wayne.z.domains08,
          gary.z.domains08,grand.forks.z.domains08,lexington.z.domains08,long.beach.z.domains08,macon.z.domains08,miami.z.domains08,
          milledgeville.z.domains08,myrtle.beach.z.domains08,palm.beach.z.domains08,philadelphia.z.domains08,
          san.jose.z.domains08,st.paul.z.domains08,state.college.z.domains08,tallahassee.z.domains08,wichita.z.domains08)

#############################################################################################################
###############################################################################################################
z08<- data.frame(z08)
z08$City = rownames(z08)
rownames(z08) = NULL
colnames(z08)<- c("zscore", 'Index')

z08<- data.frame(z08)

LOYALTY08<- data.frame(z08[1:26,1])
colnames(LOYALTY08)<- 'LOYALTY'

PASSION08<- data.frame(z08[27:52,1])
colnames(PASSION08)<- 'PASSION'

CA08<- data.frame(z08[53:78,1])
colnames(CA08)<- 'CA'

BASIC_SE08<- data.frame(z08[79:104,1])
colnames(BASIC_SE08)<- 'BASIC_SE'

LEADERSH08<- data.frame(z08[105:130,1])
colnames(LEADERSH08)<- 'LEADERSH'

EDUCATIO08<- data.frame(z08[131:156,1])
colnames(EDUCATIO08)<- 'EDUCATIO'

SAFETY08<- data.frame(z08[157:182,1])
colnames(SAFETY08)<- 'SAFETY'

AESTHETI08<- data.frame(z08[183:208,1])
colnames(AESTHETI08)<- 'AESTHETI'

ECONOMY08<- data.frame(z08[209:234,1])
colnames(ECONOMY08)<- 'ECONOMY'

SOCIAL_O08<- data.frame(z08[235:260,1])
colnames(SOCIAL_O08)<- 'SOCIAL_O'

COMMUNIT08<- data.frame(z08[261:286,1])
colnames(COMMUNIT08)<- 'COMMUNIT'

INVOLVEM08<- data.frame(z08[287:312,1])
colnames(INVOLVEM08)<- 'INVOLVEM'

OPENNESS08<- data.frame(z08[313:338,1])
colnames(OPENNESS08)<- 'OPENNESS'

SOCIAL_C08<- data.frame(z08[339:364,1])
colnames(SOCIAL_C08)<- 'SOCIAL_C'

DOMAINS08<- data.frame(z08[365:390,1])
colnames(DOMAINS08)<- 'DOMAINS'


zscores08<- cbind(LOYALTY08,PASSION08,CA08,BASIC_SE08,LEADERSH08,EDUCATIO08,SAFETY08,
                  AESTHETI08,ECONOMY08,SOCIAL_O08,COMMUNIT08,INVOLVEM08,OPENNESS08,SOCIAL_C08,DOMAINS08)
zscores08$Year<- rep(2008,26)

###############################################################################################################
aberdeen.z.loyalty09<- (d.09.means[1,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
akron.z.loyalty09<- (d.09.means[2,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
biloxi.z.loyalty09<- (d.09.means[3,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
boulder.z.loyalty09<- (d.09.means[4,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
bradenton.z.loyalty09<- (d.09.means[5,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
charlotte.z.loyalty09<- (d.09.means[6,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
columbia.z.loyalty09<- (d.09.means[7,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
columbus.z.loyalty09<- (d.09.means[8,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
detroit.z.loyalty09<- (d.09.means[9,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
duluth.z.loyalty09<- (d.09.means[10,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
fort.wayne.z.loyalty09<- (d.09.means[11,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
gary.z.loyalty09<- (d.09.means[12,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
grand.forks.z.loyalty09<- (d.09.means[13,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
lexington.z.loyalty09<- (d.09.means[14,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
long.beach.z.loyalty09<- (d.09.means[15,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
macon.z.loyalty09<- (d.09.means[16,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
miami.z.loyalty09<- (d.09.means[17,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
milledgeville.z.loyalty09<- (d.09.means[18,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
myrtle.beach.z.loyalty09<- (d.09.means[19,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
palm.beach.z.loyalty09<- (d.09.means[20,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
philadelphia.z.loyalty09<- (d.09.means[21,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
san.jose.z.loyalty09<- (d.09.means[22,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
st.paul.z.loyalty09<- (d.09.means[23,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
state.college.z.loyalty09<- (d.09.means[24,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
tallahassee.z.loyalty09<- (d.09.means[25,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09
wichita.z.loyalty09<- (d.09.means[26,1]-data09.index.means$loyalty09)/data09.index.sd$loyalty.sd09

aberdeen.z.passion09<- (d.09.means[1,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
akron.z.passion09<- (d.09.means[2,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
biloxi.z.passion09<- (d.09.means[3,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
boulder.z.passion09<- (d.09.means[4,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
bradenton.z.passion09<- (d.09.means[5,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
charlotte.z.passion09<- (d.09.means[6,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
columbia.z.passion09<- (d.09.means[7,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
columbus.z.passion09<- (d.09.means[8,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
detroit.z.passion09<- (d.09.means[9,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
duluth.z.passion09<- (d.09.means[10,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
fort.wayne.z.passion09<- (d.09.means[11,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
gary.z.passion09<- (d.09.means[12,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
grand.forks.z.passion09<- (d.09.means[13,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
lexington.z.passion09<- (d.09.means[14,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
long.beach.z.passion09<- (d.09.means[15,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
macon.z.passion09<- (d.09.means[16,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
miami.z.passion09<- (d.09.means[17,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
milledgeville.z.passion09<- (d.09.means[18,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
myrtle.beach.z.passion09<- (d.09.means[19,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
palm.beach.z.passion09<- (d.09.means[20,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
philadelphia.z.passion09<- (d.09.means[21,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
san.jose.z.passion09<- (d.09.means[22,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
st.paul.z.passion09<- (d.09.means[23,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
state.college.z.passion09<- (d.09.means[24,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
tallahassee.z.passion09<- (d.09.means[25,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09
wichita.z.passion09<- (d.09.means[26,2]-data09.index.means$passion09)/data09.index.sd$passion.sd09

aberdeen.z.ca09<- (d.09.means[1,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
akron.z.ca09<- (d.09.means[2,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
biloxi.z.ca09<- (d.09.means[3,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
boulder.z.ca09<- (d.09.means[4,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
bradenton.z.ca09<- (d.09.means[5,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
charlotte.z.ca09<- (d.09.means[6,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
columbia.z.ca09<- (d.09.means[7,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
columbus.z.ca09<- (d.09.means[8,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
detroit.z.ca09<- (d.09.means[9,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
duluth.z.ca09<- (d.09.means[10,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
fort.wayne.z.ca09<- (d.09.means[11,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
gary.z.ca09<- (d.09.means[12,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
grand.forks.z.ca09<- (d.09.means[13,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
lexington.z.ca09<- (d.09.means[14,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
long.beach.z.ca09<- (d.09.means[15,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
macon.z.ca09<- (d.09.means[16,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
miami.z.ca09<- (d.09.means[17,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
milledgeville.z.ca09<- (d.09.means[18,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
myrtle.beach.z.ca09<- (d.09.means[19,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
palm.beach.z.ca09<- (d.09.means[20,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
philadelphia.z.ca09<- (d.09.means[21,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
san.jose.z.ca09<- (d.09.means[22,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
st.paul.z.ca09<- (d.09.means[23,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
state.college.z.ca09<- (d.09.means[24,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
tallahassee.z.ca09<- (d.09.means[25,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09
wichita.z.ca09<- (d.09.means[26,3]-data09.index.means$ca09)/data09.index.sd$ca.sd09

aberdeen.z.basic_se09<- (d.09.means[1,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
akron.z.basic_se09<- (d.09.means[2,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
biloxi.z.basic_se09<- (d.09.means[3,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
boulder.z.basic_se09<- (d.09.means[4,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
bradenton.z.basic_se09<- (d.09.means[5,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
charlotte.z.basic_se09<- (d.09.means[6,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
columbia.z.basic_se09<- (d.09.means[7,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
columbus.z.basic_se09<- (d.09.means[8,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
detroit.z.basic_se09<- (d.09.means[9,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
duluth.z.basic_se09<- (d.09.means[10,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
fort.wayne.z.basic_se09<- (d.09.means[11,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
gary.z.basic_se09<- (d.09.means[12,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
grand.forks.z.basic_se09<- (d.09.means[13,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
lexington.z.basic_se09<- (d.09.means[14,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
long.beach.z.basic_se09<- (d.09.means[15,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
macon.z.basic_se09<- (d.09.means[16,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
miami.z.basic_se09<- (d.09.means[17,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
milledgeville.z.basic_se09<- (d.09.means[18,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
myrtle.beach.z.basic_se09<- (d.09.means[19,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
palm.beach.z.basic_se09<- (d.09.means[20,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
philadelphia.z.basic_se09<- (d.09.means[21,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
san.jose.z.basic_se09<- (d.09.means[22,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
st.paul.z.basic_se09<- (d.09.means[23,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
state.college.z.basic_se09<- (d.09.means[24,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
tallahassee.z.basic_se09<- (d.09.means[25,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09
wichita.z.basic_se09<- (d.09.means[26,4]-data09.index.means$basic_se09)/data09.index.sd$basic_se.sd09

aberdeen.z.leadersh09<- (d.09.means[1,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
akron.z.leadersh09<- (d.09.means[2,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
biloxi.z.leadersh09<- (d.09.means[3,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
boulder.z.leadersh09<- (d.09.means[4,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
bradenton.z.leadersh09<- (d.09.means[5,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
charlotte.z.leadersh09<- (d.09.means[6,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
columbia.z.leadersh09<- (d.09.means[7,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
columbus.z.leadersh09<- (d.09.means[8,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
detroit.z.leadersh09<- (d.09.means[9,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
duluth.z.leadersh09<- (d.09.means[10,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
fort.wayne.z.leadersh09<- (d.09.means[11,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
gary.z.leadersh09<- (d.09.means[12,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
grand.forks.z.leadersh09<- (d.09.means[13,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
lexington.z.leadersh09<- (d.09.means[14,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
long.beach.z.leadersh09<- (d.09.means[15,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
macon.z.leadersh09<- (d.09.means[16,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
miami.z.leadersh09<- (d.09.means[17,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
milledgeville.z.leadersh09<- (d.09.means[18,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
myrtle.beach.z.leadersh09<- (d.09.means[19,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
palm.beach.z.leadersh09<- (d.09.means[20,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
philadelphia.z.leadersh09<- (d.09.means[21,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
san.jose.z.leadersh09<- (d.09.means[22,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
st.paul.z.leadersh09<- (d.09.means[23,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
state.college.z.leadersh09<- (d.09.means[24,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
tallahassee.z.leadersh09<- (d.09.means[25,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09
wichita.z.leadersh09<- (d.09.means[26,5]-data09.index.means$leadersh09)/data09.index.sd$leadersh.sd09

aberdeen.z.educatio09<- (d.09.means[1,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
akron.z.educatio09<- (d.09.means[2,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
biloxi.z.educatio09<- (d.09.means[3,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
boulder.z.educatio09<- (d.09.means[4,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
bradenton.z.educatio09<- (d.09.means[5,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
charlotte.z.educatio09<- (d.09.means[6,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
columbia.z.educatio09<- (d.09.means[7,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
columbus.z.educatio09<- (d.09.means[8,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
detroit.z.educatio09<- (d.09.means[9,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
duluth.z.educatio09<- (d.09.means[10,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
fort.wayne.z.educatio09<- (d.09.means[11,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
gary.z.educatio09<- (d.09.means[12,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
grand.forks.z.educatio09<- (d.09.means[13,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
lexington.z.educatio09<- (d.09.means[14,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
long.beach.z.educatio09<- (d.09.means[15,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
macon.z.educatio09<- (d.09.means[16,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
miami.z.educatio09<- (d.09.means[17,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
milledgeville.z.educatio09<- (d.09.means[18,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
myrtle.beach.z.educatio09<- (d.09.means[19,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
palm.beach.z.educatio09<- (d.09.means[20,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
philadelphia.z.educatio09<- (d.09.means[21,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
san.jose.z.educatio09<- (d.09.means[22,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
st.paul.z.educatio09<- (d.09.means[23,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
state.college.z.educatio09<- (d.09.means[24,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
tallahassee.z.educatio09<- (d.09.means[25,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09
wichita.z.educatio09<- (d.09.means[26,6]-data09.index.means$educatio09)/data09.index.sd$educatio.sd09

aberdeen.z.safety09<- (d.09.means[1,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
akron.z.safety09<- (d.09.means[2,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
biloxi.z.safety09<- (d.09.means[3,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
boulder.z.safety09<- (d.09.means[4,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
bradenton.z.safety09<- (d.09.means[5,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
charlotte.z.safety09<- (d.09.means[6,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
columbia.z.safety09<- (d.09.means[7,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
columbus.z.safety09<- (d.09.means[8,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
detroit.z.safety09<- (d.09.means[9,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
duluth.z.safety09<- (d.09.means[10,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
fort.wayne.z.safety09<- (d.09.means[11,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
gary.z.safety09<- (d.09.means[12,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
grand.forks.z.safety09<- (d.09.means[13,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
lexington.z.safety09<- (d.09.means[14,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
long.beach.z.safety09<- (d.09.means[15,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
macon.z.safety09<- (d.09.means[16,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
miami.z.safety09<- (d.09.means[17,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
milledgeville.z.safety09<- (d.09.means[18,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
myrtle.beach.z.safety09<- (d.09.means[19,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
palm.beach.z.safety09<- (d.09.means[20,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
philadelphia.z.safety09<- (d.09.means[21,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
san.jose.z.safety09<- (d.09.means[22,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
st.paul.z.safety09<- (d.09.means[23,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
state.college.z.safety09<- (d.09.means[24,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
tallahassee.z.safety09<- (d.09.means[25,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09
wichita.z.safety09<- (d.09.means[26,7]-data09.index.means$safety09)/data09.index.sd$safety.sd09

aberdeen.z.aestheti09<- (d.09.means[1,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
akron.z.aestheti09<- (d.09.means[2,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
biloxi.z.aestheti09<- (d.09.means[3,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
boulder.z.aestheti09<- (d.09.means[4,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
bradenton.z.aestheti09<- (d.09.means[5,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
charlotte.z.aestheti09<- (d.09.means[6,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
columbia.z.aestheti09<- (d.09.means[7,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
columbus.z.aestheti09<- (d.09.means[8,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
detroit.z.aestheti09<- (d.09.means[9,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
duluth.z.aestheti09<- (d.09.means[10,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
fort.wayne.z.aestheti09<- (d.09.means[11,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
gary.z.aestheti09<- (d.09.means[12,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
grand.forks.z.aestheti09<- (d.09.means[13,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
lexington.z.aestheti09<- (d.09.means[14,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
long.beach.z.aestheti09<- (d.09.means[15,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
macon.z.aestheti09<- (d.09.means[16,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
miami.z.aestheti09<- (d.09.means[17,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
milledgeville.z.aestheti09<- (d.09.means[18,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
myrtle.beach.z.aestheti09<- (d.09.means[19,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
palm.beach.z.aestheti09<- (d.09.means[20,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
philadelphia.z.aestheti09<- (d.09.means[21,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
san.jose.z.aestheti09<- (d.09.means[22,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
st.paul.z.aestheti09<- (d.09.means[23,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
state.college.z.aestheti09<- (d.09.means[24,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
tallahassee.z.aestheti09<- (d.09.means[25,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09
wichita.z.aestheti09<- (d.09.means[26,8]-data09.index.means$aestheti09)/data09.index.sd$aestheti.sd09

aberdeen.z.economy09<- (d.09.means[1,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
akron.z.economy09<- (d.09.means[2,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
biloxi.z.economy09<- (d.09.means[3,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
boulder.z.economy09<- (d.09.means[4,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
bradenton.z.economy09<- (d.09.means[5,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
charlotte.z.economy09<- (d.09.means[6,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
columbia.z.economy09<- (d.09.means[7,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
columbus.z.economy09<- (d.09.means[8,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
detroit.z.economy09<- (d.09.means[9,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
duluth.z.economy09<- (d.09.means[10,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
fort.wayne.z.economy09<- (d.09.means[11,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
gary.z.economy09<- (d.09.means[12,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
grand.forks.z.economy09<- (d.09.means[13,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
lexington.z.economy09<- (d.09.means[14,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
long.beach.z.economy09<- (d.09.means[15,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
macon.z.economy09<- (d.09.means[16,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
miami.z.economy09<- (d.09.means[17,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
milledgeville.z.economy09<- (d.09.means[18,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
myrtle.beach.z.economy09<- (d.09.means[19,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
palm.beach.z.economy09<- (d.09.means[20,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
philadelphia.z.economy09<- (d.09.means[21,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
san.jose.z.economy09<- (d.09.means[22,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
st.paul.z.economy09<- (d.09.means[23,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
state.college.z.economy09<- (d.09.means[24,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
tallahassee.z.economy09<- (d.09.means[25,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09
wichita.z.economy09<- (d.09.means[26,9]-data09.index.means$economy09)/data09.index.sd$economy.sd09

aberdeen.z.social_o09<- (d.09.means[1,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
akron.z.social_o09<- (d.09.means[2,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
biloxi.z.social_o09<- (d.09.means[3,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
boulder.z.social_o09<- (d.09.means[4,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
bradenton.z.social_o09<- (d.09.means[5,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
charlotte.z.social_o09<- (d.09.means[6,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
columbia.z.social_o09<- (d.09.means[7,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
columbus.z.social_o09<- (d.09.means[8,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
detroit.z.social_o09<- (d.09.means[9,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
duluth.z.social_o09<- (d.09.means[10,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
fort.wayne.z.social_o09<- (d.09.means[11,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
gary.z.social_o09<- (d.09.means[12,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
grand.forks.z.social_o09<- (d.09.means[13,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
lexington.z.social_o09<- (d.09.means[14,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
long.beach.z.social_o09<- (d.09.means[15,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
macon.z.social_o09<- (d.09.means[16,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
miami.z.social_o09<- (d.09.means[17,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
milledgeville.z.social_o09<- (d.09.means[18,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
myrtle.beach.z.social_o09<- (d.09.means[19,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
palm.beach.z.social_o09<- (d.09.means[20,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
philadelphia.z.social_o09<- (d.09.means[21,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
san.jose.z.social_o09<- (d.09.means[22,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
st.paul.z.social_o09<- (d.09.means[23,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
state.college.z.social_o09<- (d.09.means[24,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
tallahassee.z.social_o09<- (d.09.means[25,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09
wichita.z.social_o09<- (d.09.means[26,10]-data09.index.means$social_o09)/data09.index.sd$social_o.sd09

aberdeen.z.communit09<- (d.09.means[1,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
akron.z.communit09<- (d.09.means[2,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
biloxi.z.communit09<- (d.09.means[3,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
boulder.z.communit09<- (d.09.means[4,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
bradenton.z.communit09<- (d.09.means[5,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
charlotte.z.communit09<- (d.09.means[6,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
columbia.z.communit09<- (d.09.means[7,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
columbus.z.communit09<- (d.09.means[8,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
detroit.z.communit09<- (d.09.means[9,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
duluth.z.communit09<- (d.09.means[10,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
fort.wayne.z.communit09<- (d.09.means[11,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
gary.z.communit09<- (d.09.means[12,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
grand.forks.z.communit09<- (d.09.means[13,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
lexington.z.communit09<- (d.09.means[14,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
long.beach.z.communit09<- (d.09.means[15,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
macon.z.communit09<- (d.09.means[16,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
miami.z.communit09<- (d.09.means[17,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
milledgeville.z.communit09<- (d.09.means[18,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
myrtle.beach.z.communit09<- (d.09.means[19,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
palm.beach.z.communit09<- (d.09.means[20,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
philadelphia.z.communit09<- (d.09.means[21,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
san.jose.z.communit09<- (d.09.means[22,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
st.paul.z.communit09<- (d.09.means[23,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
state.college.z.communit09<- (d.09.means[24,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
tallahassee.z.communit09<- (d.09.means[25,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09
wichita.z.communit09<- (d.09.means[26,11]-data09.index.means$communit09)/data09.index.sd$communit.sd09

aberdeen.z.involvem09<- (d.09.means[1,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
akron.z.involvem09<- (d.09.means[2,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
biloxi.z.involvem09<- (d.09.means[3,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
boulder.z.involvem09<- (d.09.means[4,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
bradenton.z.involvem09<- (d.09.means[5,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
charlotte.z.involvem09<- (d.09.means[6,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
columbia.z.involvem09<- (d.09.means[7,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
columbus.z.involvem09<- (d.09.means[8,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
detroit.z.involvem09<- (d.09.means[9,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
duluth.z.involvem09<- (d.09.means[10,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
fort.wayne.z.involvem09<- (d.09.means[11,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
gary.z.involvem09<- (d.09.means[12,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
grand.forks.z.involvem09<- (d.09.means[13,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
lexington.z.involvem09<- (d.09.means[14,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
long.beach.z.involvem09<- (d.09.means[15,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
macon.z.involvem09<- (d.09.means[16,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
miami.z.involvem09<- (d.09.means[17,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
milledgeville.z.involvem09<- (d.09.means[18,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
myrtle.beach.z.involvem09<- (d.09.means[19,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
palm.beach.z.involvem09<- (d.09.means[20,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
philadelphia.z.involvem09<- (d.09.means[21,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
san.jose.z.involvem09<- (d.09.means[22,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
st.paul.z.involvem09<- (d.09.means[23,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
state.college.z.involvem09<- (d.09.means[24,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
tallahassee.z.involvem09<- (d.09.means[25,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09
wichita.z.involvem09<- (d.09.means[26,12]-data09.index.means$involvem09)/data09.index.sd$involvem.sd09

aberdeen.z.openness09<- (d.09.means[1,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
akron.z.openness09<- (d.09.means[2,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
biloxi.z.openness09<- (d.09.means[3,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
boulder.z.openness09<- (d.09.means[4,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
bradenton.z.openness09<- (d.09.means[5,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
charlotte.z.openness09<- (d.09.means[6,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
columbia.z.openness09<- (d.09.means[7,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
columbus.z.openness09<- (d.09.means[8,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
detroit.z.openness09<- (d.09.means[9,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
duluth.z.openness09<- (d.09.means[10,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
fort.wayne.z.openness09<- (d.09.means[11,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
gary.z.openness09<- (d.09.means[12,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
grand.forks.z.openness09<- (d.09.means[13,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
lexington.z.openness09<- (d.09.means[14,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
long.beach.z.openness09<- (d.09.means[15,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
macon.z.openness09<- (d.09.means[16,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
miami.z.openness09<- (d.09.means[17,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
milledgeville.z.openness09<- (d.09.means[18,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
myrtle.beach.z.openness09<- (d.09.means[19,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
palm.beach.z.openness09<- (d.09.means[20,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
philadelphia.z.openness09<- (d.09.means[21,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
san.jose.z.openness09<- (d.09.means[22,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
st.paul.z.openness09<- (d.09.means[23,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
state.college.z.openness09<- (d.09.means[24,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
tallahassee.z.openness09<- (d.09.means[25,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09
wichita.z.openness09<- (d.09.means[26,13]-data09.index.means$openness09)/data09.index.sd$openness.sd09

aberdeen.z.social_c09<- (d.09.means[1,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
akron.z.social_c09<- (d.09.means[2,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
biloxi.z.social_c09<- (d.09.means[3,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
boulder.z.social_c09<- (d.09.means[4,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
bradenton.z.social_c09<- (d.09.means[5,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
charlotte.z.social_c09<- (d.09.means[6,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
columbia.z.social_c09<- (d.09.means[7,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
columbus.z.social_c09<- (d.09.means[8,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
detroit.z.social_c09<- (d.09.means[9,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
duluth.z.social_c09<- (d.09.means[10,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
fort.wayne.z.social_c09<- (d.09.means[11,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
gary.z.social_c09<- (d.09.means[12,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
grand.forks.z.social_c09<- (d.09.means[13,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
lexington.z.social_c09<- (d.09.means[14,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
long.beach.z.social_c09<- (d.09.means[15,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
macon.z.social_c09<- (d.09.means[16,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
miami.z.social_c09<- (d.09.means[17,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
milledgeville.z.social_c09<- (d.09.means[18,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
myrtle.beach.z.social_c09<- (d.09.means[19,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
palm.beach.z.social_c09<- (d.09.means[20,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
philadelphia.z.social_c09<- (d.09.means[21,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
san.jose.z.social_c09<- (d.09.means[22,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
st.paul.z.social_c09<- (d.09.means[23,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
state.college.z.social_c09<- (d.09.means[24,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
tallahassee.z.social_c09<- (d.09.means[25,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09
wichita.z.social_c09<- (d.09.means[26,14]-data09.index.means$social_c09)/data09.index.sd$social_c.sd09

aberdeen.z.domains09<- (d.09.means[1,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
akron.z.domains09<- (d.09.means[2,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
biloxi.z.domains09<- (d.09.means[3,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
boulder.z.domains09<- (d.09.means[4,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
bradenton.z.domains09<- (d.09.means[5,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
charlotte.z.domains09<- (d.09.means[6,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
columbia.z.domains09<- (d.09.means[7,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
columbus.z.domains09<- (d.09.means[8,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
detroit.z.domains09<- (d.09.means[9,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
duluth.z.domains09<- (d.09.means[10,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
fort.wayne.z.domains09<- (d.09.means[11,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
gary.z.domains09<- (d.09.means[12,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
grand.forks.z.domains09<- (d.09.means[13,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
lexington.z.domains09<- (d.09.means[14,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
long.beach.z.domains09<- (d.09.means[15,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
macon.z.domains09<- (d.09.means[16,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
miami.z.domains09<- (d.09.means[17,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
milledgeville.z.domains09<- (d.09.means[18,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
myrtle.beach.z.domains09<- (d.09.means[19,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
palm.beach.z.domains09<- (d.09.means[20,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
philadelphia.z.domains09<- (d.09.means[21,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
san.jose.z.domains09<- (d.09.means[22,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
st.paul.z.domains09<- (d.09.means[23,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
state.college.z.domains09<- (d.09.means[24,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
tallahassee.z.domains09<- (d.09.means[25,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09
wichita.z.domains09<- (d.09.means[26,15]-data09.index.means$domains09)/data09.index.sd$domains.sd09


################################
z09<- rbind(aberdeen.z.loyalty09,akron.z.loyalty09,biloxi.z.loyalty09,boulder.z.loyalty09,bradenton.z.loyalty09,
            charlotte.z.loyalty09, columbia.z.loyalty09,columbus.z.loyalty09,detroit.z.loyalty09,duluth.z.loyalty09,fort.wayne.z.loyalty09,
            gary.z.loyalty09,grand.forks.z.loyalty09,lexington.z.loyalty09,long.beach.z.loyalty09,macon.z.loyalty09,miami.z.loyalty09,
            milledgeville.z.loyalty09,myrtle.beach.z.loyalty09,palm.beach.z.loyalty09,philadelphia.z.loyalty09,
            san.jose.z.loyalty09,st.paul.z.loyalty09,state.college.z.loyalty09,tallahassee.z.loyalty09,wichita.z.loyalty09,
            aberdeen.z.passion09,akron.z.passion09,biloxi.z.passion09,boulder.z.passion09,bradenton.z.passion09,
            charlotte.z.passion09, columbia.z.passion09,columbus.z.passion09,detroit.z.passion09,duluth.z.passion09,fort.wayne.z.passion09,
            gary.z.passion09,grand.forks.z.passion09,lexington.z.passion09,long.beach.z.passion09,macon.z.passion09,miami.z.passion09,
            milledgeville.z.passion09,myrtle.beach.z.passion09,palm.beach.z.passion09,philadelphia.z.passion09,
            san.jose.z.passion09,st.paul.z.passion09,state.college.z.passion09,tallahassee.z.passion09,wichita.z.passion09,
            
            aberdeen.z.ca09,akron.z.ca09,biloxi.z.ca09,boulder.z.ca09,bradenton.z.ca09,
            charlotte.z.ca09, columbia.z.ca09,columbus.z.ca09,detroit.z.ca09,duluth.z.ca09,fort.wayne.z.ca09,
            gary.z.ca09,grand.forks.z.ca09,lexington.z.ca09,long.beach.z.ca09,macon.z.ca09,miami.z.ca09,
            milledgeville.z.ca09,myrtle.beach.z.ca09,palm.beach.z.ca09,philadelphia.z.ca09,
            san.jose.z.ca09,st.paul.z.ca09,state.college.z.ca09,tallahassee.z.ca09,wichita.z.ca09,
            aberdeen.z.basic_se09,akron.z.basic_se09,biloxi.z.basic_se09,boulder.z.basic_se09,bradenton.z.basic_se09,
            charlotte.z.basic_se09, columbia.z.basic_se09,columbus.z.basic_se09,detroit.z.basic_se09,duluth.z.basic_se09,fort.wayne.z.basic_se09,
            gary.z.basic_se09,grand.forks.z.basic_se09,lexington.z.basic_se09,long.beach.z.basic_se09,macon.z.basic_se09,miami.z.basic_se09,
            milledgeville.z.basic_se09,myrtle.beach.z.basic_se09,palm.beach.z.basic_se09,philadelphia.z.basic_se09,
            san.jose.z.basic_se09,st.paul.z.basic_se09,state.college.z.basic_se09,tallahassee.z.basic_se09,wichita.z.basic_se09,
            
            aberdeen.z.leadersh09,akron.z.leadersh09,biloxi.z.leadersh09,boulder.z.leadersh09,bradenton.z.leadersh09,
            charlotte.z.leadersh09, columbia.z.leadersh09,columbus.z.leadersh09,detroit.z.leadersh09,duluth.z.leadersh09,fort.wayne.z.leadersh09,
            gary.z.leadersh09,grand.forks.z.leadersh09,lexington.z.leadersh09,long.beach.z.leadersh09,macon.z.leadersh09,miami.z.leadersh09,
            milledgeville.z.leadersh09,myrtle.beach.z.leadersh09,palm.beach.z.leadersh09,philadelphia.z.leadersh09,
            san.jose.z.leadersh09,st.paul.z.leadersh09,state.college.z.leadersh09,tallahassee.z.leadersh09,wichita.z.leadersh09,
            
            aberdeen.z.educatio09,akron.z.educatio09,biloxi.z.educatio09,boulder.z.educatio09,bradenton.z.educatio09,
            charlotte.z.educatio09, columbia.z.educatio09,columbus.z.educatio09,detroit.z.educatio09,duluth.z.educatio09,fort.wayne.z.educatio09,
            gary.z.educatio09,grand.forks.z.educatio09,lexington.z.educatio09,long.beach.z.educatio09,macon.z.educatio09,miami.z.educatio09,
            milledgeville.z.educatio09,myrtle.beach.z.educatio09,palm.beach.z.educatio09,philadelphia.z.educatio09,
            san.jose.z.educatio09,st.paul.z.educatio09,state.college.z.educatio09,tallahassee.z.educatio09,wichita.z.educatio09,
            
            aberdeen.z.safety09,akron.z.safety09,biloxi.z.safety09,boulder.z.safety09,bradenton.z.safety09,
            charlotte.z.safety09, columbia.z.safety09,columbus.z.safety09,detroit.z.safety09,duluth.z.safety09,fort.wayne.z.safety09,
            gary.z.safety09,grand.forks.z.safety09,lexington.z.safety09,long.beach.z.safety09,macon.z.safety09,miami.z.safety09,
            milledgeville.z.safety09,myrtle.beach.z.safety09,palm.beach.z.safety09,philadelphia.z.safety09,
            san.jose.z.safety09,st.paul.z.safety09,state.college.z.safety09,tallahassee.z.safety09,wichita.z.safety09,
            
            aberdeen.z.aestheti09,akron.z.aestheti09,biloxi.z.aestheti09,boulder.z.aestheti09,bradenton.z.aestheti09,
            charlotte.z.aestheti09, columbia.z.aestheti09,columbus.z.aestheti09,detroit.z.aestheti09,duluth.z.aestheti09,fort.wayne.z.aestheti09,
            gary.z.aestheti09,grand.forks.z.aestheti09,lexington.z.aestheti09,long.beach.z.aestheti09,macon.z.aestheti09,miami.z.aestheti09,
            milledgeville.z.aestheti09,myrtle.beach.z.aestheti09,palm.beach.z.aestheti09,philadelphia.z.aestheti09,
            san.jose.z.aestheti09,st.paul.z.aestheti09,state.college.z.aestheti09,tallahassee.z.aestheti09,wichita.z.aestheti09,
            
            aberdeen.z.economy09,akron.z.economy09,biloxi.z.economy09,boulder.z.economy09,bradenton.z.economy09,
            charlotte.z.economy09, columbia.z.economy09,columbus.z.economy09,detroit.z.economy09,duluth.z.economy09,fort.wayne.z.economy09,
            gary.z.economy09,grand.forks.z.economy09,lexington.z.economy09,long.beach.z.economy09,macon.z.economy09,miami.z.economy09,
            milledgeville.z.economy09,myrtle.beach.z.economy09,palm.beach.z.economy09,philadelphia.z.economy09,
            san.jose.z.economy09,st.paul.z.economy09,state.college.z.economy09,tallahassee.z.economy09,wichita.z.economy09,
            
            aberdeen.z.social_o09,akron.z.social_o09,biloxi.z.social_o09,boulder.z.social_o09,bradenton.z.social_o09,
            charlotte.z.social_o09, columbia.z.social_o09,columbus.z.social_o09,detroit.z.social_o09,duluth.z.social_o09,fort.wayne.z.social_o09,
            gary.z.social_o09,grand.forks.z.social_o09,lexington.z.social_o09,long.beach.z.social_o09,macon.z.social_o09,miami.z.social_o09,
            milledgeville.z.social_o09,myrtle.beach.z.social_o09,palm.beach.z.social_o09,philadelphia.z.social_o09,
            san.jose.z.social_o09,st.paul.z.social_o09,state.college.z.social_o09,tallahassee.z.social_o09,wichita.z.social_o09,
            
            aberdeen.z.communit09,akron.z.communit09,biloxi.z.communit09,boulder.z.communit09,bradenton.z.communit09,
            charlotte.z.communit09, columbia.z.communit09,columbus.z.communit09,detroit.z.communit09,duluth.z.communit09,fort.wayne.z.communit09,
            gary.z.communit09,grand.forks.z.communit09,lexington.z.communit09,long.beach.z.communit09,macon.z.communit09,miami.z.communit09,
            milledgeville.z.communit09,myrtle.beach.z.communit09,palm.beach.z.communit09,philadelphia.z.communit09,
            san.jose.z.communit09,st.paul.z.communit09,state.college.z.communit09,tallahassee.z.communit09,wichita.z.communit09,
            
            aberdeen.z.involvem09,akron.z.involvem09,biloxi.z.involvem09,boulder.z.involvem09,bradenton.z.involvem09,
            charlotte.z.involvem09, columbia.z.involvem09,columbus.z.involvem09,detroit.z.involvem09,duluth.z.involvem09,fort.wayne.z.involvem09,
            gary.z.involvem09,grand.forks.z.involvem09,lexington.z.involvem09,long.beach.z.involvem09,macon.z.involvem09,miami.z.involvem09,
            milledgeville.z.involvem09,myrtle.beach.z.involvem09,palm.beach.z.involvem09,philadelphia.z.involvem09,
            san.jose.z.involvem09,st.paul.z.involvem09,state.college.z.involvem09,tallahassee.z.involvem09,wichita.z.involvem09,
            
            aberdeen.z.openness09,akron.z.openness09,biloxi.z.openness09,boulder.z.openness09,bradenton.z.openness09,
            charlotte.z.openness09, columbia.z.openness09,columbus.z.openness09,detroit.z.openness09,duluth.z.openness09,fort.wayne.z.openness09,
            gary.z.openness09,grand.forks.z.openness09,lexington.z.openness09,long.beach.z.openness09,macon.z.openness09,miami.z.openness09,
            milledgeville.z.openness09,myrtle.beach.z.openness09,palm.beach.z.openness09,philadelphia.z.openness09,
            san.jose.z.openness09,st.paul.z.openness09,state.college.z.openness09,tallahassee.z.openness09,wichita.z.openness09,
            
            
            aberdeen.z.social_c09,akron.z.social_c09,biloxi.z.social_c09,boulder.z.social_c09,bradenton.z.social_c09,
            charlotte.z.social_c09, columbia.z.social_c09,columbus.z.social_c09,detroit.z.social_c09,duluth.z.social_c09,fort.wayne.z.social_c09,
            gary.z.social_c09,grand.forks.z.social_c09,lexington.z.social_c09,long.beach.z.social_c09,macon.z.social_c09,miami.z.social_c09,
            milledgeville.z.social_c09,myrtle.beach.z.social_c09,palm.beach.z.social_c09,philadelphia.z.social_c09,
            san.jose.z.social_c09,st.paul.z.social_c09,state.college.z.social_c09,tallahassee.z.social_c09,wichita.z.social_c09,
            
            aberdeen.z.domains09,akron.z.domains09,biloxi.z.domains09,boulder.z.domains09,bradenton.z.domains09,
            charlotte.z.domains09, columbia.z.domains09,columbus.z.domains09,detroit.z.domains09,duluth.z.domains09,fort.wayne.z.domains09,
            gary.z.domains09,grand.forks.z.domains09,lexington.z.domains09,long.beach.z.domains09,macon.z.domains09,miami.z.domains09,
            milledgeville.z.domains09,myrtle.beach.z.domains09,palm.beach.z.domains09,philadelphia.z.domains09,
            san.jose.z.domains09,st.paul.z.domains09,state.college.z.domains09,tallahassee.z.domains09,wichita.z.domains09)

#############################################################################################################
z09<- data.frame(z09)
z09$City = rownames(z09)
rownames(z09) = NULL
colnames(z09)<- c("zscore", 'Index')

z09<- data.frame(z09)

LOYALTY09<- data.frame(z09[1:26,1])
colnames(LOYALTY09)<- 'LOYALTY'

PASSION09<- data.frame(z09[27:52,1])
colnames(PASSION09)<- 'PASSION'

CA09<- data.frame(z09[53:78,1])
colnames(CA09)<- 'CA'

BASIC_SE09<- data.frame(z09[79:104,1])
colnames(BASIC_SE09)<- 'BASIC_SE'

LEADERSH09<- data.frame(z09[105:130,1])
colnames(LEADERSH09)<- 'LEADERSH'

EDUCATIO09<- data.frame(z09[131:156,1])
colnames(EDUCATIO09)<- 'EDUCATIO'

SAFETY09<- data.frame(z09[157:182,1])
colnames(SAFETY09)<- 'SAFETY'

AESTHETI09<- data.frame(z09[183:208,1])
colnames(AESTHETI09)<- 'AESTHETI'

ECONOMY09<- data.frame(z09[209:234,1])
colnames(ECONOMY09)<- 'ECONOMY'

SOCIAL_O09<- data.frame(z09[235:260,1])
colnames(SOCIAL_O09)<- 'SOCIAL_O'

COMMUNIT09<- data.frame(z09[261:286,1])
colnames(COMMUNIT09)<- 'COMMUNIT'

INVOLVEM09<- data.frame(z09[287:312,1])
colnames(INVOLVEM09)<- 'INVOLVEM'

OPENNESS09<- data.frame(z09[313:338,1])
colnames(OPENNESS09)<- 'OPENNESS'

SOCIAL_C09<- data.frame(z09[339:364,1])
colnames(SOCIAL_C09)<- 'SOCIAL_C'

DOMAINS09<- data.frame(z09[365:390,1])
colnames(DOMAINS09)<- 'DOMAINS'


zscores09<- cbind(LOYALTY09,PASSION09,CA09,BASIC_SE09,LEADERSH09,EDUCATIO09,SAFETY09,
                  AESTHETI09,ECONOMY09,SOCIAL_O09,COMMUNIT09,INVOLVEM09,OPENNESS09,SOCIAL_C09,DOMAINS09)
zscores09$Year<- rep(2009,26)

#############################################################################################################



aberdeen.z.loyalty10<- (d.10.means[1,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
akron.z.loyalty10<- (d.10.means[2,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
biloxi.z.loyalty10<- (d.10.means[3,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
boulder.z.loyalty10<- (d.10.means[4,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
bradenton.z.loyalty10<- (d.10.means[5,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
charlotte.z.loyalty10<- (d.10.means[6,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
columbia.z.loyalty10<- (d.10.means[7,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
columbus.z.loyalty10<- (d.10.means[8,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
detroit.z.loyalty10<- (d.10.means[9,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
duluth.z.loyalty10<- (d.10.means[10,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
fort.wayne.z.loyalty10<- (d.10.means[11,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
gary.z.loyalty10<- (d.10.means[12,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
grand.forks.z.loyalty10<- (d.10.means[13,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
lexington.z.loyalty10<- (d.10.means[14,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
long.beach.z.loyalty10<- (d.10.means[15,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
macon.z.loyalty10<- (d.10.means[16,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
miami.z.loyalty10<- (d.10.means[17,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
milledgeville.z.loyalty10<- (d.10.means[18,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
myrtle.beach.z.loyalty10<- (d.10.means[19,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
palm.beach.z.loyalty10<- (d.10.means[20,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
philadelphia.z.loyalty10<- (d.10.means[21,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
san.jose.z.loyalty10<- (d.10.means[22,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
st.paul.z.loyalty10<- (d.10.means[23,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
state.college.z.loyalty10<- (d.10.means[24,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
tallahassee.z.loyalty10<- (d.10.means[25,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10
wichita.z.loyalty10<- (d.10.means[26,1]-data10.index.means$loyalty10)/data10.index.sd$loyalty.sd10

aberdeen.z.passion10<- (d.10.means[1,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
akron.z.passion10<- (d.10.means[2,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
biloxi.z.passion10<- (d.10.means[3,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
boulder.z.passion10<- (d.10.means[4,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
bradenton.z.passion10<- (d.10.means[5,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
charlotte.z.passion10<- (d.10.means[6,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
columbia.z.passion10<- (d.10.means[7,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
columbus.z.passion10<- (d.10.means[8,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
detroit.z.passion10<- (d.10.means[9,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
duluth.z.passion10<- (d.10.means[10,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
fort.wayne.z.passion10<- (d.10.means[11,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
gary.z.passion10<- (d.10.means[12,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
grand.forks.z.passion10<- (d.10.means[13,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
lexington.z.passion10<- (d.10.means[14,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
long.beach.z.passion10<- (d.10.means[15,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
macon.z.passion10<- (d.10.means[16,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
miami.z.passion10<- (d.10.means[17,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
milledgeville.z.passion10<- (d.10.means[18,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
myrtle.beach.z.passion10<- (d.10.means[19,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
palm.beach.z.passion10<- (d.10.means[20,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
philadelphia.z.passion10<- (d.10.means[21,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
san.jose.z.passion10<- (d.10.means[22,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
st.paul.z.passion10<- (d.10.means[23,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
state.college.z.passion10<- (d.10.means[24,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
tallahassee.z.passion10<- (d.10.means[25,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10
wichita.z.passion10<- (d.10.means[26,2]-data10.index.means$passion10)/data10.index.sd$passion.sd10

aberdeen.z.ca10<- (d.10.means[1,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
akron.z.ca10<- (d.10.means[2,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
biloxi.z.ca10<- (d.10.means[3,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
boulder.z.ca10<- (d.10.means[4,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
bradenton.z.ca10<- (d.10.means[5,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
charlotte.z.ca10<- (d.10.means[6,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
columbia.z.ca10<- (d.10.means[7,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
columbus.z.ca10<- (d.10.means[8,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
detroit.z.ca10<- (d.10.means[9,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
duluth.z.ca10<- (d.10.means[10,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
fort.wayne.z.ca10<- (d.10.means[11,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
gary.z.ca10<- (d.10.means[12,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
grand.forks.z.ca10<- (d.10.means[13,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
lexington.z.ca10<- (d.10.means[14,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
long.beach.z.ca10<- (d.10.means[15,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
macon.z.ca10<- (d.10.means[16,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
miami.z.ca10<- (d.10.means[17,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
milledgeville.z.ca10<- (d.10.means[18,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
myrtle.beach.z.ca10<- (d.10.means[19,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
palm.beach.z.ca10<- (d.10.means[20,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
philadelphia.z.ca10<- (d.10.means[21,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
san.jose.z.ca10<- (d.10.means[22,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
st.paul.z.ca10<- (d.10.means[23,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
state.college.z.ca10<- (d.10.means[24,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
tallahassee.z.ca10<- (d.10.means[25,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10
wichita.z.ca10<- (d.10.means[26,3]-data10.index.means$ca10)/data10.index.sd$ca.sd10

aberdeen.z.basic_se10<- (d.10.means[1,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
akron.z.basic_se10<- (d.10.means[2,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
biloxi.z.basic_se10<- (d.10.means[3,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
boulder.z.basic_se10<- (d.10.means[4,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
bradenton.z.basic_se10<- (d.10.means[5,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
charlotte.z.basic_se10<- (d.10.means[6,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
columbia.z.basic_se10<- (d.10.means[7,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
columbus.z.basic_se10<- (d.10.means[8,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
detroit.z.basic_se10<- (d.10.means[9,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
duluth.z.basic_se10<- (d.10.means[10,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
fort.wayne.z.basic_se10<- (d.10.means[11,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
gary.z.basic_se10<- (d.10.means[12,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
grand.forks.z.basic_se10<- (d.10.means[13,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
lexington.z.basic_se10<- (d.10.means[14,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
long.beach.z.basic_se10<- (d.10.means[15,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
macon.z.basic_se10<- (d.10.means[16,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
miami.z.basic_se10<- (d.10.means[17,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
milledgeville.z.basic_se10<- (d.10.means[18,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
myrtle.beach.z.basic_se10<- (d.10.means[19,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
palm.beach.z.basic_se10<- (d.10.means[20,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
philadelphia.z.basic_se10<- (d.10.means[21,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
san.jose.z.basic_se10<- (d.10.means[22,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
st.paul.z.basic_se10<- (d.10.means[23,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
state.college.z.basic_se10<- (d.10.means[24,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
tallahassee.z.basic_se10<- (d.10.means[25,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10
wichita.z.basic_se10<- (d.10.means[26,4]-data10.index.means$basic_se10)/data10.index.sd$basic_se.sd10

aberdeen.z.leadersh10<- (d.10.means[1,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
akron.z.leadersh10<- (d.10.means[2,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
biloxi.z.leadersh10<- (d.10.means[3,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
boulder.z.leadersh10<- (d.10.means[4,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
bradenton.z.leadersh10<- (d.10.means[5,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
charlotte.z.leadersh10<- (d.10.means[6,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
columbia.z.leadersh10<- (d.10.means[7,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
columbus.z.leadersh10<- (d.10.means[8,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
detroit.z.leadersh10<- (d.10.means[9,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
duluth.z.leadersh10<- (d.10.means[10,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
fort.wayne.z.leadersh10<- (d.10.means[11,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
gary.z.leadersh10<- (d.10.means[12,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
grand.forks.z.leadersh10<- (d.10.means[13,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
lexington.z.leadersh10<- (d.10.means[14,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
long.beach.z.leadersh10<- (d.10.means[15,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
macon.z.leadersh10<- (d.10.means[16,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
miami.z.leadersh10<- (d.10.means[17,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
milledgeville.z.leadersh10<- (d.10.means[18,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
myrtle.beach.z.leadersh10<- (d.10.means[19,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
palm.beach.z.leadersh10<- (d.10.means[20,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
philadelphia.z.leadersh10<- (d.10.means[21,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
san.jose.z.leadersh10<- (d.10.means[22,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
st.paul.z.leadersh10<- (d.10.means[23,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
state.college.z.leadersh10<- (d.10.means[24,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
tallahassee.z.leadersh10<- (d.10.means[25,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10
wichita.z.leadersh10<- (d.10.means[26,5]-data10.index.means$leadersh10)/data10.index.sd$leadersh.sd10

aberdeen.z.educatio10<- (d.10.means[1,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
akron.z.educatio10<- (d.10.means[2,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
biloxi.z.educatio10<- (d.10.means[3,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
boulder.z.educatio10<- (d.10.means[4,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
bradenton.z.educatio10<- (d.10.means[5,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
charlotte.z.educatio10<- (d.10.means[6,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
columbia.z.educatio10<- (d.10.means[7,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
columbus.z.educatio10<- (d.10.means[8,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
detroit.z.educatio10<- (d.10.means[9,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
duluth.z.educatio10<- (d.10.means[10,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
fort.wayne.z.educatio10<- (d.10.means[11,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
gary.z.educatio10<- (d.10.means[12,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
grand.forks.z.educatio10<- (d.10.means[13,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
lexington.z.educatio10<- (d.10.means[14,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
long.beach.z.educatio10<- (d.10.means[15,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
macon.z.educatio10<- (d.10.means[16,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
miami.z.educatio10<- (d.10.means[17,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
milledgeville.z.educatio10<- (d.10.means[18,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
myrtle.beach.z.educatio10<- (d.10.means[19,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
palm.beach.z.educatio10<- (d.10.means[20,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
philadelphia.z.educatio10<- (d.10.means[21,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
san.jose.z.educatio10<- (d.10.means[22,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
st.paul.z.educatio10<- (d.10.means[23,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
state.college.z.educatio10<- (d.10.means[24,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
tallahassee.z.educatio10<- (d.10.means[25,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10
wichita.z.educatio10<- (d.10.means[26,6]-data10.index.means$educatio10)/data10.index.sd$educatio.sd10

aberdeen.z.safety10<- (d.10.means[1,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
akron.z.safety10<- (d.10.means[2,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
biloxi.z.safety10<- (d.10.means[3,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
boulder.z.safety10<- (d.10.means[4,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
bradenton.z.safety10<- (d.10.means[5,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
charlotte.z.safety10<- (d.10.means[6,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
columbia.z.safety10<- (d.10.means[7,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
columbus.z.safety10<- (d.10.means[8,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
detroit.z.safety10<- (d.10.means[9,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
duluth.z.safety10<- (d.10.means[10,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
fort.wayne.z.safety10<- (d.10.means[11,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
gary.z.safety10<- (d.10.means[12,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
grand.forks.z.safety10<- (d.10.means[13,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
lexington.z.safety10<- (d.10.means[14,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
long.beach.z.safety10<- (d.10.means[15,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
macon.z.safety10<- (d.10.means[16,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
miami.z.safety10<- (d.10.means[17,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
milledgeville.z.safety10<- (d.10.means[18,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
myrtle.beach.z.safety10<- (d.10.means[19,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
palm.beach.z.safety10<- (d.10.means[20,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
philadelphia.z.safety10<- (d.10.means[21,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
san.jose.z.safety10<- (d.10.means[22,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
st.paul.z.safety10<- (d.10.means[23,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
state.college.z.safety10<- (d.10.means[24,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
tallahassee.z.safety10<- (d.10.means[25,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10
wichita.z.safety10<- (d.10.means[26,7]-data10.index.means$safety10)/data10.index.sd$safety.sd10

aberdeen.z.aestheti10<- (d.10.means[1,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
akron.z.aestheti10<- (d.10.means[2,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
biloxi.z.aestheti10<- (d.10.means[3,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
boulder.z.aestheti10<- (d.10.means[4,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
bradenton.z.aestheti10<- (d.10.means[5,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
charlotte.z.aestheti10<- (d.10.means[6,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
columbia.z.aestheti10<- (d.10.means[7,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
columbus.z.aestheti10<- (d.10.means[8,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
detroit.z.aestheti10<- (d.10.means[9,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
duluth.z.aestheti10<- (d.10.means[10,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
fort.wayne.z.aestheti10<- (d.10.means[11,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
gary.z.aestheti10<- (d.10.means[12,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
grand.forks.z.aestheti10<- (d.10.means[13,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
lexington.z.aestheti10<- (d.10.means[14,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
long.beach.z.aestheti10<- (d.10.means[15,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
macon.z.aestheti10<- (d.10.means[16,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
miami.z.aestheti10<- (d.10.means[17,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
milledgeville.z.aestheti10<- (d.10.means[18,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
myrtle.beach.z.aestheti10<- (d.10.means[19,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
palm.beach.z.aestheti10<- (d.10.means[20,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
philadelphia.z.aestheti10<- (d.10.means[21,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
san.jose.z.aestheti10<- (d.10.means[22,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
st.paul.z.aestheti10<- (d.10.means[23,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
state.college.z.aestheti10<- (d.10.means[24,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
tallahassee.z.aestheti10<- (d.10.means[25,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10
wichita.z.aestheti10<- (d.10.means[26,8]-data10.index.means$aestheti10)/data10.index.sd$aestheti.sd10

aberdeen.z.economy10<- (d.10.means[1,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
akron.z.economy10<- (d.10.means[2,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
biloxi.z.economy10<- (d.10.means[3,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
boulder.z.economy10<- (d.10.means[4,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
bradenton.z.economy10<- (d.10.means[5,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
charlotte.z.economy10<- (d.10.means[6,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
columbia.z.economy10<- (d.10.means[7,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
columbus.z.economy10<- (d.10.means[8,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
detroit.z.economy10<- (d.10.means[9,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
duluth.z.economy10<- (d.10.means[10,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
fort.wayne.z.economy10<- (d.10.means[11,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
gary.z.economy10<- (d.10.means[12,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
grand.forks.z.economy10<- (d.10.means[13,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
lexington.z.economy10<- (d.10.means[14,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
long.beach.z.economy10<- (d.10.means[15,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
macon.z.economy10<- (d.10.means[16,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
miami.z.economy10<- (d.10.means[17,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
milledgeville.z.economy10<- (d.10.means[18,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
myrtle.beach.z.economy10<- (d.10.means[19,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
palm.beach.z.economy10<- (d.10.means[20,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
philadelphia.z.economy10<- (d.10.means[21,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
san.jose.z.economy10<- (d.10.means[22,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
st.paul.z.economy10<- (d.10.means[23,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
state.college.z.economy10<- (d.10.means[24,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
tallahassee.z.economy10<- (d.10.means[25,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10
wichita.z.economy10<- (d.10.means[26,9]-data10.index.means$economy10)/data10.index.sd$economy.sd10

aberdeen.z.social_o10<- (d.10.means[1,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
akron.z.social_o10<- (d.10.means[2,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
biloxi.z.social_o10<- (d.10.means[3,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
boulder.z.social_o10<- (d.10.means[4,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
bradenton.z.social_o10<- (d.10.means[5,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
charlotte.z.social_o10<- (d.10.means[6,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
columbia.z.social_o10<- (d.10.means[7,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
columbus.z.social_o10<- (d.10.means[8,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
detroit.z.social_o10<- (d.10.means[9,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
duluth.z.social_o10<- (d.10.means[10,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
fort.wayne.z.social_o10<- (d.10.means[11,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
gary.z.social_o10<- (d.10.means[12,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
grand.forks.z.social_o10<- (d.10.means[13,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
lexington.z.social_o10<- (d.10.means[14,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
long.beach.z.social_o10<- (d.10.means[15,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
macon.z.social_o10<- (d.10.means[16,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
miami.z.social_o10<- (d.10.means[17,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
milledgeville.z.social_o10<- (d.10.means[18,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
myrtle.beach.z.social_o10<- (d.10.means[19,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
palm.beach.z.social_o10<- (d.10.means[20,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
philadelphia.z.social_o10<- (d.10.means[21,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
san.jose.z.social_o10<- (d.10.means[22,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
st.paul.z.social_o10<- (d.10.means[23,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
state.college.z.social_o10<- (d.10.means[24,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
tallahassee.z.social_o10<- (d.10.means[25,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10
wichita.z.social_o10<- (d.10.means[26,10]-data10.index.means$social_o10)/data10.index.sd$social_o.sd10

aberdeen.z.communit10<- (d.10.means[1,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
akron.z.communit10<- (d.10.means[2,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
biloxi.z.communit10<- (d.10.means[3,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
boulder.z.communit10<- (d.10.means[4,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
bradenton.z.communit10<- (d.10.means[5,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
charlotte.z.communit10<- (d.10.means[6,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
columbia.z.communit10<- (d.10.means[7,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
columbus.z.communit10<- (d.10.means[8,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
detroit.z.communit10<- (d.10.means[9,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
duluth.z.communit10<- (d.10.means[10,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
fort.wayne.z.communit10<- (d.10.means[11,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
gary.z.communit10<- (d.10.means[12,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
grand.forks.z.communit10<- (d.10.means[13,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
lexington.z.communit10<- (d.10.means[14,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
long.beach.z.communit10<- (d.10.means[15,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
macon.z.communit10<- (d.10.means[16,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
miami.z.communit10<- (d.10.means[17,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
milledgeville.z.communit10<- (d.10.means[18,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
myrtle.beach.z.communit10<- (d.10.means[19,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
palm.beach.z.communit10<- (d.10.means[20,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
philadelphia.z.communit10<- (d.10.means[21,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
san.jose.z.communit10<- (d.10.means[22,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
st.paul.z.communit10<- (d.10.means[23,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
state.college.z.communit10<- (d.10.means[24,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
tallahassee.z.communit10<- (d.10.means[25,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10
wichita.z.communit10<- (d.10.means[26,11]-data10.index.means$communit10)/data10.index.sd$communit.sd10

aberdeen.z.involvem10<- (d.10.means[1,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
akron.z.involvem10<- (d.10.means[2,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
biloxi.z.involvem10<- (d.10.means[3,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
boulder.z.involvem10<- (d.10.means[4,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
bradenton.z.involvem10<- (d.10.means[5,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
charlotte.z.involvem10<- (d.10.means[6,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
columbia.z.involvem10<- (d.10.means[7,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
columbus.z.involvem10<- (d.10.means[8,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
detroit.z.involvem10<- (d.10.means[9,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
duluth.z.involvem10<- (d.10.means[10,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
fort.wayne.z.involvem10<- (d.10.means[11,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
gary.z.involvem10<- (d.10.means[12,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
grand.forks.z.involvem10<- (d.10.means[13,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
lexington.z.involvem10<- (d.10.means[14,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
long.beach.z.involvem10<- (d.10.means[15,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
macon.z.involvem10<- (d.10.means[16,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
miami.z.involvem10<- (d.10.means[17,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
milledgeville.z.involvem10<- (d.10.means[18,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
myrtle.beach.z.involvem10<- (d.10.means[19,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
palm.beach.z.involvem10<- (d.10.means[20,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
philadelphia.z.involvem10<- (d.10.means[21,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
san.jose.z.involvem10<- (d.10.means[22,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
st.paul.z.involvem10<- (d.10.means[23,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
state.college.z.involvem10<- (d.10.means[24,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
tallahassee.z.involvem10<- (d.10.means[25,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10
wichita.z.involvem10<- (d.10.means[26,12]-data10.index.means$involvem10)/data10.index.sd$involvem.sd10

aberdeen.z.openness10<- (d.10.means[1,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
akron.z.openness10<- (d.10.means[2,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
biloxi.z.openness10<- (d.10.means[3,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
boulder.z.openness10<- (d.10.means[4,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
bradenton.z.openness10<- (d.10.means[5,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
charlotte.z.openness10<- (d.10.means[6,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
columbia.z.openness10<- (d.10.means[7,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
columbus.z.openness10<- (d.10.means[8,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
detroit.z.openness10<- (d.10.means[9,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
duluth.z.openness10<- (d.10.means[10,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
fort.wayne.z.openness10<- (d.10.means[11,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
gary.z.openness10<- (d.10.means[12,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
grand.forks.z.openness10<- (d.10.means[13,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
lexington.z.openness10<- (d.10.means[14,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
long.beach.z.openness10<- (d.10.means[15,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
macon.z.openness10<- (d.10.means[16,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
miami.z.openness10<- (d.10.means[17,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
milledgeville.z.openness10<- (d.10.means[18,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
myrtle.beach.z.openness10<- (d.10.means[19,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
palm.beach.z.openness10<- (d.10.means[20,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
philadelphia.z.openness10<- (d.10.means[21,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
san.jose.z.openness10<- (d.10.means[22,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
st.paul.z.openness10<- (d.10.means[23,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
state.college.z.openness10<- (d.10.means[24,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
tallahassee.z.openness10<- (d.10.means[25,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10
wichita.z.openness10<- (d.10.means[26,13]-data10.index.means$openness10)/data10.index.sd$openness.sd10

aberdeen.z.social_c10<- (d.10.means[1,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
akron.z.social_c10<- (d.10.means[2,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
biloxi.z.social_c10<- (d.10.means[3,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
boulder.z.social_c10<- (d.10.means[4,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
bradenton.z.social_c10<- (d.10.means[5,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
charlotte.z.social_c10<- (d.10.means[6,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
columbia.z.social_c10<- (d.10.means[7,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
columbus.z.social_c10<- (d.10.means[8,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
detroit.z.social_c10<- (d.10.means[9,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
duluth.z.social_c10<- (d.10.means[10,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
fort.wayne.z.social_c10<- (d.10.means[11,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
gary.z.social_c10<- (d.10.means[12,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
grand.forks.z.social_c10<- (d.10.means[13,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
lexington.z.social_c10<- (d.10.means[14,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
long.beach.z.social_c10<- (d.10.means[15,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
macon.z.social_c10<- (d.10.means[16,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
miami.z.social_c10<- (d.10.means[17,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
milledgeville.z.social_c10<- (d.10.means[18,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
myrtle.beach.z.social_c10<- (d.10.means[19,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
palm.beach.z.social_c10<- (d.10.means[20,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
philadelphia.z.social_c10<- (d.10.means[21,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
san.jose.z.social_c10<- (d.10.means[22,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
st.paul.z.social_c10<- (d.10.means[23,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
state.college.z.social_c10<- (d.10.means[24,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
tallahassee.z.social_c10<- (d.10.means[25,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10
wichita.z.social_c10<- (d.10.means[26,14]-data10.index.means$social_c10)/data10.index.sd$social_c.sd10

aberdeen.z.domains10<- (d.10.means[1,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
akron.z.domains10<- (d.10.means[2,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
biloxi.z.domains10<- (d.10.means[3,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
boulder.z.domains10<- (d.10.means[4,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
bradenton.z.domains10<- (d.10.means[5,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
charlotte.z.domains10<- (d.10.means[6,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
columbia.z.domains10<- (d.10.means[7,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
columbus.z.domains10<- (d.10.means[8,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
detroit.z.domains10<- (d.10.means[9,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
duluth.z.domains10<- (d.10.means[10,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
fort.wayne.z.domains10<- (d.10.means[11,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
gary.z.domains10<- (d.10.means[12,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
grand.forks.z.domains10<- (d.10.means[13,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
lexington.z.domains10<- (d.10.means[14,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
long.beach.z.domains10<- (d.10.means[15,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
macon.z.domains10<- (d.10.means[16,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
miami.z.domains10<- (d.10.means[17,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
milledgeville.z.domains10<- (d.10.means[18,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
myrtle.beach.z.domains10<- (d.10.means[19,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
palm.beach.z.domains10<- (d.10.means[20,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
philadelphia.z.domains10<- (d.10.means[21,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
san.jose.z.domains10<- (d.10.means[22,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
st.paul.z.domains10<- (d.10.means[23,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
state.college.z.domains10<- (d.10.means[24,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
tallahassee.z.domains10<- (d.10.means[25,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10
wichita.z.domains10<- (d.10.means[26,15]-data10.index.means$domains10)/data10.index.sd$domains.sd10


################################
z10<- rbind(aberdeen.z.loyalty10,akron.z.loyalty10,biloxi.z.loyalty10,boulder.z.loyalty10,bradenton.z.loyalty10,
            charlotte.z.loyalty10, columbia.z.loyalty10,columbus.z.loyalty10,detroit.z.loyalty10,duluth.z.loyalty10,fort.wayne.z.loyalty10,
            gary.z.loyalty10,grand.forks.z.loyalty10,lexington.z.loyalty10,long.beach.z.loyalty10,macon.z.loyalty10,miami.z.loyalty10,
            milledgeville.z.loyalty10,myrtle.beach.z.loyalty10,palm.beach.z.loyalty10,philadelphia.z.loyalty10,
            san.jose.z.loyalty10,st.paul.z.loyalty10,state.college.z.loyalty10,tallahassee.z.loyalty10,wichita.z.loyalty10,
            aberdeen.z.passion10,akron.z.passion10,biloxi.z.passion10,boulder.z.passion10,bradenton.z.passion10,
            charlotte.z.passion10, columbia.z.passion10,columbus.z.passion10,detroit.z.passion10,duluth.z.passion10,fort.wayne.z.passion10,
            gary.z.passion10,grand.forks.z.passion10,lexington.z.passion10,long.beach.z.passion10,macon.z.passion10,miami.z.passion10,
            milledgeville.z.passion10,myrtle.beach.z.passion10,palm.beach.z.passion10,philadelphia.z.passion10,
            san.jose.z.passion10,st.paul.z.passion10,state.college.z.passion10,tallahassee.z.passion10,wichita.z.passion10,
            
            aberdeen.z.ca10,akron.z.ca10,biloxi.z.ca10,boulder.z.ca10,bradenton.z.ca10,
            charlotte.z.ca10, columbia.z.ca10,columbus.z.ca10,detroit.z.ca10,duluth.z.ca10,fort.wayne.z.ca10,
            gary.z.ca10,grand.forks.z.ca10,lexington.z.ca10,long.beach.z.ca10,macon.z.ca10,miami.z.ca10,
            milledgeville.z.ca10,myrtle.beach.z.ca10,palm.beach.z.ca10,philadelphia.z.ca10,
            san.jose.z.ca10,st.paul.z.ca10,state.college.z.ca10,tallahassee.z.ca10,wichita.z.ca10,
            aberdeen.z.basic_se10,akron.z.basic_se10,biloxi.z.basic_se10,boulder.z.basic_se10,bradenton.z.basic_se10,
            charlotte.z.basic_se10, columbia.z.basic_se10,columbus.z.basic_se10,detroit.z.basic_se10,duluth.z.basic_se10,fort.wayne.z.basic_se10,
            gary.z.basic_se10,grand.forks.z.basic_se10,lexington.z.basic_se10,long.beach.z.basic_se10,macon.z.basic_se10,miami.z.basic_se10,
            milledgeville.z.basic_se10,myrtle.beach.z.basic_se10,palm.beach.z.basic_se10,philadelphia.z.basic_se10,
            san.jose.z.basic_se10,st.paul.z.basic_se10,state.college.z.basic_se10,tallahassee.z.basic_se10,wichita.z.basic_se10,
            
            aberdeen.z.leadersh10,akron.z.leadersh10,biloxi.z.leadersh10,boulder.z.leadersh10,bradenton.z.leadersh10,
            charlotte.z.leadersh10, columbia.z.leadersh10,columbus.z.leadersh10,detroit.z.leadersh10,duluth.z.leadersh10,fort.wayne.z.leadersh10,
            gary.z.leadersh10,grand.forks.z.leadersh10,lexington.z.leadersh10,long.beach.z.leadersh10,macon.z.leadersh10,miami.z.leadersh10,
            milledgeville.z.leadersh10,myrtle.beach.z.leadersh10,palm.beach.z.leadersh10,philadelphia.z.leadersh10,
            san.jose.z.leadersh10,st.paul.z.leadersh10,state.college.z.leadersh10,tallahassee.z.leadersh10,wichita.z.leadersh10,
            
            aberdeen.z.educatio10,akron.z.educatio10,biloxi.z.educatio10,boulder.z.educatio10,bradenton.z.educatio10,
            charlotte.z.educatio10, columbia.z.educatio10,columbus.z.educatio10,detroit.z.educatio10,duluth.z.educatio10,fort.wayne.z.educatio10,
            gary.z.educatio10,grand.forks.z.educatio10,lexington.z.educatio10,long.beach.z.educatio10,macon.z.educatio10,miami.z.educatio10,
            milledgeville.z.educatio10,myrtle.beach.z.educatio10,palm.beach.z.educatio10,philadelphia.z.educatio10,
            san.jose.z.educatio10,st.paul.z.educatio10,state.college.z.educatio10,tallahassee.z.educatio10,wichita.z.educatio10,
            
            aberdeen.z.safety10,akron.z.safety10,biloxi.z.safety10,boulder.z.safety10,bradenton.z.safety10,
            charlotte.z.safety10, columbia.z.safety10,columbus.z.safety10,detroit.z.safety10,duluth.z.safety10,fort.wayne.z.safety10,
            gary.z.safety10,grand.forks.z.safety10,lexington.z.safety10,long.beach.z.safety10,macon.z.safety10,miami.z.safety10,
            milledgeville.z.safety10,myrtle.beach.z.safety10,palm.beach.z.safety10,philadelphia.z.safety10,
            san.jose.z.safety10,st.paul.z.safety10,state.college.z.safety10,tallahassee.z.safety10,wichita.z.safety10,
            
            aberdeen.z.aestheti10,akron.z.aestheti10,biloxi.z.aestheti10,boulder.z.aestheti10,bradenton.z.aestheti10,
            charlotte.z.aestheti10, columbia.z.aestheti10,columbus.z.aestheti10,detroit.z.aestheti10,duluth.z.aestheti10,fort.wayne.z.aestheti10,
            gary.z.aestheti10,grand.forks.z.aestheti10,lexington.z.aestheti10,long.beach.z.aestheti10,macon.z.aestheti10,miami.z.aestheti10,
            milledgeville.z.aestheti10,myrtle.beach.z.aestheti10,palm.beach.z.aestheti10,philadelphia.z.aestheti10,
            san.jose.z.aestheti10,st.paul.z.aestheti10,state.college.z.aestheti10,tallahassee.z.aestheti10,wichita.z.aestheti10,
            
            aberdeen.z.economy10,akron.z.economy10,biloxi.z.economy10,boulder.z.economy10,bradenton.z.economy10,
            charlotte.z.economy10, columbia.z.economy10,columbus.z.economy10,detroit.z.economy10,duluth.z.economy10,fort.wayne.z.economy10,
            gary.z.economy10,grand.forks.z.economy10,lexington.z.economy10,long.beach.z.economy10,macon.z.economy10,miami.z.economy10,
            milledgeville.z.economy10,myrtle.beach.z.economy10,palm.beach.z.economy10,philadelphia.z.economy10,
            san.jose.z.economy10,st.paul.z.economy10,state.college.z.economy10,tallahassee.z.economy10,wichita.z.economy10,
            
            aberdeen.z.social_o10,akron.z.social_o10,biloxi.z.social_o10,boulder.z.social_o10,bradenton.z.social_o10,
            charlotte.z.social_o10, columbia.z.social_o10,columbus.z.social_o10,detroit.z.social_o10,duluth.z.social_o10,fort.wayne.z.social_o10,
            gary.z.social_o10,grand.forks.z.social_o10,lexington.z.social_o10,long.beach.z.social_o10,macon.z.social_o10,miami.z.social_o10,
            milledgeville.z.social_o10,myrtle.beach.z.social_o10,palm.beach.z.social_o10,philadelphia.z.social_o10,
            san.jose.z.social_o10,st.paul.z.social_o10,state.college.z.social_o10,tallahassee.z.social_o10,wichita.z.social_o10,
            
            aberdeen.z.communit10,akron.z.communit10,biloxi.z.communit10,boulder.z.communit10,bradenton.z.communit10,
            charlotte.z.communit10, columbia.z.communit10,columbus.z.communit10,detroit.z.communit10,duluth.z.communit10,fort.wayne.z.communit10,
            gary.z.communit10,grand.forks.z.communit10,lexington.z.communit10,long.beach.z.communit10,macon.z.communit10,miami.z.communit10,
            milledgeville.z.communit10,myrtle.beach.z.communit10,palm.beach.z.communit10,philadelphia.z.communit10,
            san.jose.z.communit10,st.paul.z.communit10,state.college.z.communit10,tallahassee.z.communit10,wichita.z.communit10,
            
            aberdeen.z.involvem10,akron.z.involvem10,biloxi.z.involvem10,boulder.z.involvem10,bradenton.z.involvem10,
            charlotte.z.involvem10, columbia.z.involvem10,columbus.z.involvem10,detroit.z.involvem10,duluth.z.involvem10,fort.wayne.z.involvem10,
            gary.z.involvem10,grand.forks.z.involvem10,lexington.z.involvem10,long.beach.z.involvem10,macon.z.involvem10,miami.z.involvem10,
            milledgeville.z.involvem10,myrtle.beach.z.involvem10,palm.beach.z.involvem10,philadelphia.z.involvem10,
            san.jose.z.involvem10,st.paul.z.involvem10,state.college.z.involvem10,tallahassee.z.involvem10,wichita.z.involvem10,
            
            aberdeen.z.openness10,akron.z.openness10,biloxi.z.openness10,boulder.z.openness10,bradenton.z.openness10,
            charlotte.z.openness10, columbia.z.openness10,columbus.z.openness10,detroit.z.openness10,duluth.z.openness10,fort.wayne.z.openness10,
            gary.z.openness10,grand.forks.z.openness10,lexington.z.openness10,long.beach.z.openness10,macon.z.openness10,miami.z.openness10,
            milledgeville.z.openness10,myrtle.beach.z.openness10,palm.beach.z.openness10,philadelphia.z.openness10,
            san.jose.z.openness10,st.paul.z.openness10,state.college.z.openness10,tallahassee.z.openness10,wichita.z.openness10,
            
            
            aberdeen.z.social_c10,akron.z.social_c10,biloxi.z.social_c10,boulder.z.social_c10,bradenton.z.social_c10,
            charlotte.z.social_c10, columbia.z.social_c10,columbus.z.social_c10,detroit.z.social_c10,duluth.z.social_c10,fort.wayne.z.social_c10,
            gary.z.social_c10,grand.forks.z.social_c10,lexington.z.social_c10,long.beach.z.social_c10,macon.z.social_c10,miami.z.social_c10,
            milledgeville.z.social_c10,myrtle.beach.z.social_c10,palm.beach.z.social_c10,philadelphia.z.social_c10,
            san.jose.z.social_c10,st.paul.z.social_c10,state.college.z.social_c10,tallahassee.z.social_c10,wichita.z.social_c10,
            
            aberdeen.z.domains10,akron.z.domains10,biloxi.z.domains10,boulder.z.domains10,bradenton.z.domains10,
            charlotte.z.domains10, columbia.z.domains10,columbus.z.domains10,detroit.z.domains10,duluth.z.domains10,fort.wayne.z.domains10,
            gary.z.domains10,grand.forks.z.domains10,lexington.z.domains10,long.beach.z.domains10,macon.z.domains10,miami.z.domains10,
            milledgeville.z.domains10,myrtle.beach.z.domains10,palm.beach.z.domains10,philadelphia.z.domains10,
            san.jose.z.domains10,st.paul.z.domains10,state.college.z.domains10,tallahassee.z.domains10,wichita.z.domains10)

#############################################################################################################
z10<- data.frame(z10)
z10$City = rownames(z10)
rownames(z10) = NULL
colnames(z10)<- c("zscore", 'Index')

z10<- data.frame(z10)

LOYALTY10<- data.frame(z10[1:26,1])
colnames(LOYALTY10)<- 'LOYALTY'

PASSION10<- data.frame(z10[27:52,1])
colnames(PASSION10)<- 'PASSION'

CA10<- data.frame(z10[53:78,1])
colnames(CA10)<- 'CA'

BASIC_SE10<- data.frame(z10[79:104,1])
colnames(BASIC_SE10)<- 'BASIC_SE'

LEADERSH10<- data.frame(z10[105:130,1])
colnames(LEADERSH10)<- 'LEADERSH'

EDUCATIO10<- data.frame(z10[131:156,1])
colnames(EDUCATIO10)<- 'EDUCATIO'

SAFETY10<- data.frame(z10[157:182,1])
colnames(SAFETY10)<- 'SAFETY'

AESTHETI10<- data.frame(z10[183:208,1])
colnames(AESTHETI10)<- 'AESTHETI'

ECONOMY10<- data.frame(z10[209:234,1])
colnames(ECONOMY10)<- 'ECONOMY'

SOCIAL_O10<- data.frame(z10[235:260,1])
colnames(SOCIAL_O10)<- 'SOCIAL_O'

COMMUNIT10<- data.frame(z10[261:286,1])
colnames(COMMUNIT10)<- 'COMMUNIT'

INVOLVEM10<- data.frame(z10[287:312,1])
colnames(INVOLVEM10)<- 'INVOLVEM'

OPENNESS10<- data.frame(z10[313:338,1])
colnames(OPENNESS10)<- 'OPENNESS'

SOCIAL_C10<- data.frame(z10[339:364,1])
colnames(SOCIAL_C10)<- 'SOCIAL_C'

DOMAINS10<- data.frame(z10[365:390,1])
colnames(DOMAINS10)<- 'DOMAINS'


zscores10<- cbind(LOYALTY10,PASSION10,CA10,BASIC_SE10,LEADERSH10,EDUCATIO10,SAFETY10,
                  AESTHETI10,ECONOMY10,SOCIAL_O10,COMMUNIT10,INVOLVEM10,OPENNESS10,SOCIAL_C10,DOMAINS10)
zscores10$Year<- rep(2010,26)


#############################################################################################################
zscores<- rbind(zscores08,zscores09,zscores10)

#######################################################################################

########## MDS on z-scores ################################
library(MASS)
zscores08.mds.dist=dist(zscores08)
mds.z08<- isoMDS(zscores08.mds.dist)

zscores09.mds.dist=dist(zscores09)
mds.z09<- isoMDS(zscores09.mds.dist)

zscores10.mds.dist=dist(zscores10)
mds.z10<- isoMDS(zscores10.mds.dist)

mds.z08<- data.frame(mds.z08)
mds.z09<- data.frame(mds.z09)
mds.z10<- data.frame(mds.z10)

mds.z08<- mds.z08[,-3]
mds.z09<- mds.z09[,-3]
mds.z10<- mds.z10[,-3]

colnames(mds.z08)<- c("Dimension 1", "Dimension 2")
colnames(mds.z09)<- c("Dimension 1", "Dimension 2")
colnames(mds.z10)<- c("Dimension 1", "Dimension 2")

#mds.z08[,1]<- mds.z08[,1]*1
#mds.z08[,2]<- mds.zs08[,2]*-1

City<- c("Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
         "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
         "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
         "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
         "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
         "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")

mds.z08$City<- City
mds.z09$City<- City
mds.z10$City<- City

year08<- rep(2008, 26)
year09<- rep(2009, 26)
year10<- rep(2010, 26)

mds.z08$Year<- year08
mds.z09$Year<- year09
mds.z10$Year<- year10

Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
mds.z08$Region<- Region
mds.z09$Region<- Region
mds.z10$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
mds.z08$Urbanicity<- Urbanicity
mds.z09$Urbanicity<- Urbanicity
mds.z10$Urbanicity<- Urbanicity

mds.z08<- mds.z08[,c(4,3,5,6,1,2)]
mds.z09<- mds.z09[,c(4,3,5,6,1,2)]
mds.z10<- mds.z10[,c(4,3,5,6,1,2)]



mds.z<- rbind(mds.z08,mds.z09,mds.z10)

library(googleVis)

plot(gvisMotionChart(mds.z, idvar='City',timevar='Year'))



######## add a column for region and urbanicity and rearrange the columns###############
Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
d.08.means$Region<- Region
d.09.means$Region<- Region
d.10.means$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
d.08.means$Urbanicity<- Urbanicity
d.09.means$Urbanicity<- Urbanicity
d.10.means$Urbanicity<- Urbanicity

### rearrange the columns ##############################################
d.08.means<- d.08.means[c(16:19,1:15)]
d.09.means<- d.09.means[c(16:19,1:15)]
d.10.means<- d.10.means[c(16:19,1:15)]

################################################################

###### create matching column names for each year's data set #####
colnames(d.08.means)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS")
colnames(d.09.means)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS")
colnames(d.10.means)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS")

#######################################################################

########  merge the three years into one data frame ###########
means<- rbind(d.08.means,d.09.means,d.10.means)

###############################################################


############# PCA on the means ####################################
library(FactoMineR)

pca08<- PCA(d.08.means[,-c(1:4)],graph=FALSE)
col.coord.08<- pca08$var$coord   #column coordinates
row.coord.08<- pca08$ind$coord   #row coordinates

pca09<- PCA(d.09.means[,-c(1:4)],graph=FALSE)
col.coord.09<- pca09$var$coord   #column coordinates
row.coord.09<- pca09$ind$coord   #row coordinates

pca10<- PCA(d.10.means[,-c(1:4)],graph=FALSE)
col.coord.10<- pca10$var$coord   #column coordinates
row.coord.10<- pca10$ind$coord   #row coordinates

########################################################################

############## create data frame for the PCA dimensions ################
Year.08<- rep(2008,41)
Category<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY","AESTHETI",
             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C","DOMAINS",
             "Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
             "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
             "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
             "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
             "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
             "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")
Category<- data.frame(Category)
Variables<- c("Loyalty","Passion","Attachment","Services","Leadership","Education","Safety",
             "Aesthetics","Economy","Social Offerings","Community","Involvement","Openness",
             "Social Capital","Domains",rep(' ',26))
Variables<- data.frame(Variables)
Type<- c(rep('Index',15),rep('City',26))
Type<- data.frame(Type)
Size<- c(rep(0.8,15), rep(0.1,26))
Size<- data.frame(Size)
Region<- c(rep('',15),"West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
Region<- data.frame(Region)
Urbanicity<- c(rep('',15),"Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop" )
Urbanicity<- data.frame(Urbanicity)


Dim1.08<- rbind(col.coord.08,row.coord.08)
Dim1.08<- data.frame(Dim1.08)

Dim1.08<- cbind(Year.08,Category,Dim1.08,Variables,Type,Size,Region,Urbanicity)
Dim1.08$Year<- Year.08
rownames(Dim1.08)<- NULL
Dim1.08<- Dim1.08[,-1]
Dim1.08<- Dim1.08[,c(12,1:11)]

Dim1.09<- rbind(col.coord.09,row.coord.09)
Dim1.09<- data.frame(Dim1.09)

Year.09<- rep(2009,41)

Dim1.09<- cbind(Year.09,Category,Dim1.09,Variables,Type,Size,Region,Urbanicity)
Dim1.09$Year<- Year.09
rownames(Dim1.09)<- NULL
Dim1.09<- Dim1.09[,-1]
Dim1.09<- Dim1.09[,c(12,1:11)]

Dim1.10<- rbind(col.coord.10,row.coord.10)
Dim1.10<- data.frame(Dim1.10)

Year.10<- rep(2010,41)


Dim1.10<- cbind(Year.10,Category,Dim1.10,Variables,Type,Size,Region,Urbanicity)
Dim1.10$Year<- Year.10
rownames(Dim1.10)<- NULL
Dim1.10<- Dim1.10[,-1]
Dim1.10<- Dim1.10[,c(12,1:11)]

pca.means<- rbind(Dim1.08,Dim1.09,Dim1.10)

##############################################################################################

############# plot motion chart of PCA means ###################################
library(googleVis)
plot(gvisMotionChart(pca.means,idvar='Category',timevar='Year'))

#############################################################################

####### MDS on means ###########################
library(MASS)
means08<- means[1:26,-(1:4)]
means09<- means[27:52,-(1:4)]
means10<- means[53:78,-(1:4)]

means08.mds.dist=dist(means08)
mds.means08<- isoMDS(means08.mds.dist)

means09.mds.dist=dist(means09)
mds.means09<- isoMDS(means09.mds.dist)

means10.mds.dist=dist(means10)
mds.means10<- isoMDS(means10.mds.dist)

mds.means08<- data.frame(mds.means08)
mds.means09<- data.frame(mds.means09)
mds.means10<- data.frame(mds.means10)

mds.means08<- mds.means08[,-3]
mds.means09<- mds.means09[,-3]
mds.means10<- mds.means10[,-3]

colnames(mds.means08)<- c("Dimension 1", "Dimension 2")
colnames(mds.means09)<- c("Dimension 1", "Dimension 2")
colnames(mds.means10)<- c("Dimension 1", "Dimension 2")

mds.means08[,1]<- mds.means08[,1]*1
mds.means08[,2]<- mds.means08[,2]*-1

City<- c("Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
         "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
         "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
         "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
         "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
         "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")

mds.means08$City<- City
mds.means09$City<- City
mds.means10$City<- City

year08<- rep(2008, 26)
year09<- rep(2009, 26)
year10<- rep(2010, 26)

mds.means08$Year<- year08
mds.means09$Year<- year09
mds.means10$Year<- year10

Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
mds.means08$Region<- Region
mds.means09$Region<- Region
mds.means10$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
mds.means08$Urbanicity<- Urbanicity
mds.means09$Urbanicity<- Urbanicity
mds.means10$Urbanicity<- Urbanicity

mds.means08<- mds.means08[,c(4,3,5,6,1,2)]
mds.means09<- mds.means09[,c(4,3,5,6,1,2)]
mds.means10<- mds.means10[,c(4,3,5,6,1,2)]



mds.means<- rbind(mds.means08,mds.means09,mds.means10)

plot(gvisMotionChart(mds.means, idvar='City',timevar='Year'))


###### calculate the standard deviations for each city #########################################
sd.LOYALTY.08<- tapply(d.08.sub2$LOYALTY, d.08.sub2$City, sd)
sd.PASSION.08<- tapply(d.08.sub2$PASSION, d.08.sub2$City, sd)
sd.CA.08<- tapply(d.08.sub2$CA, d.08.sub2$City, sd)
sd.BASIC_SE.08<- tapply(d.08.sub2$BASIC_SE, d.08.sub2$City, sd)
sd.LEADERSH.08<- tapply(d.08.sub2$LEADERSH, d.08.sub2$City, sd)
sd.EDUCATIO.08<- tapply(d.08.sub2$EDUCATIO, d.08.sub2$City, sd)
sd.SAFETY.08<- tapply(d.08.sub2$SAFETY, d.08.sub2$City, sd)
sd.AESTHETI.08<- tapply(d.08.sub2$AESTHETI, d.08.sub2$City, sd)
sd.ECONOMY.08<- tapply(d.08.sub2$ECONOMY, d.08.sub2$City, sd)
sd.SOCIAL_O.08<- tapply(d.08.sub2$SOCIAL_O, d.08.sub2$City, sd)
sd.COMMUNIT.08<- tapply(d.08.sub2$COMMUNIT, d.08.sub2$City, sd)
sd.INVOLVEM.08<- tapply(d.08.sub2$INVOLVEM, d.08.sub2$City, sd)
sd.OPENNESS.08<- tapply(d.08.sub2$OPENNESS, d.08.sub2$City, sd)
sd.SOCIAL_C.08<- tapply(d.08.sub2$SOCIAL_C, d.08.sub2$City, sd)
sd.DOMAINS.08<- tapply(d.08.sub2$DOMAINS, d.08.sub2$City, sd)

d.08.sd<- data.frame(sd.LOYALTY.08,sd.PASSION.08,sd.CA.08,sd.BASIC_SE.08,
                        sd.LEADERSH.08, sd.EDUCATIO.08,sd.SAFETY.08,
                        sd.AESTHETI.08,sd.ECONOMY.08,sd.SOCIAL_O.08,
                        sd.COMMUNIT.08,sd.INVOLVEM.08,sd.OPENNESS.08,
                        sd.SOCIAL_C.08,sd.DOMAINS.08)

sd.LOYALTY.09<- tapply(d.09.sub2$LOYALTY, d.09.sub2$City, sd)
sd.PASSION.09<- tapply(d.09.sub2$PASSION, d.09.sub2$City, sd)
sd.CA.09<- tapply(d.09.sub2$CA, d.09.sub2$City, sd)
sd.BASIC_SE.09<- tapply(d.09.sub2$BASIC_SE, d.09.sub2$City, sd)
sd.LEADERSH.09<- tapply(d.09.sub2$LEADERSH, d.09.sub2$City, sd)
sd.EDUCATIO.09<- tapply(d.09.sub2$EDUCATIO, d.09.sub2$City, sd)
sd.SAFETY.09<- tapply(d.09.sub2$SAFETY, d.09.sub2$City, sd)
sd.AESTHETI.09<- tapply(d.09.sub2$AESTHETI, d.09.sub2$City, sd)
sd.ECONOMY.09<- tapply(d.09.sub2$ECONOMY, d.09.sub2$City, sd)
sd.SOCIAL_O.09<- tapply(d.09.sub2$SOCIAL_O, d.09.sub2$City, sd)
sd.COMMUNIT.09<- tapply(d.09.sub2$COMMUNIT, d.09.sub2$City, sd)
sd.INVOLVEM.09<- tapply(d.09.sub2$INVOLVEM, d.09.sub2$City, sd)
sd.OPENNESS.09<- tapply(d.09.sub2$OPENNESS, d.09.sub2$City, sd)
sd.SOCIAL_C.09<- tapply(d.09.sub2$SOCIAL_C, d.09.sub2$City, sd)
sd.DOMAINS.09<- tapply(d.09.sub2$DOMAINS, d.09.sub2$City, sd)

d.09.sd<- data.frame(sd.LOYALTY.09,sd.PASSION.09,sd.CA.09,sd.BASIC_SE.09,
                        sd.LEADERSH.09, sd.EDUCATIO.09,sd.SAFETY.09,
                        sd.AESTHETI.09,sd.ECONOMY.09,sd.SOCIAL_O.09,
                        sd.COMMUNIT.09,sd.INVOLVEM.09,sd.OPENNESS.09,
                        sd.SOCIAL_C.09,sd.DOMAINS.09)

sd.LOYALTY.10<- tapply(d.10.sub2$LOYALTY, d.10.sub2$City, sd)
sd.PASSION.10<- tapply(d.10.sub2$PASSION, d.10.sub2$City, sd)
sd.CA.10<- tapply(d.10.sub2$CA, d.10.sub2$City, sd)
sd.BASIC_SE.10<- tapply(d.10.sub2$BASIC_SE, d.10.sub2$City, sd)
sd.LEADERSH.10<- tapply(d.10.sub2$LEADERSH, d.10.sub2$City, sd)
sd.EDUCATIO.10<- tapply(d.10.sub2$EDUCATIO, d.10.sub2$City, sd)
sd.SAFETY.10<- tapply(d.10.sub2$SAFETY, d.10.sub2$City, sd)
sd.AESTHETI.10<- tapply(d.10.sub2$AESTHETI, d.10.sub2$City, sd)
sd.ECONOMY.10<- tapply(d.10.sub2$ECONOMY, d.10.sub2$City, sd)
sd.SOCIAL_O.10<- tapply(d.10.sub2$SOCIAL_O, d.10.sub2$City, sd)
sd.COMMUNIT.10<- tapply(d.10.sub2$COMMUNIT, d.10.sub2$City, sd)
sd.INVOLVEM.10<- tapply(d.10.sub2$INVOLVEM, d.10.sub2$City, sd)
sd.OPENNESS.10<- tapply(d.10.sub2$OPENNESS, d.10.sub2$City, sd)
sd.SOCIAL_C.10<- tapply(d.10.sub2$SOCIAL_C, d.10.sub2$City, sd)
sd.DOMAINS.10<- tapply(d.10.sub2$DOMAINS, d.10.sub2$City, sd)

d.10.sd<- data.frame(sd.LOYALTY.10,sd.PASSION.10,sd.CA.10,sd.BASIC_SE.10,
                        sd.LEADERSH.10, sd.EDUCATIO.10,sd.SAFETY.10,
                        sd.AESTHETI.10,sd.ECONOMY.10,sd.SOCIAL_O.10,
                        sd.COMMUNIT.10,sd.INVOLVEM.10,sd.OPENNESS.10,
                        sd.SOCIAL_C.10,sd.DOMAINS.10)

#####################################################################################################

#####################################################################################################

######### append the years in a column to each data set ######
year08<- rep(2008,26)
year09<- rep(2009,26)
year10<- rep(2010,26)

d.08.sd$Year<- year08
d.09.sd$Year<- year09
d.10.sd$Year<- year10

################################################################

### change first column from row names into a column in the data frame ###
d.08.sd$City = rownames(d.08.sd)
rownames(d.08.sd) = NULL
d.09.sd$City = rownames(d.09.sd)
rownames(d.09.sd) = NULL
d.10.sd$City = rownames(d.10.sd)
rownames(d.10.sd) = NULL

#########################################################################

######## add a column for region and urbanicity and rearrange the columns###############
Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
d.08.sd$Region<- Region
d.09.sd$Region<- Region
d.10.sd$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
d.08.sd$Urbanicity<- Urbanicity
d.09.sd$Urbanicity<- Urbanicity
d.10.sd$Urbanicity<- Urbanicity

### rearrange the columns ##############################################
d.08.sd<- d.08.sd[c(16:19,1:15)]
d.09.sd<- d.09.sd[c(16:19,1:15)]
d.10.sd<- d.10.sd[c(16:19,1:15)]

################################################################

###### create matching column names for each year's data set #####
colnames(d.08.sd)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS")
colnames(d.09.sd)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS")
colnames(d.10.sd)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS")

#######################################################################

########  merge the three years into one data frame ###########
sds<- rbind(d.08.sd,d.09.sd,d.10.sd)

###############################################################

############# PCA on the standard deviations ####################################
pca08<- PCA(d.08.sd[,-c(1:4)],graph=FALSE)
col.coord.08<- pca08$var$coord   #column coordinates
row.coord.08<- pca08$ind$coord   #row coordinates

pca09<- PCA(d.09.sd[,-c(1:4)],graph=FALSE)
col.coord.09<- pca09$var$coord   #column coordinates
row.coord.09<- pca09$ind$coord   #row coordinates

pca10<- PCA(d.10.sd[,-c(1:4)],graph=FALSE)
col.coord.10<- pca10$var$coord   #column coordinates
row.coord.10<- pca10$ind$coord   #row coordinates

########################################################################

############## create data frame for the PCA dimensions ################
Year.08<- rep(2008,41)
Category<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY","AESTHETI",
             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C","DOMAINS",
             "Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
             "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
             "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
             "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
             "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
             "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")
Category<- data.frame(Category)
Variables<- c("Loyalty","Passion","Attachment","Services","Leadership","Education","Safety",
              "Aesthetics","Economy","Social Offerings","Community","Involvement","Openness",
              "Social Capital","Domains",rep(' ',26))
Variables<- data.frame(Variables)
Type<- c(rep('Index',15),rep('City',26))
Type<- data.frame(Type)
Size<- c(rep(0.8,15), rep(0.1,26))
Size<- data.frame(Size)
Region<- c(rep('',15),"West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
Region<- data.frame(Region)
Urbanicity<- c(rep('',15),"Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop" )
Urbanicity<- data.frame(Urbanicity)


Dim1.08<- rbind(col.coord.08,row.coord.08)
Dim1.08<- data.frame(Dim1.08)

Dim1.08<- cbind(Year.08,Category,Dim1.08,Variables,Type,Size,Region,Urbanicity)
Dim1.08$Year<- Year.08
rownames(Dim1.08)<- NULL
Dim1.08<- Dim1.08[,-1]
Dim1.08<- Dim1.08[,c(12,1:11)]

Dim1.09<- rbind(col.coord.09,row.coord.09)
Dim1.09<- data.frame(Dim1.09)

Year.09<- rep(2009,41)

Dim1.09<- cbind(Year.09,Category,Dim1.09,Variables,Type,Size,Region,Urbanicity)
Dim1.09$Year<- Year.09
rownames(Dim1.09)<- NULL
Dim1.09<- Dim1.09[,-1]
Dim1.09<- Dim1.09[,c(12,1:11)]

Dim1.10<- rbind(col.coord.10,row.coord.10)
Dim1.10<- data.frame(Dim1.10)

Year.10<- rep(2010,41)


Dim1.10<- cbind(Year.10,Category,Dim1.10,Variables,Type,Size,Region,Urbanicity)
Dim1.10$Year<- Year.10
rownames(Dim1.10)<- NULL
Dim1.10<- Dim1.10[,-1]
Dim1.10<- Dim1.10[,c(12,1:11)]

pca.sd<- rbind(Dim1.08,Dim1.09,Dim1.10)

##############################################################################################

############# plot motion chart of PCA means ###################################
plot(gvisMotionChart(pca.sd,idvar='Category',timevar='Year'))

#############################################################################

########## MDS on the standard deviations ###########################
sd08<- sds[1:26,-(1:4)]
sd09<- sds[27:52,-(1:4)]
sd10<- sds[53:78,-(1:4)]

sd08.mds.dist=dist(sd08)
mds.sd08<- isoMDS(sd08.mds.dist)

sd09.mds.dist=dist(sd09)
mds.sd09<- isoMDS(sd09.mds.dist)

sd10.mds.dist=dist(sd10)
mds.sd10<- isoMDS(sd10.mds.dist)

mds.sd08<- data.frame(mds.sd08)
mds.sd09<- data.frame(mds.sd09)
mds.sd10<- data.frame(mds.sd10)

mds.sd08<- mds.sd08[,-3]
mds.sd09<- mds.sd09[,-3]
mds.sd10<- mds.sd10[,-3]

colnames(mds.sd08)<- c("Dimension 1", "Dimension 2")
colnames(mds.sd09)<- c("Dimension 1", "Dimension 2")
colnames(mds.sd10)<- c("Dimension 1", "Dimension 2")

mds.sd10[,1]<- mds.sd10[,1]*-1
mds.sd10[,2]<- mds.sd10[,2]*-1

City<- c("Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
         "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
         "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
         "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
         "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
         "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")

mds.sd08$City<- City
mds.sd09$City<- City
mds.sd10$City<- City

year08<- rep(2008, 26)
year09<- rep(2009, 26)
year10<- rep(2010, 26)

mds.sd08$Year<- year08
mds.sd09$Year<- year09
mds.sd10$Year<- year10

Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
mds.sd08$Region<- Region
mds.sd09$Region<- Region
mds.sd10$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
mds.sd08$Urbanicity<- Urbanicity
mds.sd09$Urbanicity<- Urbanicity
mds.sd10$Urbanicity<- Urbanicity

mds.sd08<- mds.sd08[,c(4,3,5,6,1,2)]
mds.sd09<- mds.sd09[,c(4,3,5,6,1,2)]
mds.sd10<- mds.sd10[,c(4,3,5,6,1,2)]

mds.sds<- rbind(mds.sd08,mds.sd09,mds.sd10)

plot(gvisMotionChart(mds.sds, idvar='City',timevar='Year'))

### calculate the proportions for each city by index variable ###
aberdeen<- d.08.sub2[d.08.sub2$City=='Aberdeen, SD',]
aberdeen.loyalty<- subset(aberdeen,LOYALTY==5)
al.p<- nrow(aberdeen.loyalty)/nrow(aberdeen)
aberdeen.passion<- subset(aberdeen, PASSION==5)
ap.p<- nrow(aberdeen.passion)/nrow(aberdeen)
aberdeen.ca<- subset(aberdeen, CA==5)
aca.p<- nrow(aberdeen.ca)/nrow(aberdeen)
aberdeen.basic_se<- subset(aberdeen, BASIC_SE==3)
abs.p<- nrow(aberdeen.basic_se)/nrow(aberdeen)
aberdeen.leadersh<- subset(aberdeen, LEADERSH==3)
ald.p<- nrow(aberdeen.leadersh)/nrow(aberdeen)
aberdeen.educatio<- subset(aberdeen, EDUCATIO==3)
ae.p<- nrow(aberdeen.educatio)/nrow(aberdeen)
aberdeen.safety<- subset(aberdeen, SAFETY==3)
as.p<- nrow(aberdeen.safety)/nrow(aberdeen)
aberdeen.aestheti<- subset(aberdeen, AESTHETI==3)
aa.p<- nrow(aberdeen.aestheti)/nrow(aberdeen)
aberdeen.economy<- subset(aberdeen, ECONOMY==3)
aec.p<- nrow(aberdeen.economy)/nrow(aberdeen)
aberdeen.social_o<- subset(aberdeen, SOCIAL_O==3)
aso.p<- nrow(aberdeen.social_o)/nrow(aberdeen)
aberdeen.communit<- subset(aberdeen, COMMUNIT==3)
aco.p<- nrow(aberdeen.communit)/nrow(aberdeen)
aberdeen.involvem<- subset(aberdeen, INVOLVEM==3)
ai.p<- nrow(aberdeen.involvem)/nrow(aberdeen)
aberdeen.openness<- subset(aberdeen, OPENNESS==3)
ao.p<- nrow(aberdeen.openness)/nrow(aberdeen)
aberdeen.social_c<- subset(aberdeen, SOCIAL_C==3)
asc.p<- nrow(aberdeen.social_c)/nrow(aberdeen)

aberdeen08.p<- rbind(c(al.p,ap.p,aca.p,abs.p,ald.p,ae.p,as.p,aa.p,aec.p,aso.p,aco.p,ai.p,ao.p,asc.p))
colnames(aberdeen08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

akron<- d.08.sub2[d.08.sub2$City=='Akron, OH',]
akron.loyalty<- subset(akron,LOYALTY==5)
akronl.p<- nrow(akron.loyalty)/nrow(akron)
akron.passion<- subset(akron, PASSION==5)
akronp.p<- nrow(akron.passion)/nrow(akron)
akron.ca<- subset(akron, CA==5)
akronca.p<- nrow(akron.ca)/nrow(akron)
akron.basic_se<- subset(akron, BASIC_SE==3)
akronbs.p<- nrow(akron.basic_se)/nrow(akron)
akron.leadersh<- subset(akron, LEADERSH==3)
akronld.p<- nrow(akron.leadersh)/nrow(akron)
akron.educatio<- subset(akron, EDUCATIO==3)
akrone.p<- nrow(akron.educatio)/nrow(akron)
akron.safety<- subset(akron, SAFETY==3)
akrons.p<- nrow(akron.safety)/nrow(akron)
akron.aestheti<- subset(akron, AESTHETI==3)
akrona.p<- nrow(akron.aestheti)/nrow(akron)
akron.economy<- subset(akron, ECONOMY==3)
akronec.p<- nrow(akron.economy)/nrow(akron)
akron.social_o<- subset(akron, SOCIAL_O==3)
akronso.p<- nrow(akron.social_o)/nrow(akron)
akron.communit<- subset(akron, COMMUNIT==3)
akronco.p<- nrow(akron.communit)/nrow(akron)
akron.involvem<- subset(akron, INVOLVEM==3)
akroni.p<- nrow(akron.involvem)/nrow(akron)
akron.openness<- subset(akron, OPENNESS==3)
akrono.p<- nrow(akron.openness)/nrow(akron)
akron.social_c<- subset(akron, SOCIAL_C==3)
akronsc.p<- nrow(akron.social_c)/nrow(akron)

akron08.p<- rbind(c(akronl.p,akronp.p,akronca.p,akronbs.p,akronld.p,akrone.p,
                  akrons.p,akrona.p,akronec.p,akronso.p,akronco.p,akroni.p,akrono.p,akronsc.p))
colnames(akron08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

biloxi<- d.08.sub2[d.08.sub2$City=='Biloxi, MS',]
biloxi.loyalty<- subset(biloxi,LOYALTY==5)
biloxil.p<- nrow(biloxi.loyalty)/nrow(biloxi)
biloxi.passion<- subset(biloxi, PASSION==5)
biloxip.p<- nrow(biloxi.passion)/nrow(biloxi)
biloxi.ca<- subset(biloxi, CA==5)
biloxica.p<- nrow(biloxi.ca)/nrow(biloxi)
biloxi.basic_se<- subset(biloxi, BASIC_SE==3)
biloxibs.p<- nrow(biloxi.basic_se)/nrow(biloxi)
biloxi.leadersh<- subset(biloxi, LEADERSH==3)
biloxild.p<- nrow(biloxi.leadersh)/nrow(biloxi)
biloxi.educatio<- subset(biloxi, EDUCATIO==3)
biloxie.p<- nrow(biloxi.educatio)/nrow(biloxi)
biloxi.safety<- subset(biloxi, SAFETY==3)
biloxis.p<- nrow(biloxi.safety)/nrow(biloxi)
biloxi.aestheti<- subset(biloxi, AESTHETI==3)
biloxia.p<- nrow(biloxi.aestheti)/nrow(biloxi)
biloxi.economy<- subset(biloxi, ECONOMY==3)
biloxiec.p<- nrow(biloxi.economy)/nrow(biloxi)
biloxi.social_o<- subset(biloxi, SOCIAL_O==3)
biloxiso.p<- nrow(biloxi.social_o)/nrow(biloxi)
biloxi.communit<- subset(biloxi, COMMUNIT==3)
biloxico.p<- nrow(biloxi.communit)/nrow(biloxi)
biloxi.involvem<- subset(biloxi, INVOLVEM==3)
biloxii.p<- nrow(biloxi.involvem)/nrow(biloxi)
biloxi.openness<- subset(biloxi, OPENNESS==3)
biloxio.p<- nrow(biloxi.openness)/nrow(biloxi)
biloxi.social_c<- subset(biloxi, SOCIAL_C==3)
biloxisc.p<- nrow(biloxi.social_c)/nrow(biloxi)

biloxi08.p<- rbind(c(biloxil.p,biloxip.p,biloxica.p,biloxibs.p,biloxild.p,biloxie.p,
                     biloxis.p,biloxia.p,biloxiec.p,biloxiso.p,biloxico.p,biloxii.p,biloxio.p,biloxisc.p))
colnames(biloxi08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

boulder<- d.08.sub2[d.08.sub2$City=='Boulder, CO',]
boulder.loyalty<- subset(boulder,LOYALTY==5)
boulderl.p<- nrow(boulder.loyalty)/nrow(boulder)
boulder.passion<- subset(boulder, PASSION==5)
boulderp.p<- nrow(boulder.passion)/nrow(boulder)
boulder.ca<- subset(boulder, CA==5)
boulderca.p<- nrow(boulder.ca)/nrow(boulder)
boulder.basic_se<- subset(boulder, BASIC_SE==3)
boulderbs.p<- nrow(boulder.basic_se)/nrow(boulder)
boulder.leadersh<- subset(boulder, LEADERSH==3)
boulderld.p<- nrow(boulder.leadersh)/nrow(boulder)
boulder.educatio<- subset(boulder, EDUCATIO==3)
bouldere.p<- nrow(boulder.educatio)/nrow(boulder)
boulder.safety<- subset(boulder, SAFETY==3)
boulders.p<- nrow(boulder.safety)/nrow(boulder)
boulder.aestheti<- subset(boulder, AESTHETI==3)
bouldera.p<- nrow(boulder.aestheti)/nrow(boulder)
boulder.economy<- subset(boulder, ECONOMY==3)
boulderec.p<- nrow(boulder.economy)/nrow(boulder)
boulder.social_o<- subset(boulder, SOCIAL_O==3)
boulderso.p<- nrow(boulder.social_o)/nrow(boulder)
boulder.communit<- subset(boulder, COMMUNIT==3)
boulderco.p<- nrow(boulder.communit)/nrow(boulder)
boulder.involvem<- subset(boulder, INVOLVEM==3)
boulderi.p<- nrow(boulder.involvem)/nrow(boulder)
boulder.openness<- subset(boulder, OPENNESS==3)
bouldero.p<- nrow(boulder.openness)/nrow(boulder)
boulder.social_c<- subset(boulder, SOCIAL_C==3)
bouldersc.p<- nrow(boulder.social_c)/nrow(boulder)

boulder08.p<- rbind(c(boulderl.p,boulderp.p,boulderca.p,boulderbs.p,boulderld.p,bouldere.p,
                      boulders.p,bouldera.p,boulderec.p,boulderso.p,boulderco.p,boulderi.p,bouldero.p,bouldersc.p))
colnames(boulder08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

bradenton<- d.08.sub2[d.08.sub2$City=='Bradenton, FL',]
bradenton.loyalty<- subset(bradenton,LOYALTY==5)
bradentonl.p<- nrow(bradenton.loyalty)/nrow(bradenton)
bradenton.passion<- subset(bradenton, PASSION==5)
bradentonp.p<- nrow(bradenton.passion)/nrow(bradenton)
bradenton.ca<- subset(bradenton, CA==5)
bradentonca.p<- nrow(bradenton.ca)/nrow(bradenton)
bradenton.basic_se<- subset(bradenton, BASIC_SE==3)
bradentonbs.p<- nrow(bradenton.basic_se)/nrow(bradenton)
bradenton.leadersh<- subset(bradenton, LEADERSH==3)
bradentonld.p<- nrow(bradenton.leadersh)/nrow(bradenton)
bradenton.educatio<- subset(bradenton, EDUCATIO==3)
bradentone.p<- nrow(bradenton.educatio)/nrow(bradenton)
bradenton.safety<- subset(bradenton, SAFETY==3)
bradentons.p<- nrow(bradenton.safety)/nrow(bradenton)
bradenton.aestheti<- subset(bradenton, AESTHETI==3)
bradentona.p<- nrow(bradenton.aestheti)/nrow(bradenton)
bradenton.economy<- subset(bradenton, ECONOMY==3)
bradentonec.p<- nrow(bradenton.economy)/nrow(bradenton)
bradenton.social_o<- subset(bradenton, SOCIAL_O==3)
bradentonso.p<- nrow(bradenton.social_o)/nrow(bradenton)
bradenton.communit<- subset(bradenton, COMMUNIT==3)
bradentonco.p<- nrow(bradenton.communit)/nrow(bradenton)
bradenton.involvem<- subset(bradenton, INVOLVEM==3)
bradentoni.p<- nrow(bradenton.involvem)/nrow(bradenton)
bradenton.openness<- subset(bradenton, OPENNESS==3)
bradentono.p<- nrow(bradenton.openness)/nrow(bradenton)
bradenton.social_c<- subset(bradenton, SOCIAL_C==3)
bradentonsc.p<- nrow(bradenton.social_c)/nrow(bradenton)

bradenton08.p<- rbind(c(bradentonl.p,bradentonp.p,bradentonca.p,bradentonbs.p,bradentonld.p,bradentone.p,
                        bradentons.p,bradentona.p,bradentonec.p,bradentonso.p,bradentonco.p,
                        bradentoni.p,bradentono.p,bradentonsc.p))
colnames(bradenton08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

charlotte<- d.08.sub2[d.08.sub2$City=='Charlotte, NC',]
charlotte.loyalty<- subset(charlotte,LOYALTY==5)
charlottel.p<- nrow(charlotte.loyalty)/nrow(charlotte)
charlotte.passion<- subset(charlotte, PASSION==5)
charlottep.p<- nrow(charlotte.passion)/nrow(charlotte)
charlotte.ca<- subset(charlotte, CA==5)
charlotteca.p<- nrow(charlotte.ca)/nrow(charlotte)
charlotte.basic_se<- subset(charlotte, BASIC_SE==3)
charlottebs.p<- nrow(charlotte.basic_se)/nrow(charlotte)
charlotte.leadersh<- subset(charlotte, LEADERSH==3)
charlotteld.p<- nrow(charlotte.leadersh)/nrow(charlotte)
charlotte.educatio<- subset(charlotte, EDUCATIO==3)
charlottee.p<- nrow(charlotte.educatio)/nrow(charlotte)
charlotte.safety<- subset(charlotte, SAFETY==3)
charlottes.p<- nrow(charlotte.safety)/nrow(charlotte)
charlotte.aestheti<- subset(charlotte, AESTHETI==3)
charlottea.p<- nrow(charlotte.aestheti)/nrow(charlotte)
charlotte.economy<- subset(charlotte, ECONOMY==3)
charlotteec.p<- nrow(charlotte.economy)/nrow(charlotte)
charlotte.social_o<- subset(charlotte, SOCIAL_O==3)
charlotteso.p<- nrow(charlotte.social_o)/nrow(charlotte)
charlotte.communit<- subset(charlotte, COMMUNIT==3)
charlotteco.p<- nrow(charlotte.communit)/nrow(charlotte)
charlotte.involvem<- subset(charlotte, INVOLVEM==3)
charlottei.p<- nrow(charlotte.involvem)/nrow(charlotte)
charlotte.openness<- subset(charlotte, OPENNESS==3)
charlotteo.p<- nrow(charlotte.openness)/nrow(charlotte)
charlotte.social_c<- subset(charlotte, SOCIAL_C==3)
charlottesc.p<- nrow(charlotte.social_c)/nrow(charlotte)

charlotte08.p<- rbind(c(charlottel.p,charlottep.p,charlotteca.p,charlottebs.p,charlotteld.p,charlottee.p,
                        charlottes.p,charlottea.p,charlotteec.p,charlotteso.p,charlotteco.p,
                        charlottei.p,charlotteo.p,charlottesc.p))
colnames(charlotte08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

columbia<- d.08.sub2[d.08.sub2$City=='Columbia, SC',]
columbia.loyalty<- subset(columbia,LOYALTY==5)
columbial.p<- nrow(columbia.loyalty)/nrow(columbia)
columbia.passion<- subset(columbia, PASSION==5)
columbiap.p<- nrow(columbia.passion)/nrow(columbia)
columbia.ca<- subset(columbia, CA==5)
columbiaca.p<- nrow(columbia.ca)/nrow(columbia)
columbia.basic_se<- subset(columbia, BASIC_SE==3)
columbiabs.p<- nrow(columbia.basic_se)/nrow(columbia)
columbia.leadersh<- subset(columbia, LEADERSH==3)
columbiald.p<- nrow(columbia.leadersh)/nrow(columbia)
columbia.educatio<- subset(columbia, EDUCATIO==3)
columbiae.p<- nrow(columbia.educatio)/nrow(columbia)
columbia.safety<- subset(columbia, SAFETY==3)
columbias.p<- nrow(columbia.safety)/nrow(columbia)
columbia.aestheti<- subset(columbia, AESTHETI==3)
columbiaa.p<- nrow(columbia.aestheti)/nrow(columbia)
columbia.economy<- subset(columbia, ECONOMY==3)
columbiaec.p<- nrow(columbia.economy)/nrow(columbia)
columbia.social_o<- subset(columbia, SOCIAL_O==3)
columbiaso.p<- nrow(columbia.social_o)/nrow(columbia)
columbia.communit<- subset(columbia, COMMUNIT==3)
columbiaco.p<- nrow(columbia.communit)/nrow(columbia)
columbia.involvem<- subset(columbia, INVOLVEM==3)
columbiai.p<- nrow(columbia.involvem)/nrow(columbia)
columbia.openness<- subset(columbia, OPENNESS==3)
columbiao.p<- nrow(columbia.openness)/nrow(columbia)
columbia.social_c<- subset(columbia, SOCIAL_C==3)
columbiasc.p<- nrow(columbia.social_c)/nrow(columbia)

columbia08.p<- rbind(c(columbial.p,columbiap.p,columbiaca.p,columbiabs.p,columbiald.p,columbiae.p,
                       columbias.p,columbiaa.p,columbiaec.p,columbiaso.p,columbiaco.p,
                       columbiai.p,columbiao.p,columbiasc.p))
colnames(columbia08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

columbus<- d.08.sub2[d.08.sub2$City=='Columbus, GA',]
columbus.loyalty<- subset(columbus,LOYALTY==5)
columbusl.p<- nrow(columbus.loyalty)/nrow(columbus)
columbus.passion<- subset(columbus, PASSION==5)
columbusp.p<- nrow(columbus.passion)/nrow(columbus)
columbus.ca<- subset(columbus, CA==5)
columbusca.p<- nrow(columbus.ca)/nrow(columbus)
columbus.basic_se<- subset(columbus, BASIC_SE==3)
columbusbs.p<- nrow(columbus.basic_se)/nrow(columbus)
columbus.leadersh<- subset(columbus, LEADERSH==3)
columbusld.p<- nrow(columbus.leadersh)/nrow(columbus)
columbus.educatio<- subset(columbus, EDUCATIO==3)
columbuse.p<- nrow(columbus.educatio)/nrow(columbus)
columbus.safety<- subset(columbus, SAFETY==3)
columbuss.p<- nrow(columbus.safety)/nrow(columbus)
columbus.aestheti<- subset(columbus, AESTHETI==3)
columbusa.p<- nrow(columbus.aestheti)/nrow(columbus)
columbus.economy<- subset(columbus, ECONOMY==3)
columbusec.p<- nrow(columbus.economy)/nrow(columbus)
columbus.social_o<- subset(columbus, SOCIAL_O==3)
columbusso.p<- nrow(columbus.social_o)/nrow(columbus)
columbus.communit<- subset(columbus, COMMUNIT==3)
columbusco.p<- nrow(columbus.communit)/nrow(columbus)
columbus.involvem<- subset(columbus, INVOLVEM==3)
columbusi.p<- nrow(columbus.involvem)/nrow(columbus)
columbus.openness<- subset(columbus, OPENNESS==3)
columbuso.p<- nrow(columbus.openness)/nrow(columbus)
columbus.social_c<- subset(columbus, SOCIAL_C==3)
columbussc.p<- nrow(columbus.social_c)/nrow(columbus)

columbus08.p<- rbind(c(columbusl.p,columbusp.p,columbusca.p,columbusbs.p,columbusld.p,columbuse.p,
                       columbuss.p,columbusa.p,columbusec.p,columbusso.p,columbusco.p,
                       columbusi.p,columbuso.p,columbussc.p))
colnames(columbus08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


detroit<- d.08.sub2[d.08.sub2$City=='Detroit, MI',]
detroit.loyalty<- subset(detroit,LOYALTY==5)
detroitl.p<- nrow(detroit.loyalty)/nrow(detroit)
detroit.passion<- subset(detroit, PASSION==5)
detroitp.p<- nrow(detroit.passion)/nrow(detroit)
detroit.ca<- subset(detroit, CA==5)
detroitca.p<- nrow(detroit.ca)/nrow(detroit)
detroit.basic_se<- subset(detroit, BASIC_SE==3)
detroitbs.p<- nrow(detroit.basic_se)/nrow(detroit)
detroit.leadersh<- subset(detroit, LEADERSH==3)
detroitld.p<- nrow(detroit.leadersh)/nrow(detroit)
detroit.educatio<- subset(detroit, EDUCATIO==3)
detroite.p<- nrow(detroit.educatio)/nrow(detroit)
detroit.safety<- subset(detroit, SAFETY==3)
detroits.p<- nrow(detroit.safety)/nrow(detroit)
detroit.aestheti<- subset(detroit, AESTHETI==3)
detroita.p<- nrow(detroit.aestheti)/nrow(detroit)
detroit.economy<- subset(detroit, ECONOMY==3)
detroitec.p<- nrow(detroit.economy)/nrow(detroit)
detroit.social_o<- subset(detroit, SOCIAL_O==3)
detroitso.p<- nrow(detroit.social_o)/nrow(detroit)
detroit.communit<- subset(detroit, COMMUNIT==3)
detroitco.p<- nrow(detroit.communit)/nrow(detroit)
detroit.involvem<- subset(detroit, INVOLVEM==3)
detroiti.p<- nrow(detroit.involvem)/nrow(detroit)
detroit.openness<- subset(detroit, OPENNESS==3)
detroito.p<- nrow(detroit.openness)/nrow(detroit)
detroit.social_c<- subset(detroit, SOCIAL_C==3)
detroitsc.p<- nrow(detroit.social_c)/nrow(detroit)

detroit08.p<- rbind(c(detroitl.p,detroitp.p,detroitca.p,detroitbs.p,detroitld.p,detroite.p,
                      detroits.p,detroita.p,detroitec.p,detroitso.p,detroitco.p,
                      detroiti.p,detroito.p,detroitsc.p))
colnames(detroit08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

duluth<- d.08.sub2[d.08.sub2$City=='Duluth, MN',]
duluth.loyalty<- subset(duluth,LOYALTY==5)
duluthl.p<- nrow(duluth.loyalty)/nrow(duluth)
duluth.passion<- subset(duluth, PASSION==5)
duluthp.p<- nrow(duluth.passion)/nrow(duluth)
duluth.ca<- subset(duluth, CA==5)
duluthca.p<- nrow(duluth.ca)/nrow(duluth)
duluth.basic_se<- subset(duluth, BASIC_SE==3)
duluthbs.p<- nrow(duluth.basic_se)/nrow(duluth)
duluth.leadersh<- subset(duluth, LEADERSH==3)
duluthld.p<- nrow(duluth.leadersh)/nrow(duluth)
duluth.educatio<- subset(duluth, EDUCATIO==3)
duluthe.p<- nrow(duluth.educatio)/nrow(duluth)
duluth.safety<- subset(duluth, SAFETY==3)
duluths.p<- nrow(duluth.safety)/nrow(duluth)
duluth.aestheti<- subset(duluth, AESTHETI==3)
dulutha.p<- nrow(duluth.aestheti)/nrow(duluth)
duluth.economy<- subset(duluth, ECONOMY==3)
duluthec.p<- nrow(duluth.economy)/nrow(duluth)
duluth.social_o<- subset(duluth, SOCIAL_O==3)
duluthso.p<- nrow(duluth.social_o)/nrow(duluth)
duluth.communit<- subset(duluth, COMMUNIT==3)
duluthco.p<- nrow(duluth.communit)/nrow(duluth)
duluth.involvem<- subset(duluth, INVOLVEM==3)
duluthi.p<- nrow(duluth.involvem)/nrow(duluth)
duluth.openness<- subset(duluth, OPENNESS==3)
dulutho.p<- nrow(duluth.openness)/nrow(duluth)
duluth.social_c<- subset(duluth, SOCIAL_C==3)
duluthsc.p<- nrow(duluth.social_c)/nrow(duluth)


duluth08.p<- rbind(c(duluthl.p,duluthp.p,duluthca.p,duluthbs.p,duluthld.p,duluthe.p,
                     duluths.p,dulutha.p,duluthec.p,duluthso.p,duluthco.p,
                     duluthi.p,dulutho.p,duluthsc.p))
colnames(duluth08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

fort.wayne<- d.08.sub2[d.08.sub2$City=='Fort Wayne, IN',]
fort.wayne.loyalty<- subset(fort.wayne,LOYALTY==5)
fort.waynel.p<- nrow(fort.wayne.loyalty)/nrow(fort.wayne)
fort.wayne.passion<- subset(fort.wayne, PASSION==5)
fort.waynep.p<- nrow(fort.wayne.passion)/nrow(fort.wayne)
fort.wayne.ca<- subset(fort.wayne, CA==5)
fort.wayneca.p<- nrow(fort.wayne.ca)/nrow(fort.wayne)
fort.wayne.basic_se<- subset(fort.wayne, BASIC_SE==3)
fort.waynebs.p<- nrow(fort.wayne.basic_se)/nrow(fort.wayne)
fort.wayne.leadersh<- subset(fort.wayne, LEADERSH==3)
fort.wayneld.p<- nrow(fort.wayne.leadersh)/nrow(fort.wayne)
fort.wayne.educatio<- subset(fort.wayne, EDUCATIO==3)
fort.waynee.p<- nrow(fort.wayne.educatio)/nrow(fort.wayne)
fort.wayne.safety<- subset(fort.wayne, SAFETY==3)
fort.waynes.p<- nrow(fort.wayne.safety)/nrow(fort.wayne)
fort.wayne.aestheti<- subset(fort.wayne, AESTHETI==3)
fort.waynea.p<- nrow(fort.wayne.aestheti)/nrow(fort.wayne)
fort.wayne.economy<- subset(fort.wayne, ECONOMY==3)
fort.wayneec.p<- nrow(fort.wayne.economy)/nrow(fort.wayne)
fort.wayne.social_o<- subset(fort.wayne, SOCIAL_O==3)
fort.wayneso.p<- nrow(fort.wayne.social_o)/nrow(fort.wayne)
fort.wayne.communit<- subset(fort.wayne, COMMUNIT==3)
fort.wayneco.p<- nrow(fort.wayne.communit)/nrow(fort.wayne)
fort.wayne.involvem<- subset(fort.wayne, INVOLVEM==3)
fort.waynei.p<- nrow(fort.wayne.involvem)/nrow(fort.wayne)
fort.wayne.openness<- subset(fort.wayne, OPENNESS==3)
fort.wayneo.p<- nrow(fort.wayne.openness)/nrow(fort.wayne)
fort.wayne.social_c<- subset(fort.wayne, SOCIAL_C==3)
fort.waynesc.p<- nrow(fort.wayne.social_c)/nrow(fort.wayne)


fort.wayne08.p<- rbind(c(fort.waynel.p,fort.waynep.p,fort.wayneca.p,fort.waynebs.p,fort.wayneld.p,fort.waynee.p,
                         fort.waynes.p,fort.waynea.p,fort.wayneec.p,fort.wayneso.p,fort.wayneco.p,
                         fort.waynei.p,fort.wayneo.p,fort.waynesc.p))
colnames(fort.wayne08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


gary<- d.08.sub2[d.08.sub2$City=='Gary, IN',]
gary.loyalty<- subset(gary,LOYALTY==5)
garyl.p<- nrow(gary.loyalty)/nrow(gary)
gary.passion<- subset(gary, PASSION==5)
garyp.p<- nrow(gary.passion)/nrow(gary)
gary.ca<- subset(gary, CA==5)
garyca.p<- nrow(gary.ca)/nrow(gary)
gary.basic_se<- subset(gary, BASIC_SE==3)
garybs.p<- nrow(gary.basic_se)/nrow(gary)
gary.leadersh<- subset(gary, LEADERSH==3)
garyld.p<- nrow(gary.leadersh)/nrow(gary)
gary.educatio<- subset(gary, EDUCATIO==3)
garye.p<- nrow(gary.educatio)/nrow(gary)
gary.safety<- subset(gary, SAFETY==3)
garys.p<- nrow(gary.safety)/nrow(gary)
gary.aestheti<- subset(gary, AESTHETI==3)
garya.p<- nrow(gary.aestheti)/nrow(gary)
gary.economy<- subset(gary, ECONOMY==3)
garyec.p<- nrow(gary.economy)/nrow(gary)
gary.social_o<- subset(gary, SOCIAL_O==3)
garyso.p<- nrow(gary.social_o)/nrow(gary)
gary.communit<- subset(gary, COMMUNIT==3)
garyco.p<- nrow(gary.communit)/nrow(gary)
gary.involvem<- subset(gary, INVOLVEM==3)
garyi.p<- nrow(gary.involvem)/nrow(gary)
gary.openness<- subset(gary, OPENNESS==3)
garyo.p<- nrow(gary.openness)/nrow(gary)
gary.social_c<- subset(gary, SOCIAL_C==3)
garysc.p<- nrow(gary.social_c)/nrow(gary)


gary08.p<- rbind(c(garyl.p,garyp.p,garyca.p,garybs.p,garyld.p,garye.p,
                   garys.p,garya.p,garyec.p,garyso.p,garyco.p,
                   garyi.p,garyo.p,garysc.p))
colnames(gary08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


grand.forks<- d.08.sub2[d.08.sub2$City=='Grand Forks, ND',]
grand.forks.loyalty<- subset(grand.forks,LOYALTY==5)
grand.forksl.p<- nrow(grand.forks.loyalty)/nrow(grand.forks)
grand.forks.passion<- subset(grand.forks, PASSION==5)
grand.forksp.p<- nrow(grand.forks.passion)/nrow(grand.forks)
grand.forks.ca<- subset(grand.forks, CA==5)
grand.forksca.p<- nrow(grand.forks.ca)/nrow(grand.forks)
grand.forks.basic_se<- subset(grand.forks, BASIC_SE==3)
grand.forksbs.p<- nrow(grand.forks.basic_se)/nrow(grand.forks)
grand.forks.leadersh<- subset(grand.forks, LEADERSH==3)
grand.forksld.p<- nrow(grand.forks.leadersh)/nrow(grand.forks)
grand.forks.educatio<- subset(grand.forks, EDUCATIO==3)
grand.forkse.p<- nrow(grand.forks.educatio)/nrow(grand.forks)
grand.forks.safety<- subset(grand.forks, SAFETY==3)
grand.forkss.p<- nrow(grand.forks.safety)/nrow(grand.forks)
grand.forks.aestheti<- subset(grand.forks, AESTHETI==3)
grand.forksa.p<- nrow(grand.forks.aestheti)/nrow(grand.forks)
grand.forks.economy<- subset(grand.forks, ECONOMY==3)
grand.forksec.p<- nrow(grand.forks.economy)/nrow(grand.forks)
grand.forks.social_o<- subset(grand.forks, SOCIAL_O==3)
grand.forksso.p<- nrow(grand.forks.social_o)/nrow(grand.forks)
grand.forks.communit<- subset(grand.forks, COMMUNIT==3)
grand.forksco.p<- nrow(grand.forks.communit)/nrow(grand.forks)
grand.forks.involvem<- subset(grand.forks, INVOLVEM==3)
grand.forksi.p<- nrow(grand.forks.involvem)/nrow(grand.forks)
grand.forks.openness<- subset(grand.forks, OPENNESS==3)
grand.forkso.p<- nrow(grand.forks.openness)/nrow(grand.forks)
grand.forks.social_c<- subset(grand.forks, SOCIAL_C==3)
grand.forkssc.p<- nrow(grand.forks.social_c)/nrow(grand.forks)


grand.forks08.p<- rbind(c(grand.forksl.p,grand.forksp.p,grand.forksca.p,grand.forksbs.p,grand.forksld.p,
                          grand.forkse.p,grand.forkss.p,grand.forksa.p,grand.forksec.p,
                          grand.forksso.p,grand.forksco.p,grand.forksi.p,grand.forkso.p,grand.forkssc.p))
colnames(grand.forks08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                       "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


lexington<- d.08.sub2[d.08.sub2$City=='Lexington, KY',]
lexington.loyalty<- subset(lexington,LOYALTY==5)
lexingtonl.p<- nrow(lexington.loyalty)/nrow(lexington)
lexington.passion<- subset(lexington, PASSION==5)
lexingtonp.p<- nrow(lexington.passion)/nrow(lexington)
lexington.ca<- subset(lexington, CA==5)
lexingtonca.p<- nrow(lexington.ca)/nrow(lexington)
lexington.basic_se<- subset(lexington, BASIC_SE==3)
lexingtonbs.p<- nrow(lexington.basic_se)/nrow(lexington)
lexington.leadersh<- subset(lexington, LEADERSH==3)
lexingtonld.p<- nrow(lexington.leadersh)/nrow(lexington)
lexington.educatio<- subset(lexington, EDUCATIO==3)
lexingtone.p<- nrow(lexington.educatio)/nrow(lexington)
lexington.safety<- subset(lexington, SAFETY==3)
lexingtons.p<- nrow(lexington.safety)/nrow(lexington)
lexington.aestheti<- subset(lexington, AESTHETI==3)
lexingtona.p<- nrow(lexington.aestheti)/nrow(lexington)
lexington.economy<- subset(lexington, ECONOMY==3)
lexingtonec.p<- nrow(lexington.economy)/nrow(lexington)
lexington.social_o<- subset(lexington, SOCIAL_O==3)
lexingtonso.p<- nrow(lexington.social_o)/nrow(lexington)
lexington.communit<- subset(lexington, COMMUNIT==3)
lexingtonco.p<- nrow(lexington.communit)/nrow(lexington)
lexington.involvem<- subset(lexington, INVOLVEM==3)
lexingtoni.p<- nrow(lexington.involvem)/nrow(lexington)
lexington.openness<- subset(lexington, OPENNESS==3)
lexingtono.p<- nrow(lexington.openness)/nrow(lexington)
lexington.social_c<- subset(lexington, SOCIAL_C==3)
lexingtonsc.p<- nrow(lexington.social_c)/nrow(lexington)


lexington08.p<- rbind(c(lexingtonl.p,lexingtonp.p,lexingtonca.p,lexingtonbs.p,lexingtonld.p,
                        lexingtone.p,lexingtons.p,lexingtona.p,lexingtonec.p,
                        lexingtonso.p,lexingtonco.p,lexingtoni.p,lexingtono.p,lexingtonsc.p))
colnames(lexington08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                              "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

long.beach<- d.08.sub2[d.08.sub2$City=='Long Beach, CA',]
long.beach.loyalty<- subset(long.beach,LOYALTY==5)
long.beachl.p<- nrow(long.beach.loyalty)/nrow(long.beach)
long.beach.passion<- subset(long.beach, PASSION==5)
long.beachp.p<- nrow(long.beach.passion)/nrow(long.beach)
long.beach.ca<- subset(long.beach, CA==5)
long.beachca.p<- nrow(long.beach.ca)/nrow(long.beach)
long.beach.basic_se<- subset(long.beach, BASIC_SE==3)
long.beachbs.p<- nrow(long.beach.basic_se)/nrow(long.beach)
long.beach.leadersh<- subset(long.beach, LEADERSH==3)
long.beachld.p<- nrow(long.beach.leadersh)/nrow(long.beach)
long.beach.educatio<- subset(long.beach, EDUCATIO==3)
long.beache.p<- nrow(long.beach.educatio)/nrow(long.beach)
long.beach.safety<- subset(long.beach, SAFETY==3)
long.beachs.p<- nrow(long.beach.safety)/nrow(long.beach)
long.beach.aestheti<- subset(long.beach, AESTHETI==3)
long.beacha.p<- nrow(long.beach.aestheti)/nrow(long.beach)
long.beach.economy<- subset(long.beach, ECONOMY==3)
long.beachec.p<- nrow(long.beach.economy)/nrow(long.beach)
long.beach.social_o<- subset(long.beach, SOCIAL_O==3)
long.beachso.p<- nrow(long.beach.social_o)/nrow(long.beach)
long.beach.communit<- subset(long.beach, COMMUNIT==3)
long.beachco.p<- nrow(long.beach.communit)/nrow(long.beach)
long.beach.involvem<- subset(long.beach, INVOLVEM==3)
long.beachi.p<- nrow(long.beach.involvem)/nrow(long.beach)
long.beach.openness<- subset(long.beach, OPENNESS==3)
long.beacho.p<- nrow(long.beach.openness)/nrow(long.beach)
long.beach.social_c<- subset(long.beach, SOCIAL_C==3)
long.beachsc.p<- nrow(long.beach.social_c)/nrow(long.beach)


long.beach08.p<- rbind(c(long.beachl.p,long.beachp.p,long.beachca.p,long.beachbs.p,long.beachld.p,
                         long.beache.p,long.beachs.p,long.beacha.p,long.beachec.p,
                         long.beachso.p,long.beachco.p,long.beachi.p,long.beacho.p,long.beachsc.p))
colnames(long.beach08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

macon<- d.08.sub2[d.08.sub2$City=='Macon, GA',]
macon.loyalty<- subset(macon,LOYALTY==5)
maconl.p<- nrow(macon.loyalty)/nrow(macon)
macon.passion<- subset(macon, PASSION==5)
maconp.p<- nrow(macon.passion)/nrow(macon)
macon.ca<- subset(macon, CA==5)
maconca.p<- nrow(macon.ca)/nrow(macon)
macon.basic_se<- subset(macon, BASIC_SE==3)
maconbs.p<- nrow(macon.basic_se)/nrow(macon)
macon.leadersh<- subset(macon, LEADERSH==3)
maconld.p<- nrow(macon.leadersh)/nrow(macon)
macon.educatio<- subset(macon, EDUCATIO==3)
macone.p<- nrow(macon.educatio)/nrow(macon)
macon.safety<- subset(macon, SAFETY==3)
macons.p<- nrow(macon.safety)/nrow(macon)
macon.aestheti<- subset(macon, AESTHETI==3)
macona.p<- nrow(macon.aestheti)/nrow(macon)
macon.economy<- subset(macon, ECONOMY==3)
maconec.p<- nrow(macon.economy)/nrow(macon)
macon.social_o<- subset(macon, SOCIAL_O==3)
maconso.p<- nrow(macon.social_o)/nrow(macon)
macon.communit<- subset(macon, COMMUNIT==3)
maconco.p<- nrow(macon.communit)/nrow(macon)
macon.involvem<- subset(macon, INVOLVEM==3)
maconi.p<- nrow(macon.involvem)/nrow(macon)
macon.openness<- subset(macon, OPENNESS==3)
macono.p<- nrow(macon.openness)/nrow(macon)
macon.social_c<- subset(macon, SOCIAL_C==3)
maconsc.p<- nrow(macon.social_c)/nrow(macon)


macon08.p<- rbind(c(maconl.p,maconp.p,maconca.p,maconbs.p,maconld.p,
                    macone.p,macons.p,macona.p,maconec.p,
                    maconso.p,maconco.p,maconi.p,macono.p,maconsc.p))
colnames(macon08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

miami<- d.08.sub2[d.08.sub2$City=='Miami, FL',]
miami.loyalty<- subset(miami,LOYALTY==5)
miamil.p<- nrow(miami.loyalty)/nrow(miami)
miami.passion<- subset(miami, PASSION==5)
miamip.p<- nrow(miami.passion)/nrow(miami)
miami.ca<- subset(miami, CA==5)
miamica.p<- nrow(miami.ca)/nrow(miami)
miami.basic_se<- subset(miami, BASIC_SE==3)
miamibs.p<- nrow(miami.basic_se)/nrow(miami)
miami.leadersh<- subset(miami, LEADERSH==3)
miamild.p<- nrow(miami.leadersh)/nrow(miami)
miami.educatio<- subset(miami, EDUCATIO==3)
miamie.p<- nrow(miami.educatio)/nrow(miami)
miami.safety<- subset(miami, SAFETY==3)
miamis.p<- nrow(miami.safety)/nrow(miami)
miami.aestheti<- subset(miami, AESTHETI==3)
miamia.p<- nrow(miami.aestheti)/nrow(miami)
miami.economy<- subset(miami, ECONOMY==3)
miamiec.p<- nrow(miami.economy)/nrow(miami)
miami.social_o<- subset(miami, SOCIAL_O==3)
miamiso.p<- nrow(miami.social_o)/nrow(miami)
miami.communit<- subset(miami, COMMUNIT==3)
miamico.p<- nrow(miami.communit)/nrow(miami)
miami.involvem<- subset(miami, INVOLVEM==3)
miamii.p<- nrow(miami.involvem)/nrow(miami)
miami.openness<- subset(miami, OPENNESS==3)
miamio.p<- nrow(miami.openness)/nrow(miami)
miami.social_c<- subset(miami, SOCIAL_C==3)
miamisc.p<- nrow(miami.social_c)/nrow(miami)


miami08.p<- rbind(c(miamil.p,miamip.p,miamica.p,miamibs.p,miamild.p,
                    miamie.p,miamis.p,miamia.p,miamiec.p,
                    miamiso.p,miamico.p,miamii.p,miamio.p,miamisc.p))
colnames(miami08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

milledgeville<- d.08.sub2[d.08.sub2$City=='Milledgeville, GA',]
milledgeville.loyalty<- subset(milledgeville,LOYALTY==5)
milledgevillel.p<- nrow(milledgeville.loyalty)/nrow(milledgeville)
milledgeville.passion<- subset(milledgeville, PASSION==5)
milledgevillep.p<- nrow(milledgeville.passion)/nrow(milledgeville)
milledgeville.ca<- subset(milledgeville, CA==5)
milledgevilleca.p<- nrow(milledgeville.ca)/nrow(milledgeville)
milledgeville.basic_se<- subset(milledgeville, BASIC_SE==3)
milledgevillebs.p<- nrow(milledgeville.basic_se)/nrow(milledgeville)
milledgeville.leadersh<- subset(milledgeville, LEADERSH==3)
milledgevilleld.p<- nrow(milledgeville.leadersh)/nrow(milledgeville)
milledgeville.educatio<- subset(milledgeville, EDUCATIO==3)
milledgevillee.p<- nrow(milledgeville.educatio)/nrow(milledgeville)
milledgeville.safety<- subset(milledgeville, SAFETY==3)
milledgevilles.p<- nrow(milledgeville.safety)/nrow(milledgeville)
milledgeville.aestheti<- subset(milledgeville, AESTHETI==3)
milledgevillea.p<- nrow(milledgeville.aestheti)/nrow(milledgeville)
milledgeville.economy<- subset(milledgeville, ECONOMY==3)
milledgevilleec.p<- nrow(milledgeville.economy)/nrow(milledgeville)
milledgeville.social_o<- subset(milledgeville, SOCIAL_O==3)
milledgevilleso.p<- nrow(milledgeville.social_o)/nrow(milledgeville)
milledgeville.communit<- subset(milledgeville, COMMUNIT==3)
milledgevilleco.p<- nrow(milledgeville.communit)/nrow(milledgeville)
milledgeville.involvem<- subset(milledgeville, INVOLVEM==3)
milledgevillei.p<- nrow(milledgeville.involvem)/nrow(milledgeville)
milledgeville.openness<- subset(milledgeville, OPENNESS==3)
milledgevilleo.p<- nrow(milledgeville.openness)/nrow(milledgeville)
milledgeville.social_c<- subset(milledgeville, SOCIAL_C==3)
milledgevillesc.p<- nrow(milledgeville.social_c)/nrow(milledgeville)


milledgeville08.p<- rbind(c(milledgevillel.p,milledgevillep.p,milledgevilleca.p,milledgevillebs.p,
                            milledgevilleld.p,milledgevillee.p,milledgevilles.p,milledgevillea.p,
                            milledgevilleec.p,milledgevilleso.p,milledgevilleco.p,milledgevillei.p,
                            milledgevilleo.p,milledgevillesc.p))
colnames(milledgeville08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


myrtle.beach<- d.08.sub2[d.08.sub2$City=='Myrtle Beach, SC',]
myrtle.beach.loyalty<- subset(myrtle.beach,LOYALTY==5)
myrtle.beachl.p<- nrow(myrtle.beach.loyalty)/nrow(myrtle.beach)
myrtle.beach.passion<- subset(myrtle.beach, PASSION==5)
myrtle.beachp.p<- nrow(myrtle.beach.passion)/nrow(myrtle.beach)
myrtle.beach.ca<- subset(myrtle.beach, CA==5)
myrtle.beachca.p<- nrow(myrtle.beach.ca)/nrow(myrtle.beach)
myrtle.beach.basic_se<- subset(myrtle.beach, BASIC_SE==3)
myrtle.beachbs.p<- nrow(myrtle.beach.basic_se)/nrow(myrtle.beach)
myrtle.beach.leadersh<- subset(myrtle.beach, LEADERSH==3)
myrtle.beachld.p<- nrow(myrtle.beach.leadersh)/nrow(myrtle.beach)
myrtle.beach.educatio<- subset(myrtle.beach, EDUCATIO==3)
myrtle.beache.p<- nrow(myrtle.beach.educatio)/nrow(myrtle.beach)
myrtle.beach.safety<- subset(myrtle.beach, SAFETY==3)
myrtle.beachs.p<- nrow(myrtle.beach.safety)/nrow(myrtle.beach)
myrtle.beach.aestheti<- subset(myrtle.beach, AESTHETI==3)
myrtle.beacha.p<- nrow(myrtle.beach.aestheti)/nrow(myrtle.beach)
myrtle.beach.economy<- subset(myrtle.beach, ECONOMY==3)
myrtle.beachec.p<- nrow(myrtle.beach.economy)/nrow(myrtle.beach)
myrtle.beach.social_o<- subset(myrtle.beach, SOCIAL_O==3)
myrtle.beachso.p<- nrow(myrtle.beach.social_o)/nrow(myrtle.beach)
myrtle.beach.communit<- subset(myrtle.beach, COMMUNIT==3)
myrtle.beachco.p<- nrow(myrtle.beach.communit)/nrow(myrtle.beach)
myrtle.beach.involvem<- subset(myrtle.beach, INVOLVEM==3)
myrtle.beachi.p<- nrow(myrtle.beach.involvem)/nrow(myrtle.beach)
myrtle.beach.openness<- subset(myrtle.beach, OPENNESS==3)
myrtle.beacho.p<- nrow(myrtle.beach.openness)/nrow(myrtle.beach)
myrtle.beach.social_c<- subset(myrtle.beach, SOCIAL_C==3)
myrtle.beachsc.p<- nrow(myrtle.beach.social_c)/nrow(myrtle.beach)


myrtle.beach08.p<- rbind(c(myrtle.beachl.p,myrtle.beachp.p,myrtle.beachca.p,myrtle.beachbs.p,
                           myrtle.beachld.p,myrtle.beache.p,myrtle.beachs.p,myrtle.beacha.p,
                           myrtle.beachec.p,myrtle.beachso.p,myrtle.beachco.p,myrtle.beachi.p,
                           myrtle.beacho.p,myrtle.beachsc.p))
colnames(myrtle.beach08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                                "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


palm.beach<- d.08.sub2[d.08.sub2$City=='Palm Beach, FL',]
palm.beach.loyalty<- subset(palm.beach,LOYALTY==5)
palm.beachl.p<- nrow(palm.beach.loyalty)/nrow(palm.beach)
palm.beach.passion<- subset(palm.beach, PASSION==5)
palm.beachp.p<- nrow(palm.beach.passion)/nrow(palm.beach)
palm.beach.ca<- subset(palm.beach, CA==5)
palm.beachca.p<- nrow(palm.beach.ca)/nrow(palm.beach)
palm.beach.basic_se<- subset(palm.beach, BASIC_SE==3)
palm.beachbs.p<- nrow(palm.beach.basic_se)/nrow(palm.beach)
palm.beach.leadersh<- subset(palm.beach, LEADERSH==3)
palm.beachld.p<- nrow(palm.beach.leadersh)/nrow(palm.beach)
palm.beach.educatio<- subset(palm.beach, EDUCATIO==3)
palm.beache.p<- nrow(palm.beach.educatio)/nrow(palm.beach)
palm.beach.safety<- subset(palm.beach, SAFETY==3)
palm.beachs.p<- nrow(palm.beach.safety)/nrow(palm.beach)
palm.beach.aestheti<- subset(palm.beach, AESTHETI==3)
palm.beacha.p<- nrow(palm.beach.aestheti)/nrow(palm.beach)
palm.beach.economy<- subset(palm.beach, ECONOMY==3)
palm.beachec.p<- nrow(palm.beach.economy)/nrow(palm.beach)
palm.beach.social_o<- subset(palm.beach, SOCIAL_O==3)
palm.beachso.p<- nrow(palm.beach.social_o)/nrow(palm.beach)
palm.beach.communit<- subset(palm.beach, COMMUNIT==3)
palm.beachco.p<- nrow(palm.beach.communit)/nrow(palm.beach)
palm.beach.involvem<- subset(palm.beach, INVOLVEM==3)
palm.beachi.p<- nrow(palm.beach.involvem)/nrow(palm.beach)
palm.beach.openness<- subset(palm.beach, OPENNESS==3)
palm.beacho.p<- nrow(palm.beach.openness)/nrow(palm.beach)
palm.beach.social_c<- subset(palm.beach, SOCIAL_C==3)
palm.beachsc.p<- nrow(palm.beach.social_c)/nrow(palm.beach)


palm.beach08.p<- rbind(c(palm.beachl.p,palm.beachp.p,palm.beachca.p,palm.beachbs.p,
                         palm.beachld.p,palm.beache.p,palm.beachs.p,palm.beacha.p,
                         palm.beachec.p,palm.beachso.p,palm.beachco.p,palm.beachi.p,
                         palm.beacho.p,palm.beachsc.p))
colnames(palm.beach08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                               "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

philadelphia<- d.08.sub2[d.08.sub2$City=='Philadelphia, PA',]
philadelphia.loyalty<- subset(philadelphia,LOYALTY==5)
philadelphial.p<- nrow(philadelphia.loyalty)/nrow(philadelphia)
philadelphia.passion<- subset(philadelphia, PASSION==5)
philadelphiap.p<- nrow(philadelphia.passion)/nrow(philadelphia)
philadelphia.ca<- subset(philadelphia, CA==5)
philadelphiaca.p<- nrow(philadelphia.ca)/nrow(philadelphia)
philadelphia.basic_se<- subset(philadelphia, BASIC_SE==3)
philadelphiabs.p<- nrow(philadelphia.basic_se)/nrow(philadelphia)
philadelphia.leadersh<- subset(philadelphia, LEADERSH==3)
philadelphiald.p<- nrow(philadelphia.leadersh)/nrow(philadelphia)
philadelphia.educatio<- subset(philadelphia, EDUCATIO==3)
philadelphiae.p<- nrow(philadelphia.educatio)/nrow(philadelphia)
philadelphia.safety<- subset(philadelphia, SAFETY==3)
philadelphias.p<- nrow(philadelphia.safety)/nrow(philadelphia)
philadelphia.aestheti<- subset(philadelphia, AESTHETI==3)
philadelphiaa.p<- nrow(philadelphia.aestheti)/nrow(philadelphia)
philadelphia.economy<- subset(philadelphia, ECONOMY==3)
philadelphiaec.p<- nrow(philadelphia.economy)/nrow(philadelphia)
philadelphia.social_o<- subset(philadelphia, SOCIAL_O==3)
philadelphiaso.p<- nrow(philadelphia.social_o)/nrow(philadelphia)
philadelphia.communit<- subset(philadelphia, COMMUNIT==3)
philadelphiaco.p<- nrow(philadelphia.communit)/nrow(philadelphia)
philadelphia.involvem<- subset(philadelphia, INVOLVEM==3)
philadelphiai.p<- nrow(philadelphia.involvem)/nrow(philadelphia)
philadelphia.openness<- subset(philadelphia, OPENNESS==3)
philadelphiao.p<- nrow(philadelphia.openness)/nrow(philadelphia)
philadelphia.social_c<- subset(philadelphia, SOCIAL_C==3)
philadelphiasc.p<- nrow(philadelphia.social_c)/nrow(philadelphia)


philadelphia08.p<- rbind(c(philadelphial.p,philadelphiap.p,philadelphiaca.p,philadelphiabs.p,
                           philadelphiald.p,philadelphiae.p,philadelphias.p,philadelphiaa.p,
                           philadelphiaec.p,philadelphiaso.p,philadelphiaco.p,philadelphiai.p,
                           philadelphiao.p,philadelphiasc.p))
colnames(philadelphia08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

san.jose<- d.08.sub2[d.08.sub2$City=='San Jose, CA',]
san.jose.loyalty<- subset(san.jose,LOYALTY==5)
san.josel.p<- nrow(san.jose.loyalty)/nrow(san.jose)
san.jose.passion<- subset(san.jose, PASSION==5)
san.josep.p<- nrow(san.jose.passion)/nrow(san.jose)
san.jose.ca<- subset(san.jose, CA==5)
san.joseca.p<- nrow(san.jose.ca)/nrow(san.jose)
san.jose.basic_se<- subset(san.jose, BASIC_SE==3)
san.josebs.p<- nrow(san.jose.basic_se)/nrow(san.jose)
san.jose.leadersh<- subset(san.jose, LEADERSH==3)
san.joseld.p<- nrow(san.jose.leadersh)/nrow(san.jose)
san.jose.educatio<- subset(san.jose, EDUCATIO==3)
san.josee.p<- nrow(san.jose.educatio)/nrow(san.jose)
san.jose.safety<- subset(san.jose, SAFETY==3)
san.joses.p<- nrow(san.jose.safety)/nrow(san.jose)
san.jose.aestheti<- subset(san.jose, AESTHETI==3)
san.josea.p<- nrow(san.jose.aestheti)/nrow(san.jose)
san.jose.economy<- subset(san.jose, ECONOMY==3)
san.joseec.p<- nrow(san.jose.economy)/nrow(san.jose)
san.jose.social_o<- subset(san.jose, SOCIAL_O==3)
san.joseso.p<- nrow(san.jose.social_o)/nrow(san.jose)
san.jose.communit<- subset(san.jose, COMMUNIT==3)
san.joseco.p<- nrow(san.jose.communit)/nrow(san.jose)
san.jose.involvem<- subset(san.jose, INVOLVEM==3)
san.josei.p<- nrow(san.jose.involvem)/nrow(san.jose)
san.jose.openness<- subset(san.jose, OPENNESS==3)
san.joseo.p<- nrow(san.jose.openness)/nrow(san.jose)
san.jose.social_c<- subset(san.jose, SOCIAL_C==3)
san.josesc.p<- nrow(san.jose.social_c)/nrow(san.jose)


san.jose08.p<- rbind(c(san.josel.p,san.josep.p,san.joseca.p,san.josebs.p,
                       san.joseld.p,san.josee.p,san.joses.p,san.josea.p,
                       san.joseec.p,san.joseso.p,san.joseco.p,san.josei.p,
                       san.joseo.p,san.josesc.p))
colnames(san.jose08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                               "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

st.paul<- d.08.sub2[d.08.sub2$City=='St. Paul, MN',]
st.paul.loyalty<- subset(st.paul,LOYALTY==5)
st.paull.p<- nrow(st.paul.loyalty)/nrow(st.paul)
st.paul.passion<- subset(st.paul, PASSION==5)
st.paulp.p<- nrow(st.paul.passion)/nrow(st.paul)
st.paul.ca<- subset(st.paul, CA==5)
st.paulca.p<- nrow(st.paul.ca)/nrow(st.paul)
st.paul.basic_se<- subset(st.paul, BASIC_SE==3)
st.paulbs.p<- nrow(st.paul.basic_se)/nrow(st.paul)
st.paul.leadersh<- subset(st.paul, LEADERSH==3)
st.paulld.p<- nrow(st.paul.leadersh)/nrow(st.paul)
st.paul.educatio<- subset(st.paul, EDUCATIO==3)
st.paule.p<- nrow(st.paul.educatio)/nrow(st.paul)
st.paul.safety<- subset(st.paul, SAFETY==3)
st.pauls.p<- nrow(st.paul.safety)/nrow(st.paul)
st.paul.aestheti<- subset(st.paul, AESTHETI==3)
st.paula.p<- nrow(st.paul.aestheti)/nrow(st.paul)
st.paul.economy<- subset(st.paul, ECONOMY==3)
st.paulec.p<- nrow(st.paul.economy)/nrow(st.paul)
st.paul.social_o<- subset(st.paul, SOCIAL_O==3)
st.paulso.p<- nrow(st.paul.social_o)/nrow(st.paul)
st.paul.communit<- subset(st.paul, COMMUNIT==3)
st.paulco.p<- nrow(st.paul.communit)/nrow(st.paul)
st.paul.involvem<- subset(st.paul, INVOLVEM==3)
st.pauli.p<- nrow(st.paul.involvem)/nrow(st.paul)
st.paul.openness<- subset(st.paul, OPENNESS==3)
st.paulo.p<- nrow(st.paul.openness)/nrow(st.paul)
st.paul.social_c<- subset(st.paul, SOCIAL_C==3)
st.paulsc.p<- nrow(st.paul.social_c)/nrow(st.paul)


st.paul08.p<- rbind(c(st.paull.p,st.paulp.p,st.paulca.p,st.paulbs.p,
                      st.paulld.p,st.paule.p,st.pauls.p,st.paula.p,
                      st.paulec.p,st.paulso.p,st.paulco.p,st.pauli.p,
                       st.paulo.p,st.paulsc.p))
colnames(st.paul08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

state.college<- d.08.sub2[d.08.sub2$City=='State College, PA',]
state.college.loyalty<- subset(state.college,LOYALTY==5)
state.collegel.p<- nrow(state.college.loyalty)/nrow(state.college)
state.college.passion<- subset(state.college, PASSION==5)
state.collegep.p<- nrow(state.college.passion)/nrow(state.college)
state.college.ca<- subset(state.college, CA==5)
state.collegeca.p<- nrow(state.college.ca)/nrow(state.college)
state.college.basic_se<- subset(state.college, BASIC_SE==3)
state.collegebs.p<- nrow(state.college.basic_se)/nrow(state.college)
state.college.leadersh<- subset(state.college, LEADERSH==3)
state.collegeld.p<- nrow(state.college.leadersh)/nrow(state.college)
state.college.educatio<- subset(state.college, EDUCATIO==3)
state.collegee.p<- nrow(state.college.educatio)/nrow(state.college)
state.college.safety<- subset(state.college, SAFETY==3)
state.colleges.p<- nrow(state.college.safety)/nrow(state.college)
state.college.aestheti<- subset(state.college, AESTHETI==3)
state.collegea.p<- nrow(state.college.aestheti)/nrow(state.college)
state.college.economy<- subset(state.college, ECONOMY==3)
state.collegeec.p<- nrow(state.college.economy)/nrow(state.college)
state.college.social_o<- subset(state.college, SOCIAL_O==3)
state.collegeso.p<- nrow(state.college.social_o)/nrow(state.college)
state.college.communit<- subset(state.college, COMMUNIT==3)
state.collegeco.p<- nrow(state.college.communit)/nrow(state.college)
state.college.involvem<- subset(state.college, INVOLVEM==3)
state.collegei.p<- nrow(state.college.involvem)/nrow(state.college)
state.college.openness<- subset(state.college, OPENNESS==3)
state.collegeo.p<- nrow(state.college.openness)/nrow(state.college)
state.college.social_c<- subset(state.college, SOCIAL_C==3)
state.collegesc.p<- nrow(state.college.social_c)/nrow(state.college)


state.college08.p<- rbind(c(state.collegel.p,state.collegep.p,state.collegeca.p,state.collegebs.p,
                            state.collegeld.p,state.collegee.p,state.colleges.p,state.collegea.p,
                            state.collegeec.p,state.collegeso.p,state.collegeco.p,state.collegei.p,
                            state.collegeo.p,state.collegesc.p))
colnames(state.college08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


tallahassee<- d.08.sub2[d.08.sub2$City=='Tallahassee, FL',]
tallahassee.loyalty<- subset(tallahassee,LOYALTY==5)
tallahasseel.p<- nrow(tallahassee.loyalty)/nrow(tallahassee)
tallahassee.passion<- subset(tallahassee, PASSION==5)
tallahasseep.p<- nrow(tallahassee.passion)/nrow(tallahassee)
tallahassee.ca<- subset(tallahassee, CA==5)
tallahasseeca.p<- nrow(tallahassee.ca)/nrow(tallahassee)
tallahassee.basic_se<- subset(tallahassee, BASIC_SE==3)
tallahasseebs.p<- nrow(tallahassee.basic_se)/nrow(tallahassee)
tallahassee.leadersh<- subset(tallahassee, LEADERSH==3)
tallahasseeld.p<- nrow(tallahassee.leadersh)/nrow(tallahassee)
tallahassee.educatio<- subset(tallahassee, EDUCATIO==3)
tallahasseee.p<- nrow(tallahassee.educatio)/nrow(tallahassee)
tallahassee.safety<- subset(tallahassee, SAFETY==3)
tallahassees.p<- nrow(tallahassee.safety)/nrow(tallahassee)
tallahassee.aestheti<- subset(tallahassee, AESTHETI==3)
tallahasseea.p<- nrow(tallahassee.aestheti)/nrow(tallahassee)
tallahassee.economy<- subset(tallahassee, ECONOMY==3)
tallahasseeec.p<- nrow(tallahassee.economy)/nrow(tallahassee)
tallahassee.social_o<- subset(tallahassee, SOCIAL_O==3)
tallahasseeso.p<- nrow(tallahassee.social_o)/nrow(tallahassee)
tallahassee.communit<- subset(tallahassee, COMMUNIT==3)
tallahasseeco.p<- nrow(tallahassee.communit)/nrow(tallahassee)
tallahassee.involvem<- subset(tallahassee, INVOLVEM==3)
tallahasseei.p<- nrow(tallahassee.involvem)/nrow(tallahassee)
tallahassee.openness<- subset(tallahassee, OPENNESS==3)
tallahasseeo.p<- nrow(tallahassee.openness)/nrow(tallahassee)
tallahassee.social_c<- subset(tallahassee, SOCIAL_C==3)
tallahasseesc.p<- nrow(tallahassee.social_c)/nrow(tallahassee)


tallahassee08.p<- rbind(c(tallahasseel.p,tallahasseep.p,tallahasseeca.p,tallahasseebs.p,
                          tallahasseeld.p,tallahasseee.p,tallahassees.p,tallahasseea.p,
                          tallahasseeec.p,tallahasseeso.p,tallahasseeco.p,tallahasseei.p,
                          tallahasseeo.p,tallahasseesc.p))
colnames(tallahassee08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                                "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

wichita<- d.08.sub2[d.08.sub2$City=='Wichita, KS',]
wichita.loyalty<- subset(wichita,LOYALTY==5)
wichital.p<- nrow(wichita.loyalty)/nrow(wichita)
wichita.passion<- subset(wichita, PASSION==5)
wichitap.p<- nrow(wichita.passion)/nrow(wichita)
wichita.ca<- subset(wichita, CA==5)
wichitaca.p<- nrow(wichita.ca)/nrow(wichita)
wichita.basic_se<- subset(wichita, BASIC_SE==3)
wichitabs.p<- nrow(wichita.basic_se)/nrow(wichita)
wichita.leadersh<- subset(wichita, LEADERSH==3)
wichitald.p<- nrow(wichita.leadersh)/nrow(wichita)
wichita.educatio<- subset(wichita, EDUCATIO==3)
wichitae.p<- nrow(wichita.educatio)/nrow(wichita)
wichita.safety<- subset(wichita, SAFETY==3)
wichitas.p<- nrow(wichita.safety)/nrow(wichita)
wichita.aestheti<- subset(wichita, AESTHETI==3)
wichitaa.p<- nrow(wichita.aestheti)/nrow(wichita)
wichita.economy<- subset(wichita, ECONOMY==3)
wichitaec.p<- nrow(wichita.economy)/nrow(wichita)
wichita.social_o<- subset(wichita, SOCIAL_O==3)
wichitaso.p<- nrow(wichita.social_o)/nrow(wichita)
wichita.communit<- subset(wichita, COMMUNIT==3)
wichitaco.p<- nrow(wichita.communit)/nrow(wichita)
wichita.involvem<- subset(wichita, INVOLVEM==3)
wichitai.p<- nrow(wichita.involvem)/nrow(wichita)
wichita.openness<- subset(wichita, OPENNESS==3)
wichitao.p<- nrow(wichita.openness)/nrow(wichita)
wichita.social_c<- subset(wichita, SOCIAL_C==3)
wichitasc.p<- nrow(wichita.social_c)/nrow(wichita)


wichita08.p<- rbind(c(wichital.p,wichitap.p,wichitaca.p,wichitabs.p,
                      wichitald.p,wichitae.p,wichitas.p,wichitaa.p,
                      wichitaec.p,wichitaso.p,wichitaco.p,wichitai.p,
                      wichitao.p,wichitasc.p))
colnames(wichita08.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                              "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

prop08<- rbind(aberdeen08.p,akron08.p,biloxi08.p,boulder08.p,bradenton08.p,
               charlotte08.p,columbia08.p,columbus08.p,detroit08.p,duluth08.p,
               fort.wayne08.p,gary08.p,grand.forks08.p,lexington08.p,
               long.beach08.p,macon08.p,miami08.p,milledgeville08.p,
               myrtle.beach08.p,palm.beach08.p,philadelphia08.p,
               san.jose08.p,st.paul08.p,state.college08.p,
               tallahassee08.p,wichita08.p)
prop08<- data.frame(prop08)
City<- c("Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
         "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
         "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
         "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
         "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
         "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")

prop08$City<- City

Year08<- rep(2008,26)

prop08$Year<- Year08



aberdeen<- d.09.sub2[d.09.sub2$City=='Aberdeen, SD',]
aberdeen.loyalty<- subset(aberdeen,LOYALTY==5)
al.p<- nrow(aberdeen.loyalty)/nrow(aberdeen)
aberdeen.passion<- subset(aberdeen, PASSION==5)
ap.p<- nrow(aberdeen.passion)/nrow(aberdeen)
aberdeen.ca<- subset(aberdeen, CA==5)
aca.p<- nrow(aberdeen.ca)/nrow(aberdeen)
aberdeen.basic_se<- subset(aberdeen, BASIC_SE==3)
abs.p<- nrow(aberdeen.basic_se)/nrow(aberdeen)
aberdeen.leadersh<- subset(aberdeen, LEADERSH==3)
ald.p<- nrow(aberdeen.leadersh)/nrow(aberdeen)
aberdeen.educatio<- subset(aberdeen, EDUCATIO==3)
ae.p<- nrow(aberdeen.educatio)/nrow(aberdeen)
aberdeen.safety<- subset(aberdeen, SAFETY==3)
as.p<- nrow(aberdeen.safety)/nrow(aberdeen)
aberdeen.aestheti<- subset(aberdeen, AESTHETI==3)
aa.p<- nrow(aberdeen.aestheti)/nrow(aberdeen)
aberdeen.economy<- subset(aberdeen, ECONOMY==3)
aec.p<- nrow(aberdeen.economy)/nrow(aberdeen)
aberdeen.social_o<- subset(aberdeen, SOCIAL_O==3)
aso.p<- nrow(aberdeen.social_o)/nrow(aberdeen)
aberdeen.communit<- subset(aberdeen, COMMUNIT==3)
aco.p<- nrow(aberdeen.communit)/nrow(aberdeen)
aberdeen.involvem<- subset(aberdeen, INVOLVEM==3)
ai.p<- nrow(aberdeen.involvem)/nrow(aberdeen)
aberdeen.openness<- subset(aberdeen, OPENNESS==3)
ao.p<- nrow(aberdeen.openness)/nrow(aberdeen)
aberdeen.social_c<- subset(aberdeen, SOCIAL_C==3)
asc.p<- nrow(aberdeen.social_c)/nrow(aberdeen)

aberdeen09.p<- rbind(c(al.p,ap.p,aca.p,abs.p,ald.p,ae.p,as.p,aa.p,aec.p,aso.p,aco.p,ai.p,ao.p,asc.p))
colnames(aberdeen09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

akron<- d.09.sub2[d.09.sub2$City=='Akron, OH',]
akron.loyalty<- subset(akron,LOYALTY==5)
akronl.p<- nrow(akron.loyalty)/nrow(akron)
akron.passion<- subset(akron, PASSION==5)
akronp.p<- nrow(akron.passion)/nrow(akron)
akron.ca<- subset(akron, CA==5)
akronca.p<- nrow(akron.ca)/nrow(akron)
akron.basic_se<- subset(akron, BASIC_SE==3)
akronbs.p<- nrow(akron.basic_se)/nrow(akron)
akron.leadersh<- subset(akron, LEADERSH==3)
akronld.p<- nrow(akron.leadersh)/nrow(akron)
akron.educatio<- subset(akron, EDUCATIO==3)
akrone.p<- nrow(akron.educatio)/nrow(akron)
akron.safety<- subset(akron, SAFETY==3)
akrons.p<- nrow(akron.safety)/nrow(akron)
akron.aestheti<- subset(akron, AESTHETI==3)
akrona.p<- nrow(akron.aestheti)/nrow(akron)
akron.economy<- subset(akron, ECONOMY==3)
akronec.p<- nrow(akron.economy)/nrow(akron)
akron.social_o<- subset(akron, SOCIAL_O==3)
akronso.p<- nrow(akron.social_o)/nrow(akron)
akron.communit<- subset(akron, COMMUNIT==3)
akronco.p<- nrow(akron.communit)/nrow(akron)
akron.involvem<- subset(akron, INVOLVEM==3)
akroni.p<- nrow(akron.involvem)/nrow(akron)
akron.openness<- subset(akron, OPENNESS==3)
akrono.p<- nrow(akron.openness)/nrow(akron)
akron.social_c<- subset(akron, SOCIAL_C==3)
akronsc.p<- nrow(akron.social_c)/nrow(akron)

akron09.p<- rbind(c(akronl.p,akronp.p,akronca.p,akronbs.p,akronld.p,akrone.p,
                    akrons.p,akrona.p,akronec.p,akronso.p,akronco.p,akroni.p,akrono.p,akronsc.p))
colnames(akron09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

biloxi<- d.09.sub2[d.09.sub2$City=='Biloxi, MS',]
biloxi.loyalty<- subset(biloxi,LOYALTY==5)
biloxil.p<- nrow(biloxi.loyalty)/nrow(biloxi)
biloxi.passion<- subset(biloxi, PASSION==5)
biloxip.p<- nrow(biloxi.passion)/nrow(biloxi)
biloxi.ca<- subset(biloxi, CA==5)
biloxica.p<- nrow(biloxi.ca)/nrow(biloxi)
biloxi.basic_se<- subset(biloxi, BASIC_SE==3)
biloxibs.p<- nrow(biloxi.basic_se)/nrow(biloxi)
biloxi.leadersh<- subset(biloxi, LEADERSH==3)
biloxild.p<- nrow(biloxi.leadersh)/nrow(biloxi)
biloxi.educatio<- subset(biloxi, EDUCATIO==3)
biloxie.p<- nrow(biloxi.educatio)/nrow(biloxi)
biloxi.safety<- subset(biloxi, SAFETY==3)
biloxis.p<- nrow(biloxi.safety)/nrow(biloxi)
biloxi.aestheti<- subset(biloxi, AESTHETI==3)
biloxia.p<- nrow(biloxi.aestheti)/nrow(biloxi)
biloxi.economy<- subset(biloxi, ECONOMY==3)
biloxiec.p<- nrow(biloxi.economy)/nrow(biloxi)
biloxi.social_o<- subset(biloxi, SOCIAL_O==3)
biloxiso.p<- nrow(biloxi.social_o)/nrow(biloxi)
biloxi.communit<- subset(biloxi, COMMUNIT==3)
biloxico.p<- nrow(biloxi.communit)/nrow(biloxi)
biloxi.involvem<- subset(biloxi, INVOLVEM==3)
biloxii.p<- nrow(biloxi.involvem)/nrow(biloxi)
biloxi.openness<- subset(biloxi, OPENNESS==3)
biloxio.p<- nrow(biloxi.openness)/nrow(biloxi)
biloxi.social_c<- subset(biloxi, SOCIAL_C==3)
biloxisc.p<- nrow(biloxi.social_c)/nrow(biloxi)

biloxi09.p<- rbind(c(biloxil.p,biloxip.p,biloxica.p,biloxibs.p,biloxild.p,biloxie.p,
                     biloxis.p,biloxia.p,biloxiec.p,biloxiso.p,biloxico.p,biloxii.p,biloxio.p,biloxisc.p))
colnames(biloxi09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

boulder<- d.09.sub2[d.09.sub2$City=='Boulder, CO',]
boulder.loyalty<- subset(boulder,LOYALTY==5)
boulderl.p<- nrow(boulder.loyalty)/nrow(boulder)
boulder.passion<- subset(boulder, PASSION==5)
boulderp.p<- nrow(boulder.passion)/nrow(boulder)
boulder.ca<- subset(boulder, CA==5)
boulderca.p<- nrow(boulder.ca)/nrow(boulder)
boulder.basic_se<- subset(boulder, BASIC_SE==3)
boulderbs.p<- nrow(boulder.basic_se)/nrow(boulder)
boulder.leadersh<- subset(boulder, LEADERSH==3)
boulderld.p<- nrow(boulder.leadersh)/nrow(boulder)
boulder.educatio<- subset(boulder, EDUCATIO==3)
bouldere.p<- nrow(boulder.educatio)/nrow(boulder)
boulder.safety<- subset(boulder, SAFETY==3)
boulders.p<- nrow(boulder.safety)/nrow(boulder)
boulder.aestheti<- subset(boulder, AESTHETI==3)
bouldera.p<- nrow(boulder.aestheti)/nrow(boulder)
boulder.economy<- subset(boulder, ECONOMY==3)
boulderec.p<- nrow(boulder.economy)/nrow(boulder)
boulder.social_o<- subset(boulder, SOCIAL_O==3)
boulderso.p<- nrow(boulder.social_o)/nrow(boulder)
boulder.communit<- subset(boulder, COMMUNIT==3)
boulderco.p<- nrow(boulder.communit)/nrow(boulder)
boulder.involvem<- subset(boulder, INVOLVEM==3)
boulderi.p<- nrow(boulder.involvem)/nrow(boulder)
boulder.openness<- subset(boulder, OPENNESS==3)
bouldero.p<- nrow(boulder.openness)/nrow(boulder)
boulder.social_c<- subset(boulder, SOCIAL_C==3)
bouldersc.p<- nrow(boulder.social_c)/nrow(boulder)

boulder09.p<- rbind(c(boulderl.p,boulderp.p,boulderca.p,boulderbs.p,boulderld.p,bouldere.p,
                      boulders.p,bouldera.p,boulderec.p,boulderso.p,boulderco.p,boulderi.p,bouldero.p,bouldersc.p))
colnames(boulder09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

bradenton<- d.09.sub2[d.09.sub2$City=='Bradenton, FL',]
bradenton.loyalty<- subset(bradenton,LOYALTY==5)
bradentonl.p<- nrow(bradenton.loyalty)/nrow(bradenton)
bradenton.passion<- subset(bradenton, PASSION==5)
bradentonp.p<- nrow(bradenton.passion)/nrow(bradenton)
bradenton.ca<- subset(bradenton, CA==5)
bradentonca.p<- nrow(bradenton.ca)/nrow(bradenton)
bradenton.basic_se<- subset(bradenton, BASIC_SE==3)
bradentonbs.p<- nrow(bradenton.basic_se)/nrow(bradenton)
bradenton.leadersh<- subset(bradenton, LEADERSH==3)
bradentonld.p<- nrow(bradenton.leadersh)/nrow(bradenton)
bradenton.educatio<- subset(bradenton, EDUCATIO==3)
bradentone.p<- nrow(bradenton.educatio)/nrow(bradenton)
bradenton.safety<- subset(bradenton, SAFETY==3)
bradentons.p<- nrow(bradenton.safety)/nrow(bradenton)
bradenton.aestheti<- subset(bradenton, AESTHETI==3)
bradentona.p<- nrow(bradenton.aestheti)/nrow(bradenton)
bradenton.economy<- subset(bradenton, ECONOMY==3)
bradentonec.p<- nrow(bradenton.economy)/nrow(bradenton)
bradenton.social_o<- subset(bradenton, SOCIAL_O==3)
bradentonso.p<- nrow(bradenton.social_o)/nrow(bradenton)
bradenton.communit<- subset(bradenton, COMMUNIT==3)
bradentonco.p<- nrow(bradenton.communit)/nrow(bradenton)
bradenton.involvem<- subset(bradenton, INVOLVEM==3)
bradentoni.p<- nrow(bradenton.involvem)/nrow(bradenton)
bradenton.openness<- subset(bradenton, OPENNESS==3)
bradentono.p<- nrow(bradenton.openness)/nrow(bradenton)
bradenton.social_c<- subset(bradenton, SOCIAL_C==3)
bradentonsc.p<- nrow(bradenton.social_c)/nrow(bradenton)

bradenton09.p<- rbind(c(bradentonl.p,bradentonp.p,bradentonca.p,bradentonbs.p,bradentonld.p,bradentone.p,
                        bradentons.p,bradentona.p,bradentonec.p,bradentonso.p,bradentonco.p,
                        bradentoni.p,bradentono.p,bradentonsc.p))
colnames(bradenton09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

charlotte<- d.09.sub2[d.09.sub2$City=='Charlotte, NC',]
charlotte.loyalty<- subset(charlotte,LOYALTY==5)
charlottel.p<- nrow(charlotte.loyalty)/nrow(charlotte)
charlotte.passion<- subset(charlotte, PASSION==5)
charlottep.p<- nrow(charlotte.passion)/nrow(charlotte)
charlotte.ca<- subset(charlotte, CA==5)
charlotteca.p<- nrow(charlotte.ca)/nrow(charlotte)
charlotte.basic_se<- subset(charlotte, BASIC_SE==3)
charlottebs.p<- nrow(charlotte.basic_se)/nrow(charlotte)
charlotte.leadersh<- subset(charlotte, LEADERSH==3)
charlotteld.p<- nrow(charlotte.leadersh)/nrow(charlotte)
charlotte.educatio<- subset(charlotte, EDUCATIO==3)
charlottee.p<- nrow(charlotte.educatio)/nrow(charlotte)
charlotte.safety<- subset(charlotte, SAFETY==3)
charlottes.p<- nrow(charlotte.safety)/nrow(charlotte)
charlotte.aestheti<- subset(charlotte, AESTHETI==3)
charlottea.p<- nrow(charlotte.aestheti)/nrow(charlotte)
charlotte.economy<- subset(charlotte, ECONOMY==3)
charlotteec.p<- nrow(charlotte.economy)/nrow(charlotte)
charlotte.social_o<- subset(charlotte, SOCIAL_O==3)
charlotteso.p<- nrow(charlotte.social_o)/nrow(charlotte)
charlotte.communit<- subset(charlotte, COMMUNIT==3)
charlotteco.p<- nrow(charlotte.communit)/nrow(charlotte)
charlotte.involvem<- subset(charlotte, INVOLVEM==3)
charlottei.p<- nrow(charlotte.involvem)/nrow(charlotte)
charlotte.openness<- subset(charlotte, OPENNESS==3)
charlotteo.p<- nrow(charlotte.openness)/nrow(charlotte)
charlotte.social_c<- subset(charlotte, SOCIAL_C==3)
charlottesc.p<- nrow(charlotte.social_c)/nrow(charlotte)

charlotte09.p<- rbind(c(charlottel.p,charlottep.p,charlotteca.p,charlottebs.p,charlotteld.p,charlottee.p,
                        charlottes.p,charlottea.p,charlotteec.p,charlotteso.p,charlotteco.p,
                        charlottei.p,charlotteo.p,charlottesc.p))
colnames(charlotte09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

columbia<- d.09.sub2[d.09.sub2$City=='Columbia, SC',]
columbia.loyalty<- subset(columbia,LOYALTY==5)
columbial.p<- nrow(columbia.loyalty)/nrow(columbia)
columbia.passion<- subset(columbia, PASSION==5)
columbiap.p<- nrow(columbia.passion)/nrow(columbia)
columbia.ca<- subset(columbia, CA==5)
columbiaca.p<- nrow(columbia.ca)/nrow(columbia)
columbia.basic_se<- subset(columbia, BASIC_SE==3)
columbiabs.p<- nrow(columbia.basic_se)/nrow(columbia)
columbia.leadersh<- subset(columbia, LEADERSH==3)
columbiald.p<- nrow(columbia.leadersh)/nrow(columbia)
columbia.educatio<- subset(columbia, EDUCATIO==3)
columbiae.p<- nrow(columbia.educatio)/nrow(columbia)
columbia.safety<- subset(columbia, SAFETY==3)
columbias.p<- nrow(columbia.safety)/nrow(columbia)
columbia.aestheti<- subset(columbia, AESTHETI==3)
columbiaa.p<- nrow(columbia.aestheti)/nrow(columbia)
columbia.economy<- subset(columbia, ECONOMY==3)
columbiaec.p<- nrow(columbia.economy)/nrow(columbia)
columbia.social_o<- subset(columbia, SOCIAL_O==3)
columbiaso.p<- nrow(columbia.social_o)/nrow(columbia)
columbia.communit<- subset(columbia, COMMUNIT==3)
columbiaco.p<- nrow(columbia.communit)/nrow(columbia)
columbia.involvem<- subset(columbia, INVOLVEM==3)
columbiai.p<- nrow(columbia.involvem)/nrow(columbia)
columbia.openness<- subset(columbia, OPENNESS==3)
columbiao.p<- nrow(columbia.openness)/nrow(columbia)
columbia.social_c<- subset(columbia, SOCIAL_C==3)
columbiasc.p<- nrow(columbia.social_c)/nrow(columbia)

columbia09.p<- rbind(c(columbial.p,columbiap.p,columbiaca.p,columbiabs.p,columbiald.p,columbiae.p,
                       columbias.p,columbiaa.p,columbiaec.p,columbiaso.p,columbiaco.p,
                       columbiai.p,columbiao.p,columbiasc.p))
colnames(columbia09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

columbus<- d.09.sub2[d.09.sub2$City=='Columbus, GA',]
columbus.loyalty<- subset(columbus,LOYALTY==5)
columbusl.p<- nrow(columbus.loyalty)/nrow(columbus)
columbus.passion<- subset(columbus, PASSION==5)
columbusp.p<- nrow(columbus.passion)/nrow(columbus)
columbus.ca<- subset(columbus, CA==5)
columbusca.p<- nrow(columbus.ca)/nrow(columbus)
columbus.basic_se<- subset(columbus, BASIC_SE==3)
columbusbs.p<- nrow(columbus.basic_se)/nrow(columbus)
columbus.leadersh<- subset(columbus, LEADERSH==3)
columbusld.p<- nrow(columbus.leadersh)/nrow(columbus)
columbus.educatio<- subset(columbus, EDUCATIO==3)
columbuse.p<- nrow(columbus.educatio)/nrow(columbus)
columbus.safety<- subset(columbus, SAFETY==3)
columbuss.p<- nrow(columbus.safety)/nrow(columbus)
columbus.aestheti<- subset(columbus, AESTHETI==3)
columbusa.p<- nrow(columbus.aestheti)/nrow(columbus)
columbus.economy<- subset(columbus, ECONOMY==3)
columbusec.p<- nrow(columbus.economy)/nrow(columbus)
columbus.social_o<- subset(columbus, SOCIAL_O==3)
columbusso.p<- nrow(columbus.social_o)/nrow(columbus)
columbus.communit<- subset(columbus, COMMUNIT==3)
columbusco.p<- nrow(columbus.communit)/nrow(columbus)
columbus.involvem<- subset(columbus, INVOLVEM==3)
columbusi.p<- nrow(columbus.involvem)/nrow(columbus)
columbus.openness<- subset(columbus, OPENNESS==3)
columbuso.p<- nrow(columbus.openness)/nrow(columbus)
columbus.social_c<- subset(columbus, SOCIAL_C==3)
columbussc.p<- nrow(columbus.social_c)/nrow(columbus)

columbus09.p<- rbind(c(columbusl.p,columbusp.p,columbusca.p,columbusbs.p,columbusld.p,columbuse.p,
                       columbuss.p,columbusa.p,columbusec.p,columbusso.p,columbusco.p,
                       columbusi.p,columbuso.p,columbussc.p))
colnames(columbus09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


detroit<- d.09.sub2[d.09.sub2$City=='Detroit, MI',]
detroit.loyalty<- subset(detroit,LOYALTY==5)
detroitl.p<- nrow(detroit.loyalty)/nrow(detroit)
detroit.passion<- subset(detroit, PASSION==5)
detroitp.p<- nrow(detroit.passion)/nrow(detroit)
detroit.ca<- subset(detroit, CA==5)
detroitca.p<- nrow(detroit.ca)/nrow(detroit)
detroit.basic_se<- subset(detroit, BASIC_SE==3)
detroitbs.p<- nrow(detroit.basic_se)/nrow(detroit)
detroit.leadersh<- subset(detroit, LEADERSH==3)
detroitld.p<- nrow(detroit.leadersh)/nrow(detroit)
detroit.educatio<- subset(detroit, EDUCATIO==3)
detroite.p<- nrow(detroit.educatio)/nrow(detroit)
detroit.safety<- subset(detroit, SAFETY==3)
detroits.p<- nrow(detroit.safety)/nrow(detroit)
detroit.aestheti<- subset(detroit, AESTHETI==3)
detroita.p<- nrow(detroit.aestheti)/nrow(detroit)
detroit.economy<- subset(detroit, ECONOMY==3)
detroitec.p<- nrow(detroit.economy)/nrow(detroit)
detroit.social_o<- subset(detroit, SOCIAL_O==3)
detroitso.p<- nrow(detroit.social_o)/nrow(detroit)
detroit.communit<- subset(detroit, COMMUNIT==3)
detroitco.p<- nrow(detroit.communit)/nrow(detroit)
detroit.involvem<- subset(detroit, INVOLVEM==3)
detroiti.p<- nrow(detroit.involvem)/nrow(detroit)
detroit.openness<- subset(detroit, OPENNESS==3)
detroito.p<- nrow(detroit.openness)/nrow(detroit)
detroit.social_c<- subset(detroit, SOCIAL_C==3)
detroitsc.p<- nrow(detroit.social_c)/nrow(detroit)

detroit09.p<- rbind(c(detroitl.p,detroitp.p,detroitca.p,detroitbs.p,detroitld.p,detroite.p,
                      detroits.p,detroita.p,detroitec.p,detroitso.p,detroitco.p,
                      detroiti.p,detroito.p,detroitsc.p))
colnames(detroit09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

duluth<- d.09.sub2[d.09.sub2$City=='Duluth, MN',]
duluth.loyalty<- subset(duluth,LOYALTY==5)
duluthl.p<- nrow(duluth.loyalty)/nrow(duluth)
duluth.passion<- subset(duluth, PASSION==5)
duluthp.p<- nrow(duluth.passion)/nrow(duluth)
duluth.ca<- subset(duluth, CA==5)
duluthca.p<- nrow(duluth.ca)/nrow(duluth)
duluth.basic_se<- subset(duluth, BASIC_SE==3)
duluthbs.p<- nrow(duluth.basic_se)/nrow(duluth)
duluth.leadersh<- subset(duluth, LEADERSH==3)
duluthld.p<- nrow(duluth.leadersh)/nrow(duluth)
duluth.educatio<- subset(duluth, EDUCATIO==3)
duluthe.p<- nrow(duluth.educatio)/nrow(duluth)
duluth.safety<- subset(duluth, SAFETY==3)
duluths.p<- nrow(duluth.safety)/nrow(duluth)
duluth.aestheti<- subset(duluth, AESTHETI==3)
dulutha.p<- nrow(duluth.aestheti)/nrow(duluth)
duluth.economy<- subset(duluth, ECONOMY==3)
duluthec.p<- nrow(duluth.economy)/nrow(duluth)
duluth.social_o<- subset(duluth, SOCIAL_O==3)
duluthso.p<- nrow(duluth.social_o)/nrow(duluth)
duluth.communit<- subset(duluth, COMMUNIT==3)
duluthco.p<- nrow(duluth.communit)/nrow(duluth)
duluth.involvem<- subset(duluth, INVOLVEM==3)
duluthi.p<- nrow(duluth.involvem)/nrow(duluth)
duluth.openness<- subset(duluth, OPENNESS==3)
dulutho.p<- nrow(duluth.openness)/nrow(duluth)
duluth.social_c<- subset(duluth, SOCIAL_C==3)
duluthsc.p<- nrow(duluth.social_c)/nrow(duluth)


duluth09.p<- rbind(c(duluthl.p,duluthp.p,duluthca.p,duluthbs.p,duluthld.p,duluthe.p,
                     duluths.p,dulutha.p,duluthec.p,duluthso.p,duluthco.p,
                     duluthi.p,dulutho.p,duluthsc.p))
colnames(duluth09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

fort.wayne<- d.09.sub2[d.09.sub2$City=='Fort Wayne, IN',]
fort.wayne.loyalty<- subset(fort.wayne,LOYALTY==5)
fort.waynel.p<- nrow(fort.wayne.loyalty)/nrow(fort.wayne)
fort.wayne.passion<- subset(fort.wayne, PASSION==5)
fort.waynep.p<- nrow(fort.wayne.passion)/nrow(fort.wayne)
fort.wayne.ca<- subset(fort.wayne, CA==5)
fort.wayneca.p<- nrow(fort.wayne.ca)/nrow(fort.wayne)
fort.wayne.basic_se<- subset(fort.wayne, BASIC_SE==3)
fort.waynebs.p<- nrow(fort.wayne.basic_se)/nrow(fort.wayne)
fort.wayne.leadersh<- subset(fort.wayne, LEADERSH==3)
fort.wayneld.p<- nrow(fort.wayne.leadersh)/nrow(fort.wayne)
fort.wayne.educatio<- subset(fort.wayne, EDUCATIO==3)
fort.waynee.p<- nrow(fort.wayne.educatio)/nrow(fort.wayne)
fort.wayne.safety<- subset(fort.wayne, SAFETY==3)
fort.waynes.p<- nrow(fort.wayne.safety)/nrow(fort.wayne)
fort.wayne.aestheti<- subset(fort.wayne, AESTHETI==3)
fort.waynea.p<- nrow(fort.wayne.aestheti)/nrow(fort.wayne)
fort.wayne.economy<- subset(fort.wayne, ECONOMY==3)
fort.wayneec.p<- nrow(fort.wayne.economy)/nrow(fort.wayne)
fort.wayne.social_o<- subset(fort.wayne, SOCIAL_O==3)
fort.wayneso.p<- nrow(fort.wayne.social_o)/nrow(fort.wayne)
fort.wayne.communit<- subset(fort.wayne, COMMUNIT==3)
fort.wayneco.p<- nrow(fort.wayne.communit)/nrow(fort.wayne)
fort.wayne.involvem<- subset(fort.wayne, INVOLVEM==3)
fort.waynei.p<- nrow(fort.wayne.involvem)/nrow(fort.wayne)
fort.wayne.openness<- subset(fort.wayne, OPENNESS==3)
fort.wayneo.p<- nrow(fort.wayne.openness)/nrow(fort.wayne)
fort.wayne.social_c<- subset(fort.wayne, SOCIAL_C==3)
fort.waynesc.p<- nrow(fort.wayne.social_c)/nrow(fort.wayne)


fort.wayne09.p<- rbind(c(fort.waynel.p,fort.waynep.p,fort.wayneca.p,fort.waynebs.p,fort.wayneld.p,fort.waynee.p,
                         fort.waynes.p,fort.waynea.p,fort.wayneec.p,fort.wayneso.p,fort.wayneco.p,
                         fort.waynei.p,fort.wayneo.p,fort.waynesc.p))
colnames(fort.wayne09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


gary<- d.09.sub2[d.09.sub2$City=='Gary, IN',]
gary.loyalty<- subset(gary,LOYALTY==5)
garyl.p<- nrow(gary.loyalty)/nrow(gary)
gary.passion<- subset(gary, PASSION==5)
garyp.p<- nrow(gary.passion)/nrow(gary)
gary.ca<- subset(gary, CA==5)
garyca.p<- nrow(gary.ca)/nrow(gary)
gary.basic_se<- subset(gary, BASIC_SE==3)
garybs.p<- nrow(gary.basic_se)/nrow(gary)
gary.leadersh<- subset(gary, LEADERSH==3)
garyld.p<- nrow(gary.leadersh)/nrow(gary)
gary.educatio<- subset(gary, EDUCATIO==3)
garye.p<- nrow(gary.educatio)/nrow(gary)
gary.safety<- subset(gary, SAFETY==3)
garys.p<- nrow(gary.safety)/nrow(gary)
gary.aestheti<- subset(gary, AESTHETI==3)
garya.p<- nrow(gary.aestheti)/nrow(gary)
gary.economy<- subset(gary, ECONOMY==3)
garyec.p<- nrow(gary.economy)/nrow(gary)
gary.social_o<- subset(gary, SOCIAL_O==3)
garyso.p<- nrow(gary.social_o)/nrow(gary)
gary.communit<- subset(gary, COMMUNIT==3)
garyco.p<- nrow(gary.communit)/nrow(gary)
gary.involvem<- subset(gary, INVOLVEM==3)
garyi.p<- nrow(gary.involvem)/nrow(gary)
gary.openness<- subset(gary, OPENNESS==3)
garyo.p<- nrow(gary.openness)/nrow(gary)
gary.social_c<- subset(gary, SOCIAL_C==3)
garysc.p<- nrow(gary.social_c)/nrow(gary)


gary09.p<- rbind(c(garyl.p,garyp.p,garyca.p,garybs.p,garyld.p,garye.p,
                   garys.p,garya.p,garyec.p,garyso.p,garyco.p,
                   garyi.p,garyo.p,garysc.p))
colnames(gary09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                       "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


grand.forks<- d.09.sub2[d.09.sub2$City=='Grand Forks, ND',]
grand.forks.loyalty<- subset(grand.forks,LOYALTY==5)
grand.forksl.p<- nrow(grand.forks.loyalty)/nrow(grand.forks)
grand.forks.passion<- subset(grand.forks, PASSION==5)
grand.forksp.p<- nrow(grand.forks.passion)/nrow(grand.forks)
grand.forks.ca<- subset(grand.forks, CA==5)
grand.forksca.p<- nrow(grand.forks.ca)/nrow(grand.forks)
grand.forks.basic_se<- subset(grand.forks, BASIC_SE==3)
grand.forksbs.p<- nrow(grand.forks.basic_se)/nrow(grand.forks)
grand.forks.leadersh<- subset(grand.forks, LEADERSH==3)
grand.forksld.p<- nrow(grand.forks.leadersh)/nrow(grand.forks)
grand.forks.educatio<- subset(grand.forks, EDUCATIO==3)
grand.forkse.p<- nrow(grand.forks.educatio)/nrow(grand.forks)
grand.forks.safety<- subset(grand.forks, SAFETY==3)
grand.forkss.p<- nrow(grand.forks.safety)/nrow(grand.forks)
grand.forks.aestheti<- subset(grand.forks, AESTHETI==3)
grand.forksa.p<- nrow(grand.forks.aestheti)/nrow(grand.forks)
grand.forks.economy<- subset(grand.forks, ECONOMY==3)
grand.forksec.p<- nrow(grand.forks.economy)/nrow(grand.forks)
grand.forks.social_o<- subset(grand.forks, SOCIAL_O==3)
grand.forksso.p<- nrow(grand.forks.social_o)/nrow(grand.forks)
grand.forks.communit<- subset(grand.forks, COMMUNIT==3)
grand.forksco.p<- nrow(grand.forks.communit)/nrow(grand.forks)
grand.forks.involvem<- subset(grand.forks, INVOLVEM==3)
grand.forksi.p<- nrow(grand.forks.involvem)/nrow(grand.forks)
grand.forks.openness<- subset(grand.forks, OPENNESS==3)
grand.forkso.p<- nrow(grand.forks.openness)/nrow(grand.forks)
grand.forks.social_c<- subset(grand.forks, SOCIAL_C==3)
grand.forkssc.p<- nrow(grand.forks.social_c)/nrow(grand.forks)


grand.forks09.p<- rbind(c(grand.forksl.p,grand.forksp.p,grand.forksca.p,grand.forksbs.p,grand.forksld.p,
                          grand.forkse.p,grand.forkss.p,grand.forksa.p,grand.forksec.p,
                          grand.forksso.p,grand.forksco.p,grand.forksi.p,grand.forkso.p,grand.forkssc.p))
colnames(grand.forks09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                              "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


lexington<- d.09.sub2[d.09.sub2$City=='Lexington, KY',]
lexington.loyalty<- subset(lexington,LOYALTY==5)
lexingtonl.p<- nrow(lexington.loyalty)/nrow(lexington)
lexington.passion<- subset(lexington, PASSION==5)
lexingtonp.p<- nrow(lexington.passion)/nrow(lexington)
lexington.ca<- subset(lexington, CA==5)
lexingtonca.p<- nrow(lexington.ca)/nrow(lexington)
lexington.basic_se<- subset(lexington, BASIC_SE==3)
lexingtonbs.p<- nrow(lexington.basic_se)/nrow(lexington)
lexington.leadersh<- subset(lexington, LEADERSH==3)
lexingtonld.p<- nrow(lexington.leadersh)/nrow(lexington)
lexington.educatio<- subset(lexington, EDUCATIO==3)
lexingtone.p<- nrow(lexington.educatio)/nrow(lexington)
lexington.safety<- subset(lexington, SAFETY==3)
lexingtons.p<- nrow(lexington.safety)/nrow(lexington)
lexington.aestheti<- subset(lexington, AESTHETI==3)
lexingtona.p<- nrow(lexington.aestheti)/nrow(lexington)
lexington.economy<- subset(lexington, ECONOMY==3)
lexingtonec.p<- nrow(lexington.economy)/nrow(lexington)
lexington.social_o<- subset(lexington, SOCIAL_O==3)
lexingtonso.p<- nrow(lexington.social_o)/nrow(lexington)
lexington.communit<- subset(lexington, COMMUNIT==3)
lexingtonco.p<- nrow(lexington.communit)/nrow(lexington)
lexington.involvem<- subset(lexington, INVOLVEM==3)
lexingtoni.p<- nrow(lexington.involvem)/nrow(lexington)
lexington.openness<- subset(lexington, OPENNESS==3)
lexingtono.p<- nrow(lexington.openness)/nrow(lexington)
lexington.social_c<- subset(lexington, SOCIAL_C==3)
lexingtonsc.p<- nrow(lexington.social_c)/nrow(lexington)


lexington09.p<- rbind(c(lexingtonl.p,lexingtonp.p,lexingtonca.p,lexingtonbs.p,lexingtonld.p,
                        lexingtone.p,lexingtons.p,lexingtona.p,lexingtonec.p,
                        lexingtonso.p,lexingtonco.p,lexingtoni.p,lexingtono.p,lexingtonsc.p))
colnames(lexington09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

long.beach<- d.09.sub2[d.09.sub2$City=='Long Beach, CA',]
long.beach.loyalty<- subset(long.beach,LOYALTY==5)
long.beachl.p<- nrow(long.beach.loyalty)/nrow(long.beach)
long.beach.passion<- subset(long.beach, PASSION==5)
long.beachp.p<- nrow(long.beach.passion)/nrow(long.beach)
long.beach.ca<- subset(long.beach, CA==5)
long.beachca.p<- nrow(long.beach.ca)/nrow(long.beach)
long.beach.basic_se<- subset(long.beach, BASIC_SE==3)
long.beachbs.p<- nrow(long.beach.basic_se)/nrow(long.beach)
long.beach.leadersh<- subset(long.beach, LEADERSH==3)
long.beachld.p<- nrow(long.beach.leadersh)/nrow(long.beach)
long.beach.educatio<- subset(long.beach, EDUCATIO==3)
long.beache.p<- nrow(long.beach.educatio)/nrow(long.beach)
long.beach.safety<- subset(long.beach, SAFETY==3)
long.beachs.p<- nrow(long.beach.safety)/nrow(long.beach)
long.beach.aestheti<- subset(long.beach, AESTHETI==3)
long.beacha.p<- nrow(long.beach.aestheti)/nrow(long.beach)
long.beach.economy<- subset(long.beach, ECONOMY==3)
long.beachec.p<- nrow(long.beach.economy)/nrow(long.beach)
long.beach.social_o<- subset(long.beach, SOCIAL_O==3)
long.beachso.p<- nrow(long.beach.social_o)/nrow(long.beach)
long.beach.communit<- subset(long.beach, COMMUNIT==3)
long.beachco.p<- nrow(long.beach.communit)/nrow(long.beach)
long.beach.involvem<- subset(long.beach, INVOLVEM==3)
long.beachi.p<- nrow(long.beach.involvem)/nrow(long.beach)
long.beach.openness<- subset(long.beach, OPENNESS==3)
long.beacho.p<- nrow(long.beach.openness)/nrow(long.beach)
long.beach.social_c<- subset(long.beach, SOCIAL_C==3)
long.beachsc.p<- nrow(long.beach.social_c)/nrow(long.beach)


long.beach09.p<- rbind(c(long.beachl.p,long.beachp.p,long.beachca.p,long.beachbs.p,long.beachld.p,
                         long.beache.p,long.beachs.p,long.beacha.p,long.beachec.p,
                         long.beachso.p,long.beachco.p,long.beachi.p,long.beacho.p,long.beachsc.p))
colnames(long.beach09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

macon<- d.09.sub2[d.09.sub2$City=='Macon, GA',]
macon.loyalty<- subset(macon,LOYALTY==5)
maconl.p<- nrow(macon.loyalty)/nrow(macon)
macon.passion<- subset(macon, PASSION==5)
maconp.p<- nrow(macon.passion)/nrow(macon)
macon.ca<- subset(macon, CA==5)
maconca.p<- nrow(macon.ca)/nrow(macon)
macon.basic_se<- subset(macon, BASIC_SE==3)
maconbs.p<- nrow(macon.basic_se)/nrow(macon)
macon.leadersh<- subset(macon, LEADERSH==3)
maconld.p<- nrow(macon.leadersh)/nrow(macon)
macon.educatio<- subset(macon, EDUCATIO==3)
macone.p<- nrow(macon.educatio)/nrow(macon)
macon.safety<- subset(macon, SAFETY==3)
macons.p<- nrow(macon.safety)/nrow(macon)
macon.aestheti<- subset(macon, AESTHETI==3)
macona.p<- nrow(macon.aestheti)/nrow(macon)
macon.economy<- subset(macon, ECONOMY==3)
maconec.p<- nrow(macon.economy)/nrow(macon)
macon.social_o<- subset(macon, SOCIAL_O==3)
maconso.p<- nrow(macon.social_o)/nrow(macon)
macon.communit<- subset(macon, COMMUNIT==3)
maconco.p<- nrow(macon.communit)/nrow(macon)
macon.involvem<- subset(macon, INVOLVEM==3)
maconi.p<- nrow(macon.involvem)/nrow(macon)
macon.openness<- subset(macon, OPENNESS==3)
macono.p<- nrow(macon.openness)/nrow(macon)
macon.social_c<- subset(macon, SOCIAL_C==3)
maconsc.p<- nrow(macon.social_c)/nrow(macon)


macon09.p<- rbind(c(maconl.p,maconp.p,maconca.p,maconbs.p,maconld.p,
                    macone.p,macons.p,macona.p,maconec.p,
                    maconso.p,maconco.p,maconi.p,macono.p,maconsc.p))
colnames(macon09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

miami<- d.09.sub2[d.09.sub2$City=='Miami, FL',]
miami.loyalty<- subset(miami,LOYALTY==5)
miamil.p<- nrow(miami.loyalty)/nrow(miami)
miami.passion<- subset(miami, PASSION==5)
miamip.p<- nrow(miami.passion)/nrow(miami)
miami.ca<- subset(miami, CA==5)
miamica.p<- nrow(miami.ca)/nrow(miami)
miami.basic_se<- subset(miami, BASIC_SE==3)
miamibs.p<- nrow(miami.basic_se)/nrow(miami)
miami.leadersh<- subset(miami, LEADERSH==3)
miamild.p<- nrow(miami.leadersh)/nrow(miami)
miami.educatio<- subset(miami, EDUCATIO==3)
miamie.p<- nrow(miami.educatio)/nrow(miami)
miami.safety<- subset(miami, SAFETY==3)
miamis.p<- nrow(miami.safety)/nrow(miami)
miami.aestheti<- subset(miami, AESTHETI==3)
miamia.p<- nrow(miami.aestheti)/nrow(miami)
miami.economy<- subset(miami, ECONOMY==3)
miamiec.p<- nrow(miami.economy)/nrow(miami)
miami.social_o<- subset(miami, SOCIAL_O==3)
miamiso.p<- nrow(miami.social_o)/nrow(miami)
miami.communit<- subset(miami, COMMUNIT==3)
miamico.p<- nrow(miami.communit)/nrow(miami)
miami.involvem<- subset(miami, INVOLVEM==3)
miamii.p<- nrow(miami.involvem)/nrow(miami)
miami.openness<- subset(miami, OPENNESS==3)
miamio.p<- nrow(miami.openness)/nrow(miami)
miami.social_c<- subset(miami, SOCIAL_C==3)
miamisc.p<- nrow(miami.social_c)/nrow(miami)


miami09.p<- rbind(c(miamil.p,miamip.p,miamica.p,miamibs.p,miamild.p,
                    miamie.p,miamis.p,miamia.p,miamiec.p,
                    miamiso.p,miamico.p,miamii.p,miamio.p,miamisc.p))
colnames(miami09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

milledgeville<- d.09.sub2[d.09.sub2$City=='Milledgeville, GA',]
milledgeville.loyalty<- subset(milledgeville,LOYALTY==5)
milledgevillel.p<- nrow(milledgeville.loyalty)/nrow(milledgeville)
milledgeville.passion<- subset(milledgeville, PASSION==5)
milledgevillep.p<- nrow(milledgeville.passion)/nrow(milledgeville)
milledgeville.ca<- subset(milledgeville, CA==5)
milledgevilleca.p<- nrow(milledgeville.ca)/nrow(milledgeville)
milledgeville.basic_se<- subset(milledgeville, BASIC_SE==3)
milledgevillebs.p<- nrow(milledgeville.basic_se)/nrow(milledgeville)
milledgeville.leadersh<- subset(milledgeville, LEADERSH==3)
milledgevilleld.p<- nrow(milledgeville.leadersh)/nrow(milledgeville)
milledgeville.educatio<- subset(milledgeville, EDUCATIO==3)
milledgevillee.p<- nrow(milledgeville.educatio)/nrow(milledgeville)
milledgeville.safety<- subset(milledgeville, SAFETY==3)
milledgevilles.p<- nrow(milledgeville.safety)/nrow(milledgeville)
milledgeville.aestheti<- subset(milledgeville, AESTHETI==3)
milledgevillea.p<- nrow(milledgeville.aestheti)/nrow(milledgeville)
milledgeville.economy<- subset(milledgeville, ECONOMY==3)
milledgevilleec.p<- nrow(milledgeville.economy)/nrow(milledgeville)
milledgeville.social_o<- subset(milledgeville, SOCIAL_O==3)
milledgevilleso.p<- nrow(milledgeville.social_o)/nrow(milledgeville)
milledgeville.communit<- subset(milledgeville, COMMUNIT==3)
milledgevilleco.p<- nrow(milledgeville.communit)/nrow(milledgeville)
milledgeville.involvem<- subset(milledgeville, INVOLVEM==3)
milledgevillei.p<- nrow(milledgeville.involvem)/nrow(milledgeville)
milledgeville.openness<- subset(milledgeville, OPENNESS==3)
milledgevilleo.p<- nrow(milledgeville.openness)/nrow(milledgeville)
milledgeville.social_c<- subset(milledgeville, SOCIAL_C==3)
milledgevillesc.p<- nrow(milledgeville.social_c)/nrow(milledgeville)


milledgeville09.p<- rbind(c(milledgevillel.p,milledgevillep.p,milledgevilleca.p,milledgevillebs.p,
                            milledgevilleld.p,milledgevillee.p,milledgevilles.p,milledgevillea.p,
                            milledgevilleec.p,milledgevilleso.p,milledgevilleco.p,milledgevillei.p,
                            milledgevilleo.p,milledgevillesc.p))
colnames(milledgeville09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                                "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


myrtle.beach<- d.09.sub2[d.09.sub2$City=='Myrtle Beach, SC',]
myrtle.beach.loyalty<- subset(myrtle.beach,LOYALTY==5)
myrtle.beachl.p<- nrow(myrtle.beach.loyalty)/nrow(myrtle.beach)
myrtle.beach.passion<- subset(myrtle.beach, PASSION==5)
myrtle.beachp.p<- nrow(myrtle.beach.passion)/nrow(myrtle.beach)
myrtle.beach.ca<- subset(myrtle.beach, CA==5)
myrtle.beachca.p<- nrow(myrtle.beach.ca)/nrow(myrtle.beach)
myrtle.beach.basic_se<- subset(myrtle.beach, BASIC_SE==3)
myrtle.beachbs.p<- nrow(myrtle.beach.basic_se)/nrow(myrtle.beach)
myrtle.beach.leadersh<- subset(myrtle.beach, LEADERSH==3)
myrtle.beachld.p<- nrow(myrtle.beach.leadersh)/nrow(myrtle.beach)
myrtle.beach.educatio<- subset(myrtle.beach, EDUCATIO==3)
myrtle.beache.p<- nrow(myrtle.beach.educatio)/nrow(myrtle.beach)
myrtle.beach.safety<- subset(myrtle.beach, SAFETY==3)
myrtle.beachs.p<- nrow(myrtle.beach.safety)/nrow(myrtle.beach)
myrtle.beach.aestheti<- subset(myrtle.beach, AESTHETI==3)
myrtle.beacha.p<- nrow(myrtle.beach.aestheti)/nrow(myrtle.beach)
myrtle.beach.economy<- subset(myrtle.beach, ECONOMY==3)
myrtle.beachec.p<- nrow(myrtle.beach.economy)/nrow(myrtle.beach)
myrtle.beach.social_o<- subset(myrtle.beach, SOCIAL_O==3)
myrtle.beachso.p<- nrow(myrtle.beach.social_o)/nrow(myrtle.beach)
myrtle.beach.communit<- subset(myrtle.beach, COMMUNIT==3)
myrtle.beachco.p<- nrow(myrtle.beach.communit)/nrow(myrtle.beach)
myrtle.beach.involvem<- subset(myrtle.beach, INVOLVEM==3)
myrtle.beachi.p<- nrow(myrtle.beach.involvem)/nrow(myrtle.beach)
myrtle.beach.openness<- subset(myrtle.beach, OPENNESS==3)
myrtle.beacho.p<- nrow(myrtle.beach.openness)/nrow(myrtle.beach)
myrtle.beach.social_c<- subset(myrtle.beach, SOCIAL_C==3)
myrtle.beachsc.p<- nrow(myrtle.beach.social_c)/nrow(myrtle.beach)


myrtle.beach09.p<- rbind(c(myrtle.beachl.p,myrtle.beachp.p,myrtle.beachca.p,myrtle.beachbs.p,
                           myrtle.beachld.p,myrtle.beache.p,myrtle.beachs.p,myrtle.beacha.p,
                           myrtle.beachec.p,myrtle.beachso.p,myrtle.beachco.p,myrtle.beachi.p,
                           myrtle.beacho.p,myrtle.beachsc.p))
colnames(myrtle.beach09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                               "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


palm.beach<- d.09.sub2[d.09.sub2$City=='Palm Beach, FL',]
palm.beach.loyalty<- subset(palm.beach,LOYALTY==5)
palm.beachl.p<- nrow(palm.beach.loyalty)/nrow(palm.beach)
palm.beach.passion<- subset(palm.beach, PASSION==5)
palm.beachp.p<- nrow(palm.beach.passion)/nrow(palm.beach)
palm.beach.ca<- subset(palm.beach, CA==5)
palm.beachca.p<- nrow(palm.beach.ca)/nrow(palm.beach)
palm.beach.basic_se<- subset(palm.beach, BASIC_SE==3)
palm.beachbs.p<- nrow(palm.beach.basic_se)/nrow(palm.beach)
palm.beach.leadersh<- subset(palm.beach, LEADERSH==3)
palm.beachld.p<- nrow(palm.beach.leadersh)/nrow(palm.beach)
palm.beach.educatio<- subset(palm.beach, EDUCATIO==3)
palm.beache.p<- nrow(palm.beach.educatio)/nrow(palm.beach)
palm.beach.safety<- subset(palm.beach, SAFETY==3)
palm.beachs.p<- nrow(palm.beach.safety)/nrow(palm.beach)
palm.beach.aestheti<- subset(palm.beach, AESTHETI==3)
palm.beacha.p<- nrow(palm.beach.aestheti)/nrow(palm.beach)
palm.beach.economy<- subset(palm.beach, ECONOMY==3)
palm.beachec.p<- nrow(palm.beach.economy)/nrow(palm.beach)
palm.beach.social_o<- subset(palm.beach, SOCIAL_O==3)
palm.beachso.p<- nrow(palm.beach.social_o)/nrow(palm.beach)
palm.beach.communit<- subset(palm.beach, COMMUNIT==3)
palm.beachco.p<- nrow(palm.beach.communit)/nrow(palm.beach)
palm.beach.involvem<- subset(palm.beach, INVOLVEM==3)
palm.beachi.p<- nrow(palm.beach.involvem)/nrow(palm.beach)
palm.beach.openness<- subset(palm.beach, OPENNESS==3)
palm.beacho.p<- nrow(palm.beach.openness)/nrow(palm.beach)
palm.beach.social_c<- subset(palm.beach, SOCIAL_C==3)
palm.beachsc.p<- nrow(palm.beach.social_c)/nrow(palm.beach)


palm.beach09.p<- rbind(c(palm.beachl.p,palm.beachp.p,palm.beachca.p,palm.beachbs.p,
                         palm.beachld.p,palm.beache.p,palm.beachs.p,palm.beacha.p,
                         palm.beachec.p,palm.beachso.p,palm.beachco.p,palm.beachi.p,
                         palm.beacho.p,palm.beachsc.p))
colnames(palm.beach09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

philadelphia<- d.09.sub2[d.09.sub2$City=='Philadelphia, PA',]
philadelphia.loyalty<- subset(philadelphia,LOYALTY==5)
philadelphial.p<- nrow(philadelphia.loyalty)/nrow(philadelphia)
philadelphia.passion<- subset(philadelphia, PASSION==5)
philadelphiap.p<- nrow(philadelphia.passion)/nrow(philadelphia)
philadelphia.ca<- subset(philadelphia, CA==5)
philadelphiaca.p<- nrow(philadelphia.ca)/nrow(philadelphia)
philadelphia.basic_se<- subset(philadelphia, BASIC_SE==3)
philadelphiabs.p<- nrow(philadelphia.basic_se)/nrow(philadelphia)
philadelphia.leadersh<- subset(philadelphia, LEADERSH==3)
philadelphiald.p<- nrow(philadelphia.leadersh)/nrow(philadelphia)
philadelphia.educatio<- subset(philadelphia, EDUCATIO==3)
philadelphiae.p<- nrow(philadelphia.educatio)/nrow(philadelphia)
philadelphia.safety<- subset(philadelphia, SAFETY==3)
philadelphias.p<- nrow(philadelphia.safety)/nrow(philadelphia)
philadelphia.aestheti<- subset(philadelphia, AESTHETI==3)
philadelphiaa.p<- nrow(philadelphia.aestheti)/nrow(philadelphia)
philadelphia.economy<- subset(philadelphia, ECONOMY==3)
philadelphiaec.p<- nrow(philadelphia.economy)/nrow(philadelphia)
philadelphia.social_o<- subset(philadelphia, SOCIAL_O==3)
philadelphiaso.p<- nrow(philadelphia.social_o)/nrow(philadelphia)
philadelphia.communit<- subset(philadelphia, COMMUNIT==3)
philadelphiaco.p<- nrow(philadelphia.communit)/nrow(philadelphia)
philadelphia.involvem<- subset(philadelphia, INVOLVEM==3)
philadelphiai.p<- nrow(philadelphia.involvem)/nrow(philadelphia)
philadelphia.openness<- subset(philadelphia, OPENNESS==3)
philadelphiao.p<- nrow(philadelphia.openness)/nrow(philadelphia)
philadelphia.social_c<- subset(philadelphia, SOCIAL_C==3)
philadelphiasc.p<- nrow(philadelphia.social_c)/nrow(philadelphia)


philadelphia09.p<- rbind(c(philadelphial.p,philadelphiap.p,philadelphiaca.p,philadelphiabs.p,
                           philadelphiald.p,philadelphiae.p,philadelphias.p,philadelphiaa.p,
                           philadelphiaec.p,philadelphiaso.p,philadelphiaco.p,philadelphiai.p,
                           philadelphiao.p,philadelphiasc.p))
colnames(philadelphia09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                               "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

san.jose<- d.09.sub2[d.09.sub2$City=='San Jose, CA',]
san.jose.loyalty<- subset(san.jose,LOYALTY==5)
san.josel.p<- nrow(san.jose.loyalty)/nrow(san.jose)
san.jose.passion<- subset(san.jose, PASSION==5)
san.josep.p<- nrow(san.jose.passion)/nrow(san.jose)
san.jose.ca<- subset(san.jose, CA==5)
san.joseca.p<- nrow(san.jose.ca)/nrow(san.jose)
san.jose.basic_se<- subset(san.jose, BASIC_SE==3)
san.josebs.p<- nrow(san.jose.basic_se)/nrow(san.jose)
san.jose.leadersh<- subset(san.jose, LEADERSH==3)
san.joseld.p<- nrow(san.jose.leadersh)/nrow(san.jose)
san.jose.educatio<- subset(san.jose, EDUCATIO==3)
san.josee.p<- nrow(san.jose.educatio)/nrow(san.jose)
san.jose.safety<- subset(san.jose, SAFETY==3)
san.joses.p<- nrow(san.jose.safety)/nrow(san.jose)
san.jose.aestheti<- subset(san.jose, AESTHETI==3)
san.josea.p<- nrow(san.jose.aestheti)/nrow(san.jose)
san.jose.economy<- subset(san.jose, ECONOMY==3)
san.joseec.p<- nrow(san.jose.economy)/nrow(san.jose)
san.jose.social_o<- subset(san.jose, SOCIAL_O==3)
san.joseso.p<- nrow(san.jose.social_o)/nrow(san.jose)
san.jose.communit<- subset(san.jose, COMMUNIT==3)
san.joseco.p<- nrow(san.jose.communit)/nrow(san.jose)
san.jose.involvem<- subset(san.jose, INVOLVEM==3)
san.josei.p<- nrow(san.jose.involvem)/nrow(san.jose)
san.jose.openness<- subset(san.jose, OPENNESS==3)
san.joseo.p<- nrow(san.jose.openness)/nrow(san.jose)
san.jose.social_c<- subset(san.jose, SOCIAL_C==3)
san.josesc.p<- nrow(san.jose.social_c)/nrow(san.jose)


san.jose09.p<- rbind(c(san.josel.p,san.josep.p,san.joseca.p,san.josebs.p,
                       san.joseld.p,san.josee.p,san.joses.p,san.josea.p,
                       san.joseec.p,san.joseso.p,san.joseco.p,san.josei.p,
                       san.joseo.p,san.josesc.p))
colnames(san.jose09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

st.paul<- d.09.sub2[d.09.sub2$City=='St. Paul, MN',]
st.paul.loyalty<- subset(st.paul,LOYALTY==5)
st.paull.p<- nrow(st.paul.loyalty)/nrow(st.paul)
st.paul.passion<- subset(st.paul, PASSION==5)
st.paulp.p<- nrow(st.paul.passion)/nrow(st.paul)
st.paul.ca<- subset(st.paul, CA==5)
st.paulca.p<- nrow(st.paul.ca)/nrow(st.paul)
st.paul.basic_se<- subset(st.paul, BASIC_SE==3)
st.paulbs.p<- nrow(st.paul.basic_se)/nrow(st.paul)
st.paul.leadersh<- subset(st.paul, LEADERSH==3)
st.paulld.p<- nrow(st.paul.leadersh)/nrow(st.paul)
st.paul.educatio<- subset(st.paul, EDUCATIO==3)
st.paule.p<- nrow(st.paul.educatio)/nrow(st.paul)
st.paul.safety<- subset(st.paul, SAFETY==3)
st.pauls.p<- nrow(st.paul.safety)/nrow(st.paul)
st.paul.aestheti<- subset(st.paul, AESTHETI==3)
st.paula.p<- nrow(st.paul.aestheti)/nrow(st.paul)
st.paul.economy<- subset(st.paul, ECONOMY==3)
st.paulec.p<- nrow(st.paul.economy)/nrow(st.paul)
st.paul.social_o<- subset(st.paul, SOCIAL_O==3)
st.paulso.p<- nrow(st.paul.social_o)/nrow(st.paul)
st.paul.communit<- subset(st.paul, COMMUNIT==3)
st.paulco.p<- nrow(st.paul.communit)/nrow(st.paul)
st.paul.involvem<- subset(st.paul, INVOLVEM==3)
st.pauli.p<- nrow(st.paul.involvem)/nrow(st.paul)
st.paul.openness<- subset(st.paul, OPENNESS==3)
st.paulo.p<- nrow(st.paul.openness)/nrow(st.paul)
st.paul.social_c<- subset(st.paul, SOCIAL_C==3)
st.paulsc.p<- nrow(st.paul.social_c)/nrow(st.paul)


st.paul09.p<- rbind(c(st.paull.p,st.paulp.p,st.paulca.p,st.paulbs.p,
                      st.paulld.p,st.paule.p,st.pauls.p,st.paula.p,
                      st.paulec.p,st.paulso.p,st.paulco.p,st.pauli.p,
                      st.paulo.p,st.paulsc.p))
colnames(st.paul09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

state.college<- d.09.sub2[d.09.sub2$City=='State College, PA',]
state.college.loyalty<- subset(state.college,LOYALTY==5)
state.collegel.p<- nrow(state.college.loyalty)/nrow(state.college)
state.college.passion<- subset(state.college, PASSION==5)
state.collegep.p<- nrow(state.college.passion)/nrow(state.college)
state.college.ca<- subset(state.college, CA==5)
state.collegeca.p<- nrow(state.college.ca)/nrow(state.college)
state.college.basic_se<- subset(state.college, BASIC_SE==3)
state.collegebs.p<- nrow(state.college.basic_se)/nrow(state.college)
state.college.leadersh<- subset(state.college, LEADERSH==3)
state.collegeld.p<- nrow(state.college.leadersh)/nrow(state.college)
state.college.educatio<- subset(state.college, EDUCATIO==3)
state.collegee.p<- nrow(state.college.educatio)/nrow(state.college)
state.college.safety<- subset(state.college, SAFETY==3)
state.colleges.p<- nrow(state.college.safety)/nrow(state.college)
state.college.aestheti<- subset(state.college, AESTHETI==3)
state.collegea.p<- nrow(state.college.aestheti)/nrow(state.college)
state.college.economy<- subset(state.college, ECONOMY==3)
state.collegeec.p<- nrow(state.college.economy)/nrow(state.college)
state.college.social_o<- subset(state.college, SOCIAL_O==3)
state.collegeso.p<- nrow(state.college.social_o)/nrow(state.college)
state.college.communit<- subset(state.college, COMMUNIT==3)
state.collegeco.p<- nrow(state.college.communit)/nrow(state.college)
state.college.involvem<- subset(state.college, INVOLVEM==3)
state.collegei.p<- nrow(state.college.involvem)/nrow(state.college)
state.college.openness<- subset(state.college, OPENNESS==3)
state.collegeo.p<- nrow(state.college.openness)/nrow(state.college)
state.college.social_c<- subset(state.college, SOCIAL_C==3)
state.collegesc.p<- nrow(state.college.social_c)/nrow(state.college)


state.college09.p<- rbind(c(state.collegel.p,state.collegep.p,state.collegeca.p,state.collegebs.p,
                            state.collegeld.p,state.collegee.p,state.colleges.p,state.collegea.p,
                            state.collegeec.p,state.collegeso.p,state.collegeco.p,state.collegei.p,
                            state.collegeo.p,state.collegesc.p))
colnames(state.college09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                                "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


tallahassee<- d.09.sub2[d.09.sub2$City=='Tallahassee, FL',]
tallahassee.loyalty<- subset(tallahassee,LOYALTY==5)
tallahasseel.p<- nrow(tallahassee.loyalty)/nrow(tallahassee)
tallahassee.passion<- subset(tallahassee, PASSION==5)
tallahasseep.p<- nrow(tallahassee.passion)/nrow(tallahassee)
tallahassee.ca<- subset(tallahassee, CA==5)
tallahasseeca.p<- nrow(tallahassee.ca)/nrow(tallahassee)
tallahassee.basic_se<- subset(tallahassee, BASIC_SE==3)
tallahasseebs.p<- nrow(tallahassee.basic_se)/nrow(tallahassee)
tallahassee.leadersh<- subset(tallahassee, LEADERSH==3)
tallahasseeld.p<- nrow(tallahassee.leadersh)/nrow(tallahassee)
tallahassee.educatio<- subset(tallahassee, EDUCATIO==3)
tallahasseee.p<- nrow(tallahassee.educatio)/nrow(tallahassee)
tallahassee.safety<- subset(tallahassee, SAFETY==3)
tallahassees.p<- nrow(tallahassee.safety)/nrow(tallahassee)
tallahassee.aestheti<- subset(tallahassee, AESTHETI==3)
tallahasseea.p<- nrow(tallahassee.aestheti)/nrow(tallahassee)
tallahassee.economy<- subset(tallahassee, ECONOMY==3)
tallahasseeec.p<- nrow(tallahassee.economy)/nrow(tallahassee)
tallahassee.social_o<- subset(tallahassee, SOCIAL_O==3)
tallahasseeso.p<- nrow(tallahassee.social_o)/nrow(tallahassee)
tallahassee.communit<- subset(tallahassee, COMMUNIT==3)
tallahasseeco.p<- nrow(tallahassee.communit)/nrow(tallahassee)
tallahassee.involvem<- subset(tallahassee, INVOLVEM==3)
tallahasseei.p<- nrow(tallahassee.involvem)/nrow(tallahassee)
tallahassee.openness<- subset(tallahassee, OPENNESS==3)
tallahasseeo.p<- nrow(tallahassee.openness)/nrow(tallahassee)
tallahassee.social_c<- subset(tallahassee, SOCIAL_C==3)
tallahasseesc.p<- nrow(tallahassee.social_c)/nrow(tallahassee)


tallahassee09.p<- rbind(c(tallahasseel.p,tallahasseep.p,tallahasseeca.p,tallahasseebs.p,
                          tallahasseeld.p,tallahasseee.p,tallahassees.p,tallahasseea.p,
                          tallahasseeec.p,tallahasseeso.p,tallahasseeco.p,tallahasseei.p,
                          tallahasseeo.p,tallahasseesc.p))
colnames(tallahassee09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                              "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

wichita<- d.09.sub2[d.09.sub2$City=='Wichita, KS',]
wichita.loyalty<- subset(wichita,LOYALTY==5)
wichital.p<- nrow(wichita.loyalty)/nrow(wichita)
wichita.passion<- subset(wichita, PASSION==5)
wichitap.p<- nrow(wichita.passion)/nrow(wichita)
wichita.ca<- subset(wichita, CA==5)
wichitaca.p<- nrow(wichita.ca)/nrow(wichita)
wichita.basic_se<- subset(wichita, BASIC_SE==3)
wichitabs.p<- nrow(wichita.basic_se)/nrow(wichita)
wichita.leadersh<- subset(wichita, LEADERSH==3)
wichitald.p<- nrow(wichita.leadersh)/nrow(wichita)
wichita.educatio<- subset(wichita, EDUCATIO==3)
wichitae.p<- nrow(wichita.educatio)/nrow(wichita)
wichita.safety<- subset(wichita, SAFETY==3)
wichitas.p<- nrow(wichita.safety)/nrow(wichita)
wichita.aestheti<- subset(wichita, AESTHETI==3)
wichitaa.p<- nrow(wichita.aestheti)/nrow(wichita)
wichita.economy<- subset(wichita, ECONOMY==3)
wichitaec.p<- nrow(wichita.economy)/nrow(wichita)
wichita.social_o<- subset(wichita, SOCIAL_O==3)
wichitaso.p<- nrow(wichita.social_o)/nrow(wichita)
wichita.communit<- subset(wichita, COMMUNIT==3)
wichitaco.p<- nrow(wichita.communit)/nrow(wichita)
wichita.involvem<- subset(wichita, INVOLVEM==3)
wichitai.p<- nrow(wichita.involvem)/nrow(wichita)
wichita.openness<- subset(wichita, OPENNESS==3)
wichitao.p<- nrow(wichita.openness)/nrow(wichita)
wichita.social_c<- subset(wichita, SOCIAL_C==3)
wichitasc.p<- nrow(wichita.social_c)/nrow(wichita)


wichita09.p<- rbind(c(wichital.p,wichitap.p,wichitaca.p,wichitabs.p,
                      wichitald.p,wichitae.p,wichitas.p,wichitaa.p,
                      wichitaec.p,wichitaso.p,wichitaco.p,wichitai.p,
                      wichitao.p,wichitasc.p))
colnames(wichita09.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

prop09<- rbind(aberdeen09.p,akron09.p,biloxi09.p,boulder09.p,bradenton09.p,
               charlotte09.p,columbia09.p,columbus09.p,detroit09.p,duluth09.p,
               fort.wayne09.p,gary09.p,grand.forks09.p,lexington09.p,
               long.beach09.p,macon09.p,miami09.p,milledgeville09.p,
               myrtle.beach09.p,palm.beach09.p,philadelphia09.p,
               san.jose09.p,st.paul09.p,state.college09.p,
               tallahassee09.p,wichita09.p)
prop09<- data.frame(prop09)

prop09$City<- City

Year09<- rep(2009,26)


prop09$Year<- Year09


aberdeen<- d.10.sub2[d.10.sub2$City=='Aberdeen, SD',]
aberdeen.loyalty<- subset(aberdeen,LOYALTY==5)
al.p<- nrow(aberdeen.loyalty)/nrow(aberdeen)
aberdeen.passion<- subset(aberdeen, PASSION==5)
ap.p<- nrow(aberdeen.passion)/nrow(aberdeen)
aberdeen.ca<- subset(aberdeen, CA==5)
aca.p<- nrow(aberdeen.ca)/nrow(aberdeen)
aberdeen.basic_se<- subset(aberdeen, BASIC_SE==3)
abs.p<- nrow(aberdeen.basic_se)/nrow(aberdeen)
aberdeen.leadersh<- subset(aberdeen, LEADERSH==3)
ald.p<- nrow(aberdeen.leadersh)/nrow(aberdeen)
aberdeen.educatio<- subset(aberdeen, EDUCATIO==3)
ae.p<- nrow(aberdeen.educatio)/nrow(aberdeen)
aberdeen.safety<- subset(aberdeen, SAFETY==3)
as.p<- nrow(aberdeen.safety)/nrow(aberdeen)
aberdeen.aestheti<- subset(aberdeen, AESTHETI==3)
aa.p<- nrow(aberdeen.aestheti)/nrow(aberdeen)
aberdeen.economy<- subset(aberdeen, ECONOMY==3)
aec.p<- nrow(aberdeen.economy)/nrow(aberdeen)
aberdeen.social_o<- subset(aberdeen, SOCIAL_O==3)
aso.p<- nrow(aberdeen.social_o)/nrow(aberdeen)
aberdeen.communit<- subset(aberdeen, COMMUNIT==3)
aco.p<- nrow(aberdeen.communit)/nrow(aberdeen)
aberdeen.involvem<- subset(aberdeen, INVOLVEM==3)
ai.p<- nrow(aberdeen.involvem)/nrow(aberdeen)
aberdeen.openness<- subset(aberdeen, OPENNESS==3)
ao.p<- nrow(aberdeen.openness)/nrow(aberdeen)
aberdeen.social_c<- subset(aberdeen, SOCIAL_C==3)
asc.p<- nrow(aberdeen.social_c)/nrow(aberdeen)

aberdeen10.p<- rbind(c(al.p,ap.p,aca.p,abs.p,ald.p,ae.p,as.p,aa.p,aec.p,aso.p,aco.p,ai.p,ao.p,asc.p))
colnames(aberdeen10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

akron<- d.10.sub2[d.10.sub2$City=='Akron, OH',]
akron.loyalty<- subset(akron,LOYALTY==5)
akronl.p<- nrow(akron.loyalty)/nrow(akron)
akron.passion<- subset(akron, PASSION==5)
akronp.p<- nrow(akron.passion)/nrow(akron)
akron.ca<- subset(akron, CA==5)
akronca.p<- nrow(akron.ca)/nrow(akron)
akron.basic_se<- subset(akron, BASIC_SE==3)
akronbs.p<- nrow(akron.basic_se)/nrow(akron)
akron.leadersh<- subset(akron, LEADERSH==3)
akronld.p<- nrow(akron.leadersh)/nrow(akron)
akron.educatio<- subset(akron, EDUCATIO==3)
akrone.p<- nrow(akron.educatio)/nrow(akron)
akron.safety<- subset(akron, SAFETY==3)
akrons.p<- nrow(akron.safety)/nrow(akron)
akron.aestheti<- subset(akron, AESTHETI==3)
akrona.p<- nrow(akron.aestheti)/nrow(akron)
akron.economy<- subset(akron, ECONOMY==3)
akronec.p<- nrow(akron.economy)/nrow(akron)
akron.social_o<- subset(akron, SOCIAL_O==3)
akronso.p<- nrow(akron.social_o)/nrow(akron)
akron.communit<- subset(akron, COMMUNIT==3)
akronco.p<- nrow(akron.communit)/nrow(akron)
akron.involvem<- subset(akron, INVOLVEM==3)
akroni.p<- nrow(akron.involvem)/nrow(akron)
akron.openness<- subset(akron, OPENNESS==3)
akrono.p<- nrow(akron.openness)/nrow(akron)
akron.social_c<- subset(akron, SOCIAL_C==3)
akronsc.p<- nrow(akron.social_c)/nrow(akron)

akron10.p<- rbind(c(akronl.p,akronp.p,akronca.p,akronbs.p,akronld.p,akrone.p,
                    akrons.p,akrona.p,akronec.p,akronso.p,akronco.p,akroni.p,akrono.p,akronsc.p))
colnames(akron10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

biloxi<- d.10.sub2[d.10.sub2$City=='Biloxi, MS',]
biloxi.loyalty<- subset(biloxi,LOYALTY==5)
biloxil.p<- nrow(biloxi.loyalty)/nrow(biloxi)
biloxi.passion<- subset(biloxi, PASSION==5)
biloxip.p<- nrow(biloxi.passion)/nrow(biloxi)
biloxi.ca<- subset(biloxi, CA==5)
biloxica.p<- nrow(biloxi.ca)/nrow(biloxi)
biloxi.basic_se<- subset(biloxi, BASIC_SE==3)
biloxibs.p<- nrow(biloxi.basic_se)/nrow(biloxi)
biloxi.leadersh<- subset(biloxi, LEADERSH==3)
biloxild.p<- nrow(biloxi.leadersh)/nrow(biloxi)
biloxi.educatio<- subset(biloxi, EDUCATIO==3)
biloxie.p<- nrow(biloxi.educatio)/nrow(biloxi)
biloxi.safety<- subset(biloxi, SAFETY==3)
biloxis.p<- nrow(biloxi.safety)/nrow(biloxi)
biloxi.aestheti<- subset(biloxi, AESTHETI==3)
biloxia.p<- nrow(biloxi.aestheti)/nrow(biloxi)
biloxi.economy<- subset(biloxi, ECONOMY==3)
biloxiec.p<- nrow(biloxi.economy)/nrow(biloxi)
biloxi.social_o<- subset(biloxi, SOCIAL_O==3)
biloxiso.p<- nrow(biloxi.social_o)/nrow(biloxi)
biloxi.communit<- subset(biloxi, COMMUNIT==3)
biloxico.p<- nrow(biloxi.communit)/nrow(biloxi)
biloxi.involvem<- subset(biloxi, INVOLVEM==3)
biloxii.p<- nrow(biloxi.involvem)/nrow(biloxi)
biloxi.openness<- subset(biloxi, OPENNESS==3)
biloxio.p<- nrow(biloxi.openness)/nrow(biloxi)
biloxi.social_c<- subset(biloxi, SOCIAL_C==3)
biloxisc.p<- nrow(biloxi.social_c)/nrow(biloxi)

biloxi10.p<- rbind(c(biloxil.p,biloxip.p,biloxica.p,biloxibs.p,biloxild.p,biloxie.p,
                     biloxis.p,biloxia.p,biloxiec.p,biloxiso.p,biloxico.p,biloxii.p,biloxio.p,biloxisc.p))
colnames(biloxi10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

boulder<- d.10.sub2[d.10.sub2$City=='Boulder, CO',]
boulder.loyalty<- subset(boulder,LOYALTY==5)
boulderl.p<- nrow(boulder.loyalty)/nrow(boulder)
boulder.passion<- subset(boulder, PASSION==5)
boulderp.p<- nrow(boulder.passion)/nrow(boulder)
boulder.ca<- subset(boulder, CA==5)
boulderca.p<- nrow(boulder.ca)/nrow(boulder)
boulder.basic_se<- subset(boulder, BASIC_SE==3)
boulderbs.p<- nrow(boulder.basic_se)/nrow(boulder)
boulder.leadersh<- subset(boulder, LEADERSH==3)
boulderld.p<- nrow(boulder.leadersh)/nrow(boulder)
boulder.educatio<- subset(boulder, EDUCATIO==3)
bouldere.p<- nrow(boulder.educatio)/nrow(boulder)
boulder.safety<- subset(boulder, SAFETY==3)
boulders.p<- nrow(boulder.safety)/nrow(boulder)
boulder.aestheti<- subset(boulder, AESTHETI==3)
bouldera.p<- nrow(boulder.aestheti)/nrow(boulder)
boulder.economy<- subset(boulder, ECONOMY==3)
boulderec.p<- nrow(boulder.economy)/nrow(boulder)
boulder.social_o<- subset(boulder, SOCIAL_O==3)
boulderso.p<- nrow(boulder.social_o)/nrow(boulder)
boulder.communit<- subset(boulder, COMMUNIT==3)
boulderco.p<- nrow(boulder.communit)/nrow(boulder)
boulder.involvem<- subset(boulder, INVOLVEM==3)
boulderi.p<- nrow(boulder.involvem)/nrow(boulder)
boulder.openness<- subset(boulder, OPENNESS==3)
bouldero.p<- nrow(boulder.openness)/nrow(boulder)
boulder.social_c<- subset(boulder, SOCIAL_C==3)
bouldersc.p<- nrow(boulder.social_c)/nrow(boulder)

boulder10.p<- rbind(c(boulderl.p,boulderp.p,boulderca.p,boulderbs.p,boulderld.p,bouldere.p,
                      boulders.p,bouldera.p,boulderec.p,boulderso.p,boulderco.p,boulderi.p,bouldero.p,bouldersc.p))
colnames(boulder10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

bradenton<- d.10.sub2[d.10.sub2$City=='Bradenton, FL',]
bradenton.loyalty<- subset(bradenton,LOYALTY==5)
bradentonl.p<- nrow(bradenton.loyalty)/nrow(bradenton)
bradenton.passion<- subset(bradenton, PASSION==5)
bradentonp.p<- nrow(bradenton.passion)/nrow(bradenton)
bradenton.ca<- subset(bradenton, CA==5)
bradentonca.p<- nrow(bradenton.ca)/nrow(bradenton)
bradenton.basic_se<- subset(bradenton, BASIC_SE==3)
bradentonbs.p<- nrow(bradenton.basic_se)/nrow(bradenton)
bradenton.leadersh<- subset(bradenton, LEADERSH==3)
bradentonld.p<- nrow(bradenton.leadersh)/nrow(bradenton)
bradenton.educatio<- subset(bradenton, EDUCATIO==3)
bradentone.p<- nrow(bradenton.educatio)/nrow(bradenton)
bradenton.safety<- subset(bradenton, SAFETY==3)
bradentons.p<- nrow(bradenton.safety)/nrow(bradenton)
bradenton.aestheti<- subset(bradenton, AESTHETI==3)
bradentona.p<- nrow(bradenton.aestheti)/nrow(bradenton)
bradenton.economy<- subset(bradenton, ECONOMY==3)
bradentonec.p<- nrow(bradenton.economy)/nrow(bradenton)
bradenton.social_o<- subset(bradenton, SOCIAL_O==3)
bradentonso.p<- nrow(bradenton.social_o)/nrow(bradenton)
bradenton.communit<- subset(bradenton, COMMUNIT==3)
bradentonco.p<- nrow(bradenton.communit)/nrow(bradenton)
bradenton.involvem<- subset(bradenton, INVOLVEM==3)
bradentoni.p<- nrow(bradenton.involvem)/nrow(bradenton)
bradenton.openness<- subset(bradenton, OPENNESS==3)
bradentono.p<- nrow(bradenton.openness)/nrow(bradenton)
bradenton.social_c<- subset(bradenton, SOCIAL_C==3)
bradentonsc.p<- nrow(bradenton.social_c)/nrow(bradenton)

bradenton10.p<- rbind(c(bradentonl.p,bradentonp.p,bradentonca.p,bradentonbs.p,bradentonld.p,bradentone.p,
                        bradentons.p,bradentona.p,bradentonec.p,bradentonso.p,bradentonco.p,
                        bradentoni.p,bradentono.p,bradentonsc.p))
colnames(bradenton10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

charlotte<- d.10.sub2[d.10.sub2$City=='Charlotte, NC',]
charlotte.loyalty<- subset(charlotte,LOYALTY==5)
charlottel.p<- nrow(charlotte.loyalty)/nrow(charlotte)
charlotte.passion<- subset(charlotte, PASSION==5)
charlottep.p<- nrow(charlotte.passion)/nrow(charlotte)
charlotte.ca<- subset(charlotte, CA==5)
charlotteca.p<- nrow(charlotte.ca)/nrow(charlotte)
charlotte.basic_se<- subset(charlotte, BASIC_SE==3)
charlottebs.p<- nrow(charlotte.basic_se)/nrow(charlotte)
charlotte.leadersh<- subset(charlotte, LEADERSH==3)
charlotteld.p<- nrow(charlotte.leadersh)/nrow(charlotte)
charlotte.educatio<- subset(charlotte, EDUCATIO==3)
charlottee.p<- nrow(charlotte.educatio)/nrow(charlotte)
charlotte.safety<- subset(charlotte, SAFETY==3)
charlottes.p<- nrow(charlotte.safety)/nrow(charlotte)
charlotte.aestheti<- subset(charlotte, AESTHETI==3)
charlottea.p<- nrow(charlotte.aestheti)/nrow(charlotte)
charlotte.economy<- subset(charlotte, ECONOMY==3)
charlotteec.p<- nrow(charlotte.economy)/nrow(charlotte)
charlotte.social_o<- subset(charlotte, SOCIAL_O==3)
charlotteso.p<- nrow(charlotte.social_o)/nrow(charlotte)
charlotte.communit<- subset(charlotte, COMMUNIT==3)
charlotteco.p<- nrow(charlotte.communit)/nrow(charlotte)
charlotte.involvem<- subset(charlotte, INVOLVEM==3)
charlottei.p<- nrow(charlotte.involvem)/nrow(charlotte)
charlotte.openness<- subset(charlotte, OPENNESS==3)
charlotteo.p<- nrow(charlotte.openness)/nrow(charlotte)
charlotte.social_c<- subset(charlotte, SOCIAL_C==3)
charlottesc.p<- nrow(charlotte.social_c)/nrow(charlotte)

charlotte10.p<- rbind(c(charlottel.p,charlottep.p,charlotteca.p,charlottebs.p,charlotteld.p,charlottee.p,
                        charlottes.p,charlottea.p,charlotteec.p,charlotteso.p,charlotteco.p,
                        charlottei.p,charlotteo.p,charlottesc.p))
colnames(charlotte10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

columbia<- d.10.sub2[d.10.sub2$City=='Columbia, SC',]
columbia.loyalty<- subset(columbia,LOYALTY==5)
columbial.p<- nrow(columbia.loyalty)/nrow(columbia)
columbia.passion<- subset(columbia, PASSION==5)
columbiap.p<- nrow(columbia.passion)/nrow(columbia)
columbia.ca<- subset(columbia, CA==5)
columbiaca.p<- nrow(columbia.ca)/nrow(columbia)
columbia.basic_se<- subset(columbia, BASIC_SE==3)
columbiabs.p<- nrow(columbia.basic_se)/nrow(columbia)
columbia.leadersh<- subset(columbia, LEADERSH==3)
columbiald.p<- nrow(columbia.leadersh)/nrow(columbia)
columbia.educatio<- subset(columbia, EDUCATIO==3)
columbiae.p<- nrow(columbia.educatio)/nrow(columbia)
columbia.safety<- subset(columbia, SAFETY==3)
columbias.p<- nrow(columbia.safety)/nrow(columbia)
columbia.aestheti<- subset(columbia, AESTHETI==3)
columbiaa.p<- nrow(columbia.aestheti)/nrow(columbia)
columbia.economy<- subset(columbia, ECONOMY==3)
columbiaec.p<- nrow(columbia.economy)/nrow(columbia)
columbia.social_o<- subset(columbia, SOCIAL_O==3)
columbiaso.p<- nrow(columbia.social_o)/nrow(columbia)
columbia.communit<- subset(columbia, COMMUNIT==3)
columbiaco.p<- nrow(columbia.communit)/nrow(columbia)
columbia.involvem<- subset(columbia, INVOLVEM==3)
columbiai.p<- nrow(columbia.involvem)/nrow(columbia)
columbia.openness<- subset(columbia, OPENNESS==3)
columbiao.p<- nrow(columbia.openness)/nrow(columbia)
columbia.social_c<- subset(columbia, SOCIAL_C==3)
columbiasc.p<- nrow(columbia.social_c)/nrow(columbia)

columbia10.p<- rbind(c(columbial.p,columbiap.p,columbiaca.p,columbiabs.p,columbiald.p,columbiae.p,
                       columbias.p,columbiaa.p,columbiaec.p,columbiaso.p,columbiaco.p,
                       columbiai.p,columbiao.p,columbiasc.p))
colnames(columbia10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

columbus<- d.10.sub2[d.10.sub2$City=='Columbus, GA',]
columbus.loyalty<- subset(columbus,LOYALTY==5)
columbusl.p<- nrow(columbus.loyalty)/nrow(columbus)
columbus.passion<- subset(columbus, PASSION==5)
columbusp.p<- nrow(columbus.passion)/nrow(columbus)
columbus.ca<- subset(columbus, CA==5)
columbusca.p<- nrow(columbus.ca)/nrow(columbus)
columbus.basic_se<- subset(columbus, BASIC_SE==3)
columbusbs.p<- nrow(columbus.basic_se)/nrow(columbus)
columbus.leadersh<- subset(columbus, LEADERSH==3)
columbusld.p<- nrow(columbus.leadersh)/nrow(columbus)
columbus.educatio<- subset(columbus, EDUCATIO==3)
columbuse.p<- nrow(columbus.educatio)/nrow(columbus)
columbus.safety<- subset(columbus, SAFETY==3)
columbuss.p<- nrow(columbus.safety)/nrow(columbus)
columbus.aestheti<- subset(columbus, AESTHETI==3)
columbusa.p<- nrow(columbus.aestheti)/nrow(columbus)
columbus.economy<- subset(columbus, ECONOMY==3)
columbusec.p<- nrow(columbus.economy)/nrow(columbus)
columbus.social_o<- subset(columbus, SOCIAL_O==3)
columbusso.p<- nrow(columbus.social_o)/nrow(columbus)
columbus.communit<- subset(columbus, COMMUNIT==3)
columbusco.p<- nrow(columbus.communit)/nrow(columbus)
columbus.involvem<- subset(columbus, INVOLVEM==3)
columbusi.p<- nrow(columbus.involvem)/nrow(columbus)
columbus.openness<- subset(columbus, OPENNESS==3)
columbuso.p<- nrow(columbus.openness)/nrow(columbus)
columbus.social_c<- subset(columbus, SOCIAL_C==3)
columbussc.p<- nrow(columbus.social_c)/nrow(columbus)

columbus10.p<- rbind(c(columbusl.p,columbusp.p,columbusca.p,columbusbs.p,columbusld.p,columbuse.p,
                       columbuss.p,columbusa.p,columbusec.p,columbusso.p,columbusco.p,
                       columbusi.p,columbuso.p,columbussc.p))
colnames(columbus10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


detroit<- d.10.sub2[d.10.sub2$City=='Detroit, MI',]
detroit.loyalty<- subset(detroit,LOYALTY==5)
detroitl.p<- nrow(detroit.loyalty)/nrow(detroit)
detroit.passion<- subset(detroit, PASSION==5)
detroitp.p<- nrow(detroit.passion)/nrow(detroit)
detroit.ca<- subset(detroit, CA==5)
detroitca.p<- nrow(detroit.ca)/nrow(detroit)
detroit.basic_se<- subset(detroit, BASIC_SE==3)
detroitbs.p<- nrow(detroit.basic_se)/nrow(detroit)
detroit.leadersh<- subset(detroit, LEADERSH==3)
detroitld.p<- nrow(detroit.leadersh)/nrow(detroit)
detroit.educatio<- subset(detroit, EDUCATIO==3)
detroite.p<- nrow(detroit.educatio)/nrow(detroit)
detroit.safety<- subset(detroit, SAFETY==3)
detroits.p<- nrow(detroit.safety)/nrow(detroit)
detroit.aestheti<- subset(detroit, AESTHETI==3)
detroita.p<- nrow(detroit.aestheti)/nrow(detroit)
detroit.economy<- subset(detroit, ECONOMY==3)
detroitec.p<- nrow(detroit.economy)/nrow(detroit)
detroit.social_o<- subset(detroit, SOCIAL_O==3)
detroitso.p<- nrow(detroit.social_o)/nrow(detroit)
detroit.communit<- subset(detroit, COMMUNIT==3)
detroitco.p<- nrow(detroit.communit)/nrow(detroit)
detroit.involvem<- subset(detroit, INVOLVEM==3)
detroiti.p<- nrow(detroit.involvem)/nrow(detroit)
detroit.openness<- subset(detroit, OPENNESS==3)
detroito.p<- nrow(detroit.openness)/nrow(detroit)
detroit.social_c<- subset(detroit, SOCIAL_C==3)
detroitsc.p<- nrow(detroit.social_c)/nrow(detroit)

detroit10.p<- rbind(c(detroitl.p,detroitp.p,detroitca.p,detroitbs.p,detroitld.p,detroite.p,
                      detroits.p,detroita.p,detroitec.p,detroitso.p,detroitco.p,
                      detroiti.p,detroito.p,detroitsc.p))
colnames(detroit10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

duluth<- d.10.sub2[d.10.sub2$City=='Duluth, MN',]
duluth.loyalty<- subset(duluth,LOYALTY==5)
duluthl.p<- nrow(duluth.loyalty)/nrow(duluth)
duluth.passion<- subset(duluth, PASSION==5)
duluthp.p<- nrow(duluth.passion)/nrow(duluth)
duluth.ca<- subset(duluth, CA==5)
duluthca.p<- nrow(duluth.ca)/nrow(duluth)
duluth.basic_se<- subset(duluth, BASIC_SE==3)
duluthbs.p<- nrow(duluth.basic_se)/nrow(duluth)
duluth.leadersh<- subset(duluth, LEADERSH==3)
duluthld.p<- nrow(duluth.leadersh)/nrow(duluth)
duluth.educatio<- subset(duluth, EDUCATIO==3)
duluthe.p<- nrow(duluth.educatio)/nrow(duluth)
duluth.safety<- subset(duluth, SAFETY==3)
duluths.p<- nrow(duluth.safety)/nrow(duluth)
duluth.aestheti<- subset(duluth, AESTHETI==3)
dulutha.p<- nrow(duluth.aestheti)/nrow(duluth)
duluth.economy<- subset(duluth, ECONOMY==3)
duluthec.p<- nrow(duluth.economy)/nrow(duluth)
duluth.social_o<- subset(duluth, SOCIAL_O==3)
duluthso.p<- nrow(duluth.social_o)/nrow(duluth)
duluth.communit<- subset(duluth, COMMUNIT==3)
duluthco.p<- nrow(duluth.communit)/nrow(duluth)
duluth.involvem<- subset(duluth, INVOLVEM==3)
duluthi.p<- nrow(duluth.involvem)/nrow(duluth)
duluth.openness<- subset(duluth, OPENNESS==3)
dulutho.p<- nrow(duluth.openness)/nrow(duluth)
duluth.social_c<- subset(duluth, SOCIAL_C==3)
duluthsc.p<- nrow(duluth.social_c)/nrow(duluth)


duluth10.p<- rbind(c(duluthl.p,duluthp.p,duluthca.p,duluthbs.p,duluthld.p,duluthe.p,
                     duluths.p,dulutha.p,duluthec.p,duluthso.p,duluthco.p,
                     duluthi.p,dulutho.p,duluthsc.p))
colnames(duluth10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                         "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

fort.wayne<- d.10.sub2[d.10.sub2$City=='Fort Wayne, IN',]
fort.wayne.loyalty<- subset(fort.wayne,LOYALTY==5)
fort.waynel.p<- nrow(fort.wayne.loyalty)/nrow(fort.wayne)
fort.wayne.passion<- subset(fort.wayne, PASSION==5)
fort.waynep.p<- nrow(fort.wayne.passion)/nrow(fort.wayne)
fort.wayne.ca<- subset(fort.wayne, CA==5)
fort.wayneca.p<- nrow(fort.wayne.ca)/nrow(fort.wayne)
fort.wayne.basic_se<- subset(fort.wayne, BASIC_SE==3)
fort.waynebs.p<- nrow(fort.wayne.basic_se)/nrow(fort.wayne)
fort.wayne.leadersh<- subset(fort.wayne, LEADERSH==3)
fort.wayneld.p<- nrow(fort.wayne.leadersh)/nrow(fort.wayne)
fort.wayne.educatio<- subset(fort.wayne, EDUCATIO==3)
fort.waynee.p<- nrow(fort.wayne.educatio)/nrow(fort.wayne)
fort.wayne.safety<- subset(fort.wayne, SAFETY==3)
fort.waynes.p<- nrow(fort.wayne.safety)/nrow(fort.wayne)
fort.wayne.aestheti<- subset(fort.wayne, AESTHETI==3)
fort.waynea.p<- nrow(fort.wayne.aestheti)/nrow(fort.wayne)
fort.wayne.economy<- subset(fort.wayne, ECONOMY==3)
fort.wayneec.p<- nrow(fort.wayne.economy)/nrow(fort.wayne)
fort.wayne.social_o<- subset(fort.wayne, SOCIAL_O==3)
fort.wayneso.p<- nrow(fort.wayne.social_o)/nrow(fort.wayne)
fort.wayne.communit<- subset(fort.wayne, COMMUNIT==3)
fort.wayneco.p<- nrow(fort.wayne.communit)/nrow(fort.wayne)
fort.wayne.involvem<- subset(fort.wayne, INVOLVEM==3)
fort.waynei.p<- nrow(fort.wayne.involvem)/nrow(fort.wayne)
fort.wayne.openness<- subset(fort.wayne, OPENNESS==3)
fort.wayneo.p<- nrow(fort.wayne.openness)/nrow(fort.wayne)
fort.wayne.social_c<- subset(fort.wayne, SOCIAL_C==3)
fort.waynesc.p<- nrow(fort.wayne.social_c)/nrow(fort.wayne)


fort.wayne10.p<- rbind(c(fort.waynel.p,fort.waynep.p,fort.wayneca.p,fort.waynebs.p,fort.wayneld.p,fort.waynee.p,
                         fort.waynes.p,fort.waynea.p,fort.wayneec.p,fort.wayneso.p,fort.wayneco.p,
                         fort.waynei.p,fort.wayneo.p,fort.waynesc.p))
colnames(fort.wayne10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


gary<- d.10.sub2[d.10.sub2$City=='Gary, IN',]
gary.loyalty<- subset(gary,LOYALTY==5)
garyl.p<- nrow(gary.loyalty)/nrow(gary)
gary.passion<- subset(gary, PASSION==5)
garyp.p<- nrow(gary.passion)/nrow(gary)
gary.ca<- subset(gary, CA==5)
garyca.p<- nrow(gary.ca)/nrow(gary)
gary.basic_se<- subset(gary, BASIC_SE==3)
garybs.p<- nrow(gary.basic_se)/nrow(gary)
gary.leadersh<- subset(gary, LEADERSH==3)
garyld.p<- nrow(gary.leadersh)/nrow(gary)
gary.educatio<- subset(gary, EDUCATIO==3)
garye.p<- nrow(gary.educatio)/nrow(gary)
gary.safety<- subset(gary, SAFETY==3)
garys.p<- nrow(gary.safety)/nrow(gary)
gary.aestheti<- subset(gary, AESTHETI==3)
garya.p<- nrow(gary.aestheti)/nrow(gary)
gary.economy<- subset(gary, ECONOMY==3)
garyec.p<- nrow(gary.economy)/nrow(gary)
gary.social_o<- subset(gary, SOCIAL_O==3)
garyso.p<- nrow(gary.social_o)/nrow(gary)
gary.communit<- subset(gary, COMMUNIT==3)
garyco.p<- nrow(gary.communit)/nrow(gary)
gary.involvem<- subset(gary, INVOLVEM==3)
garyi.p<- nrow(gary.involvem)/nrow(gary)
gary.openness<- subset(gary, OPENNESS==3)
garyo.p<- nrow(gary.openness)/nrow(gary)
gary.social_c<- subset(gary, SOCIAL_C==3)
garysc.p<- nrow(gary.social_c)/nrow(gary)


gary10.p<- rbind(c(garyl.p,garyp.p,garyca.p,garybs.p,garyld.p,garye.p,
                   garys.p,garya.p,garyec.p,garyso.p,garyco.p,
                   garyi.p,garyo.p,garysc.p))
colnames(gary10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                       "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


grand.forks<- d.10.sub2[d.10.sub2$City=='Grand Forks, ND',]
grand.forks.loyalty<- subset(grand.forks,LOYALTY==5)
grand.forksl.p<- nrow(grand.forks.loyalty)/nrow(grand.forks)
grand.forks.passion<- subset(grand.forks, PASSION==5)
grand.forksp.p<- nrow(grand.forks.passion)/nrow(grand.forks)
grand.forks.ca<- subset(grand.forks, CA==5)
grand.forksca.p<- nrow(grand.forks.ca)/nrow(grand.forks)
grand.forks.basic_se<- subset(grand.forks, BASIC_SE==3)
grand.forksbs.p<- nrow(grand.forks.basic_se)/nrow(grand.forks)
grand.forks.leadersh<- subset(grand.forks, LEADERSH==3)
grand.forksld.p<- nrow(grand.forks.leadersh)/nrow(grand.forks)
grand.forks.educatio<- subset(grand.forks, EDUCATIO==3)
grand.forkse.p<- nrow(grand.forks.educatio)/nrow(grand.forks)
grand.forks.safety<- subset(grand.forks, SAFETY==3)
grand.forkss.p<- nrow(grand.forks.safety)/nrow(grand.forks)
grand.forks.aestheti<- subset(grand.forks, AESTHETI==3)
grand.forksa.p<- nrow(grand.forks.aestheti)/nrow(grand.forks)
grand.forks.economy<- subset(grand.forks, ECONOMY==3)
grand.forksec.p<- nrow(grand.forks.economy)/nrow(grand.forks)
grand.forks.social_o<- subset(grand.forks, SOCIAL_O==3)
grand.forksso.p<- nrow(grand.forks.social_o)/nrow(grand.forks)
grand.forks.communit<- subset(grand.forks, COMMUNIT==3)
grand.forksco.p<- nrow(grand.forks.communit)/nrow(grand.forks)
grand.forks.involvem<- subset(grand.forks, INVOLVEM==3)
grand.forksi.p<- nrow(grand.forks.involvem)/nrow(grand.forks)
grand.forks.openness<- subset(grand.forks, OPENNESS==3)
grand.forkso.p<- nrow(grand.forks.openness)/nrow(grand.forks)
grand.forks.social_c<- subset(grand.forks, SOCIAL_C==3)
grand.forkssc.p<- nrow(grand.forks.social_c)/nrow(grand.forks)


grand.forks10.p<- rbind(c(grand.forksl.p,grand.forksp.p,grand.forksca.p,grand.forksbs.p,grand.forksld.p,
                          grand.forkse.p,grand.forkss.p,grand.forksa.p,grand.forksec.p,
                          grand.forksso.p,grand.forksco.p,grand.forksi.p,grand.forkso.p,grand.forkssc.p))
colnames(grand.forks10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                              "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


lexington<- d.10.sub2[d.10.sub2$City=='Lexington, KY',]
lexington.loyalty<- subset(lexington,LOYALTY==5)
lexingtonl.p<- nrow(lexington.loyalty)/nrow(lexington)
lexington.passion<- subset(lexington, PASSION==5)
lexingtonp.p<- nrow(lexington.passion)/nrow(lexington)
lexington.ca<- subset(lexington, CA==5)
lexingtonca.p<- nrow(lexington.ca)/nrow(lexington)
lexington.basic_se<- subset(lexington, BASIC_SE==3)
lexingtonbs.p<- nrow(lexington.basic_se)/nrow(lexington)
lexington.leadersh<- subset(lexington, LEADERSH==3)
lexingtonld.p<- nrow(lexington.leadersh)/nrow(lexington)
lexington.educatio<- subset(lexington, EDUCATIO==3)
lexingtone.p<- nrow(lexington.educatio)/nrow(lexington)
lexington.safety<- subset(lexington, SAFETY==3)
lexingtons.p<- nrow(lexington.safety)/nrow(lexington)
lexington.aestheti<- subset(lexington, AESTHETI==3)
lexingtona.p<- nrow(lexington.aestheti)/nrow(lexington)
lexington.economy<- subset(lexington, ECONOMY==3)
lexingtonec.p<- nrow(lexington.economy)/nrow(lexington)
lexington.social_o<- subset(lexington, SOCIAL_O==3)
lexingtonso.p<- nrow(lexington.social_o)/nrow(lexington)
lexington.communit<- subset(lexington, COMMUNIT==3)
lexingtonco.p<- nrow(lexington.communit)/nrow(lexington)
lexington.involvem<- subset(lexington, INVOLVEM==3)
lexingtoni.p<- nrow(lexington.involvem)/nrow(lexington)
lexington.openness<- subset(lexington, OPENNESS==3)
lexingtono.p<- nrow(lexington.openness)/nrow(lexington)
lexington.social_c<- subset(lexington, SOCIAL_C==3)
lexingtonsc.p<- nrow(lexington.social_c)/nrow(lexington)


lexington10.p<- rbind(c(lexingtonl.p,lexingtonp.p,lexingtonca.p,lexingtonbs.p,lexingtonld.p,
                        lexingtone.p,lexingtons.p,lexingtona.p,lexingtonec.p,
                        lexingtonso.p,lexingtonco.p,lexingtoni.p,lexingtono.p,lexingtonsc.p))
colnames(lexington10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                            "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

long.beach<- d.10.sub2[d.10.sub2$City=='Long Beach, CA',]
long.beach.loyalty<- subset(long.beach,LOYALTY==5)
long.beachl.p<- nrow(long.beach.loyalty)/nrow(long.beach)
long.beach.passion<- subset(long.beach, PASSION==5)
long.beachp.p<- nrow(long.beach.passion)/nrow(long.beach)
long.beach.ca<- subset(long.beach, CA==5)
long.beachca.p<- nrow(long.beach.ca)/nrow(long.beach)
long.beach.basic_se<- subset(long.beach, BASIC_SE==3)
long.beachbs.p<- nrow(long.beach.basic_se)/nrow(long.beach)
long.beach.leadersh<- subset(long.beach, LEADERSH==3)
long.beachld.p<- nrow(long.beach.leadersh)/nrow(long.beach)
long.beach.educatio<- subset(long.beach, EDUCATIO==3)
long.beache.p<- nrow(long.beach.educatio)/nrow(long.beach)
long.beach.safety<- subset(long.beach, SAFETY==3)
long.beachs.p<- nrow(long.beach.safety)/nrow(long.beach)
long.beach.aestheti<- subset(long.beach, AESTHETI==3)
long.beacha.p<- nrow(long.beach.aestheti)/nrow(long.beach)
long.beach.economy<- subset(long.beach, ECONOMY==3)
long.beachec.p<- nrow(long.beach.economy)/nrow(long.beach)
long.beach.social_o<- subset(long.beach, SOCIAL_O==3)
long.beachso.p<- nrow(long.beach.social_o)/nrow(long.beach)
long.beach.communit<- subset(long.beach, COMMUNIT==3)
long.beachco.p<- nrow(long.beach.communit)/nrow(long.beach)
long.beach.involvem<- subset(long.beach, INVOLVEM==3)
long.beachi.p<- nrow(long.beach.involvem)/nrow(long.beach)
long.beach.openness<- subset(long.beach, OPENNESS==3)
long.beacho.p<- nrow(long.beach.openness)/nrow(long.beach)
long.beach.social_c<- subset(long.beach, SOCIAL_C==3)
long.beachsc.p<- nrow(long.beach.social_c)/nrow(long.beach)


long.beach10.p<- rbind(c(long.beachl.p,long.beachp.p,long.beachca.p,long.beachbs.p,long.beachld.p,
                         long.beache.p,long.beachs.p,long.beacha.p,long.beachec.p,
                         long.beachso.p,long.beachco.p,long.beachi.p,long.beacho.p,long.beachsc.p))
colnames(long.beach10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

macon<- d.10.sub2[d.10.sub2$City=='Macon, GA',]
macon.loyalty<- subset(macon,LOYALTY==5)
maconl.p<- nrow(macon.loyalty)/nrow(macon)
macon.passion<- subset(macon, PASSION==5)
maconp.p<- nrow(macon.passion)/nrow(macon)
macon.ca<- subset(macon, CA==5)
maconca.p<- nrow(macon.ca)/nrow(macon)
macon.basic_se<- subset(macon, BASIC_SE==3)
maconbs.p<- nrow(macon.basic_se)/nrow(macon)
macon.leadersh<- subset(macon, LEADERSH==3)
maconld.p<- nrow(macon.leadersh)/nrow(macon)
macon.educatio<- subset(macon, EDUCATIO==3)
macone.p<- nrow(macon.educatio)/nrow(macon)
macon.safety<- subset(macon, SAFETY==3)
macons.p<- nrow(macon.safety)/nrow(macon)
macon.aestheti<- subset(macon, AESTHETI==3)
macona.p<- nrow(macon.aestheti)/nrow(macon)
macon.economy<- subset(macon, ECONOMY==3)
maconec.p<- nrow(macon.economy)/nrow(macon)
macon.social_o<- subset(macon, SOCIAL_O==3)
maconso.p<- nrow(macon.social_o)/nrow(macon)
macon.communit<- subset(macon, COMMUNIT==3)
maconco.p<- nrow(macon.communit)/nrow(macon)
macon.involvem<- subset(macon, INVOLVEM==3)
maconi.p<- nrow(macon.involvem)/nrow(macon)
macon.openness<- subset(macon, OPENNESS==3)
macono.p<- nrow(macon.openness)/nrow(macon)
macon.social_c<- subset(macon, SOCIAL_C==3)
maconsc.p<- nrow(macon.social_c)/nrow(macon)


macon10.p<- rbind(c(maconl.p,maconp.p,maconca.p,maconbs.p,maconld.p,
                    macone.p,macons.p,macona.p,maconec.p,
                    maconso.p,maconco.p,maconi.p,macono.p,maconsc.p))
colnames(macon10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

miami<- d.10.sub2[d.10.sub2$City=='Miami, FL',]
miami.loyalty<- subset(miami,LOYALTY==5)
miamil.p<- nrow(miami.loyalty)/nrow(miami)
miami.passion<- subset(miami, PASSION==5)
miamip.p<- nrow(miami.passion)/nrow(miami)
miami.ca<- subset(miami, CA==5)
miamica.p<- nrow(miami.ca)/nrow(miami)
miami.basic_se<- subset(miami, BASIC_SE==3)
miamibs.p<- nrow(miami.basic_se)/nrow(miami)
miami.leadersh<- subset(miami, LEADERSH==3)
miamild.p<- nrow(miami.leadersh)/nrow(miami)
miami.educatio<- subset(miami, EDUCATIO==3)
miamie.p<- nrow(miami.educatio)/nrow(miami)
miami.safety<- subset(miami, SAFETY==3)
miamis.p<- nrow(miami.safety)/nrow(miami)
miami.aestheti<- subset(miami, AESTHETI==3)
miamia.p<- nrow(miami.aestheti)/nrow(miami)
miami.economy<- subset(miami, ECONOMY==3)
miamiec.p<- nrow(miami.economy)/nrow(miami)
miami.social_o<- subset(miami, SOCIAL_O==3)
miamiso.p<- nrow(miami.social_o)/nrow(miami)
miami.communit<- subset(miami, COMMUNIT==3)
miamico.p<- nrow(miami.communit)/nrow(miami)
miami.involvem<- subset(miami, INVOLVEM==3)
miamii.p<- nrow(miami.involvem)/nrow(miami)
miami.openness<- subset(miami, OPENNESS==3)
miamio.p<- nrow(miami.openness)/nrow(miami)
miami.social_c<- subset(miami, SOCIAL_C==3)
miamisc.p<- nrow(miami.social_c)/nrow(miami)


miami10.p<- rbind(c(miamil.p,miamip.p,miamica.p,miamibs.p,miamild.p,
                    miamie.p,miamis.p,miamia.p,miamiec.p,
                    miamiso.p,miamico.p,miamii.p,miamio.p,miamisc.p))
colnames(miami10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                        "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

milledgeville<- d.10.sub2[d.10.sub2$City=='Milledgeville, GA',]
milledgeville.loyalty<- subset(milledgeville,LOYALTY==5)
milledgevillel.p<- nrow(milledgeville.loyalty)/nrow(milledgeville)
milledgeville.passion<- subset(milledgeville, PASSION==5)
milledgevillep.p<- nrow(milledgeville.passion)/nrow(milledgeville)
milledgeville.ca<- subset(milledgeville, CA==5)
milledgevilleca.p<- nrow(milledgeville.ca)/nrow(milledgeville)
milledgeville.basic_se<- subset(milledgeville, BASIC_SE==3)
milledgevillebs.p<- nrow(milledgeville.basic_se)/nrow(milledgeville)
milledgeville.leadersh<- subset(milledgeville, LEADERSH==3)
milledgevilleld.p<- nrow(milledgeville.leadersh)/nrow(milledgeville)
milledgeville.educatio<- subset(milledgeville, EDUCATIO==3)
milledgevillee.p<- nrow(milledgeville.educatio)/nrow(milledgeville)
milledgeville.safety<- subset(milledgeville, SAFETY==3)
milledgevilles.p<- nrow(milledgeville.safety)/nrow(milledgeville)
milledgeville.aestheti<- subset(milledgeville, AESTHETI==3)
milledgevillea.p<- nrow(milledgeville.aestheti)/nrow(milledgeville)
milledgeville.economy<- subset(milledgeville, ECONOMY==3)
milledgevilleec.p<- nrow(milledgeville.economy)/nrow(milledgeville)
milledgeville.social_o<- subset(milledgeville, SOCIAL_O==3)
milledgevilleso.p<- nrow(milledgeville.social_o)/nrow(milledgeville)
milledgeville.communit<- subset(milledgeville, COMMUNIT==3)
milledgevilleco.p<- nrow(milledgeville.communit)/nrow(milledgeville)
milledgeville.involvem<- subset(milledgeville, INVOLVEM==3)
milledgevillei.p<- nrow(milledgeville.involvem)/nrow(milledgeville)
milledgeville.openness<- subset(milledgeville, OPENNESS==3)
milledgevilleo.p<- nrow(milledgeville.openness)/nrow(milledgeville)
milledgeville.social_c<- subset(milledgeville, SOCIAL_C==3)
milledgevillesc.p<- nrow(milledgeville.social_c)/nrow(milledgeville)


milledgeville10.p<- rbind(c(milledgevillel.p,milledgevillep.p,milledgevilleca.p,milledgevillebs.p,
                            milledgevilleld.p,milledgevillee.p,milledgevilles.p,milledgevillea.p,
                            milledgevilleec.p,milledgevilleso.p,milledgevilleco.p,milledgevillei.p,
                            milledgevilleo.p,milledgevillesc.p))
colnames(milledgeville10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                                "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


myrtle.beach<- d.10.sub2[d.10.sub2$City=='Myrtle Beach, SC',]
myrtle.beach.loyalty<- subset(myrtle.beach,LOYALTY==5)
myrtle.beachl.p<- nrow(myrtle.beach.loyalty)/nrow(myrtle.beach)
myrtle.beach.passion<- subset(myrtle.beach, PASSION==5)
myrtle.beachp.p<- nrow(myrtle.beach.passion)/nrow(myrtle.beach)
myrtle.beach.ca<- subset(myrtle.beach, CA==5)
myrtle.beachca.p<- nrow(myrtle.beach.ca)/nrow(myrtle.beach)
myrtle.beach.basic_se<- subset(myrtle.beach, BASIC_SE==3)
myrtle.beachbs.p<- nrow(myrtle.beach.basic_se)/nrow(myrtle.beach)
myrtle.beach.leadersh<- subset(myrtle.beach, LEADERSH==3)
myrtle.beachld.p<- nrow(myrtle.beach.leadersh)/nrow(myrtle.beach)
myrtle.beach.educatio<- subset(myrtle.beach, EDUCATIO==3)
myrtle.beache.p<- nrow(myrtle.beach.educatio)/nrow(myrtle.beach)
myrtle.beach.safety<- subset(myrtle.beach, SAFETY==3)
myrtle.beachs.p<- nrow(myrtle.beach.safety)/nrow(myrtle.beach)
myrtle.beach.aestheti<- subset(myrtle.beach, AESTHETI==3)
myrtle.beacha.p<- nrow(myrtle.beach.aestheti)/nrow(myrtle.beach)
myrtle.beach.economy<- subset(myrtle.beach, ECONOMY==3)
myrtle.beachec.p<- nrow(myrtle.beach.economy)/nrow(myrtle.beach)
myrtle.beach.social_o<- subset(myrtle.beach, SOCIAL_O==3)
myrtle.beachso.p<- nrow(myrtle.beach.social_o)/nrow(myrtle.beach)
myrtle.beach.communit<- subset(myrtle.beach, COMMUNIT==3)
myrtle.beachco.p<- nrow(myrtle.beach.communit)/nrow(myrtle.beach)
myrtle.beach.involvem<- subset(myrtle.beach, INVOLVEM==3)
myrtle.beachi.p<- nrow(myrtle.beach.involvem)/nrow(myrtle.beach)
myrtle.beach.openness<- subset(myrtle.beach, OPENNESS==3)
myrtle.beacho.p<- nrow(myrtle.beach.openness)/nrow(myrtle.beach)
myrtle.beach.social_c<- subset(myrtle.beach, SOCIAL_C==3)
myrtle.beachsc.p<- nrow(myrtle.beach.social_c)/nrow(myrtle.beach)


myrtle.beach10.p<- rbind(c(myrtle.beachl.p,myrtle.beachp.p,myrtle.beachca.p,myrtle.beachbs.p,
                           myrtle.beachld.p,myrtle.beache.p,myrtle.beachs.p,myrtle.beacha.p,
                           myrtle.beachec.p,myrtle.beachso.p,myrtle.beachco.p,myrtle.beachi.p,
                           myrtle.beacho.p,myrtle.beachsc.p))
colnames(myrtle.beach10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                               "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


palm.beach<- d.10.sub2[d.10.sub2$City=='Palm Beach, FL',]
palm.beach.loyalty<- subset(palm.beach,LOYALTY==5)
palm.beachl.p<- nrow(palm.beach.loyalty)/nrow(palm.beach)
palm.beach.passion<- subset(palm.beach, PASSION==5)
palm.beachp.p<- nrow(palm.beach.passion)/nrow(palm.beach)
palm.beach.ca<- subset(palm.beach, CA==5)
palm.beachca.p<- nrow(palm.beach.ca)/nrow(palm.beach)
palm.beach.basic_se<- subset(palm.beach, BASIC_SE==3)
palm.beachbs.p<- nrow(palm.beach.basic_se)/nrow(palm.beach)
palm.beach.leadersh<- subset(palm.beach, LEADERSH==3)
palm.beachld.p<- nrow(palm.beach.leadersh)/nrow(palm.beach)
palm.beach.educatio<- subset(palm.beach, EDUCATIO==3)
palm.beache.p<- nrow(palm.beach.educatio)/nrow(palm.beach)
palm.beach.safety<- subset(palm.beach, SAFETY==3)
palm.beachs.p<- nrow(palm.beach.safety)/nrow(palm.beach)
palm.beach.aestheti<- subset(palm.beach, AESTHETI==3)
palm.beacha.p<- nrow(palm.beach.aestheti)/nrow(palm.beach)
palm.beach.economy<- subset(palm.beach, ECONOMY==3)
palm.beachec.p<- nrow(palm.beach.economy)/nrow(palm.beach)
palm.beach.social_o<- subset(palm.beach, SOCIAL_O==3)
palm.beachso.p<- nrow(palm.beach.social_o)/nrow(palm.beach)
palm.beach.communit<- subset(palm.beach, COMMUNIT==3)
palm.beachco.p<- nrow(palm.beach.communit)/nrow(palm.beach)
palm.beach.involvem<- subset(palm.beach, INVOLVEM==3)
palm.beachi.p<- nrow(palm.beach.involvem)/nrow(palm.beach)
palm.beach.openness<- subset(palm.beach, OPENNESS==3)
palm.beacho.p<- nrow(palm.beach.openness)/nrow(palm.beach)
palm.beach.social_c<- subset(palm.beach, SOCIAL_C==3)
palm.beachsc.p<- nrow(palm.beach.social_c)/nrow(palm.beach)


palm.beach10.p<- rbind(c(palm.beachl.p,palm.beachp.p,palm.beachca.p,palm.beachbs.p,
                         palm.beachld.p,palm.beache.p,palm.beachs.p,palm.beacha.p,
                         palm.beachec.p,palm.beachso.p,palm.beachco.p,palm.beachi.p,
                         palm.beacho.p,palm.beachsc.p))
colnames(palm.beach10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

philadelphia<- d.10.sub2[d.10.sub2$City=='Philadelphia, PA',]
philadelphia.loyalty<- subset(philadelphia,LOYALTY==5)
philadelphial.p<- nrow(philadelphia.loyalty)/nrow(philadelphia)
philadelphia.passion<- subset(philadelphia, PASSION==5)
philadelphiap.p<- nrow(philadelphia.passion)/nrow(philadelphia)
philadelphia.ca<- subset(philadelphia, CA==5)
philadelphiaca.p<- nrow(philadelphia.ca)/nrow(philadelphia)
philadelphia.basic_se<- subset(philadelphia, BASIC_SE==3)
philadelphiabs.p<- nrow(philadelphia.basic_se)/nrow(philadelphia)
philadelphia.leadersh<- subset(philadelphia, LEADERSH==3)
philadelphiald.p<- nrow(philadelphia.leadersh)/nrow(philadelphia)
philadelphia.educatio<- subset(philadelphia, EDUCATIO==3)
philadelphiae.p<- nrow(philadelphia.educatio)/nrow(philadelphia)
philadelphia.safety<- subset(philadelphia, SAFETY==3)
philadelphias.p<- nrow(philadelphia.safety)/nrow(philadelphia)
philadelphia.aestheti<- subset(philadelphia, AESTHETI==3)
philadelphiaa.p<- nrow(philadelphia.aestheti)/nrow(philadelphia)
philadelphia.economy<- subset(philadelphia, ECONOMY==3)
philadelphiaec.p<- nrow(philadelphia.economy)/nrow(philadelphia)
philadelphia.social_o<- subset(philadelphia, SOCIAL_O==3)
philadelphiaso.p<- nrow(philadelphia.social_o)/nrow(philadelphia)
philadelphia.communit<- subset(philadelphia, COMMUNIT==3)
philadelphiaco.p<- nrow(philadelphia.communit)/nrow(philadelphia)
philadelphia.involvem<- subset(philadelphia, INVOLVEM==3)
philadelphiai.p<- nrow(philadelphia.involvem)/nrow(philadelphia)
philadelphia.openness<- subset(philadelphia, OPENNESS==3)
philadelphiao.p<- nrow(philadelphia.openness)/nrow(philadelphia)
philadelphia.social_c<- subset(philadelphia, SOCIAL_C==3)
philadelphiasc.p<- nrow(philadelphia.social_c)/nrow(philadelphia)


philadelphia10.p<- rbind(c(philadelphial.p,philadelphiap.p,philadelphiaca.p,philadelphiabs.p,
                           philadelphiald.p,philadelphiae.p,philadelphias.p,philadelphiaa.p,
                           philadelphiaec.p,philadelphiaso.p,philadelphiaco.p,philadelphiai.p,
                           philadelphiao.p,philadelphiasc.p))
colnames(philadelphia10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                               "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

san.jose<- d.10.sub2[d.10.sub2$City=='San Jose, CA',]
san.jose.loyalty<- subset(san.jose,LOYALTY==5)
san.josel.p<- nrow(san.jose.loyalty)/nrow(san.jose)
san.jose.passion<- subset(san.jose, PASSION==5)
san.josep.p<- nrow(san.jose.passion)/nrow(san.jose)
san.jose.ca<- subset(san.jose, CA==5)
san.joseca.p<- nrow(san.jose.ca)/nrow(san.jose)
san.jose.basic_se<- subset(san.jose, BASIC_SE==3)
san.josebs.p<- nrow(san.jose.basic_se)/nrow(san.jose)
san.jose.leadersh<- subset(san.jose, LEADERSH==3)
san.joseld.p<- nrow(san.jose.leadersh)/nrow(san.jose)
san.jose.educatio<- subset(san.jose, EDUCATIO==3)
san.josee.p<- nrow(san.jose.educatio)/nrow(san.jose)
san.jose.safety<- subset(san.jose, SAFETY==3)
san.joses.p<- nrow(san.jose.safety)/nrow(san.jose)
san.jose.aestheti<- subset(san.jose, AESTHETI==3)
san.josea.p<- nrow(san.jose.aestheti)/nrow(san.jose)
san.jose.economy<- subset(san.jose, ECONOMY==3)
san.joseec.p<- nrow(san.jose.economy)/nrow(san.jose)
san.jose.social_o<- subset(san.jose, SOCIAL_O==3)
san.joseso.p<- nrow(san.jose.social_o)/nrow(san.jose)
san.jose.communit<- subset(san.jose, COMMUNIT==3)
san.joseco.p<- nrow(san.jose.communit)/nrow(san.jose)
san.jose.involvem<- subset(san.jose, INVOLVEM==3)
san.josei.p<- nrow(san.jose.involvem)/nrow(san.jose)
san.jose.openness<- subset(san.jose, OPENNESS==3)
san.joseo.p<- nrow(san.jose.openness)/nrow(san.jose)
san.jose.social_c<- subset(san.jose, SOCIAL_C==3)
san.josesc.p<- nrow(san.jose.social_c)/nrow(san.jose)


san.jose10.p<- rbind(c(san.josel.p,san.josep.p,san.joseca.p,san.josebs.p,
                       san.joseld.p,san.josee.p,san.joses.p,san.josea.p,
                       san.joseec.p,san.joseso.p,san.joseco.p,san.josei.p,
                       san.joseo.p,san.josesc.p))
colnames(san.jose10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                           "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

st.paul<- d.10.sub2[d.10.sub2$City=='St. Paul, MN',]
st.paul.loyalty<- subset(st.paul,LOYALTY==5)
st.paull.p<- nrow(st.paul.loyalty)/nrow(st.paul)
st.paul.passion<- subset(st.paul, PASSION==5)
st.paulp.p<- nrow(st.paul.passion)/nrow(st.paul)
st.paul.ca<- subset(st.paul, CA==5)
st.paulca.p<- nrow(st.paul.ca)/nrow(st.paul)
st.paul.basic_se<- subset(st.paul, BASIC_SE==3)
st.paulbs.p<- nrow(st.paul.basic_se)/nrow(st.paul)
st.paul.leadersh<- subset(st.paul, LEADERSH==3)
st.paulld.p<- nrow(st.paul.leadersh)/nrow(st.paul)
st.paul.educatio<- subset(st.paul, EDUCATIO==3)
st.paule.p<- nrow(st.paul.educatio)/nrow(st.paul)
st.paul.safety<- subset(st.paul, SAFETY==3)
st.pauls.p<- nrow(st.paul.safety)/nrow(st.paul)
st.paul.aestheti<- subset(st.paul, AESTHETI==3)
st.paula.p<- nrow(st.paul.aestheti)/nrow(st.paul)
st.paul.economy<- subset(st.paul, ECONOMY==3)
st.paulec.p<- nrow(st.paul.economy)/nrow(st.paul)
st.paul.social_o<- subset(st.paul, SOCIAL_O==3)
st.paulso.p<- nrow(st.paul.social_o)/nrow(st.paul)
st.paul.communit<- subset(st.paul, COMMUNIT==3)
st.paulco.p<- nrow(st.paul.communit)/nrow(st.paul)
st.paul.involvem<- subset(st.paul, INVOLVEM==3)
st.pauli.p<- nrow(st.paul.involvem)/nrow(st.paul)
st.paul.openness<- subset(st.paul, OPENNESS==3)
st.paulo.p<- nrow(st.paul.openness)/nrow(st.paul)
st.paul.social_c<- subset(st.paul, SOCIAL_C==3)
st.paulsc.p<- nrow(st.paul.social_c)/nrow(st.paul)


st.paul10.p<- rbind(c(st.paull.p,st.paulp.p,st.paulca.p,st.paulbs.p,
                      st.paulld.p,st.paule.p,st.pauls.p,st.paula.p,
                      st.paulec.p,st.paulso.p,st.paulco.p,st.pauli.p,
                      st.paulo.p,st.paulsc.p))
colnames(st.paul10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

state.college<- d.10.sub2[d.10.sub2$City=='State College, PA',]
state.college.loyalty<- subset(state.college,LOYALTY==5)
state.collegel.p<- nrow(state.college.loyalty)/nrow(state.college)
state.college.passion<- subset(state.college, PASSION==5)
state.collegep.p<- nrow(state.college.passion)/nrow(state.college)
state.college.ca<- subset(state.college, CA==5)
state.collegeca.p<- nrow(state.college.ca)/nrow(state.college)
state.college.basic_se<- subset(state.college, BASIC_SE==3)
state.collegebs.p<- nrow(state.college.basic_se)/nrow(state.college)
state.college.leadersh<- subset(state.college, LEADERSH==3)
state.collegeld.p<- nrow(state.college.leadersh)/nrow(state.college)
state.college.educatio<- subset(state.college, EDUCATIO==3)
state.collegee.p<- nrow(state.college.educatio)/nrow(state.college)
state.college.safety<- subset(state.college, SAFETY==3)
state.colleges.p<- nrow(state.college.safety)/nrow(state.college)
state.college.aestheti<- subset(state.college, AESTHETI==3)
state.collegea.p<- nrow(state.college.aestheti)/nrow(state.college)
state.college.economy<- subset(state.college, ECONOMY==3)
state.collegeec.p<- nrow(state.college.economy)/nrow(state.college)
state.college.social_o<- subset(state.college, SOCIAL_O==3)
state.collegeso.p<- nrow(state.college.social_o)/nrow(state.college)
state.college.communit<- subset(state.college, COMMUNIT==3)
state.collegeco.p<- nrow(state.college.communit)/nrow(state.college)
state.college.involvem<- subset(state.college, INVOLVEM==3)
state.collegei.p<- nrow(state.college.involvem)/nrow(state.college)
state.college.openness<- subset(state.college, OPENNESS==3)
state.collegeo.p<- nrow(state.college.openness)/nrow(state.college)
state.college.social_c<- subset(state.college, SOCIAL_C==3)
state.collegesc.p<- nrow(state.college.social_c)/nrow(state.college)


state.college10.p<- rbind(c(state.collegel.p,state.collegep.p,state.collegeca.p,state.collegebs.p,
                            state.collegeld.p,state.collegee.p,state.colleges.p,state.collegea.p,
                            state.collegeec.p,state.collegeso.p,state.collegeco.p,state.collegei.p,
                            state.collegeo.p,state.collegesc.p))
colnames(state.college10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                                "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")


tallahassee<- d.10.sub2[d.10.sub2$City=='Tallahassee, FL',]
tallahassee.loyalty<- subset(tallahassee,LOYALTY==5)
tallahasseel.p<- nrow(tallahassee.loyalty)/nrow(tallahassee)
tallahassee.passion<- subset(tallahassee, PASSION==5)
tallahasseep.p<- nrow(tallahassee.passion)/nrow(tallahassee)
tallahassee.ca<- subset(tallahassee, CA==5)
tallahasseeca.p<- nrow(tallahassee.ca)/nrow(tallahassee)
tallahassee.basic_se<- subset(tallahassee, BASIC_SE==3)
tallahasseebs.p<- nrow(tallahassee.basic_se)/nrow(tallahassee)
tallahassee.leadersh<- subset(tallahassee, LEADERSH==3)
tallahasseeld.p<- nrow(tallahassee.leadersh)/nrow(tallahassee)
tallahassee.educatio<- subset(tallahassee, EDUCATIO==3)
tallahasseee.p<- nrow(tallahassee.educatio)/nrow(tallahassee)
tallahassee.safety<- subset(tallahassee, SAFETY==3)
tallahassees.p<- nrow(tallahassee.safety)/nrow(tallahassee)
tallahassee.aestheti<- subset(tallahassee, AESTHETI==3)
tallahasseea.p<- nrow(tallahassee.aestheti)/nrow(tallahassee)
tallahassee.economy<- subset(tallahassee, ECONOMY==3)
tallahasseeec.p<- nrow(tallahassee.economy)/nrow(tallahassee)
tallahassee.social_o<- subset(tallahassee, SOCIAL_O==3)
tallahasseeso.p<- nrow(tallahassee.social_o)/nrow(tallahassee)
tallahassee.communit<- subset(tallahassee, COMMUNIT==3)
tallahasseeco.p<- nrow(tallahassee.communit)/nrow(tallahassee)
tallahassee.involvem<- subset(tallahassee, INVOLVEM==3)
tallahasseei.p<- nrow(tallahassee.involvem)/nrow(tallahassee)
tallahassee.openness<- subset(tallahassee, OPENNESS==3)
tallahasseeo.p<- nrow(tallahassee.openness)/nrow(tallahassee)
tallahassee.social_c<- subset(tallahassee, SOCIAL_C==3)
tallahasseesc.p<- nrow(tallahassee.social_c)/nrow(tallahassee)


tallahassee10.p<- rbind(c(tallahasseel.p,tallahasseep.p,tallahasseeca.p,tallahasseebs.p,
                          tallahasseeld.p,tallahasseee.p,tallahassees.p,tallahasseea.p,
                          tallahasseeec.p,tallahasseeso.p,tallahasseeco.p,tallahasseei.p,
                          tallahasseeo.p,tallahasseesc.p))
colnames(tallahassee10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                              "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

wichita<- d.10.sub2[d.10.sub2$City=='Wichita, KS',]
wichita.loyalty<- subset(wichita,LOYALTY==5)
wichital.p<- nrow(wichita.loyalty)/nrow(wichita)
wichita.passion<- subset(wichita, PASSION==5)
wichitap.p<- nrow(wichita.passion)/nrow(wichita)
wichita.ca<- subset(wichita, CA==5)
wichitaca.p<- nrow(wichita.ca)/nrow(wichita)
wichita.basic_se<- subset(wichita, BASIC_SE==3)
wichitabs.p<- nrow(wichita.basic_se)/nrow(wichita)
wichita.leadersh<- subset(wichita, LEADERSH==3)
wichitald.p<- nrow(wichita.leadersh)/nrow(wichita)
wichita.educatio<- subset(wichita, EDUCATIO==3)
wichitae.p<- nrow(wichita.educatio)/nrow(wichita)
wichita.safety<- subset(wichita, SAFETY==3)
wichitas.p<- nrow(wichita.safety)/nrow(wichita)
wichita.aestheti<- subset(wichita, AESTHETI==3)
wichitaa.p<- nrow(wichita.aestheti)/nrow(wichita)
wichita.economy<- subset(wichita, ECONOMY==3)
wichitaec.p<- nrow(wichita.economy)/nrow(wichita)
wichita.social_o<- subset(wichita, SOCIAL_O==3)
wichitaso.p<- nrow(wichita.social_o)/nrow(wichita)
wichita.communit<- subset(wichita, COMMUNIT==3)
wichitaco.p<- nrow(wichita.communit)/nrow(wichita)
wichita.involvem<- subset(wichita, INVOLVEM==3)
wichitai.p<- nrow(wichita.involvem)/nrow(wichita)
wichita.openness<- subset(wichita, OPENNESS==3)
wichitao.p<- nrow(wichita.openness)/nrow(wichita)
wichita.social_c<- subset(wichita, SOCIAL_C==3)
wichitasc.p<- nrow(wichita.social_c)/nrow(wichita)


wichita10.p<- rbind(c(wichital.p,wichitap.p,wichitaca.p,wichitabs.p,
                      wichitald.p,wichitae.p,wichitas.p,wichitaa.p,
                      wichitaec.p,wichitaso.p,wichitaco.p,wichitai.p,
                      wichitao.p,wichitasc.p))
colnames(wichita10.p)<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY",'AESTHETI',
                          "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C")

prop10<- rbind(aberdeen10.p,akron10.p,biloxi10.p,boulder10.p,bradenton10.p,
               charlotte10.p,columbia10.p,columbus10.p,detroit10.p,duluth10.p,
               fort.wayne10.p,gary10.p,grand.forks10.p,lexington10.p,
               long.beach10.p,macon10.p,miami10.p,milledgeville10.p,
               myrtle.beach10.p,palm.beach10.p,philadelphia10.p,
               san.jose10.p,st.paul10.p,state.college10.p,
               tallahassee10.p,wichita10.p)
prop10<- data.frame(prop10)


prop10$City<- City

Year10<- rep(2010,26)


prop10$Year<- Year10


### rearrange the columns ##############################################
prop08<- prop08[c(16,15,1:14)]
prop09<- prop09[c(16,15,1:14)]
prop10<- prop10[c(16,15,1:14)]


########  merge the three years into one data frame ###########
props<- rbind(prop08,prop09,prop10)

###############################################################

### PCA on proportions ########################################
library(FactoMineR)
props08<- props[1:26,-(1:2)]
props09<- props[27:52,-(1:2)]
props10<- props[53:78,-(1:2)]

pca08<- PCA(props08,graph=FALSE)
col.coord.08<- pca08$var$coord   #column coordinates
row.coord.08<- pca08$ind$coord   #row coordinates

pca09<- PCA(props09,graph=FALSE)
col.coord.09<- pca09$var$coord   #column coordinates
row.coord.09<- pca09$ind$coord   #row coordinates

pca10<- PCA(props10,graph=FALSE)
col.coord.10<- pca10$var$coord   #column coordinates
row.coord.10<- pca10$ind$coord   #row coordinates

############## create data frame for the PCA dimensions ################
Year.08<- rep(2008,40)
Category<- c("LOYALTY","PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO","SAFETY","AESTHETI",
             "ECONOMY","SOCIAL_O","COMMUNIT","INVOLVEM","OPENNESS","SOCIAL_C",
             "Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
             "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
             "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
             "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
             "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
             "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")
Category<- data.frame(Category)
Variables<- c("Loyalty","Passion","Attachment","Services","Leadership","Education","Safety",
              "Aesthetics","Economy","Social Offerings","Community","Involvement","Openness",
              "Social Capital",rep(' ',26))
Variables<- data.frame(Variables)
Type<- c(rep('Index',14),rep('City',26))
Type<- data.frame(Type)
Size<- c(rep(0.8,14), rep(0.1,26))
Size<- data.frame(Size)
Region<- c(rep('',14),"West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
Region<- data.frame(Region)
Urbanicity<- c(rep('',14),"Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop" )
Urbanicity<- data.frame(Urbanicity)


Dim1.08<- rbind(col.coord.08,row.coord.08)
Dim1.08<- data.frame(Dim1.08)

Dim1.08<- cbind(Year.08,Category,Dim1.08,Variables,Type,Size,Region,Urbanicity)
Dim1.08$Year<- Year.08
rownames(Dim1.08)<- NULL
Dim1.08<- Dim1.08[,-1]
Dim1.08<- Dim1.08[,c(12,1:11)]

Dim1.09<- rbind(col.coord.09,row.coord.09)
Dim1.09<- data.frame(Dim1.09)

Year.09<- rep(2009,40)

Dim1.09<- cbind(Year.09,Category,Dim1.09,Variables,Type,Size,Region,Urbanicity)
Dim1.09$Year<- Year.09
rownames(Dim1.09)<- NULL
Dim1.09<- Dim1.09[,-1]
Dim1.09<- Dim1.09[,c(12,1:11)]

Dim1.10<- rbind(col.coord.10,row.coord.10)
Dim1.10<- data.frame(Dim1.10)

Year.10<- rep(2010,40)


Dim1.10<- cbind(Year.10,Category,Dim1.10,Variables,Type,Size,Region,Urbanicity)
Dim1.10$Year<- Year.10
rownames(Dim1.10)<- NULL
Dim1.10<- Dim1.10[,-1]
Dim1.10<- Dim1.10[,c(12,1:11)]

pca.props<- rbind(Dim1.08,Dim1.09,Dim1.10)

##############################################################################################

############# plot motion chart of PCA means ###################################
library(googleVis)
plot(gvisMotionChart(pca.props,idvar='Category',timevar='Year'))

#############################################################################

### MDS on proportions ########################
library(MASS)
proportions08.mds.dist=dist(props08)
mds.p08<- isoMDS(proportions08.mds.dist)

proportions09.mds.dist=dist(props09)
mds.p09<- isoMDS(proportions09.mds.dist)

proportions10.mds.dist=dist(props10)
mds.p10<- isoMDS(proportions10.mds.dist)

mds.p08<- data.frame(mds.p08)
mds.p09<- data.frame(mds.p09)
mds.p10<- data.frame(mds.p10)

mds.p08<- mds.p08[,-3]
mds.p09<- mds.p09[,-3]
mds.p10<- mds.p10[,-3]

colnames(mds.p08)<- c("Dimension 1", "Dimension 2")
colnames(mds.p09)<- c("Dimension 1", "Dimension 2")
colnames(mds.p10)<- c("Dimension 1", "Dimension 2")

mds.p10[,1]<- mds.p10[,1]*-1
mds.p10[,2]<- mds.p10[,2]*-1

City<- c("Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
         "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
         "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
         "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
         "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
         "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")

mds.p08$City<- City
mds.p09$City<- City
mds.p10$City<- City

year08<- rep(2008, 26)
year09<- rep(2009, 26)
year10<- rep(2010, 26)

mds.p08$Year<- year08
mds.p09$Year<- year09
mds.p10$Year<- year10

Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
mds.p08$Region<- Region
mds.p09$Region<- Region
mds.p10$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
mds.p08$Urbanicity<- Urbanicity
mds.p09$Urbanicity<- Urbanicity
mds.p10$Urbanicity<- Urbanicity

mds.p08<- mds.p08[,c(4,3,5,6,1,2)]
mds.p09<- mds.p09[,c(4,3,5,6,1,2)]
mds.p10<- mds.p10[,c(4,3,5,6,1,2)]

mds.props<- rbind(mds.p08,mds.p09,mds.p10)

plot(gvisMotionChart(mds.props, idvar='City',timevar='Year'))

#####################################################################
################### read in the SOTC data sets ################################################

d.08<- read.csv('http://streaming.stat.iastate.edu/dataexpo/2013/data/sotc-08.csv', header=T)
d.09<- read.csv('http://streaming.stat.iastate.edu/dataexpo/2013/data/sotc-09.csv', header=T)
d.10<- read.csv('http://streaming.stat.iastate.edu/dataexpo/2013/data/sotc-10.csv', header=T)

################################################################################################

###### subset the variables for each year #################
###### subset the index variables for each year #################
d.08.sub<- d.08[,c(2,85,95:96,102:103,107,136,138,140,143:154)]
d.09.sub<- d.09[,c(2,98,107:108,114:115,118,151,153,155,158:164,166:170)]
d.10.sub<- d.10[,c(3,127,136:137,143:144,147,184,186,188,191:202)]

#################################################################

###### create matching column names across the years ############
colnames(d.08.sub)[1]<- "City"
colnames(d.09.sub)[1]<- "City"
colnames(d.10.sub)[1]<- "City"
colnames(d.08.sub)[2]<- "Urbanicity"
colnames(d.09.sub)[2]<- "Urbanicity"
colnames(d.10.sub)[2]<- "Urbanicity"
colnames(d.08.sub)[3]<- "GreenPlaces"
colnames(d.09.sub)[3]<- "GreenPlaces"
colnames(d.10.sub)[3]<- "GreenPlaces"
colnames(d.08.sub)[4]<- "Beauty"
colnames(d.09.sub)[4]<- "Beauty"
colnames(d.10.sub)[4]<- "Beauty"
colnames(d.08.sub)[5]<- "Entertainment"
colnames(d.09.sub)[5]<- "Entertainment"
colnames(d.10.sub)[5]<- "Entertainment"
colnames(d.08.sub)[6]<- "MeetingPlaces"
colnames(d.09.sub)[6]<- "MeetingPlaces"
colnames(d.10.sub)[6]<- "MeetingPlaces"
colnames(d.08.sub)[7]<- "Caring"
colnames(d.09.sub)[7]<- "Caring"
colnames(d.10.sub)[7]<- "Caring"
colnames(d.08.sub)[10]<- "CA"
colnames(d.10.sub)[10]<- "CA"

##################################################################

###### remove missing values from the data subsets ##############
d.08.sub2<- d.08.sub[complete.cases(d.08.sub),]
d.09.sub2<- d.09.sub[complete.cases(d.09.sub),]
d.10.sub2<- d.10.sub[complete.cases(d.10.sub),]

#################################################################

######### Convert categorical variables to numeric ############
d.08.sub2$GreenPlaces2<- with(d.08.sub2,ifelse(d.08.sub2$GreenPlaces=='Low',1, 
                                               ifelse(d.08.sub2$GreenPlaces=='Medium',2,3)))
d.08.sub2$Beauty2<- with(d.08.sub2,ifelse(d.08.sub2$Beauty=='Low',1, 
                                          ifelse(d.08.sub2$Beauty=='Medium',2,3)))
d.08.sub2$Entertainment2<- with(d.08.sub2,ifelse(d.08.sub2$Entertainment=='Low',1, 
                                                 ifelse(d.08.sub2$Entertainment=='Medium',2,3)))
d.08.sub2$MeetingPlaces2<- with(d.08.sub2,ifelse(d.08.sub2$MeetingPlaces=='Low',1, 
                                                 ifelse(d.08.sub2$MeetingPlaces=='Medium',2,3)))
d.08.sub2$Caring2<- with(d.08.sub2,ifelse(d.08.sub2$Caring=='Low',1, 
                                          ifelse(d.08.sub2$Caring=='Medium',2,3)))

d.09.sub2$GreenPlaces2<- with(d.09.sub2,ifelse(d.09.sub2$GreenPlaces=='Low',1, 
                                               ifelse(d.09.sub2$GreenPlaces=='Medium',2,3)))
d.09.sub2$Beauty2<- with(d.09.sub2,ifelse(d.09.sub2$Beauty=='Low',1, 
                                          ifelse(d.09.sub2$Beauty=='Medium',2,3)))
d.09.sub2$Entertainment2<- with(d.09.sub2,ifelse(d.09.sub2$Entertainment=='Low',1, 
                                                 ifelse(d.09.sub2$Entertainment=='Medium',2,3)))
d.09.sub2$MeetingPlaces2<- with(d.09.sub2,ifelse(d.09.sub2$MeetingPlaces=='Low',1, 
                                                 ifelse(d.09.sub2$MeetingPlaces=='Medium',2,3)))
d.09.sub2$Caring2<- with(d.09.sub2,ifelse(d.09.sub2$Caring=='Low',1, 
                                          ifelse(d.09.sub2$Caring=='Medium',2,3)))

d.10.sub2$GreenPlaces2<- with(d.10.sub2,ifelse(d.10.sub2$GreenPlaces=='Low',1, 
                                               ifelse(d.10.sub2$GreenPlaces=='Medium',2,3)))
d.10.sub2$Beauty2<- with(d.10.sub2,ifelse(d.10.sub2$Beauty=='Low',1, 
                                          ifelse(d.10.sub2$Beauty=='Medium',2,3)))
d.10.sub2$Entertainment2<- with(d.10.sub2,ifelse(d.10.sub2$Entertainment=='Low',1, 
                                                 ifelse(d.10.sub2$Entertainment=='Medium',2,3)))
d.10.sub2$MeetingPlaces2<- with(d.10.sub2,ifelse(d.10.sub2$MeetingPlaces=='Low',1, 
                                                 ifelse(d.10.sub2$MeetingPlaces=='Medium',2,3)))
d.10.sub2$Caring2<- with(d.10.sub2,ifelse(d.10.sub2$Caring=='Low',1, 
                                          ifelse(d.10.sub2$Caring=='Medium',2,3)))

d.08.sub2<- d.08.sub2[,-c(3:7)]
d.09.sub2<- d.09.sub2[,-c(3:7)]
d.10.sub2<- d.10.sub2[,-c(3:7)]

colnames(d.08.sub2)[18]<- "GreenPlaces"
colnames(d.09.sub2)[18]<- "GreenPlaces"
colnames(d.10.sub2)[18]<- "GreenPlaces"
colnames(d.08.sub2)[19]<- "Beauty"
colnames(d.09.sub2)[19]<- "Beauty"
colnames(d.10.sub2)[19]<- "Beauty"
colnames(d.08.sub2)[20]<- "Entertainment"
colnames(d.09.sub2)[20]<- "Entertainment"
colnames(d.10.sub2)[20]<- "Entertainment"
colnames(d.08.sub2)[21]<- "MeetingPlaces"
colnames(d.09.sub2)[21]<- "MeetingPlaces"
colnames(d.10.sub2)[21]<- "MeetingPlaces"
colnames(d.08.sub2)[22]<- "Caring"
colnames(d.09.sub2)[22]<- "Caring"
colnames(d.10.sub2)[22]<- "Caring"

###### calculate the means for each city ############################################################
means.LOYALTY.08<- tapply(d.08.sub2$LOYALTY, d.08.sub2$City, mean)
means.PASSION.08<- tapply(d.08.sub2$PASSION, d.08.sub2$City, mean)
means.CA.08<- tapply(d.08.sub2$CA, d.08.sub2$City, mean)
means.BASIC_SE.08<- tapply(d.08.sub2$BASIC_SE, d.08.sub2$City, mean)
means.LEADERSH.08<- tapply(d.08.sub2$LEADERSH, d.08.sub2$City, mean)
means.EDUCATIO.08<- tapply(d.08.sub2$EDUCATIO, d.08.sub2$City, mean)
means.SAFETY.08<- tapply(d.08.sub2$SAFETY, d.08.sub2$City, mean)
means.AESTHETI.08<- tapply(d.08.sub2$AESTHETI, d.08.sub2$City, mean)
means.ECONOMY.08<- tapply(d.08.sub2$ECONOMY, d.08.sub2$City, mean)
means.SOCIAL_O.08<- tapply(d.08.sub2$SOCIAL_O, d.08.sub2$City, mean)
means.COMMUNIT.08<- tapply(d.08.sub2$COMMUNIT, d.08.sub2$City, mean)
means.INVOLVEM.08<- tapply(d.08.sub2$INVOLVEM, d.08.sub2$City, mean)
means.OPENNESS.08<- tapply(d.08.sub2$OPENNESS, d.08.sub2$City, mean)
means.SOCIAL_C.08<- tapply(d.08.sub2$SOCIAL_C, d.08.sub2$City, mean)
means.DOMAINS.08<- tapply(d.08.sub2$DOMAINS, d.08.sub2$City, mean)
means.GreenPlaces.08<- tapply(d.08.sub2$GreenPlaces, d.08.sub2$City, mean)
means.Beauty.08<- tapply(d.08.sub2$Beauty, d.08.sub2$City, mean)
means.Entertainment.08<- tapply(d.08.sub2$Entertainment, d.08.sub2$City, mean)
means.MeetingPlaces.08<- tapply(d.08.sub2$MeetingPlaces, d.08.sub2$City, mean)
means.Caring.08<- tapply(d.08.sub2$Caring, d.08.sub2$City, mean)

d.08.means<- data.frame(means.LOYALTY.08,means.PASSION.08,means.CA.08,means.BASIC_SE.08,
                        means.LEADERSH.08, means.EDUCATIO.08,means.SAFETY.08,
                        means.AESTHETI.08,means.ECONOMY.08,means.SOCIAL_O.08,
                        means.COMMUNIT.08,means.INVOLVEM.08,means.OPENNESS.08,
                        means.SOCIAL_C.08,means.DOMAINS.08,means.GreenPlaces.08,
                        means.Beauty.08,means.Entertainment.08,means.MeetingPlaces.08,means.Caring.08)

means.LOYALTY.09<- tapply(d.09.sub2$LOYALTY, d.09.sub2$City, mean)
means.PASSION.09<- tapply(d.09.sub2$PASSION, d.09.sub2$City, mean)
means.CA.09<- tapply(d.09.sub2$CA, d.09.sub2$City, mean)
means.BASIC_SE.09<- tapply(d.09.sub2$BASIC_SE, d.09.sub2$City, mean)
means.LEADERSH.09<- tapply(d.09.sub2$LEADERSH, d.09.sub2$City, mean)
means.EDUCATIO.09<- tapply(d.09.sub2$EDUCATIO, d.09.sub2$City, mean)
means.SAFETY.09<- tapply(d.09.sub2$SAFETY, d.09.sub2$City, mean)
means.AESTHETI.09<- tapply(d.09.sub2$AESTHETI, d.09.sub2$City, mean)
means.ECONOMY.09<- tapply(d.09.sub2$ECONOMY, d.09.sub2$City, mean)
means.SOCIAL_O.09<- tapply(d.09.sub2$SOCIAL_O, d.09.sub2$City, mean)
means.COMMUNIT.09<- tapply(d.09.sub2$COMMUNIT, d.09.sub2$City, mean)
means.INVOLVEM.09<- tapply(d.09.sub2$INVOLVEM, d.09.sub2$City, mean)
means.OPENNESS.09<- tapply(d.09.sub2$OPENNESS, d.09.sub2$City, mean)
means.SOCIAL_C.09<- tapply(d.09.sub2$SOCIAL_C, d.09.sub2$City, mean)
means.DOMAINS.09<- tapply(d.09.sub2$DOMAINS, d.09.sub2$City, mean)
means.GreenPlaces.09<- tapply(d.09.sub2$GreenPlaces, d.09.sub2$City, mean)
means.Beauty.09<- tapply(d.09.sub2$Beauty, d.09.sub2$City, mean)
means.Entertainment.09<- tapply(d.09.sub2$Entertainment, d.09.sub2$City, mean)
means.MeetingPlaces.09<- tapply(d.09.sub2$MeetingPlaces, d.09.sub2$City, mean)
means.Caring.09<- tapply(d.09.sub2$Caring, d.09.sub2$City, mean)

d.09.means<- data.frame(means.LOYALTY.09,means.PASSION.09,means.CA.09,means.BASIC_SE.09,
                        means.LEADERSH.09, means.EDUCATIO.09,means.SAFETY.09,
                        means.AESTHETI.09,means.ECONOMY.09,means.SOCIAL_O.09,
                        means.COMMUNIT.09,means.INVOLVEM.09,means.OPENNESS.09,
                        means.SOCIAL_C.09,means.DOMAINS.09,means.GreenPlaces.09,
                        means.Beauty.09,means.Entertainment.09,means.MeetingPlaces.09,means.Caring.09)

means.LOYALTY.10<- tapply(d.10.sub2$LOYALTY, d.10.sub2$City, mean)
means.PASSION.10<- tapply(d.10.sub2$PASSION, d.10.sub2$City, mean)
means.CA.10<- tapply(d.10.sub2$CA, d.10.sub2$City, mean)
means.BASIC_SE.10<- tapply(d.10.sub2$BASIC_SE, d.10.sub2$City, mean)
means.LEADERSH.10<- tapply(d.10.sub2$LEADERSH, d.10.sub2$City, mean)
means.EDUCATIO.10<- tapply(d.10.sub2$EDUCATIO, d.10.sub2$City, mean)
means.SAFETY.10<- tapply(d.10.sub2$SAFETY, d.10.sub2$City, mean)
means.AESTHETI.10<- tapply(d.10.sub2$AESTHETI, d.10.sub2$City, mean)
means.ECONOMY.10<- tapply(d.10.sub2$ECONOMY, d.10.sub2$City, mean)
means.SOCIAL_O.10<- tapply(d.10.sub2$SOCIAL_O, d.10.sub2$City, mean)
means.COMMUNIT.10<- tapply(d.10.sub2$COMMUNIT, d.10.sub2$City, mean)
means.INVOLVEM.10<- tapply(d.10.sub2$INVOLVEM, d.10.sub2$City, mean)
means.OPENNESS.10<- tapply(d.10.sub2$OPENNESS, d.10.sub2$City, mean)
means.SOCIAL_C.10<- tapply(d.10.sub2$SOCIAL_C, d.10.sub2$City, mean)
means.DOMAINS.10<- tapply(d.10.sub2$DOMAINS, d.10.sub2$City, mean)
means.GreenPlaces.10<- tapply(d.10.sub2$GreenPlaces, d.10.sub2$City, mean)
means.Beauty.10<- tapply(d.10.sub2$Beauty, d.10.sub2$City, mean)
means.Entertainment.10<- tapply(d.10.sub2$Entertainment, d.10.sub2$City, mean)
means.MeetingPlaces.10<- tapply(d.10.sub2$MeetingPlaces, d.10.sub2$City, mean)
means.Caring.10<- tapply(d.10.sub2$Caring, d.10.sub2$City, mean)

d.10.means<- data.frame(means.LOYALTY.10,means.PASSION.10,means.CA.10,means.BASIC_SE.10,
                        means.LEADERSH.10, means.EDUCATIO.10,means.SAFETY.10,
                        means.AESTHETI.10,means.ECONOMY.10,means.SOCIAL_O.10,
                        means.COMMUNIT.10,means.INVOLVEM.10,means.OPENNESS.10,
                        means.SOCIAL_C.10,means.DOMAINS.10,means.GreenPlaces.10,
                        means.Beauty.10,means.Entertainment.10,means.MeetingPlaces.10,means.Caring.10)

year08<- rep(2008, 26)
year09<- rep(2009, 26)
year10<- rep(2010, 26)

d.08.means$Year<- year08
d.09.means$Year<- year09
d.10.means$Year<- year10

City<- c("Aberdeen, SD","Akron, OH","Biloxi, MS","Boulder, CO","Bradenton, FL",
         "Charlotte, NC","Columbia, SC","Columbus, GA","Detroit, MI","Duluth, MN",
         "Fort Wayne, IN","Gary, IN","Grand Forks, ND","Lexington, KY",
         "Long Beach, CA","Macon, GA","Miami, FL","Milledgeville, GA",
         "Myrtle Beach, SC","Palm Beach, FL","Philadelphia, PA",
         "San Jose, CA","St. Paul, MN","State College, PA","Tallahassee, FL","Wichita, KS")

d.08.means$City<- City
d.09.means$City<- City
d.10.means$City<- City

Region<- c("West North Central","East North Central", "East South Central",
           "Mountain","South Atlantic","South Atlantic","South Atlantic",
           "South Atlantic","East North Central","West North Central",
           "East North Central","East North Central","West North Central",
           "East South Central","Pacific","South Atlantic","South Atlantic",
           "South Atlantic","South Atlantic","South Atlantic",
           "Middle Atlantic","Pacific","West North Central",
           "Middle Atlantic","South Atlantic","West North Central")
d.08.means$Region<- Region
d.09.means$Region<- Region
d.10.means$Region<- Region

Urbanicity<- c("Med/Low-low pop","Very High-med pop","Med/Low-low pop",
               "Very High-med pop","Very High-med pop","Very High-large pop",
               "High-med pop","High-med pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-med pop",
               "Med/Low-low pop","High-med pop","Very High-med pop",
               "Med/Low-low pop","Very High-very large pop",
               "Med/Low-low pop","Med/Low-low pop","Very High-large pop",
               "Very High-very large pop","Very High-large pop",
               "Very High-large pop","Med/Low-low pop","High-med pop",
               "High-med pop")
d.08.means$Urbanicity<- Urbanicity
d.09.means$Urbanicity<- Urbanicity
d.10.means$Urbanicity<- Urbanicity

### rearrange the columns ##############################################
d.08.means<- d.08.means[c(21:24,1:20)]
d.09.means<- d.09.means[c(21:24,1:20)]
d.10.means<- d.10.means[c(21:24,1:20)]

################################################################

###### create matching column names for each year's data set #####
colnames(d.08.means)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS","GreenPlaces","Beauty","Entertainment",
                         "MeetingPlaces","Caring")
colnames(d.09.means)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS","GreenPlaces","Beauty","Entertainment",
                         "MeetingPlaces","Caring")
colnames(d.10.means)<- c("Year","City","Region","Urbanicity","LOYALTY",
                         "PASSION","CA","BASIC_SE","LEADERSH","EDUCATIO",
                         "SAFETY","AESTHETI","ECONOMY","SOCIAL_O",
                         "COMMUNIT","INVOLVEM","OPENNESS",
                         "SOCIAL_C","DOMAINS","GreenPlaces","Beauty","Entertainment",
                         "MeetingPlaces","Caring")

#######################################################################

########  merge the three years into one data frame ###########
pa.means<- rbind(d.08.means,d.09.means,d.10.means)

###############################################################

#### Partial Least Squares Path Modeling ##############################
library(plspm)

############ Hypothesis: ###################################
##The better the quality of social offerings, openness,
##and aesthetics, the higher the level of attachment. 
############################################################

#### 2008 ######

# rows of the inner model matrix
Social.Offerings = c(0, 0, 0, 0)
Openness = c(0, 0, 0, 0)
Aesthetics = c(0, 0, 0, 0)
Attachment = c(1, 1, 1, 0)

# path matrix created by row binding
foot_path = rbind(Social.Offerings, Openness, Aesthetics, Attachment)

# add column names (optional)
colnames(foot_path) = rownames(foot_path)

# plot the path matrix
innerplot(foot_path)

# define list of indicators: what variables are associated with
# what latent variables
foot_blocks = list(22:23, 24, 20:21, 5:6)

# all latent variables are measured in a reflective way
foot_modes = c("A", "A", "A", "A")

# run plspm analysis
foot_pls = plspm(d.08.means, foot_path, foot_blocks, modes = foot_modes)

# summarized results
summary(foot_pls)

# plotting results (inner model)
plot(foot_pls, main=2008)

# plotting loadings of the outer model
plot(foot_pls, what = "loadings", arr.width = 0.1)

# unidimensionality
foot_pls$unidim

## All the blocks are reflective
## 2 manifest variables in social.offerings, aesthetics, and attachment
## 1 manifest variable in openness

# cronbach's alpha
foot_pls$unidim[, 3, drop = FALSE]

## Evaluates how well a block of indicators measure their
## corresponding latent construct.
## (Average inter-variable correlation between indicators of
## a reflective construct)
## unidimensional --> high correlation (> 0.7)
## Openness and Attachement are good blocks, but not social.offerings
## and aesthetics

# dillon-goldstein rho
foot_pls$unidim[, 4, drop = FALSE]

## Looks at the variance of the sum of variables in the block of interest
## unidimensional --> DG.rho > 0.7
## This index is considered to be a better indicator than the Cronbach's
## alpha because it takes into account to which extent the laten variable
## explains its block of indicators. 
## All blocks are good according to DG.rho

# eigenvalues
foot_pls$unidim[, 5:6]

## Asses the correlation matrix of each set of indicators
## eig.1st should be larger than 1 and eig.2nd smaller than 1
## all blocks are good

# plotting weights
plot(foot_pls, what = "weights")

# loadings and communalities
foot_pls$outer_model

## Loadings are correlations between a latent variable
## and its indicators. Communalities are squared correlations.
## Acceptable loadings are > 0.7 (Entertainment/social.offerings=.609)
## Acceptable communalities are > 0.5 (Entertainment/Social.offerings=0.37)
## Communalities are calculated to check that indicators in a block are 
## well explained by their latent variables

# cross-loadings
foot_pls$crossloadings

## Cross-loadings: loadings of an indicator with the rest of the 
## latent variables. Looking for traitor indicators. 
## A given loading in one of the sections in the diagonal must be greater
## than any other loading in its row. Check with bar plots below. 

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")
# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat= 'identity',position = 'dodge') +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("2008 Crossloadings") 

# inner model
foot_pls$inner_model

# inner model summary
foot_pls$inner_summary

## R^2 is 0.789
## Redundancy reflects the ability of a set of independent
## latent variables to explain variation in the dependent latent variable
## High redundancy--> high ability to predict
## Social.Offerings, Openness, and Aesthetics predict 76.6% of the 
## variability of Attachment indicators. 

# gof index
foot_pls$gof

## GoF can be used a global criterion that helps us to evaluate 
## the performance of the model in both the inner and the outer models. 
## Basically, GoF assess the overall prediction performance of the model.
## Acceptable values of GoF are > 0.7
## GoF = .79
## We have a good model overall. 

# running bootstrap validation
foot_val = plspm(d.08.means, foot_path, foot_blocks, modes = foot_modes,
                 boot.val = TRUE, br = 200)
plot(foot_val)

# bootstrap results
foot_val$boot
foot_val$boot$paths




#### 2009 ######

# rows of the inner model matrix
Social.Offerings = c(0, 0, 0, 0)
Openness = c(0, 0, 0, 0)
Aesthetics = c(0, 0, 0, 0)
Attachment = c(1, 1, 1, 0)

# path matrix created by row binding
foot_path = rbind(Social.Offerings, Openness, Aesthetics, Attachment)

# add column names (optional)
colnames(foot_path) = rownames(foot_path)

# plot the path matrix
innerplot(foot_path)

# define list of indicators: what variables are associated with
# what latent variables
foot_blocks = list(22:23, 24, 20:21, 5:6)

# all latent variables are measured in a reflective way
foot_modes = c("A", "A", "A", "A")

# run plspm analysis
foot_pls = plspm(d.09.means, foot_path, foot_blocks, modes = foot_modes)

# summarized results
summary(foot_pls)

# plotting results (inner model)
plot(foot_pls, main='2009')

# plotting loadings of the outer model
plot(foot_pls, what = "loadings", arr.width = 0.1)

# unidimensionality
foot_pls$unidim

## All the blocks are reflective
## 2 manifest variables in social.offerings, aesthetics, and attachment
## 1 manifest variable in openness

# cronbach's alpha
foot_pls$unidim[, 3, drop = FALSE]

## Evaluates how well a block of indicators measure their
## corresponding latent construct.
## (Average inter-variable correlation between indicators of
## a reflective construct)
## unidimensional --> high correlation (> 0.7)
## Openness and Attachement are good blocks, but not social.offereings
## and aesthetics

# dillon-goldstein rho
foot_pls$unidim[, 4, drop = FALSE]

## Looks at the variance of the sum of variables in the block of interest
## unidimensional --> DG.rho > 0.7
## This index is considered to be a better indicator than the Cronbach's
## alpha because it takes into account to which extent the latent variable
## explains its block of indicators. 
## All blocks are good according to DG.rho

# eigenvalues
foot_pls$unidim[, 5:6]

## Assess the correlation matrix of each set of indicators
## eig.1st should be larger than 1 and eig.2nd smaller than 1
## all blocks are good

# plotting weights
plot(foot_pls, what = "weights")

# loadings and communalities
foot_pls$outer_model

## Loadings are correlations between a latent variable
## and its indicators. Communalities are squared correlations.
## Acceptable loadings are > 0.7 All are above 0.7
## Acceptable communalities are > 0.5 All are above 0.5
## Communalities are calculated to check that indicators in a block are 
## well explained by their latent variables

# cross-loadings
foot_pls$crossloadings

## Cross-loadings: loadings of an indicator with the rest of the 
## latent variables. Looking for traitor indicators. 
## A given loading in one of the sections in the diagonal must be greater
## than any other loading in its row. Check with bar plots below. 

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")
# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat= 'identity',position = 'dodge') +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("2009 Crossloadings") 

# inner model
foot_pls$inner_model

# inner model summary
foot_pls$inner_summary

## R^2 is 0.86
## Redundancy reflects the ability of a set of independent
## latent variables to explain variation in the dependent latent variable
## High redundancy--> high ability to predict
## Social.Offerings, Openness, and Aesthetics predict 85% of the 
## variability of Attachment indicators. 

# gof index
foot_pls$gof

## GoF can be used a global criterion that helps us to evaluate 
## the performance of the model in both the inner and the outer models. 
## Basically, GoF assess the overall prediction performance of the model.
## Acceptable values of GoF are > 0.7
## GoF = .84
## We have a good model overall. 

# running bootstrap validation
foot_val = plspm(d.09.means, foot_path, foot_blocks, modes = foot_modes,
                 boot.val = TRUE, br = 200)

# bootstrap results
foot_val$boot

#### 2010 ######

# rows of the inner model matrix
Social.Offerings = c(0, 0, 0, 0)
Openness = c(0, 0, 0, 0)
Aesthetics = c(0, 0, 0, 0)
Attachment = c(1, 1, 1, 0)

# path matrix created by row binding
foot_path = rbind(Social.Offerings, Openness, Aesthetics, Attachment)

# add column names (optional)
colnames(foot_path) = rownames(foot_path)

# plot the path matrix
innerplot(foot_path)

# define list of indicators: what variables are associated with
# what latent variables
foot_blocks = list(22:23, 24, 20:21, 5:6)

# all latent variables are measured in a reflective way
foot_modes = c("A", "A", "A", "A")

# run plspm analysis
foot_pls = plspm(d.10.means, foot_path, foot_blocks, modes = foot_modes)

# summarized results
summary(foot_pls)

# plotting results (inner model)
plot(foot_pls,main='2010')

# plotting loadings of the outer model
plot(foot_pls, what = "loadings", arr.width = 0.1)

# unidimensionality
foot_pls$unidim

## All the blocks are reflective
## 2 manifest variables in social.offerings, aesthetics, and attachment
## 1 manifest variable in openness

# cronbach's alpha
foot_pls$unidim[, 3, drop = FALSE]

## Evaluates how well a block of indicators measure their
## corresponding latent construct.
## (Average inter-variable correlation between indicators of
## a reflective construct)
## unidimensional --> high correlation (> 0.7)
## All are good blocks

# dillon-goldstein rho
foot_pls$unidim[, 4, drop = FALSE]

## Looks at the variance of the sum of variables in the block of interest
## unidimensional --> DG.rho > 0.7
## This index is considered to be a better indicator than the Cronbach's
## alpha because it takes into account to which extent the latent variable
## explains its block of indicators. 
## All blocks are good according to DG.rho

# eigenvalues
foot_pls$unidim[, 5:6]

## Assess the correlation matrix of each set of indicators
## eig.1st should be larger than 1 and eig.2nd smaller than 1
## all blocks are good

# plotting weights
plot(foot_pls, what = "weights")

# loadings and communalities
foot_pls$outer_model

## Loadings are correlations between a latent variable
## and its indicators. Communalities are squared correlations.
## Acceptable loadings are > 0.7 All are above 0.7
## Acceptable communalities are > 0.5 All are above 0.5
## Communalities are calculated to check that indicators in a block are 
## well explained by their latent variables

# cross-loadings
foot_pls$crossloadings

## Cross-loadings: loadings of an indicator with the rest of the 
## latent variables. Looking for traitor indicators. 
## A given loading in one of the sections in the diagonal must be greater
## than any other loading in its row. Check with bar plots below. 

# load ggplot2 and reshape
library(ggplot2)
library(reshape)

# reshape crossloadings data.frame for ggplot
xloads = melt(foot_pls$crossloadings, id.vars = c("name", "block"),
              variable_name = "LV")
# bar-charts of crossloadings by block
ggplot(data = xloads,
       aes(x = name, y = value, fill = block)) +
  # add horizontal reference lines
  geom_hline(yintercept = 0, color = "gray75") +
  geom_hline(yintercept = 0.5, color = "gray70", linetype = 2) +
  # indicate the use of car-charts
  geom_bar(stat= 'identity',position = 'dodge') +
  # panel display (i.e. faceting)
  facet_wrap(block ~ LV) +
  # tweaking some grahical elements
  theme(axis.text.x = element_text(angle = 90),
        line = element_blank(),
        plot.title = element_text(size = 12)) +
  # add title
  ggtitle("2010 Crossloadings") 

# inner model
foot_pls$inner_model

# inner model summary
foot_pls$inner_summary

## R^2 is 0.845
## Redundancy reflects the ability of a set of independent
## latent variables to explain variation in the dependent latent variable
## High redundancy--> high ability to predict
## Social.Offerings, Openness, and Aesthetics predict 83.5% of the 
## variability of Attachment indicators. 

# gof index
foot_pls$gof

## GoF can be used a global criterion that helps us to evaluate 
## the performance of the model in both the inner and the outer models. 
## Basically, GoF assess the overall prediction performance of the model.
## Acceptable values of GoF are > 0.7
## GoF = .845
## We have a good model overall. 

# running bootstrap validation
foot_val = plspm(d.10.means, foot_path, foot_blocks, modes = foot_modes,
                 boot.val = TRUE, br = 200)

# bootstrap results
foot_val$boot

#####################################################################################################

