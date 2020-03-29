library(data.table)
library(dplyr)
library(igraph)

library(writexl)
library(tibble)
library(readxl)


setwd("C:/Users/mengc/Desktop/Fall/Social Network Analytics/Final Project")

route <- fread("routes.dat")
airport <- fread("airports.dat")
airline <- fread("airlines.dat")
gdp <- read_excel("gdp.xlsx")
pop <- fread("pop.csv")
pop <- pop[,c(3,5)]
names(pop)[2]<-"pop"

colnames(route) <- c('Airline','AirlineID','Source_Airport','Source_AirportID','Destination_Airport','Destination_AirportID','Codeshare','Stops','Plane')
colnames(airport) <- c('AirportID','Name','City','Country','IATA','ICAO','Lat','Long','Altitude',
'Timezone','DST','Tz','Type','Source')
colnames(airline) <- c('AirlineID','Name','Alias','IATA','ICAO','Callsign','Country','Active')

data <- merge(route[,c(1,3,5,7,8)],airport[,c(3,4,5,7,8,9,10)],by.x = "Source_Airport",by.y="IATA",all.x=TRUE )

colnames(data) <- c("Source_Airport","Airline","Destination_Airport","Codeshare","Stops","City_S","Country_S","Lat_S","Long_S","Altitude_S","Timezone_S")

data <- merge(data,airport[,c(3,4,5,7,8,9,10)],by.x = "Destination_Airport",by.y="IATA",all.x=TRUE )

colnames(data) <- c("Destination_Airport","Source_Airport","Airline","Codeshare","Stops",
                    "City_S","Country_S","Lat_S","Long_S","Altitude_S","Timezone_S",
                    "City_D","Country_D","Lat_D","Long_D","Altitude_D","Timezone_D")

#unique value involved
uniqueAirline <- unique(data$Airline) #568
uniqueRoute <- unique(data[,c(1,2)]) #37595
uniqueAirportD <- unique(data$Destination_Airport) #3418
uniqueAirportS <- unique(data$Source_Airport) #3409
uniqueAirport <- unique(append(uniqueAirportD,uniqueAirportS)) #3425
uniqueCountry <- unique(append(data$Country_S,data$Country_D)) #226
uniqueCity <- unique(append(data$City_S,data$City_D)) #3142

#codeshare involved
nshare <- sum(data$Codeshare=="Y") #14597

#data remove codeshare route
datac <- data[which(data$Codeshare!="Y"),]
nrow(datac) #53066

#number of routes by Airline #including codeshare
t1 <- data %>%
  group_by(Airline) %>%
  summarize(flight_number = n()) %>%
  setorderv(cols = 'flight_number',order='-1')

#number of routes by Source-Destination
#Airport level
t2 <- datac %>%
  group_by(Source_Airport,Destination_Airport) %>%
  summarize(weight=n()) %>%
  setorderv(cols = 'weight', order='-1')

#City level
t3 <- datac %>%
  group_by(City_S,City_D) %>%
  summarize(weight=n()) %>%
  setorderv(cols = 'weight', order='-1')

#Country level
t4 <- datac %>%
  group_by(Country_S,Country_D) %>%
  summarize(weight=n()) %>%
  setorderv(cols = 'weight', order='-1')

#number of international routes
InternationRoute <- datac[which(datac$Country_S!=datac$Country_D),] #28604
nrow(InternationRoute)/nrow(datac) #53.9%

t5 <- t4[which(t4$Country_D!=t4$Country_S),]

library(WriteXLS)


library(countrycode)
datac$continent_S <- countrycode(sourcevar = datac$Country_S,
                                 origin = "country.name",
                                 destination = "continent")
datac$continent_D <- countrycode(sourcevar = datac$Country_D,
                                 origin = "country.name",
                                 destination = "continent")


#h1 <- h[h$count >= 3,]
#network graph
AirportG <- graph.data.frame(t2,directed=TRUE)
plot.igraph(AirportG,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.05,edge.curved=FALSE)

CityG <- graph.data.frame(na.omit(t3),directed=TRUE)
plot.igraph(CityG,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

CountryG <- graph.data.frame(na.omit(t4),directed=TRUE)
plot.igraph(CountryG,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

IntG <- graph.data.frame(na.omit(t5),directed=TRUE)
plot.igraph(IntG,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)


#regional graph 
#city level
unique(datac$continent_S) #"Africa" "Europe" "Asia" "Americas" "Oceania"
AsiaG1 <- graph.data.frame(datac[which(datac$continent_S=="Asia"&datac$continent_D=="Asia"),c(6,12)],directed=TRUE)
plot.igraph(AsiaG1,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

AfrG1 <- graph.data.frame(datac[which(datac$continent_S=="Africa"&datac$continent_D=="Africa"),c(6,12)],directed=TRUE)
plot.igraph(AfrG1,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

EuG1 <- graph.data.frame(datac[which(datac$continent_S=="Europe"&datac$continent_D=="Europe"),c(6,12)],directed=TRUE)
plot.igraph(EuG1,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

AmG1 <- graph.data.frame(datac[which(datac$continent_S=="Americas"&datac$continent_D=="Americas"),c(6,12)],directed=TRUE)
plot.igraph(AmG1,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

OcG1 <- graph.data.frame(datac[which(datac$continent_S=="Oceania"&datac$continent_D=="Oceania"),c(6,12)],directed=TRUE)
plot.igraph(OcG1,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)


#Country level
AsiaG2 <- graph.data.frame(datac[which(datac$continent_S=="Asia"&datac$continent_D=="Asia"),c(7,13)],directed=TRUE)
plot.igraph(AsiaG2,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

AfrG2 <- graph.data.frame(datac[which(datac$continent_S=="Africa"&datac$continent_D=="Africa"),c(7,13)],directed=TRUE)
plot.igraph(AfrG2,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

EuG2 <- graph.data.frame(datac[which(datac$continent_S=="Europe"&datac$continent_D=="Europe"),c(7,13)],directed=TRUE)
plot.igraph(EuG2,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

AmG2 <- graph.data.frame(datac[which(datac$continent_S=="Americas"&datac$continent_D=="Americas"),c(7,13)],directed=TRUE)
plot.igraph(AmG2,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

OcG2 <- graph.data.frame(datac[which(datac$continent_S=="Oceania"&datac$continent_D=="Oceania"),c(7,13)],directed=TRUE)
plot.igraph(OcG2,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)


#Internal Country level
AsiaG3 <- graph.data.frame(datac[which(datac$continent_S=="Asia"&datac$continent_D=="Asia"&datac$Country_S!=datac$Country_D),c(7,13)],directed=TRUE)
plot.igraph(AsiaG3,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

AfrG3 <- graph.data.frame(datac[which(datac$continent_S=="Africa"&datac$continent_D=="Africa"&datac$Country_S!=datac$Country_D),c(7,13)],directed=TRUE)
plot.igraph(AfrG3,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

EuG3 <- graph.data.frame(datac[which(datac$continent_S=="Europe"&datac$continent_D=="Europe"&datac$Country_S!=datac$Country_D),c(7,13)],directed=TRUE)
plot.igraph(EuG3,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

AmG3 <- graph.data.frame(datac[which(datac$continent_S=="Americas"&datac$continent_D=="Americas"&datac$Country_S!=datac$Country_D),c(7,13)],directed=TRUE)
plot.igraph(AmG3,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)

OcG3 <- graph.data.frame(datac[which(datac$continent_S=="Oceania"&datac$continent_D=="Oceania"&datac$Country_S!=datac$Country_D),c(7,13)],directed=TRUE)
plot.igraph(OcG3,vertex.label.color="black",vertex.size=5,edge.arrow.size=0.02,label.cex=0.02,edge.curved=FALSE)


#node value measure
#Airport Level
d1 <- as.data.frame(strength(AirportG,mode="all"))
c1 <- as.data.frame(coreness(AirportG))
pr1 <- as.data.frame(page.rank(AirportG)$vector)
b1 <- as.data.frame(betweenness(AirportG))
cl1 <- as.data.frame(closeness(AirportG))
e1 <- as.data.frame(eigen_centrality(AirportG)$vector)
r1 <- cbind(d1,c1,pr1,b1,cl1,e1)
colnames(r1) <- c("Node","strength","coreness","page.rank","betweenness","closeness","eigen_centrality")
write_xlsx(r1, path = "airport.xlsx", col_names = TRUE)


#City Level
d2 <- as.data.frame(strength(CityG,mode="all"))
c2 <- as.data.frame(coreness(CityG))
pr2 <- as.data.frame(page.rank(CityG)$vector)
b2 <- as.data.frame(betweenness(CityG))
cl2 <- as.data.frame(closeness(CityG))
e2 <- as.data.frame(eigen_centrality(CityG)$vector)
r2 <- cbind(d2,c2,pr2,b2,cl2,e2)
colnames(r2) <- c("Node","strength","coreness","page.rank","betweenness","closeness","eigen_centrality")
write_xlsx(r2, path = "city.xlsx", col_names = TRUE)

#Country Level
d3 <- as.data.frame(strength(CountryG,mode="all"))
c3 <- as.data.frame(coreness(CountryG))
pr3 <- as.data.frame(page.rank(CountryG)$vector)
b3 <- as.data.frame(betweenness(CountryG))
cl3 <- as.data.frame(closeness(CountryG))
e3 <- as.data.frame(eigen_centrality(CountryG)$vector)
r3 <- cbind(d3,c3,pr3,b3,cl3,e3)
colnames(r3) <- c("Node","strength","coreness","page.rank","betweenness","closeness","eigen_centrality")
write_xlsx(r3, path = "country.xlsx", col_names = TRUE)

#International Level
d4 <- as.data.frame(strength(IntG,mode="all"))
c4 <- as.data.frame(coreness(IntG))
pr4 <- as.data.frame(page.rank(IntG)$vector)
b4 <- as.data.frame(betweenness(IntG))
cl4 <- as.data.frame(closeness(IntG))
e4 <- as.data.frame(eigen_centrality(IntG)$vector)
r4 <- cbind(d4,c4,pr4,b4,cl4,e4)
colnames(r4) <- c("Node","strength","coreness","page.rank","betweenness","closeness","eigen_centrality")
write_xlsx(r4, path = "international.xlsx", col_names = TRUE)


# network-level measure 
#Airport Level
centr_clo(AirportG,normalized=TRUE)$centralization #0.0008350746
centr_betw(AirportG,normalized=TRUE)$centralization #0.06548108
centr_degree(AirportG,normalized=TRUE)$centralization #0.07034328

?centr_betw
#City Level
centr_clo(CityG,normalized=TRUE)$centralization #0.001333726
centr_betw(CityG,normalized=TRUE)$centralization #0.1054135
centr_degree(CityG,normalized=TRUE)$centralization #0.1142897

#Country Level
centr_clo(CountryG,normalized=TRUE)$centralization #0.09233869
centr_betw(CountryG,normalized=TRUE)$centralization #0.1717404
centr_degree(CountryG,normalized=TRUE)$centralization #0.4247119


#cliques #ingnore directionality
clique_num(AirportG) #20
clique_num(CityG) #21
clique_num(CountryG) #18

clique_num(AirportG) #20
CliqueAirport <- largest_cliques(AirportG)
CliqueA <- as.data.table(matrix(data=names(unlist(CliqueAirport)),byrow = TRUE,ncol=20))
write_xlsx(CliqueA, path = "CliqueAirport.xlsx", col_names = TRUE)

clique_num(CityG) #21
CliqueCity <- largest_cliques(CityG)
CliqueC <-as.data.table(matrix(data=names(unlist(CliqueCity)),byrow = TRUE,ncol=21))
write_xlsx(CliqueC,path = "CliqueCity.xlsx", col_names = TRUE)

clique_num(CountryG) #18
CliqueCountry <- largest_cliques(CountryG)
CliqueCo <-as.data.table(matrix(data=names(unlist(CliqueCountry)),byrow = TRUE,ncol=18))
write_xlsx(CliqueCo, path = "CliqueCountry.xlsx", col_names = TRUE)


#regression

reg <- merge(r3,gdp,by.x="Node",by.y = "Country",all.x = TRUE)
reg <- merge(reg,pop,by.x="Node",by.y = "Country Name",all.x = TRUE)
regc <- na.omit(reg) 

colnames(regc) <- c("Country","strength","coreness","page.rank","betweenness","closeness","eigen_centrality","GDPP","pop")
regc$GDPP <- as.numeric(regc$GDPP)
regc$pop <- as.numeric(regc$pop)
regc <- na.omit(regc)

R1 <- lm(coreness~GDPP+pop,data=regc)
summary(R1)
R2 <- lm(betweenness~GDPP+pop,data=regc)
summary(R2)
R3 <- lm(eigen_centrality~GDPP+pop,data=regc)
summary(R3)
R4 <- lm(page.rank~GDPP+pop,data=regc)
summary(R4)



####
library(ggplot2)
library(ggthemes)

  geom_density()


p11 <- ggplot(r1,aes(x=coreness)) +
  geom_histogram()+
  labs(title="Distribution of Corness, Airport level")+
  theme_minimal()

p12 <- ggplot(r1,aes(x=betweenness)) +
  geom_histogram()+
  labs(title="Distribution of Betweenness, Airport level")+
  theme_minimal()

p13 <- ggplot(r1,aes(x=eigen_centrality)) +
  geom_histogram()+
  labs(title="Distribution of eigenvector centrality, Airport level")+
  theme_minimal()


p21 <- ggplot(r2,aes(x=coreness)) +
  geom_histogram()+
  labs(title="Distribution of Corness, City level")+
  theme_minimal()

p22 <- ggplot(r2,aes(x=betweenness)) +
  geom_histogram()+
  labs(title="Distribution of Betweenness, City level")+
  theme_minimal()

p23 <- ggplot(r2,aes(x=eigen_centrality)) +
  geom_histogram()+
  labs(title="Distribution of eigenvector centrality, City level")+
  theme_minimal()

#
p31 <- ggplot(r3,aes(x=coreness)) +
  geom_histogram()+
  labs(title="Distribution of Corness, Country level")+
  theme_minimal()

p32 <- ggplot(r3,aes(x=betweenness)) +
  geom_histogram()+
  labs(title="Distribution of Betweenness, Country level")+
  theme_minimal()

p33 <- ggplot(r3,aes(x=eigen_centrality)) +
  geom_histogram()+
  labs(title="Distribution of eigenvector centrality, Country level")+
  theme_minimal()

library(gridExtra)
grid.arrange(p11, p12,p13,p21,p22,p23,p31,p32,p33, nrow = 3)

#internation
ggplot(r4,aes(x=coreness)) +
  geom_histogram()+
  labs(title="Distribution of Corness, Country level")+
  theme_minimal()

ggplot(r4,aes(x=betweenness)) +
  geom_histogram()+
  labs(title="Distribution of Betweenness, Country level")+
  theme_minimal()

ggplot(r4,aes(x=eigen_centrality)) +
  geom_histogram()+
  labs(title="Distribution of eigenvector centrality, Country level")+
  theme_minimal()


ranges = E(combine_Graph2)$weight
ranges[ranges >0.5] = "purple"
ranges[(ranges < 1)&(ranges > 0)] = "red"
plot.igraph(combine_Graph2,layout=layout.fruchterman.reingold, 
            vertex.label.color="black",edge.color=ranges,vertex.size = 8, edge.arrow.size=.1,edge.curved=FALSE)


