# EPP-data-Brewer-at-al.
v
#Summary of experimental efforts underlying this dataset

The data presented here are parental care data in comparison to paternity status from adult and nestling wild song sparrows (Melospiza melodia). In socially monogamous species, individuals of both sexes often seek extra-pair copulation (EPCs), and there are likely behavioral correlates with parental investment within individuals that seek out EPCs, though this relationship has been examined predominantly in males. However, females likely benefit from EPCs, and have additional information about the likelihood of extra-pair young (EPY) in their own nest compared to their male partners and this information is expected to influence maternal investment. In this study, we examined how the presence and abundance of EPY in a nest effect the parental behavior of a socially monogamous songbird, song sparrows. We predicted that females who mated outside the social pairs would invest more in a clutch with a higher probability of EPY, and that males would decrease their investment. We monitored nest visitation rates by both male and female social partners for 10 days during the nestling period as a proxy for parental investment and quantified extra-pair paternity in 45 song sparrow nests. 

Definitions of all variables, abbreviations, missing data codes, and units are listed below. 

#CODE: Data Analysis for paper 02.09.24.R

##############---------Parental visitation as it relates to the presence/absence of extra-pair young----------####################
#DATA:extra-pair paternity data.csv

#VARIABLES:

visist.per.id - Visits per hour for each day of the nestling period (0-10 days)
cuck.95 (yes/no) - Cuckolding determined by any method (95%) 
sex (male/female)- assumed sex based on physical and behavioral characteristics 
day - Julian day of year sampled
total.number.nestlings - Brood size at time of sampling
SOSP.Age (0-10) - Nestling age on day of behavioral sampling.
year (2018-2019) = calender year bird was sampled 
nest.id = Unique nest id

FIGURE
#DATA: extra-pair paternity data.csv
#VARIABLES:

visist.per.id - Visits per hour for each day of the nestling period (0-10 days)
cuck.95 (yes/no) - Cuckolding determined by any method (95%) 
sex (male/female)- assumed sex based on physical and behavioral characteristics 

##############---------Parental visitation as it relates to the number of extra-pair young----------####################
#DATA:extra-pair paternity data.csv

#VARIABLES:

visist.per.id - Visits per hour for each day of the nestling period (0-10 days)
X..of.epp.offspring.in.nest - Number of nestlings in the nest that were not sired by the social male
sex (male/female)- assumed sex based on physical and behavioral characteristics 
day - Julian day of year sampled
total.number.nestlings - Brood size at time of sampling
SOSP.Age (0-10) - Nestling age on day of behavioral sampling.
year (2018-2019) = calender year bird was sampled 
nest.id = Unique nest id

##############---------Parental visitation as it relates to the proportion of extra-pair young----------####################
#DATA:extra-pair paternity data.csv

#VARIABLES:

visist.per.id - Visits per hour for each day of the nestling period (0-10 days)
X..of.offspring.sampled.that.are.epp - Proportion of nestlings in the nest that were not sired by the social male
sex (male/female)- assumed sex based on physical and behavioral characteristics 
day - Julian day of year sampled
total.number.nestlings - Brood size at time of sampling
SOSP.Age (0-10) - Nestling age on day of behavioral sampling.
year (2018-2019) = calender year bird was sampled 
nest.id = Unique nest id

