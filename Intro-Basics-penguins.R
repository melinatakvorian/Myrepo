source("setup.R")

data("penguins")

#---- 6.3.1 Exercises----
# finding dif species of penguins
unique(penguins$species)

# finding dif islands
unique(penguins$island)

# new table of 3 columns of the island Dream
Dream <- penguins[penguins$island == "Dream", c("species", "island", "flipper_length_mm")]

Adelie <- Dream[Dream$species == "Adelie", "flipper_length_mm"]

# finding the mean of Adelie species on Dream
meanAdelie <- mean(Adelie$flipper_length_mm)


#---- 6.4----
select(penguins, species, sex)

# Select a range of variables
select(penguins, species:flipper_length_mm)

# Rename columns within select
select(penguins, genus = species, island)

# Select column variables that are recorded in mm
select(penguins, contains("mm"))

# New variable that calculates bill length in cm
mutate(penguins, bill_length_cm = bill_length_mm/10)

# mutate based on conditional statements
mutate <- mutate(penguins, species_sex = if_else(sex == 'male', paste0(species,"_m"), paste0(species, "_f")))


#----6.3.2 Exercises----

# make year be the first column
penguins %>% 
 relocate(year)

# new column with body masses greater than overall avg
avg <- mean(penguins$body_mass_g, na.rm = TRUE)
chubbiness_factor <- mutate(penguins, chubby_penguins = if_else(body_mass_g >= avg, paste0("large"), paste0("small")))

# which year the penguins were the fattest
Adelie_obese <- chubbiness_factor %>% 
  filter(species == "Adelie") %>% 
  group_by(year) %>% 
  summarise(body_mass_avg = mean(body_mass_g, na.rm = TRUE))

Gentoo_obese <- chubbiness_factor %>% 
  filter(species == "Gentoo") %>% 
  group_by(year) %>% 
  summarise(body_mass_avg = mean(body_mass_g, na.rm = TRUE))

Chinstrap_obese <- chubbiness_factor %>% 
  filter(species == "Chinstrap") %>% 
  group_by(year) %>% 
  summarise(body_mass_g = mean(body_mass_g, na.rm = TRUE))
  

# troubleshooting
Adelie %>% 
  filter(year != c(2008, 2009))

#----7 Visualize Examples----
# GENERAL FORMAT FOR USING GGPLOT
#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

# Histogram example: flipper length by species
ggplot(penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))
##Creating a histogram with different charts per species
ggplot(penguins) +
  geom_histogram(aes(x = flipper_length_mm, fill = species), alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  facet_wrap(~species)

#Creating a bar chart
ggplot(penguins) +
  geom_bar(mapping = aes(x = island, fill = species))



#----7.1 Exercises----
# barplot showing the avg flipper length for each species
ggplot(penguins)+
  geom_col(aes(x = species, y = mean(flipper_length_mm, na.rm = TRUE), fill = species))+
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))

# bill length v. bill depth for Dream Island
ggplot(subset(penguins, island == "Dream", drop = FALSE))+
  geom_point(aes(x = bill_length_mm, y = bill_depth_mm, color = species))+
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4"))






























Intro-Basics-penguins.R