
# PROJECT FOR R COURSE
# Explore_Bikeshare


# Autor: [Juan Enamorado]
# Author: [Juan Enamorado]

# Fecha: [8/7/23]
# Date: [07/08/2023]



# Descripción: [En este proyecto, utilizará R para explorar datos relacionados con los sistemas de bicicletas compartidas para tres ciudades importantes de los Estados Unidos: Chicago, Nueva York y Washington. ¡Escribirá código para importar los datos y responder preguntas interesantes al calcular estadísticas descriptivas y hacer visualizaciones!]



# Description: [In this project, you will make use of R to explore data related to bike share systems for three major cities in the United States—Chicago, New York City, and Washington. You will write code to import the data and answer interesting questions about it by computing descriptive statistics and making visualizations!]

# Archivo: [ProyectWorkspace]
# File: [ProyectWorkspace]

# ------------------------------------------
# START CODE      /     INICIO DEL CÓDIGO
# ------------------------------------------

#Install Packages

install.packages("readr")

#Library

library(readr)


# Export and rename CSV files "chicago.csv", "new_york_city.csv" and "whashinton.csv"

#ny <- read.csv('new_york_city.csv', header = TRUE, sep = ';')
#wash <- read.csv('washington.csv', header = TRUE, sep = ';')
#chi <- read.csv('chicago.csv', header = TRUE, sep = ';')


# Cargar los datos de Chicago
chi <- read.csv("C:/Users/juan2/OneDrive/Escritorio/TRABAJO/Cursos/SQL R Shiny apps/R/ProjectWorkspace/DB/chicago.csv", header = TRUE, sep = ";")

# Cargar los datos de Nueva York
ny <- read.csv("C:/Users/juan2/OneDrive/Escritorio/TRABAJO/Cursos/SQL R Shiny apps/R/ProjectWorkspace/DB/new_york_city.csv", header = TRUE, sep = ";")

# Cargar los datos de Washington
wash <- read.csv("C:/Users/juan2/OneDrive/Escritorio/TRABAJO/Cursos/SQL R Shiny apps/R/ProjectWorkspace/DB/washington.csv", header = TRUE, sep = ";")





# Data verification
str(ny)
head(ny)

str(wash)
head(wash)

str(chi)
head(chi)


#1 Popular times of travel (i.e., occurs most often in the start time)

#1.1--------What is the most common month?


# Combine dates from all three databases
fechas <- c(ny$Start.Time, wash$Start.Time, chi$Start.Time)

# Convert dates to date objects
fechas <- as.Date(fechas)

# Extract day of week from dates
dia_semana <- weekdays(fechas)

# Calculate the frequency of days of the week
frecuencia_dias <- table(dia_semana)

# Identify the most common day of the week
dia_comun <- names(frecuencia_dias)[which.max(frecuencia_dias)]

# Print the most common day of the week
cat("The most common day of the week is", dia_comun, "\n")






#1.2------------What is the most common day of week?

# Calculate the frequency of weekdays in New York City
weekday_frequency_ny <- table(weekdays(as.Date(ny$`Start Time`)))

# Remove the margin aggregate
weekday_frequency_ny <- weekday_frequency_ny[-8]

# Identify the most common weekday
most_common_weekday_ny <- names(weekday_frequency_ny)[which.max(weekday_frequency_ny)]

# Print the most common weekday
cat("The most common weekday in New York City is", most_common_weekday_ny, "\n")




#1.3------------What is the most common hour of day?
# Convert dates and times to objects of type POSIXct
ny$Start.Time <- as.POSIXct(ny$Start.Time, format = "%d/%m/%Y %H:%M")
wash$Start.Time <- as.POSIXct(wash$Start.Time, format = "%d/%m/%Y %H:%M")
chi$Start.Time <- as.POSIXct(chi$Start.Time, format = "%d/%m/%Y %H:%M")

# Extract only times from dates
hora_ny <- format(ny$Start.Time, format = "%H")
hora_wash <- format(wash$Start.Time, format = "%H")
hora_chi <- format(chi$Start.Time, format = "%H")

# Unify the time data from the three databases
all_hours <- c(hora_ny, hora_wash, hora_chi)

# Calculate the frequency of hours
frecuencia_total <- table(all_hours)

# Identify the most common time
hora_comun <- names(frecuencia_total)[which.max(frecuencia_total)]

# Imprimir la hora más común
cat("The most common time of day is", hora_comun, "\n")







#2 Popular stations and trip

#2.1------------------What is the most common start station?

# Calculate the frequency of starting stations
frecuencia_estaciones <- table(ny$Start.Station)

# Identify the most common starting station
estacion_comun <- names(frecuencia_estaciones)[which.max(frecuencia_estaciones)]

# Print the most common starting station
cat("The most common starting station is", estacion_comun, "\n")



#2.2------------------What is the most common end station?


# Calculate the frequency of the end stations
frecuencia_estaciones <- table(ny$End.Station)

# Identify the most common end station
estacion_comun <- names(frecuencia_estaciones)[which.max(frecuencia_estaciones)]

# Print the most common end station
cat("The most common end station is", estacion_comun, "\n")



#2.3------------------What is the most common trip from start to end (i.e., most frequent combination of start station and end station)?

# Create a new column with the trip identifier
ny$Trip <- paste(ny$Start.Station, ny$End.Station, sep = " to ")

# Calculate the frequency of the trips
frecuencia_viajes <- table(ny$Trip)

# Identify the most common trip
viaje_comun <- names(frecuencia_viajes)[which.max(frecuencia_viajes)]

# Print the most common trip
cat("The most common trip from start to end is", viaje_comun, "\n")






#3 Trip duration



#3.1------------------What is the total travel time for users in different cities?
# Convert Trip.Duration column to numeric values
ny$Trip.Duration <- as.numeric(ny$Trip.Duration)
wash$Trip.Duration <- as.numeric(wash$Trip.Duration)
chi$Trip.Duration <- as.numeric(chi$Trip.Duration)

# Calculate the total travel time for each city.
total_time_ny <- sum(ny$Trip.Duration, na.rm = TRUE)
total_time_wash <- sum(wash$Trip.Duration, na.rm = TRUE)
total_time_chi <- sum(chi$Trip.Duration, na.rm = TRUE)

# Combine total travel times for each city
total_time <- total_time_ny + total_time_wash + total_time_chi

# Print total travel time
cat("Total travel time for users in different cities:", total_time, "minutes\n")












#3.2------------------What is the average travel time for users in different cities?

# Calculate the average travel time for each city
average_time_ny <- mean(ny$Trip.Duration, na.rm = TRUE)
average_time_wash <- mean(wash$Trip.Duration, na.rm = TRUE)
average_time_chi <- mean(chi$Trip.Duration, na.rm = TRUE)

# Combine the average travel times for each city
average_time <- (average_time_ny + average_time_wash + average_time_chi) / 3

# Print the average travel time
cat("Average travel time for users in different cities:", average_time, "minutes\n")







#4 User info

#4.1------------------What are the counts of each user type?


# Calculate the count of each user type in New York
recuento_ny <- table(ny$User.Type)

# Calculate the count of each user type in Chicago
recuento_chi <- table(chi$User.Type)

# Calcular el recuento de cada tipo de usuario en Washington
recuento_wash <- table(wash$User.Type)

# Display the counts of each user type in New York
cat("Count of each user type in New York:\n")
print(recuento_ny)

# Display the counts of each user type in Chicago
cat("Count of each user type in Chicago:\n")
print(recuento_chi)

# Display the counts of each user type in Washington
cat("Count of each user type in Washington:\n")
print(recuento_wash)

# Calculate the total count of each user type
recuento_total <- recuento_ny + recuento_chi + recuento_wash

# Display the total count of each user type
cat("Total count of each user type:\n")
print(recuento_total)



#4.2------------------What are the counts of each gender (only available for NYC and Chicago)?

# Calcular los recuentos de cada género en Nueva York y Chicago
recuento_gender_ny <- table(ny$Gender)
recuento_gender_chi <- table(chi$Gender)

# Mostrar los recuentos de cada género en Nueva York
cat("New York Gender Count:\n")
print(recuento_gender_ny)

# Mostrar los recuentos de cada género en Chicago
cat("Chicago Gender Count:\n")
print(recuento_gender_chi)

# Sumar los recuentos de cada género en Nueva York y Chicago
recuento_gender_total <- recuento_gender_ny + recuento_gender_chi

# Mostrar la suma de los recuentos de cada género en Nueva York y Chicago
cat("Sum of counts of each gender in New York and Chicago:\n")
print(recuento_gender_total)




#4.3------------------What are the earliest, most recent, most common year of birth (only available for NYC and Chicago)?


# Filter the data to remove missing birth year values
ny_birth_year <- na.omit(ny$Birth.Year)
chi_birth_year <- na.omit(chi$Birth.Year)

# Calculate the earliest birth year
earliest_birth_year <- min(ny_birth_year, na.rm = TRUE)
earliest_birth_year_chi <- min(chi_birth_year, na.rm = TRUE)
earliest_birth_year <- min(earliest_birth_year, earliest_birth_year_chi)

# Calculate the most recent year of birth
latest_birth_year <- max(ny_birth_year, na.rm = TRUE)
latest_birth_year_chi <- max(chi_birth_year, na.rm = TRUE)
latest_birth_year <- max(latest_birth_year, latest_birth_year_chi)

# Calculate the most common year of birth
common_birth_year <- table(ny_birth_year) + table(chi_birth_year)
most_common_birth_year <- as.numeric(names(common_birth_year[common_birth_year == max(common_birth_year)]))

# show results
cat("Earliest birth year:", earliest_birth_year, "\n")
cat("Latest birth year:", latest_birth_year, "\n")
cat("Most common birth year:", most_common_birth_year, "\n")

