# R Intro GradAB Sept 2024
# Chapter 1 - Solutions

# Store the number of students at the University of Oldenburg (15643) in stud.
stud <- 15643
# Store the number of professorships at the University of Oldenburg (210) in prof.
prof <- 210
# Calculate the number of students per professorship at the University of Oldenburg using the objects stud and prof.
stud/prof
# Store the result in studprof and recall the object again!
studprof <- stud/prof
studprof

# Do you see the created variables in the Environment window?

# Store the student numbers of the University of Bremen (19173), University of Vechta (5333), and University of Oldenburg (15643) together in studs.
studs <- c(19173, 5333, 15643)
# Store the number of professors at the University of Bremen (322), University of Vechta (67), and University of Oldenburg (210) together in profs.
profs <- c(322,67,210)


# Calculate the number of students per professorship for all three universities.
studs/profs

# You also want to include the student numbers (14000) and professorships (217) of the University of Osnabrück in studs and profs. How would you do that?
# Calculate the ratio of students to professorships for all four universities!
studs2 <- c(studs,14000)
profs2 <- c(profs,217)
studs2/profs2


# Delete the object stud. How can you tell that it worked?
rm(stud)
stud 
exists("stud")

# Delete all objects from the Environment. How can you tell that it worked?
ls()
rm(list=ls())  
ls(pattern = "^s") # allows regex
  
