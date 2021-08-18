# Autograding Script for a Statistics course I & II (in Finnish)

This R autograder script provides the main automated functions to grade a subset of officially registered students (=suorituslista) within a massive pool of other participants in an open online course. 

## Installation and use

```
# install packages
install.packages("readxl")
install.packages("dplyr")

# set working directory
setwd("/path/to/the/location/where-my-suorituslista-lies")
```
Further instructions embedded in the script.

The script matches the course data with the subset of students given by a bureau register. To do that it uses several identification variables (studentID, email or login-studentID). It automatically collects the subset of students, idenfies their course points, grades them and returns an excel-file of registered students and their grades. For quality assurance the script identifies the students from bureau list that did _not_ match the grading data.

