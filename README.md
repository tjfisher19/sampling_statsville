# sampling_statsville

This repository builds a hands-on activity studying sampling methods. It is based on the Random Rectangles activity used in many introductory statistics courses but has been modified to add (fictional) context that is more relate-able than the abstract size (or area) of rectangles.

We build a population of 100 individuals -- the characteristics or parameter of interest is how many social media apps they regularly you. The population is provided as a whole (all 100), stratified by age groups, and clustered by the street of residence. 

Two main features are provided in this repository:

* Code and images for **Population Generation**
* Website code for **Student Exploration**

## Population Generation

Code for generating a 'population' for the fictional town of Statsville. In last August 2026 I worked with Google Gemini to make clipart style images of non-descriptive humans along with a clipart of a generic social media app icon. These images are part of the activity to provide some visual context.

The main function is `code_to_generate_images.R`.  It generates a fictional population based on independent Poisson random variables.  The rate of the Poisson variates varies across the 5 age groups considered (strata). The data is then shuffled and assigned to Street names (clusters) for additional sampling ideas.

The function generate three LARGE images.  One is a 10x10 grid of people along withe icons corresponding to the number of social media apps they regularly use.

Two other images include those same people grouped by ages (strata) and grouped by streets (clusters).

## Student Exploration

A Quarto file (`statsville_census.qmd`) provides a fictional narrative written by me along with links to the images generate. The rendered document `statsville_census.html` is linked on my website for students to easily explore. Further, they can zoom into the images.





