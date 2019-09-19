data("iris")

iris[iris$Petal.Length > 5,] ##This shows everything in Petal.Length that is greater than 5

iris[iris$Sepal.Length >= 5 & iris$Sepal.Length <= 6,] ##This shows greater or equal to 5, and less than or equal to 6.
##Don't forget the & in the above function to tie together the less than or equal to, and the greater than or equal to.

iris[iris$Sepal.Length >= 5 & iris$Sepal.Length <= 6 & iris$Species == "virginica",]##This adds the request to only show the rows that show "virginica"
