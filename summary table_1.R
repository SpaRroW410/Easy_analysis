#x is independent variable, 
#y is a vector of dependent variable; 
#names of variable need to be put in a string format
#default p is false, if test need to be applied set p = TRUE
#ylab is a vector for labelling dependent variable; default is NULL



tbl_s <- function(x,y,data, p = FALSE, ylab = NULL){
  require(flextable)
  require(gtsummary)
  require(Hmisc)
  
  if(length(ylab) >= 1){
    label(data)<- as.list(ylab[match(names(data),names(ylab))])
  }
  else{data <- data}
  
  table <- data |> 
    select(all_of(c(x,y))) |> 
    tbl_summary(by = all_of(x)) |> 
    bold_labels() |> 
    add_overall() 
  
  if (p == TRUE){
    table <- table |> 
    add_p()
  }
  else
  {table <- table 
  }
	
	table <- table |> 
  as_flex_table() |> 
  theme_box()
	
  print(table)
}




#creating vector for labelling dependent variable
lab_y <- c("Sepal.Width" = "Sepal width","Sepal.Length" = "Sepal length")
#example using iris data set with default labels
tbl_s(x = "Species", y = c("Sepal.Width","Sepal.Length"), data = iris)
#using modified labels with p = TRUE
tbl_s(x = "Species", y = c("Sepal.Width","Sepal.Length"), data = iris, p = TRUE, ylab = lab_y)
