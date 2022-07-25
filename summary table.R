#x is independent variable, 
#y is a vector of dependent variable; 
#names of variable need to be put in a string format
#default p is false, if test need to be applied set p = TRUE

tbl_s <- function(x,y,data, p = FALSE){
  require(flextable)
  require(gtsummary)
  
  table <- z |> 
    select(all_of(c(x,y))) |> 
    tbl_summary(by = all_of(x))
  
  if (p == TRUE){
    table <- table |> 
      add_p() |> 
      bold_labels() |> 
      add_overall() |> 
      as_flex_table() |> 
      theme_box()
    
  } else{table <- table |>
    add_overall() |> 
    bold_labels() |> 
    as_flex_table() |> 
    theme_box()
  }
  print(table)
}


#example using iris data set

tbl_s(x = "Species", y = c("Sepal.Width","Sepal.Length"), data = iris)


