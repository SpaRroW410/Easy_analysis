#x is independent variable, 
#y is a vector of dependent variable; 
#names of variable need to be put in a string format
#default p is false, if test need to be applied set p = TRUE
#ylab is a vector for labelling dependent variable; default is NULL



tbl_s <- function(x = NULL,
                  y,
                  data, 
                  p = FALSE, 
                  ylab = NULL,
                  caption = "",
                  span_header = NULL,
                  mean = FALSE,
                  flex = TRUE){
  require(flextable)
  require(gtsummary)
  require(Hmisc)
  require(officer)
  
  
  if(length(ylab) >= 1){
    label(data)<- as.list(ylab[match(names(data),names(ylab))])
  }
  else{data <- data}
  
  table <- data |> 
    select(all_of(c(x,y))) 
  
  if(mean == FALSE){
    table <- table  |> 
      tbl_summary(by = all_of(x),
                  type = all_dichotomous() ~ "categorical") 
  }
  else{
    table <- table  |> 
      tbl_summary(by = all_of(x),
                  type = all_dichotomous() ~ "categorical",
                  statistic = all_continuous() ~ "{mean} ± {sd}")
  }
  
  
  if(length(span_header) == 0){
    table <- table 
  }
  else{
    table <- table |> 
      modify_spanning_header(c("stat_1":
                                 paste0("stat_",length(table[["table_body"]])-5) ) ~ span_header)
  }
  
  if(length(x) == 0){
    table <- table
  }
  else{
    table <- table |> 
      modify_header(all_stat_cols() ~ "**{level}**, N = {n} ({n*100/N}%)") |> 
      add_overall() |> 
      modify_header(stat_0 ~ "**Total**, N = {N}") |> 
      modify_table_body(~.x |> dplyr::relocate(stat_0, .after = last_col()))
  }
  
  if(caption == ""){
    table <- table
  }
  else{
    table <- table|> 
      modify_caption(paste(caption, "N = {N}"))
    
  }
  
  if (p == TRUE){
    table <- table |> 
      add_p()
  }
  else
  {table <- table 
  }
  
  if(length(y) == 1){
    table <- table |> 
      modify_header(label ~ y)
  }
  else{
    table <- table |> 
      modify_header(label ~ "**Variable**")
  }
  
  table <- table |> 
    bold_labels()
  
  if (flex == TRUE){
    table <- table|> 
      as_flex_table() |> 
      theme_box() 
  }
  else
  {table <- table 
  }
  
  
  table
}




#creating vector for labelling dependent variable
lab_y <- c("Sepal.Width" = "Sepal width","Sepal.Length" = "Sepal length")
#example using iris data set with default labels
tbl_s( y = c("Sepal.Width","Sepal.Length"), data = iris)
#using modified labels with p = TRUE
tbl_s(x = "Species", y = c("Sepal.Width","Sepal.Length"), data = iris, p = TRUE,  caption = "**Comparison**")
#using modified labels with p = TRUE and labelling

