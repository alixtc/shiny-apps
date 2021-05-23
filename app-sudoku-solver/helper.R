make_ui <- function(x, id){
  val_id = paste0('idx',id)
  if (is.na(x)){
    numericInput(val_id, NULL, NA,  1, 9, 1, width = "90px")
  }else {
    tableOutput(val_id, NULL, width = "90px")
  }
}

numericInput("id1", NULL, NA,  1, 9, 1, width = "90px")





generate_fields <- function(sub_index, global_index){ 
  current_index <- (global_index - 1) * 9 + current_index
  numericInput(
    paste0("idx", current_index), 
    paste0("idx", current_index), value = 1,  min = 1,max =  9, step = 1)
}
generate_columns <- function(global_index){ 
  column(map( 1:9, generate_fields, global_index))
}

map(1:9,generate_columns)

map(1:9, function(x){
  global_index <- x
  
})

# Working code to generate ui
fluidRow(map(1:9, function(x){
  column(imap( 1:9, ~numericInput(.x, paste0("x", .y), NA,  1, 9, 1, width = "90px")), width = 1)
})
)