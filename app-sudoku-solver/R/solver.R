library(tidyverse)

grid_fetch <- function(difficulty){
  stopifnot(difficulty %in% 1:4)
  url = paste0("https://grid.websudoku.com/?level=", difficulty)
  htmlgrid <- RCurl::getURL(url)
  # Isolate the Sudoku Grid from the HTML template
  grid <- htmlgrid %>% 
    strsplit("<TABLE id=\"puzzle_grid\" CELLSPACING=0 CELLPADDING=0 CLASS=t>") %>% 
    pluck(1, 2) %>% 
    str_split('</TABLE>') %>% 
    pluck(1, 1) 
  
  
  # Put the grid into a list where <TR>  = columns and <TD>  = row
  grid <- grid %>% 
    str_split("</TR>") %>% 
    map(str_split, "</TD>") %>% 
    pluck(1)
  
  # Clean the grid from unwanted values => length 81 list
  grid <- grid %>% flatten() %>% discard(~str_length(.x) == 0) %>% 
    
    # get the initial digits in the grid to start the puzzle 
    map(str_extract, pattern = "\"[:digit:]") %>% 
    map(str_sub, 2) 
  
  # Converts 1D list to integer vector
  grid <- as.integer(grid)
  
  # HTML arranged by row while R arrange values by column
  # take care of that problem
  grid <- matrix(grid, nrow = 9, byrow = TRUE)
  
  grid
}

# Level 2 sudoku solver, WORKING


# ------
# For a cell in the grid get a vector of all numbers already in the row 
row_analysis <- function(matrix, r){
  row_impossible <- na.omit(matrix[r, ])
  row_impossible
}

# For a cell in the grid get a vector of all numbers already in the column 
col_analysis <- function(matrix, c){    
  col_impossible <- na.omit(matrix[, c])
  col_impossible
}  

# For a given cell in the grid,
#Returns all the numbers that are already in the associated 3x3 square 
square_ana <- function(grid_to_test = NULL, row = NULL, column = NULL){
  row_block <- ceiling(row / 3)
  col_block <- ceiling(column /3)
  
  range_row <- ((row_block - 1) * 3 + 1) : (row_block * 3)
  range_col <- ((col_block - 1) * 3 + 1) : (col_block * 3)
  
  square_impossible <- 
    grid_to_test[range_row, range_col] %>% 
    as.integer() %>% 
    na.omit()
  
  square_impossible  
}

cell_analysis <- function(matrix, r, c){
  
  impossible_values <- c(row_analysis(matrix, r), 
                         col_analysis(matrix, c), 
                         square_ana(matrix, r, c))
  #impossible_values <- unique(impossible_values)
  
  possible_values <- setdiff(1:9, impossible_values)
  possible_values
}


# Find all possible values for a given grid, even multiple answers
one_pass_over_grid <- function(grid_to_test){
  
  # C
  possibles_values <- vector("list" ,length = 9 * 9)
  dim(possibles_values) <- c(9, 9)
  
  for(col in 1:9){
    for(row in 1:9) {
      if(is.null(row)  || is.null(col)){ browser()}
      
      if( is.na(grid_to_test[row, col]) ) {
        possibles_values[[row, col]] <- cell_analysis(grid_to_test, row, col)
      } else {
        possibles_values[[row, col]] <- NA
      }
    }
  }
  
  possibles_values
  
}


# Input only certain values in the grid
# If any further search is impossible returns with status == "Dead3
# If no evident inputs returns with has_evident_inputs == FALSE
input_new_values <- function(grid_to_test){
  
  new_values <- one_pass_over_grid(grid_to_test$grid)
  
  #Find cells that cannot be completed
  no_fill_values <-  new_values %>% 
    map(~length(.x) == 0 && !is.na(.x)) %>% 
    as.logical()
  
  # Signal to stop iteration if grid becomes impossible to complete
  # Triggers jump to next decision value
  if (sum(no_fill_values > 0) | (sum(is.na(no_fill_values)) > 0)) {
    grid_to_test$status <- "Dead"
    grid_to_test$has_evident_inputs <- FALSE
  }else {
    
    # Get index of all certain values
    certain_index <-  
      new_values %>% 
      map(~length(.x) == 1 && !is.na(.x)) %>% 
      as.logical() %>% 
      which()
    
    if(length(certain_index) > 0){
      
      # Replace missing values in grid_to_test with only certain values
      for (idx in certain_index) {
        grid_to_test$grid[[idx]] <- new_values[[idx]]
        #cat("Add value ", new_values[[idx]], " To grid\n")      
      }
      
    } else { # Only multiple choices per cell 
      grid_to_test$has_evident_inputs <- FALSE
      grid_to_test$new_values <- new_values
    }
  }
  
  # Return the modified version
  grid_to_test
}




# Core function of the search, analyzes the grid until only multiple choices per cell
# Or no possible solution with current grid
successive_search <- function(grid_to_test){
  
  # Initialization of the object
  if(is.matrix(grid_to_test)){
    results <- list()
    results$grid <- grid_to_test
    results$new_values <- list()
    results$decisions <- tibble(index = integer(), 
                                choices = list())
    results$has_evident_inputs <- TRUE
    results$status <- ""
    results$iteration <- 1
    
  } else{
    results <- grid_to_test
  }
  
  # Input all evident values in the grid
  while(results$has_evident_inputs){
    results <- input_new_values(results)
    
  }
  
  # Stops further search if sudoku is solved
  if( sum(is.na(results$grid)) == 0 ){
    cat("Sudoku Solved !\n")
    results$status <- "Done"
    return(results)
    
    # Stops Further iteration if status is dead  
  }else if (results$status == "Dead"){
    return(results)
    
  }else {
    
    # NEED TO KEEP SEARChING
    
    results$has_evident_inputs <- TRUE
    results <- possible_decisions(results)
    results$iteration <- results$iteration + 1
    
    results <- iterate_decisions(results)
  }
}




# ---- Functions for recursive seach -----

# Determine all possible choices for a given grid in a nice tibble
# Index are ordered by increasing number of possible values

possible_decisions <- function(grid_object){
  possibles <- grid_object$new_values
  nb_possib <- map(possibles, ~length(na.omit(.x))) %>% as.double()
  
  # Find cells with possible answers
  idx_nb_possib <- which(nb_possib != 0)
  
  # Put possible decisions in a long format tibble
  decisions <- tibble(index = idx_nb_possib, 
                      nchoices = nb_possib[nb_possib != 0]) %>% 
    mutate(values = map(index, ~possibles[[.x]])) %>% 
    unnest(values) %>% 
    arrange(nchoices)
  
  grid_object$decisions <- decisions
  
  grid_object
}


# Iterates through all possible decisions one after another
iterate_decisions <- function(grid_object){
  modified_object <- grid_object
  modified_grid <- grid_object$grid
  
  decisions <- grid_object$decisions
  
  for (idx in 1:nrow(decisions)){
    # cat( "At index ", decisions$index[[idx]], 
    #      " value chosen:" , decisions$values[[idx]],
    #      " Among : ", decisions$nchoices[[idx]], "\n")
    
    modified_grid[ decisions$index[[idx]] ] <-  decisions$values[[idx]]
    
    modified_object$grid <- modified_grid
    
    new_results <- successive_search(modified_object)
    
    # Cancels previous modification and skip to next if status == "Dead"
    if (new_results$status == "Dead"){
      modified_object$grid[[ decisions$index[[idx]] ]] <- NA
      next
    }
    
    return(new_results)
  }
}

# sudoku_solve <- function(difficulty){
#   grid <- grid_fetch(difficulty)
#   solution <- successive_search(grid)
#   list(grid, solution$grid)
# }


