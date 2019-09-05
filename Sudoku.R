library(sudoku)

#Creates a sudoka puzzle from library

puz = sudoku::generateSudoku(38)
original = puz
zeros = matrix(0,9,9)
fixed = t(data.frame(c(1:9)))
hold = matrix(0,9,9)

#Assign zero matrix to compare to 

#Create function which works out how many possible numbers can go in each space so we can run this
# everytime a number is filled in - Maybe add a while loop untill whole array is zeros

checker = function(puzzle,x){
  
  index = which(puzzle == x)
  
  return(index)

  
}

section = function(r,c,puzzle){
  
  if ((r %in% c(1,2,3)) && (c%in%c(1,2,3)))
  {
    return(puzzle[1:3,1:3])
  }
  else if ((r%in% c(1,2,3)) && (c%in%c(4:6)))
  {
    return(puzzle[1:3,4:6])
  }
  else if ((r%in%c(1,2,3)) && (c%in%c(7:9)))
  {
    return(puzzle[1:3,7:9])
  }
  else if ((r %in% c(4:6)) && (c%in%c(1,2,3)))
  {
    return(puzzle[4:6,1:3])
  }
  else if ((r %in% c(4:6)) && (c%in%c(4:6)))
  {
    return(puzzle[4:6,4:6])
  }
  else if ((r %in% c(4:6)) && (c%in%c(7:9)))
  {
    return(puzzle[4:6,7:9])
  }
  else if ((r %in% c(7:9)) && (c%in%c(1,2,3)))
  {
    return(puzzle[7:9,1:3])
  }
  else if ((r %in% c(7:9)) && (c%in%c(4:6)))
  {
    return(puzzle[7:9,4:6])
  }
  else if ((r %in% c(7:9)) && (c%in%c(7:9)))
  {
    return(puzzle[7:9,7:9])
  }
  
}

possible = function(row,colum, puzzle){
  
  row_numbers = as.vector(puzzle[row,])
  colum_numbers = as.vector(puzzle[,colum])
  section_matrix = (as.vector(section(row, colum,puzzle)))
  
  w1 = fixed[!fixed %in% row_numbers]
  w2 = w1[!w1 %in% colum_numbers]
  w3 = w2[!w2 %in% section_matrix]
  
  return(list(w3, length(w3)))
  
}

main_function = function(index,puzzle){
  
  amount = matrix(0,9,9)
  
  for (i in index){
    

    colum = ceiling(i/9)
    row = (i%%9)
    
        if (row== 0) {
      row = 9
        }
    
    x = possible(row, colum, puzzle)
    
    w3 = x[[1]]
    amount[row, colum] = x[[2]]
    
    if (length(w3) == 1){
      puzzle[row,colum] = w3
      amount[row, colum] = amount[row, colum] -1
    }
    
    
  }

  return(list(puzzle, amount))
}

test = checker(puz,0)
results = main_function(test,puz)

current_puzzle = results[[1]]
current_amount = results[[2]]


while (identical(zeros,current_amount) == F) {
  
  test = checker(current_puzzle,0)
  results = main_function(test,current_puzzle)
  
  current_puzzle = results[[1]]
  current_amount = results[[2]]
  
  if (identical(( current_amount == 1 ),matrix(F,9,9))) {
  
    break
    
  }
  
}

Final_Puzzle = current_puzzle

if (identical(zeros,current_amount)) {
  
  print('The Sudoku Puzzle has been solved')
  
} else {
  print('The Sudoku Puzzle could not be solved')
  
}

## Now solving the puzzle by trying both possible options when there are 2
# Create if statement to run this code if necessary

twos_index = checker(current_amount,2)

first_two_index = twos_index[1:2]
colum = ceiling(first_two_index/9)
row = (first_two_index%%9)
if (identical(row, c(0,0))){
  row = c(9,9)
}


y = possible(row[1], colum[1], current_puzzle)
t = possible(row[2], colum[2], current_puzzle)

two_trials_1 = y[[1]]
two_trials_2 = t[[1]]

half_solved = current_puzzle
half_amount = current_amount

solved = 0 #Iterate through each position of two pssible inputs until solved = 1 

for (i in c(1:2)){
  for (j in c(1:2)){
    
    current_puzzle = half_solved 
    current_amount = half_amount
    
    input_1 = two_trials_1[i]
    input_2 = two_trials_2[j]
    
    # if ((input_1 == input_2)){
    #   next #as this is against sudoku rules
    # }
    
    current_puzzle[row[1],colum[1]] = input_1
    current_puzzle[row[2],colum[2]] = input_2
    
    test = checker(current_puzzle,0)
    results = main_function(test,puz)
    
    current_puzzle = results[[1]]
    current_amount = results[[2]]
    
    # for (k in c(1:81)){
    #   colum = ceiling(k/9)
    #   row = (k%%9)
    #   if (identical(row, c(0,0))){
    #     row = c(9,9)
    #   }
    #   z = possible(row, colum, current_puzzle)
    #   hold[row, colum] = z[[2]]
    # }
    
    current_amount = hold
    
    test2 = checker(current_puzzle,0)
    results = main_function(test2,current_puzzle)
    
    current_puzzle = results[[1]]
    current_amount = results[[2]]
    
    while (identical(zeros,current_amount) == F) {
      
      test = checker(current_puzzle,0)
      results = main_function(test,current_puzzle)
      
      current_puzzle = results[[1]]
      current_amount = results[[2]]
      
      if (identical(( current_amount == 1 ),matrix(F,9,9))) {
        break
      }
    }
  }
}


# Creat function to check if puzzle abides by rules















