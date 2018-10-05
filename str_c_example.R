# Write a function that turns c("a", "b", "c") into "a, b, and c."
str_vec2series = function(vec){
  # Convert "vec" into a comma separated series for printing.
  #
  # Argument: vec a character vector
  # Returns: A single string with the entries of vec listed in series.
  
  n = length(vec)
  if( n == 0 ) return("none.")
  
  if( n == 1 ) return( str_c(vec[1], '.') )

  if( n == 2 ) return( str_c(vec[1], " and ", vec[2], '.') )

  if ( n > 2 ){
    out = str_c( vec[1:{n-1}], collapse = ', ')
    str_c(out, " and ", vec[n], '.')
  }
}

# Tests: ----------------------------------------------------------------------
vec = c("a", "b", "c") 
str_vec2series(vec)
str_vec2series(letters)
str_vec2series(letters[1:2])
str_vec2series(letters[1])
str_vec2series(NULL)

#! Challenge: add an argument to optionall add an "Oxford comma" before the 
# final and.
