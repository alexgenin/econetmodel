# 
# Tests
# 

context('List-altering functions')

flatlist <- list(a=1,b=2,c=3)
deeplist <- list(z=flatlist, d=4, e=5)

# alter_list_(flatlist, a=-1, b=-1)

test_that("Test if alter_list works", { 
  
  # SE 
  # ------------
  
  # Flat list replacement and addition
  expect_identical(alter_list_(flatlist, a=-1,b=-1, e=6), 
                   structure(list(a = -1, b = -1, c = 3, e = 6), 
                             .Names = c("a", "b", "c", "e")))
  
  # Deep list replacement
  expect_identical(alter_list_(deeplist, d=list(haha=-1), b=-1),
                   structure(list(z = structure(list(a = 1, b = 2, c = 3), .Names = c("a", 
                   "b", "c")), d = structure(list(haha = -1), .Names = "haha"), 
                   e = 5, b = -1), .Names = c("z", "d", "e", "b")))
  
  # NSE 
  # ------------
  expect_identical(alter_list(flatlist, a=10*b),
                   structure(list(a = 20, b = 2, c = 3), 
                             .Names = c("a", "b", "c")))
  expect_identical(alter_list(deeplist, z=list(b=-10*a), e=7, a=1), 
                   structure(list(z = structure(list(a = 1, b = -10, c = 3), .Names = c("a", 
                   "b", "c")), d = 4, e = 7, a = 1), .Names = c("z", "d", "e", "a")))
  
})

