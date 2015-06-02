# 
# 
# Global tests for the rockyshore model.
# 
# 

context('Testing rockyshore model behavior')

# Turn off non-trophic interactions for the moment
nt_topology <- matrix(0, ncol=8, nrow=8)
diag(nt_topology) <- 0

# Absolute paths as test_that uses its own working directory
# basedir  <- "/home/alex/docs/work/2014-2015/network_modelling/dev"
basedir  <- "/home/alex/work/projects/network_modelling/dev"
template <- paste0(basedir, "/src/templates/rockyshore.c.template")
cfile    <- paste0(basedir, "/src/compiled_systems/last_run.c")
lib.dir  <- "/tmp"
include.dir <- paste0(basedir, "/src/include")

# Create a system object
suppressMessages(
  sys <- syspreset_rockyshore(tmax=5000) %>% 
          alter_system_(cfile=cfile, 
                        source = list(template = template)) %>% 
          alter_system(state=rep(.5,8)) %>% 
          alter_parms(nt=nt_topology) %>% 
          compile(lib.dir=lib.dir, 
                  include.dir=include.dir, 
                  quiet=TRUE)
)
# Get last line of a matrix
last_state <- function(mat) { mat[nrow(mat), -1] }

# Some useful functionals 
random_state <- . %>% alter_system(state=runif(get_size(sys),0,1))



test_that("Rockyshore model behaves correctly", { 
  
  # Remove all species and expect everyone dead at the end
  final_max <- . %>% run %>% last_state %>% max
  expect_equal(sys %>% set_removal(1:8, at=3000) %>% final_max, 0)
  
  # Remove individual species and expect them dead
  final_i <- function(sys,i) { sys %>% run %>% last_state %>% `[`(i) }
  for (i in seq.int(8)) 
    expect_equal(sys %>% set_removal(i, at=3000) %>% final_i(i), 0, 
                 check.names=FALSE)
  
  # Check for any nans or negative values in computations
  for (i in seq.int(10))
    expect_false(sys %>% random_state %>% run %>% { . < 0} %>% any)
  
  # Give species very small abundance and see them stay dead
  for (i in seq.int(10)) 
    expect_equal(sys %>% alter_system(state=rep(1e-10,8)) %>% run %>% 
                   last_state %>% unique,
                 1e-10)
  
  
})
  
test_that("Integration of rockyshore model is successful", { 
  
  # Check that no warnings appears
  for (i in seq.int(10))
    expect_that(sys %>% random_state %>% run, not(gives_warning()))
  
})
