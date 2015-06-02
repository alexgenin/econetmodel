# # 
# # Test some functions to analyse secondary extinctions
# # 
# 
# # 
# context('Testing the secondary extinctions counting functions')
# 
# # Sp 5 is secondary extinct
# testmat <- cbind(matrix(c(.1,.1,1,0,0,.3,.4,.1), ncol=8, nrow=1),
#                 matrix(c(0,0,0,1,0,0,0,0), ncol=8, nrow=1))
# testmat <- rbind(testmat, testmat)
# 
# colnames(testmat) <- c(paste0("sp",seq.int(8)),
#                       paste0('removedspecies',seq.int(8)))
# 
# 
# test_that('sec_extinctions works', { 
#   
#   expected_result <- 
#     structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  
#     FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(2L,
#     8L), .Dimnames = list(NULL, c("sp1", "sp2", "sp3", "sp4", "sp5", 
#     "sp6", "sp7", "sp8")))
#               
#   expect_equal(count_sec_extinctions(testmat, sp1:sp8, 
#                                removedspecies1:removedspecies8),
#                expected_result)
#   
#   # Should provide the same testmat even if testmat if a df
#   expect_equal(count_sec_extinctions(as.data.frame(testmat), sp1:sp8, 
#                                removedspecies1:removedspecies8),
#                expected_result)
#               
#   
# })
