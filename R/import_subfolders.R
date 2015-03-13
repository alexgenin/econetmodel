
# Imports all subfolders file so it works with document()

# Grab a list of these subfolders
folders <- list.dirs(".", full=TRUE)
folders <- folders[folders != "."]

# Source all R files in subdirs
for (f in folders) { 
  message('Loading files in ', f)
  files <- list.files(f, full=TRUE) 
  sapply(files,source)
}
