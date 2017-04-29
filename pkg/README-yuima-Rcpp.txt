When you add Rcpp or other C code,
please always do, within R-devel Console,

move to svn/pkg
run R-devel
and type

tools::package_native_routine_registration_skeleton('yuima',,,FALSE)

take the output and update the file

yuima/src/yuima_init.c

with the above output
# see the help of "package_native_routine_registration_skeleton"
library(tools)
?package_native_routine_registration_skeleton

# this page is also of interest
# http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols
