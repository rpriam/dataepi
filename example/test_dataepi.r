

# install.packages("pracma")
# install.packages("pwr")
# install.packages("plyr")
# install.packages("officer")
# sudo apt-get install libxml2-dev
# install.packages("xtable")
# install.packages("Rcpp", INSTALL_opts = '--no-lock')
# install.packages("minqa", INSTALL_opts = '--no-lock')
# install.packages("RcppEigen", INSTALL_opts = '--no-lock')
# install.packages("lme4", INSTALL_opts = '--no-lock')
# install.packages("car")
# ldconfig -p | grep fortran
# sudo ln -s /usr/lib/x86_64-linux-gnu/libgfortran.so.5 /usr/lib/libgfortran.so
# sudo ldconfig
# ldconfig -p | grep quadmath
# sudo ln -s /usr/lib/x86_64-linux-gnu/libquadmath.so.0 /usr/lib/libquadmath.so
# sudo ldconfig
#

library(dataepi)


## ----------------------  DATA PREPARATION ------------------------------------

library(MASS)
data("Pima.tr")
data("Pima.te")
A = rbind(Pima.tr, Pima.te)
A$id = 1:nrow(A)
A$npreg = as.character(A$npreg)
A$npreg[A$npreg %in% as.character(10:17)] = "10_17"
A$diabet = as.numeric(as.character(A$type) == "Yes")
A$bp_pb = as.character(as.numeric(as.numeric(as.character(A$bp)) > 90))
A$glu_cl3 = as.character(cut( A$glu,  c(56, 103, 129, 199),  labels = c("56_103", "103_129", "129_199") ))
A$skin_cl3 = as.character(cut(A$skin, c(7, 24, 33, 99), labels = c("7_24", "24_33", "33_99") ))
A$bmigroups = as.character(cut( A$bmi,  c(0, 25, 30, 100),   labels = c("a_normal", "b_overweight", "c_obese") ))
A$ped_cl3 = as.character(cut( A$ped, c(0.085, 0.295, 0.557, 2.420),   labels = c("low", "middle", "large") ))

A$age_cl3 = as.character(cut(A$age, c(21, 24, 33, 81), labels = c("21_24", "24_33", "33_81")))

var_y = "diabet"
vars_cont = c("age", "bp", "glu", "skin", "bmi", "ped", "age")
vars_disc = c("npreg",
              "bp_pb",
              "bmigroups",
              "glu_cl3",
              "skin_cl3",
              "ped_cl3",
              "age_cl3")

vars_x = c("npreg", "bp_pb", "bmigroups", "skin_cl3", "ped_cl3", "age_cl3")
vars_int = NULL
var_id = "id"

vars_cont = unique(vars_cont)
vars_disc = unique(vars_disc)
vars_x = unique(vars_x)

for (j in 1:ncol(A)) {
  nv = names(A)[j]
  if (substr(nv, nchar(nv), nchar(nv)) %in% paste(0:9))
    names(A)[j] = paste(names(A)[j], "_", sep = "")
  if (sum(nv %in% vars_cont))   {
    l = which(vars_cont %in% nv)
    vars_cont[l] = names(A)[j]
    
  }
  if (sum(nv %in% vars_disc))   {
    l = which(vars_disc %in% nv)
    vars_disc[l] = names(A)[j]
  }
  if (sum(nv %in% vars_x))   {
    l = which(vars_x %in% nv)
    vars_x[l] = names(A)[j]
  }
}

# list_supp = list()
# list_supp$where = " "
# list_supp$who = " "
# list_supp$disease = " "
# list_supp$objective = " "
# list_supp$project = " "
# list_supp$inex = " "

str(A)

A0 = A


## ----------------------  REPORT GENERATION -----------------------------------

fp = data_prepare(A, var_y, vars_cont, vars_disc, var_id)
A = fp$A
A = A[, unique(c(var_y, vars_cont, vars_disc, vars_x, var_id))]

desc_all <- tab_contents(A)
print(desc_all)

desc_cont <- tab_desc_cont(A, vars_cont)
desc_disc <- tab_desc_disc(A, vars_disc)
desc_biv <- tab_desc2class_cont(A, vars_cont, var_y)
test_tt <- tab_tt2classes_cont(A, vars_cont, var_y)
test_anova <- tab_ttanova_cont(A, vars_cont, vars_disc)
test_chi2 <- tab_chi2all(A, c(var_y, vars_disc), pvalue_seuil_ = 0.05)
or <- tab_all2x2(A, vars_x, var_y, stat_oddsratio)
rr <- tab_all2x2(A, vars_x, var_y, stat_relativerisk)
gg <- tab_glmorr(A, vars_x, var_y)


au = dataepi::rep_compute(A, var_y, vars_x, vars_cont, vars_disc, vars_int, var_id)

data_ = "diabet_pima"
fnl <- paste("./report_dataepi_", data_, ".tex", sep = "")
# if (file.exists(fnl)) { file.remove(fnl) }

if (!exists("list_supp")) list_supp = NULL

wr = dataepi::rep_write(fnl,
                        "tex",
                        A,
                        var_y,
                        vars_x,
                        vars_cont,
                        vars_disc,
                        vars_int,
                        var_id,
                        list_supp)

file_tex = read.csv(fnl, header = FALSE)
print(file_tex[1:10, ])
