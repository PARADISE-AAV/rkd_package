# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(rkdpipeline)



testthat::test_that("function LoadRKD", {
  file_name=1
  testthat::expect_error(load_rkd(file_name), "Your argument need to be a character")
  
  file_name="a/b"
  testthat::expect_error(load_rkd(file_name), 'Your input folder doesn\'t exist')
})

testthat::test_that("function cleanRKD", {
  library(lubridate)
  rkd=as.data.frame(NULL)
  output_path=1
  testthat::expect_error(clean_rkd(rkd, output_path), "Your argument need to be a character")
  
  rkd=NULL
  output_path=getwd()
  testthat::expect_error(clean_rkd(rkd, output_path), "Your argument need to be a data frame")
  
  rkd=as.data.frame(NULL)
  output_path="a"
  testthat::expect_error(clean_rkd(rkd, output_path), "Specified output folder does not exist")
  
  rkd=as.data.frame(NULL)
  output_path=getwd()
  testthat::expect_error(clean_rkd(rkd, output_path), "You supplied an empty file")
  
  rkd=cbind.data.frame(RKD.ID=c(1,1,2,2,3,3),
                       Date.Of.Visit=c("1985/10/01","1987/12/02","1998/11/14", "1999/11/19", 
                                       "1987/12/25", "2005/01/03"),
                       Date.of.Birth.known.unknown=c("Known","Known", "Unknown", "Unknown", "Unknown", "Unknown"))
  #testthat::expect_equal(is.Date(rkd_parse_dates(rkd)$Date.Of.Visit), TRUE)
  #testthat::expect_equal(is.Date(rkd_parse_dates(rkd)$Date.of.Birth.known.unknown), FALSE)
  
  rkd2=cbind.data.frame(RKD.ID=c(1,1,2,2,3,3,4,5,6,7),
                        Ethnicity=c("White-Irish", "White-Irish","White-French", "White-French", "B2 - African", "Black","Asian", "Mixed-Black-White", "NS - Not Stated", "Other"))
  testthat::expect_equal(nrow(table(rkd_collapse_ethnicity(rkd2)$Ethnicity)),6)
  
  rkd3=cbind.data.frame(RKD.ID=c(1,1,2,2,3,3,"1",6),
                       Date.Of.Visit=c("1985/10/01","1987/12/02","1998/11/14", "1999/11/19", 
                                       "1987/12/25", "2005/01/03","2013/02/05","2023/05/14"),
                       Patient.ID=c(1,1,2,2,3,3,1,6)
                       )
  testthat::expect_equal(rkd_fix_ids(rkd3)$RKD.ID[7],"1")
  
  
}) 


testthat::test_that("function DemographicFilterRKD", {
  
  rkd=as.data.frame(NULL)
  output_path=1
  testthat::expect_error(DemographicFilterRKD(rkd, output_path), "The argument output_path need to be a character argument")
  
  rkd=NULL
  output_path=getwd()
  testthat::expect_error(DemographicFilterRKD(rkd, output_path), "The argument RKDdata need to be a dataframe argument")
  
  rkd=as.data.frame(NULL)
  output_path=getwd()
  testthat::expect_error(DemographicFilterRKD(rkd, output_path), "You give an empty files")
  
  output_path=getwd()
  rkd=cbind.data.frame(RKD.ID=c(1,2,3,4,5,6),
                       Small.vessel.vasculitis..ANCA.associated.=c("Granulomatosis with polyangiitis (Wegener) - Orpha:900","Granulomatosis with polyangiitis (Wegener) - Orpha:900","vasculitis","Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727","HV", "Microscopic polyangiitis (including renal limited vasculitis) - ORPHA:727"),
                       Diagnosis.confidence=c("Definite", "Definite","", "probably", "maybe", "Definite"),
                       Small.vessel.vasculitis..Immune.complex.=c("", "", "", "ANTI/GBM", "", ""),
                       Secondary.vasculitis=c("No", "Yes","No", "Yes", "No", "No"),
                       Other=c("No", "Yes","No", "Yes", "No", "No"),
                       Medium.vessel.vasculitis=c("", "Kawasaky", "PAN", "", "", ""),
                       Large.vessel.vasculitis= c("", "GCA", "Takayazu", "GCA", "", ""),
                       Variable.vessel.vasculitis= c("", "", "Behcet", "", "Cogan", ""),
                       At.any.point.ANCA.specificity=c("PR3", "MPO", "", "", "", ""),
                       Biopsy.performed.=c("Yes", "no", "", "Yes", "No", "Yes"),
                       Histologically.confirmed.diagnosis=c("No", "Yes", "No", "Yes", "Yes", "Yes"))
  testthat::expect_equal(nrow(DemographicFilterRKD(rkd, output_path)), 2)
  
}) 