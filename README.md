# pRebal4Rpath

## Introduction

This temporary GitHub repository is the first stone of a framework for conducting in an easier way the PreBal test on Ecopath models ([Link 2010](https://doi.org/10.1016/j.ecolmodel.2010.03.012])). 
As the recently developed Rpath package ([Lucey et al., 2020](https://doi.org/10.1016/j.ecolmodel.2020.109057)) opens new perspectives for reproducibility of mass-balance food-web models, and robust of sensitivity analysis of the latter, we aim here to design functions that are transferable to Rpath objects. 

This is an undergoing project. So far, it allows modelers to conduct PreBal on an Ecopath model built with the EwE user interface.
We are currently working on the transferability to Rpath objects.

## Translating from EwE to Rpath

<h5 style="color:red;">Under development</h5>

## Conducting PreBal

The function `run_pRebal`(available in `function_run_pRebal.R`, in the [R folder](https://github.com/pyhernvann/pRebal4Rpath)) allows to run the regression and plot the graphs that are part of a classic PreBal diagnostic.

The `.Rmd` and associated `.html` `example_Prebal` give guidelines on how to use R to import information from the EwE user interface and use `run_pRebal` to conduct a PreBal analysis. We illustrate this process using a example of the Celtic Sea Ecopath model taken from [Hernvann et al., 2020](https://doi.org/10.3389/fmars.2020.578717) (version anterior to the one published).

<h5 style="color:red;">The run_pRebal is currently being adapted to work with Rpath objects.</h5>

