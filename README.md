# zooprocessr

A package to deal with data produced by [zooprocess](http://www.obs-vlfr.fr/LOV/ZooPart/ZooScan/rubrique.php3?id_rubrique=49?lang=en "ZooScan - Home page").

## Installation

The package is under development and is not on CRAN yet. To install it, the simplest method is therefore

    # install.packages("devtools")
    devtools::install_github("jiho/zooprocessr")

## Usage

Currently, this package mainly facilitates reading data from zooprocess projects. Please see the help for the functions

    ?read_ids
    ?read_variables
    ?read_meta

and the lower lever function `read_pid` which reads pid-like files.
