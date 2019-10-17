# inbolims

## Description

R scripts supporting the LIMS functionality (specific for INBO - so not useful outside of the institute and its partners)
The main focus are data queries based on the data warehouse of the lab results in the INBO institute.

It accesses both the main database of the LIMS system and the data warehouse around it, and these databases are only accessible from within the institute of via the VPN of the institute.

This repository contains a subdirectory with the same name: inbolims.

## Usage by the LIMS system

The subdirectory called inbolims is an R package to be installed on the R client on the LIMS server. The `inst/lims_scripts` folder of the package contains the scripts that are used by the LIMS software itself and have to be copied into the Data/R_SCRIPTS folder on the LIMS server each time the package is updated.The package has a function included that does this for you: `copy_scripts_to_lims_path(pathName)`

When the LIMS system accesses the scripts, the program Rscript is called with parameters by the LIMS software. These can be accessed in R via commandArgs(). It checks if there are at least 5 arguments and then these will be used. If there are less than 5 arguments, the package assumes that it is a testing environment, and the commandArgs will have to be specified manually during the test and will use the locally available dbcredentials.txt for the database information, which is not a part of the git repository. The function that controls tihis procedure is `prepare_session()`. Reproducing errors happens to give the call_id to the function like this: `prepare_session(args = commandArgs(), min_args = 5, first_arg = min_args, cred_file = "dbcredentials.txt", call_id = 25)`. The call_id can be found in the C_RSCRIPT_ARGS table in the LIMS DB. This table logs all the R calls by the LIMS system and also contains the variables that are passed by the lims system to R.


