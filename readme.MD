*Instructions for how to use this file are assuming the user has never used R before on their system.*

This file takes a set of bank information in a file named `Email Logs.csv` and creates a new .csv file called `processed_email_logs.csv` in the same directory. This new .csv file will contain information on the start and end times of bank emails, along with the duration of emails.



To use this file, the user will place this script in the same directory as `Email Logs.csv`. Then, run `$ Rscript "Email Logs.csv"` in a bash terminal.

Should R not be installed on the machine already, install R [here](https://cran.rstudio.com/). To check if the install was completed, run `$ R` in a bash terminal. Should this work, you will see a prompt welcoming you to R. run `q()` to leave R and return to bash. If this does not work, add the installation path of R to your system environment PATH variable. For example, my installation on Windows needed `"C:\Program Files\R\R-4.0.1\bin"` to be added to PATH. Once this is completed, the command `Rscript` should work on your machine, running from bash.

To run this file, run `$ Rstudio email_log_processor.r` in bash. If this returns no errors, you will see a new `processed_email_logs.csv` in your directory.

Note that this file will take longer to run the first time, as it has to install required packages first.
