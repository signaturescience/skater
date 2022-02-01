## Test environments

- Local MacOS install, R 4.0.4
- R hub
    - Fedora Linux, R-devel, clang, gfortran
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC
    - Windows Server 2022, R-devel, 64 bit

## R CMD check results

- Local `R CMD check`: Status OK, 0 errors, 0 warnings, 0 notes
- R hub: 
    - Fedora Linux, R-devel, clang, gfortran: OK
    - Ubuntu Linux 20.04.1 LTS, R-release, GCC: OK
    - Windows Server 2022, R-devel, 64 bit: 
        ```
        * checking for detritus in the temp directory ... NOTE
        Found the following files/directories:
          'lastMiKTeXException'
        ```
