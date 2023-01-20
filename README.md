Last edited: 20 January 2023

# Refining the LTS Concept

This repository is for the "Refining Lake Trophic Status Concept"
Project. Michael F Meyer (mfmeyer@usgs.gov) is the main owner of the
group, but owner permissions may change through time.

The repository is meant to contain all scripts, data, and figures
relating to the project. MFM structured the project with a few
components that he hopes will allow for members to be able to work most
efficiently, but he recognizes that this may not be realized. If you
have suggestions for improvements, please feel free to send them over
email or as a GitHub Issue directed at Michael.

Directory Architecture: MFM designed the main directories to be
something like this:
```
~/refining_lts
├───data
│   ├───derived_products
│   └───nla_all_years
├───figures
└───scripts
```

Ideally all scripts should be read in from the `nla_all_years`
directory, and then we can output any derived data products to the
`derived_products` directory. All figures can be output to the `figures`
directory.

As this project is more limited with respect to coding and analyses, you
are welcome to either fork or branch the repository or create a script
with your name/initials in the file name, and then commit that script to
the scripts folder.

To be aware of what others are working on, please make a Git Issue with
a short write-up of your figure ideas.

# Disclaimer
This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.
