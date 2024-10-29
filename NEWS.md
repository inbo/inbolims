# inbolims 0.2.12

* Toevoegen functionaliteit om staalinformatie op te halen
    * `lims_sample_information` (hoofdfunctie)
    * `lims_table_fields`
    * `sample_fields_from_template`

# inbolims 0.2.11

* some improvements in texture_parsing
    * handle files with only one sample (slightly different format)
    * handle sample_file_names that have no underscore behind the sample name

# inbolims 0.2.9

* Change the minimal necessary fields
* Recode functions to work with these minimal fields

# inbolims 0.2.8

* Change the code so it only depends on:
    * combi field
    * original sample
    * project
which makes customizing report field easier

# inbolims 0.2.7

# inbolims 0.2.6

* Added a `NEWS.md` file to track changes to the package.
* Add [`checklist`](https://inbo.github.io/checklist/) infrastructure.
