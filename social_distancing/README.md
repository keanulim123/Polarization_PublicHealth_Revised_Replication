# VirusGPS README

## Overview
The results in "Polarization and Public Health: Partisan Differences in Social Distancing During the Coronavirus Pandemic" can be reproduced by completing the following steps:

1. Ensure that your computing environment satisfies the **Requirements** section below.

2. Download the data described in the **External Data** section to your local computer.

3. Complete the steps described in the **Setup** section.

4. Run ```python run_all.py``` from the root of the repository (as described in the **Build** section).

The final paper will then be produced at `paper_slides/output/paper.pdf`.

## Folder Structure
This repository is structured as follows:

* `setup/` contains programs to download required packages and otherwise assist in setup.
* `raw/` contains raw data used elsewhere in the repo.
* `data/` contains code and output related to the cleaning of datasets.
* `analysis/` contains code and output related to the analysis of cleaned data.
* `paper_slides/` contains code and output related to paper production itself, using the output produced in `analysis`.
* `lib/` stores programs used to facilitate running the repository, including tools to link together different modules (eg `data/` and `analysis/`).

## Requirements
All requirements must be installed and set up for command line usage. For further detail, see the **Command Line Usage** section below.

* Python (2.7/3.7)
* pip (>=10.0)

To build the repository as-is, the following applications are additionally required:

* git-lfs
* LyX
* R
* Stata

These software are used by the scripts contained in the repository. By default, the **Setup** and **Build** instructions below will assume their usage.

## External Data
In addition to the data contained in this repository, reproduction requires access to the following pieces of data:
* **SafeGraph**: Including SafeGraph's Monthly Patterns, Weekly Pattterns, Daily Social Distancing, Open Census, and Places datasets. Also includes cleaned weather data shared with the SafeGraph consortium by Jude Bayham. For the required folder structure of an external SafeGraph data folder, see `data/external.txt`.
* **Precinct Shapefiles**: Can be downloaded from [https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NH5S2I](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/NH5S2I)
* **Census Block Group Shapefiles**: Can be downloaded by state from [ftp://ftp2.census.gov/geo/tiger/TIGER2019/BG/](ftp://ftp2.census.gov/geo/tiger/TIGER2019/BG/)

## Setup
**If you are using Windows, you may need to run certain bash commands in administrator mode due to permission errors. To do so, open your terminal by right clicking and selecting `Run as administrator`. To set administrator mode on permanently, refer to the [RA manual](https://github.com/gentzkow/template/wiki/Repository-Usage#Administrator-Mode).**

1. Create a `config_user.yaml` file in the root directory. A template can be found in the `setup` subdirectory. See the **User Configuration** section below for further detail.

2. Install `spatialindex`, to be used in mapping programs. This can be done for example with Homebrew by running `brew install spatialindex` in a terminal. 

3. Install Python dependencies listed in the `requirements.txt` file using pip. One way to do this is to run the following bash command in a terminal from the `setup` subdirectory:
   ```
   python -m pip install --user -r requirements.txt
   ```

4. Run the `check_setup.py` file. One way to do this is to run the following bash command in a terminal from the `setup` subdirectory:
   ```
   python check_setup.py
   ```

5. Install Stata dependencies using the `setup_stata.do` file. One way to do this is to use the following bash command from the `setup` subdirectory:
   ```
   stata-mp -e setup_stata.do
   ```

   If you are using a Windows, you will likely have to adjust this bash command:
   ```
   stata_executable -e setup_stata.do
   ```

   `stata_executable` refers to the name of your Stata executable. For example, if your Stata executable was located in `C:\Program Files\Stata15\StataMP-64.exe`, you would want to use the following bash command:

   ```
   StataMP-64 -e setup_stata.do
   ```

6. Install R dependencies using the `setup_r.r` file. One way to do this is to run the following bash command in a terminal from the `setup` subdirectory:
   ```
   Rscript setup_r.r
   ```
   
7. Install the [libspatialindex](https://libspatialindex.org/) library: If you use homebrew, this can be done via `brew install spatialindex`.


## Build
**If you are using Windows, you may need to run certain bash commands in administrator mode due to permission errors. To do so, open your terminal by right clicking and selecting `Run as administrator`. To set administrator mode on permanently, refer to the [RA manual](https://github.com/gentzkow/template/wiki/Repository-Usage#Administrator-Mode).**

1. Follow the *Setup* instructions above.

2. From the root of repository, run the following bash command:
   ```
   python run_all.py
   ```

## Command Line Usage

For specific instructions on how to set up command line usage for an application, refer to the [RA manual](https://github.com/gentzkow/template/wiki/Command-Line-Usage).

By default, the repository assumes the following executable names for the following applications:

```
application : executable
python      : python
git-lfs     : git-lfs
lyx         : lyx
r           : Rscript
stata       : statamp (will need to be updated if using a version of Stata that is not Stata-MP)
```

These are the standard executable names for Mac and are likely to differ on your computer if you are using Windows. Executable names for Windows will typically look like the following:

```
application : executable
python      : python
git-lfs     : git-lfs
lyx         : LyX#.# (where #.# refers to the version number)
r           : Rscript
stata       : StataMP-64 (will need to be updated if using a version of Stata that is not Stata-MP or 64-bit)
```

Default executable names can be updated in `config_user.yaml`. For further detail, see the **User Configuration** section below.

## User Configuration
`config_user.yaml` contains settings and metadata such as local paths that are specific to an individual user and thus should not be committed to Git. For this repository, this includes local paths to [external dependencies](https://github.com/gentzkow/template/wiki/External-Dependencies) as well as executable names for locally installed software.

Required applications may be set up for command line usage on your computer with a different executable name from the default. If so, specify the correct executable name in `config_user.yaml`. This configuration step is explained further in the [RA manual](https://github.com/gentzkow/template/wiki/Repository-Structure#Configuration-Files).