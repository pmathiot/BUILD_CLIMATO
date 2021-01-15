=== BUILD_CLIMATO ===

== Purpose ==

The only purpose of the BUILD_CLIMATO tool is to build multiyear mean from NEMO output

== Usage ==

Once set up, the usage is very simple:

```
./build_climato.bash [CONFIG] [CASE] [YEARB] [YEARE]
```
for example:
```
./build_climato.bash eORCA025.L121 OPM006 2009 2018
```

== How to set it up ==

- step 1: compile the f90 tools in src/f90 (modify the Makefile to fit your HPC architecture, default is CINES Occigen computer)
- step 2: modify the param.bash file
   * module:
      + load the module you need
   * variables:
      + STOPATH  = path to where your original data are store
      + WRKPATH  = path to where you want the processing to happen
      + GRID_LST = the grid list you want to process
      + LOGDIR   = where you want the log to be store 
   * functions:
      Functions are useful for portability to other output name format. The most useful variable know when the functions are call are
      RUNID=CONFIG-CASE, TAG=output from get_tag, YEAR?=input parameter and GRID= grid name.
      + get_triggername  = name for the trigger file (no need to be changed)
      + get_nemofilename = format of your NEMO output file
      + get_tag          = format of the tag name in your nemo input files
      + get_mymfilename  = format of your multi year mean output files
