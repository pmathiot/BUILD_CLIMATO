#!/bin/bash
# DIRECTIVES:
#PBS -N put_data
#PBS -q shared
#PBS -V
#PBS -l walltime=03:00:00
#PBS -l mem=4gb
#PBS -l ncpus=2
#===========================================
ulimit -s unlimited
#===========================================
cd $BUILD_DIR/$YEAR
MOO_DIR=/crum/$RUN_ID/onm.nc.file
NFILE=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1m_${YEAR}??01_${YEAR}????_grid_?.nc | wc -l `
if [[ $NFILE -ne 48 ]]; then echo "error in number of monthly files $YEAR : $NFILE"; exit 1; fi
for MM in 01 02 03 04 05 06 07 08 09 10 11 12 ; do
   FILE_LST=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1m_${YEAR}${MM}01_${YEAR}${MM}??_grid_?.nc`
   echo " saving file : "
   for FILE in $FILE_LST; do
       echo "-- $FILE"
       moo put $FILE moose:$MOO_DIR/.
   done
done

MOO_DIR=/crum/$RUN_ID/ons.nc.file
NFILE=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1s_??????01_${YEAR}????_grid_?.nc | wc -l `
if [[ $NFILE -ne 16 ]]; then echo "error in number of season files $YEAR : $NFILE"; exit 1; fi
FILE_LST=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1s_$((YEAR-1))??01_${YEAR}????_grid_?.nc`
echo " saving file : "
for FILE in $FILE_LST; do
    echo "-- $FILE"
    moo put $FILE moose:$MOO_DIR/.
done

FILE_LST=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1s_${YEAR}??01_${YEAR}????_grid_?.nc`
echo " saving file : "
for FILE in $FILE_LST; do
    echo "-- $FILE"
    moo put $FILE moose:$MOO_DIR/.
done

MOO_DIR=/crum/$RUN_ID/ony.nc.file
NFILE=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1y_$((YEAR-1))??01_${YEAR}????_grid_?.nc | wc -l `
if [[ $NFILE -ne 4 ]]; then echo "error in number of annual files $YEAR : $NFILE"; exit 1; fi
FILE_LST=`ls $BUILD_DIR/$YEAR/${RUN_ID}o_1y_$((YEAR-1))??01_${YEAR}????_grid_?.nc`
echo " saving file : "
for FILE in $FILE_LST; do
    echo "-- $FILE"
    moo put $FILE moose:$MOO_DIR/.
done
#===========================================

