#!/opt/python/gnu/2.7.9/bin/python

import calendar
import os

cdir = os.getcwd()
cmonth=cdir[-2:]
cyear=cdir.split('/')[-3]
cendday=str(calendar.monthrange(1,int(cmonth))[-1])

print cyear+cmonth+'01'+'_'+cyear+cmonth+cendday

