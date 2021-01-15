#!/opt/python/gnu/2.7.9/bin/python

import glob 
from datetime import datetime
import os

cdir = os.getcwd()
cmonth=cdir[-2:]
cpattern = '*.nc'

file_list = sorted(glob.glob(cpattern))
tag_list = [name.rstrip('.nc').split('_')[-1] for name in file_list]
str_list = [datetime.strptime(name.split('-')[0],'%Y%m%d') for name in tag_list]
end_list = [datetime.strptime(name.split('-')[-1]+'235959','%Y%m%d%H%M%S') for name in tag_list]
eff_list = [ str_list[ii] + (end_list[ii] - str_list[ii])//2 for ii in range(0,len(file_list))]
out_list = [ file_list[ii] for ii in range(0,len(file_list)) if eff_list[ii].month == int(cmonth)]
print " ".join(out_list)
