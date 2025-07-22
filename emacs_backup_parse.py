#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys
import re
from shutil import copyfile


basenames = []
mapping = {}
for backup in os.listdir(sys.argv[1]):
    pathParts = backup.split('!')
    if pathParts[-2] == 'tex' or (pathParts[-3] == 'tex' and pathParts[-2] == 'include'):
        m = re.search('([^~]+)\.?~?([0-9]*)~?',pathParts[-1])
        basename = m.group(1).rstrip('.')
        postfix = m.group(2)
        if not postfix:
            postfix = '0'
        number = int(postfix)
        real_path = os.path.join(*['/']+pathParts[1:-1]+[basename])
        if basename in mapping:
            if number > mapping[basename][0]:
                mapping[basename] = [number,os.path.abspath(backup),real_path]
        else:
            mapping[basename] = [number,os.path.abspath(backup),real_path]
for key in mapping:
    print('Copying',mapping[key][1],'->',mapping[key][2])
    copyfile(mapping[key][1],mapping[key][2])
    
