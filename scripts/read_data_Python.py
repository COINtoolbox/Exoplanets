# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import xml.etree.ElementTree as ET, urllib, gzip, io
url = "https://github.com/OpenExoplanetCatalogue/oec_gzip/raw/master/systems.xml.gz"
oec = ET.parse(gzip.GzipFile(fileobj=io.BytesIO(urllib.urlopen(url).read())))


#name of output file
filename = 'exoplanet_catalog.dat'
filename2 = 'exoplanet_catalog2.dat'
# Output mass and radius of all planets 
op1 = open(filename, 'w')
op1.write('name         nplanets\n')
op1.write('\n')
for system in oec.findall(".//system"):
    op1.write( str(system.findtext('name')) + '        ' +  str(len(system.findall(".//planet"))) + '\n')      

op1.close()


op2 = open(filename2, 'w')
op2.write('mass        radius\n')
op2.write('\n')
for planet in oec.findall(".//planet"):
    op2.write( str(planet.findtext("mass"))+'        '+str(planet.findtext("radius"))+'\n')
op2.close()