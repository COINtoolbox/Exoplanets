try:
    from urllib2 import urlopen
except ImportError:
    from urllib.request import urlopen


#read data
sock = urlopen("http://www.openexoplanetcatalogue.com/systems/?filters=habitable") 
htmlSource = sock.read()                            
sock.close()              

###########################################################################

import xml.etree.ElementTree as ET, gzip, io
url = "https://github.com/OpenExoplanetCatalogue/oec_gzip/raw/master/systems.xml.gz"
oec = ET.parse(gzip.GzipFile(fileobj=io.BytesIO(urlopen(url).read())))

#get all planet names
planet_name = []
for planet in oec.findall('.//planet'):
    planet_name.append(planet.findtext('name'))

#read html file
lin2 = htmlSource.split('\n')

#get planet name
hab_planet = []
for line in lin2:
    for planet in planet_name:
        if planet in line and planet not in hab_planet:
            hab_planet.append(planet)

#get the number of planets in the habitable zone per system
dic_hab_planets = {}
for system in oec.findall('.//system'):
    n_hab_planets = 0
    for planet in system.findall('.//planet'):
        name = planet.findtext('name')
        if name in hab_planet:
            n_hab_planets = n_hab_planets + 1
    dic_hab_planets[system.findtext('name')] = [str(n_hab_planets)]

#get all systems names
sys_name = []
for system in oec.findall('.//system'):
    sys_name.append(str(system.findtext('name')))

#get number of stars
dic_n_stars = {}
for system in oec.findall('.//system'):
   dic_n_stars[system.findtext('name')] = [len(system.findall('.//star'))]

#get number of planets in system
dic_n_planets = {}
for system in oec.findall('.//system'):
    dic_n_planets[system.findtext('name')] = [len(system.findall('.//planet'))] 

#get discovery method
dic_discovery = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_discovery[name] = []
    for planet in system.findall('.//planet'):
        dic_discovery[name].append(planet.findtext('discoverymethod'))

#get temperature
dic_temperature = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_temperature[name] = []
    for planet in system.findall('.//planet'):
        dic_temperature[name].append(planet.findtext('temperature'))

#get metalicity
dic_metalicity = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_metalicity[name] = []
    for planet in system.findall('.//planet'):
        dic_metalicity[name].append(planet.findtext('metalicity'))

#get planet radius
dic_radius = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_radius[name] = []
    for planet in system.findall('.//planet'):
        dic_radius[name].append(planet.findtext('radius'))

#get planet period
dic_period = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_period[name] = []
    for planet in system.findall('.//planet'):
        dic_period[name].append(planet.findtext('period'))

#get system declination
dic_declination = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_declination[name] = [system.findtext('declination')]

#get planet excentricity
dic_eccentricity = {}
for system in oec.findall('.//system'):
    name = system.findtext('name')
    dic_eccentricity[name] = []
    for planet in system.findall('.//planet'):
        dic_eccentricity[name].append(planet.findtext('eccentricity'))


variables = {'system_name': sys_name,
             'n_habitable_planets': dic_hab_planets,
             'n_stars': dic_n_stars,
             'n_planets': dic_n_planets,
             'discovery_method': dic_discovery,
             'temperature': dic_temperature,
             'metalicity': dic_metalicity,
             'radius': dic_radius,
             'period': dic_period,
             'declination': dic_declination,
             'eccentricity': dic_eccentricity   
             }

variables_names = [['system_name', 'name'],
                   ['n_habitable_planets', 'n_hab_planets'],
                   ['n_stars', 'n_stars'],
                   ['n_planets', 'n_planets'],
                   ['discovery_method', 'discovery_method'],
                   ['temperature', 'temperature'],
                   ['metalicity', 'metalicity'],
                   ['radius', 'radius'],
                   ['period', 'period'],
                   ['declination', 'declination'],
                   ['eccentricity', 'eccentricity']   
                   ]


# output file
op3 = open('exoplanet.dat', 'w')
for line in variables_names:
    op3.write(line[1] + '    ')
op3.write('\n')
for name in sys_name:
    if ' ' in name:
        name2 = name.replace(' ', '_')
    else:
        name2 = name 

    if variables['n_planets'][name][0] <= 1:
        op3.write(str(name2) + '    ')
        for var in variables_names[1:]:
            op3.write(str(variables[var[0]][name][0]) + '    ')
        op3.write('\n')

    else:
        for i in xrange(variables['n_planets'][name][0]):
            op3.write(str(name2) + '    ')
            for var in variables_names[1:]:
                if len(variables[var[0]][name]) == 1:
                    op3.write(str(variables[var[0]][name][0]) + '    ')
                elif len(variables[var[0]][name]) > 1:
                    op3.write(str(variables[var[0]][name][i]) + '    ') 
            op3.write('\n')                 
    
op3.close()



