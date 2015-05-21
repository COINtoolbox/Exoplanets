#name of output file
filename = '/home/emille/Dropbox2/Dropbox/WGC/exoplanets/scripts/exoplanet_catalog.dat'
file2 = '/home/emille/Dropbox2/Dropbox/WGC/exoplanets/scripts/html_hab_table.tex'
###########################################################################

import xml.etree.ElementTree as ET, urllib, gzip, io
url = "https://github.com/OpenExoplanetCatalogue/oec_gzip/raw/master/systems.xml.gz"
oec = ET.parse(gzip.GzipFile(fileobj=io.BytesIO(urllib.urlopen(url).read())))

#get all planet names
planet_name = []
for planet in oec.findall('.//planet'):
    planet_name.append(planet.findtext('name'))

#read html file
op2 = open(file2, 'r')
lin2 = op2.readlines()
op2.close()

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

#get name of binary systems
binary_sys = []
for system in sys_name:
    if dic_n_stars[system][0] > 1:
        binary_sys.append(system)

#get star temperature (exclude binaries)
dic_star_temp = {}
for system in oec.findall('.//system'):
    if system.findtext('name') not in binary_sys:
        if len(system.findall('.//star')) > 0:
            for star in system.findall('.//star'):
                dic_star_temp[system.findtext('name')] = [star.findtext('temperature')]
        else:
            dic_star_temp[system.findtext('name')] = [None]

#get star metalicity (exclude binaries)
dic_star_met = {}
for system in oec.findall('.//system'):
    if system.findtext('name') not in binary_sys:
        if len(system.findall('.//star')) > 0:
            for star in system.findall('.//star'):
                dic_star_met[system.findtext('name')] = [star.findtext('metallicity')]
        else:
            dic_star_met[system.findtext('name')] = [None]    

#get star radius (exclude binaries)
dic_star_radius = {}
for system in oec.findall('.//system'):
    if system.findtext('name') not in binary_sys:
        if len(system.findall('.//star')) > 0:
            for star in system.findall('.//star'):
                dic_star_radius[system.findtext('name')] = [star.findtext('radius')]
        else:
            dic_star_radius[system.findtext('name')] = [None]    


#get star age (exclude binaries)
dic_star_age = {}
for system in oec.findall('.//system'):
    if system.findtext('name') not in binary_sys:
        if len(system.findall('.//star')) > 0:
            for star in system.findall('.//star'):
                dic_star_age[system.findtext('name')] = [star.findtext('age')]
        else:
            dic_star_age[system.findtext('name')] = [None]    

#get star spectral type (exclude binaries)
dic_star_type = {}
for system in oec.findall('.//system'):
    if system.findtext('name') not in binary_sys:
        if len(system.findall('.//star')) > 0:
            for star in system.findall('.//star'):
                dic_star_type[system.findtext('name')] = [star.findtext('spectraltype')]
        else:
            dic_star_type[system.findtext('name')] = [None] 

#get star mass (exclude binaries)
dic_star_mass = {}
for system in oec.findall('.//system'):
    if system.findtext('name') not in binary_sys:
        if len(system.findall('.//star')) > 0:
            for star in system.findall('.//star'):
                dic_star_mass[system.findtext('name')] = [star.findtext('mass')]
        else:
            dic_star_mass[system.findtext('name')] = [None] 


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


op3 = open('full_catalogue.dat', 'w')
for line in variables_names:
    op3.write(line[1] + '\t')
op3.write('\n')
for name in sys_name:
    if ' ' in name:
        name2 = name.replace(' ', '_')
    else:
        name2 = name 

    if variables['n_planets'][name][0] <= 1:
        op3.write(str(name2) + '\t')
        for var in variables_names[1:]:
            op3.write(str(variables[var[0]][name][0]) + '\t')
        op3.write('\n')

    else:
        for i in xrange(variables['n_planets'][name][0]):
            op3.write(str(name2) + '\t')
            for var in variables_names[1:]:
                if len(variables[var[0]][name]) == 1:
                    op3.write(str(variables[var[0]][name][0]) + '\t')
                elif len(variables[var[0]][name]) > 1:
                    op3.write(str(variables[var[0]][name][i]) + '\t') 
            op3.write('\n')                 
    
op3.close()


single_sys = [item for item in sys_name if item not in binary_sys]
variables_sys = {
             'system_name': single_sys, 
             'n_habitable_planets': dic_hab_planets,
             'n_stars': dic_n_stars,
             'n_planets': dic_n_planets,
             'declination': dic_declination, 
             's_temperature': dic_star_temp,
             's_metalicity': dic_star_met,
             's_radius': dic_star_radius,
             's_age': dic_star_age,
             's_type': dic_star_type,                           
             's_mass': dic_star_mass   
             }

variables_sys_names = [['system_name', 'name'],
                   ['n_habitable_planets', 'n_hab_planets'],
                   ['n_stars', 'n_stars'],
                   ['n_planets', 'n_planets'],
                   ['declination', 'declination'],
                   ['s_temperature', 'star_temperature'],
                   ['s_metalicity', 'star_metallicity'],
                   ['s_radius', 'star_radius'],
                   ['s_age', 'star_age'],                   
                   ['s_type', 'star_spec_type'],
                   ['s_mass', 'star_mass']
                   ]





op4 = open('system_catalogue.dat', 'w')
for line in variables_sys_names:
    op4.write(line[1] + '\t')
op4.write('\n')
for name in single_sys:
    if ' ' in name:
        name2 = name.replace(' ', '_')
    else:
        name2 = name

    op4.write(name2 + '\t')
    for item in variables_sys_names[1:]:
        print variables_sys[item[0]][name][0]
        try:
            op4.write(str(variables_sys[item[0]][name][0]) + '\t')
        except UnicodeError:
            l1 =  'M3.5pm0.5'
            op4.write(l1 + '\t')
    op4.write('\n')
op4.close()

