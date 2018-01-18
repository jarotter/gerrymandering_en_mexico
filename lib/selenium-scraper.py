from selenium import webdriver
from selenium.webdriver.support.ui import Select
import csv

URL = 'http://cartografia.ife.org.mx/sige7/?mapoteca=planos&psi'

driver = webdriver.Firefox()
driver.get(URL)

mapo_entidad = Select(driver.find_element_by_id('mapo-entidad'))
mapo_distrito = Select(driver.find_element_by_id('mapo-distrito'))

mapo_entidad.select_by_value('9')

with open('ine18.csv', 'w') as csvfile:
    filewriter = csv.writer(csvfile, delimiter=',')

    filewriter.writerow(['distrito', 'seccion'])

    for i in range(1,28): #We use integer indexing to avoid value="0"
        mapo_distrito.select_by_value(str(i))
        mapo_seccion = Select(driver.find_element_by_id('mapo-seccion'))
        secciones = mapo_seccion.options
        for seccion in secciones:
            text = seccion.text
            if text[0] == 'S':
                continue #To avoid the default text
            filewriter.writerow([i,text])
