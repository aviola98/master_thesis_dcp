from requests import get
from bs4 import BeautifulSoup

url= "https://www.gov.br/planalto/pt-br/acompanhe-o-planalto/discursos/2020/discurso-do-presidente-da-republica-jair-bolsonaro-na-abertura-da-75a-assembleia-geral-da-organizacao-das-nacoes-unidas-onu"
response=get(url)

html_soup = BeautifulSoup(response.text, "html.parser")
speech_text = html_soup.find_all("p")

speech_text

