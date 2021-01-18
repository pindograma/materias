import requests
import csv

with open('st_cities.csv') as f:
    oup = []

    reader = csv.reader(f)
    for line in reader:
        print(line)

        try:
            data = requests.get('https://resultados.tse.jus.br/oficial/ele2020/divulgacao/oficial/427/dados-simplificados/{}/{}{}-c0011-e000427-r.json'.format(
                line[1].lower(), line[1].lower(), line[0]
            )).json()

            for cand in data['cand']:
                oup.append({
                    'codigo_cidade_tse': line[0],
                    'urnas_apuradas': data['pst'].strip(),
                    'eleitorado': data['e'],
                    'abstencoes': data['a'],
                    'total_votos': data['tv'],
                    'abstencoes_pct': data['pa'].strip(),
                    'brancos_pct': data['pvb'].strip(),
                    'nulos_pct': data['ptvn'].strip(),
                    'nome': cand['nm'],
                    'numero': cand['n'],
                    'votos_validos': cand['vap'],
                    'pct_votos_validos': cand['pvap'].strip(),
                    'eleito': cand['st']
                })
        except:
            print('Error! Continuing...')

    keys = oup[0].keys()
    with open('resultados_prefeito_final.csv', 'w', newline='') as output_file:
        dict_writer = csv.DictWriter(output_file, keys)
        dict_writer.writeheader()
        dict_writer.writerows(oup)
