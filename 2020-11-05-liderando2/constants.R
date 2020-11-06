# constants.R
# (c) 2020 CincoNoveSeis Jornalismo Ltda.
# 
# This file is licensed under the GNU General Public License, version 3.

mayor_years = c(2012, 2016, 2020)

credits = 'O <i>Pindograma</i> não faz pesquisas eleitorais. Os resultados acima são de responsabilidade das empresas que os produziram.<br><br>A maior parte das pesquisas desse agregador foi coletada por <b>Pedro Fonseca, Maricélia Antonieto, Maria Clara Rodrigues, Raquel Fernandes, Natália Costard, Rodrigo Adinolfi, Fabrício Donnangelo,</b> e <b>Yasmin Bom</b>. Outra parte foi retirada do site <b>Poder360</b>. Os dados foram organizados por <b>Daniel Ferreira</b>, que também desenvolveu o agregador. Além dos números coletados pelo <i>Pindograma</i>, foram usados dados de candidatos, votações, e pesquisas eleitorais disponibilizados pelo Tribunal Superior Eleitoral. A íntegra das pesquisas coletadas pelo <i>Pindograma</i> está disponível sob a Open Database License, versão 1.0, e pode ser encontrada <a href="https://github.com/pindograma/dados">aqui</a>.'

nota_string = 'Para saber como o <i>Pindograma</i> calcula a nota dos institutos de pesquisa, <a href="https://pindograma.com.br/2020/09/07/ranking.html">confira nossa metodologia</a>. E para saber como selecionamos as pesquisas no agregador e calculamos a média de cada candidato, <a href="https://pindograma.com.br/2020/09/07/agregador.html">clique aqui</a>.'

hired_string = 'Esta pesquisa foi contratada por uma campanha política.'
probably_hired_string = 'Esta pesquisa foi contratada pelo próprio instituto.'

contractor_string = 'As pesquisas listadas acima foram contratadas pelas seguintes entidades, respectivamente: '

party_palette = tibble(
  party = c(45, 17, 10, 11, 14, 55, 43,
            15, 25, 30, 19, 22, 20, 12,
            13, 65, 50, 23, 40, 18, 16,
            29, 21, 77, 28, 33, 36, 51,
            70, 80, 90, 54, 44, 27, 31,
            35, 99),
  party_color = c('#5C88DA', '#003A70', '#41748D', '#56B7E6', '#131E29', '#F6BE00', '#006747',
            '#009A44', '#0857C3', '#FFA400', '#98B6E4', '#287DA1', '#B47E00', '#DB8A06',
            '#F93822', '#C6007E', '#FFCD00', '#F1A7DC', '#FA4616', '#78D64B', '#76232F',
            '#543A3B', '#AF272F', '#F19C49', '#919D9D', '#C1C6C8', '#978C87', '#62685A',
            '#AEA8A5', '#97999B', '#566361', '#7ACC00', '#C4D600', '#BB85AB', '#C964CF',
            '#672146', '#696969'),
  party_name = c('PSDB', 'PSL', 'Republicanos', 'Progressistas', 'PTB', 'PSD', 'PV',
                 'MDB', 'DEM', 'NOVO', 'PODE', 'PL', 'PSC', 'PDT',
                 'PT', 'PCdoB', 'PSOL', 'CIDADANIA', 'PSB', 'REDE', 'PSTU',
                 'PCO', 'PCB', 'SD', 'PRTB', 'PMN', 'PTC', 'PATRI',
                 'Avante', 'UP', 'PROS', 'PPL', 'PRP', 'DC', 'PHS',
                 'PMB', 'Branco/Nulo')
)

partisan_exceptions = c()
