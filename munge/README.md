# Processamento

Nesta pasta *munge* são processadas todas as análises para avaliação das recomendações.

## Ajusta e salva os dados dos jogadores. (esta etapa é opcional)

Os scripts *01 - TabelasGerais.R*, *02 - CachearDados.R*, *03 - JuntarTabelas.R* e *04 - Ajusta dados.R* buscam os dados ajustam os dados obtidos do Cartola FC e agrupam todos num único dataset "dados_cartola_2017_por_rodada.RData".


## Análise dos modelos

O *script* **05 - ForcaClubes** adiciona ao dataset informações sobre desempenho dos clubes como mandante e visitante, além de adicionar informações sobre cobranças de falta.

Os *scripts* **06 - ModeloAtacantes.R**, **07 - ModeloLaterais.R**, **08 - ModeloMeiocampo.R**, **09 - ModeloZagueiros.R**, **10 - ModeloGoleiros.R** e **11 - ModeloTreinadores** ajustam os modelos de projeção da pontuação para cada posição. (os modelos implementados ainda são de regressão linear).

O *script* **12 - Escalacoes.R** obtém as escalações dos jogadores que apresentaram as melhores projeções da respectiva rodada.