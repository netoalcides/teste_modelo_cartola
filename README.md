# Teste Modelo Cartola

O objetivo deste projeto é desenvolver algoritmos para recomendação de jogadores no Cartola FC. Para realizar todas as análises e ver os resultados rodar o *scritpt* **RealizaTodasAnalises.R**.


## Decisões

O cartoleiro precisa tomar decisões de forma a  maximizar a pontuação do jogo em cada rodada. Estas decisões podem ser:

- Escolher os melhores jogadores por posição.
- Escolher o melhor esquema tático para cada rodada.
- Escolher os jogadores que estejam dentro dos limites de cartoletas existentes na carteira.

Estas decisões fornecem um gama de desafios, sendo que as soluções podem ser adaptadas para outras áreas.

Em nossa solução atual, buscamos desenvolver um modelo de otimização considerando esquema tático com laterais e outro modelo considerando esquema sem laterais apresentados nas fórmulas abaixo. 

Para solucionar estes problemas, atualmente, estamos utilizando programação linear inteira. Porém, existem diversas formas de montar o problema e uma gama de algoritmos de solução, por exemplo, algoritmos genéticos.

O principal desafio é projetar as pontuações de cada jogador, ou pelo menos,  conseguir ranquear a performance para a próxima rodada.


### Otimização com laterais

![](http://mathurl.com/yco6b8q8.png)


### Otimização sem laterais

![](http://mathurl.com/ybhw3nkl.png)
