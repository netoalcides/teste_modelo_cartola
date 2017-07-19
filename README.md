# Teste Modelo Cartola

O objetivo deste projeto é desenvolver algoritmos para recomendação de jogadores no Cartola FC.

## Decisões

O cartoleiro precisa tomar decisões de forma a  maximizar a pontuação do jogo em cada rodada. Estas decisões podem ser:

- Escolher os melhores jogadores por posição.
- Escolher o melhor esquema tático para cada rodada.
- Escolher os jogadores que estejam dentro dos limites de cartoletas existentes na carteira.

Estas decisões fornecem um gama de desafios, sendo que as soluções podem ser adaptadas para outras áreas.

## Problema de otimização

Dado as decisões a serem realizadas para otimizar a pontuação do jogo, formulamos dois problemas simples de programação inteira linear. A formulação deste problema pode ser modificado com o tempo, além da forma de solução também (ainda não testamos algoritmos genéticos, por exemplo).

A primeiro problema busca maximizar a pontuação considerando esquema tático com laterais. O segundo considera um esquema tático sem laterais.

### Otimização com laterais

\begin{equation*}
    \begin{aligned}
        & \text{Máx}
        \sum^{n}_{i=1} pont_i \cdot ata_i + \sum^{n}_{i=1} pont_i \cdot mei_i + \sum^{n}_{i=1} pont_i \cdot lat_i + \sum^{n}_{i=1} pont_i \cdot zag_i + pont \cdot gol + pont \cdot tec\\
        & \text{sujeito a:} \\
        & \sum^{n}_{i=1} ata_i + \sum^{n}_{i=1} mei_i + \sum^{n}_{i=1} lat_i + \sum^{n}_{i=1} zag_i + gol + tec = 12 \\
        & \sum^{n}_{i=1} preco_i \cdot ata_i + \sum^{n}_{i=1} preco_i \cdot mei_i + \sum^{n}_{i=1} preco_i \cdot lat_i + \sum^{n}_{i=1} preco_i \cdot zag_i + preco \cdot gol + preco \cdot tec \leq cartoletas \\
        & 1 \geq \sum^{n}_{i=1} ata_i \geq 3 \\
        & 3 \geq \sum^{n}_{i=1} mei_i \geq 5 \\
        & 2 \geq \sum^{n}_{i=1} zag_i \geq 3 \\
        & \sum^{n}_{i=1} lat_i = 2 \\
        & gol = 1,\text{ }tec = 1 \\
        & ata, mei, zag, lat \in \{0,1\} \\
        & ata, mei, zag, lat \in \mathbb{Z}^n
    \end{aligned}
\end{equation*}

![equation](http://latex.codecogs.com/gif.latex?Concentration%3D%5Cfrac%7BTotalTemplate%7D%7BTotalVolume%7D)  



### Otimização sem laterais

\begin{equation*}
    \begin{aligned}
        & \text{Máx}
        \sum^{n}_{i=1} pont_i \cdot ata_i + \sum^{n}_{i=1} pont_i \cdot mei_i + \sum^{n}_{i=1} pont_i \cdot zag_i + pont \cdot gol + pont \cdot tec\\
        & \text{sujeito a:} \\
        & \sum^{n}_{i=1} ata_i + \sum^{n}_{i=1} mei_i + \sum^{n}_{i=1} lat_i + \sum^{n}_{i=1} zag_i + gol + tec = 12 \\
        & \sum^{n}_{i=1} preco_i \cdot ata_i + \sum^{n}_{i=1} preco_i \cdot mei_i + \sum^{n}_{i=1} preco_i \cdot zag_i + preco \cdot gol + preco \cdot tec \leq cartoletas \\
        & 1 \geq \sum^{n}_{i=1} ata_i \geq 3 \\
        & 3 \geq \sum^{n}_{i=1} mei_i \geq 5 \\
        & 2 \geq \sum^{n}_{i=1} zag_i \geq 3 \\
        & gol = 1,\text{ }tec = 1 \\
        & ata, mei, zag \in \{0,1\} \\
        & ata, mei, zag \in \mathbb{Z}^n
    \end{aligned}
\end{equation*}