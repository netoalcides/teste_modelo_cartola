# Teste Modelo Cartola

O objetivo deste projeto é desenvolver algoritmos para recomendação de jogadores no Cartola FC.

## Decisões

O cartoleiro precisa tomar decisões de forma a  maximizar a pontuação do jogo em cada rodada. Estas decisões podem ser:

- Escolher os melhores jogadores por posição.
- Escolher o melhor esquema tático para cada rodada.
- Escolher os jogadores que estejam dentro dos limites de cartoletas existentes na carteira.

Estas decisões fornecem um gama de desafios, sendo que as soluções podem ser adaptadas para outras áreas.

Para realizar todas as análises e ver os resultados rodar o *scritpt* **RealizaTodasAnalises.R**.

![equation](http://www.sciweavers.org/tex2img.php?eq=%5Cbegin%7Bequation%2A%7D%0A%20%20%20%20%5Cbegin%7Baligned%7D%0A%20%20%20%20%20%20%20%20%26%20%5Ctext%7BMax%7D%0A%20%20%20%20%20%20%20%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20pont_i%20%5Ccdot%20ata_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20pont_i%20%5Ccdot%20mei_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20pont_i%20%5Ccdot%20lat_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20pont_i%20%5Ccdot%20zag_i%20%2B%20pont%20%5Ccdot%20gol%20%2B%20pont%20%5Ccdot%20tec%5C%5C%0A%20%20%20%20%20%20%20%20%26%20%5Ctext%7Bsujeito%20a%3A%7D%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20ata_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20mei_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20lat_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20zag_i%20%2B%20gol%20%2B%20tec%20%3D%2012%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20preco_i%20%5Ccdot%20ata_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20preco_i%20%5Ccdot%20mei_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20preco_i%20%5Ccdot%20lat_i%20%2B%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20preco_i%20%5Ccdot%20zag_i%20%2B%20preco%20%5Ccdot%20gol%20%2B%20preco%20%5Ccdot%20tec%20%5Cleq%20cartoletas%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%201%20%5Cgeq%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20ata_i%20%5Cgeq%203%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%203%20%5Cgeq%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20mei_i%20%5Cgeq%205%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%202%20%5Cgeq%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20zag_i%20%5Cgeq%203%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%20%5Csum%5E%7Bn%7D_%7Bi%3D1%7D%20lat_i%20%3D%202%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%20gol%20%3D%201%2C%5Ctext%7B%20%7Dtec%20%3D%201%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%20ata%2C%20mei%2C%20zag%2C%20lat%20%5Cin%20%5C%7B0%2C1%5C%7D%20%5C%5C%0A%20%20%20%20%20%20%20%20%26%20ata%2C%20mei%2C%20zag%2C%20lat%20%5Cin%20%5Cmathbb%7BZ%7D%5En%0A%20%20%20%20%5Cend%7Baligned%7D%0A%5Cend%7Bequation%2A%7D&bc=White&fc=Black&im=jpg&fs=12&ff=arev&edit=0)