-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g035 where

import LI11819
import Tarefa0_2018li1g035

-- * Relatório
--
-- * Introdução 
--
-- $int
-- Inicialmente, será explicado o conceito da tarefa 6 e estratégia que nós adotamos para a sua realização e ,por fim ,a sua utilidade. Iremos falar dos resultados
--obtidos após a programação deste bot , e também o porquê de ter sido realizada da forma que foi. Sendo assim , o principal desafio desta tarefa foi criar um bot 
--o mais eficiaz possível , isto é , programá-lo de modo a conesguir o máximo de pontos possíveis ao fim de 200 ticks , duração de um jogo.

-- * Objetivos
--
-- $obj
-- O principal objetivo desta tarefa era contruir um bot de modo a que ao fim de 200 ticks ele tivesse mais pontuação que todos os outros jogadores em jogo. A fim deste
--objetivo utilizamos a seguinte estretégia : primeiro ter em conta o que o bot poderia fazer com os tiros disponíveis pois , afinal , a nosso ver é a melhor maneira de
--obter uma boa pontuação e só depois pensar no que ele poderia fazer com os movimentos de uma forma equilibrada e não disparar para qualquer Direcao.
--Deu-se assim prioridade aos disparos em deterimento dos movimentos.


-- * Procedimento
--
-- $proc
-- O método de pensamento usado foi dividir a estratégia de compressão por 2 partes : a possível utilização de disparos como prioridade e ,logo após, a possível utilização dos
--movimentos. Dentro dos disparos , e sendo o disparo mais forte , inicialmente defenimos que se houver justificção para tal deveria-se usar o laser , visto que é o mais 
--propício à obtenção de pontos , depois e canhão e por último o choque. Quanto a possiveis movimentos e se não se justificar um disparo , defenimos como prioritário o avançar
--se tiverem reunidas condições para tal acontecimento e só depois mudar a sua direção.
--
-- > Utilização de disparos 
--
-- > a - Utilização do laser
--
-- Sendo este o disparo mais temível , foi à utilização do mesmo a que demos prioridade , sendo que , segundo o que nós achamos , ele irá jogar um "Dispara Laser" se houver
--algum jogador no seu campo de disparo , isto é , se até o laser se parar de propagar (até encontrar uma parede indestrutivel) algum jogador estiver nessa linha de disparo, 
--o laser irá ser utilizado , e a outra ocasião é se ele encontrar paredes indestrutiveis nesse mesmo campo , sendo que apenas o fará nestas 2 ocasiões se e só se tiver
--o contador de laser superior a 0. Assim o defenimos porque o tirar vidas a jogadores e o destruir paredes destrutiveis contam 4 e 1 pontos respetivamente para a 
--pontuação total do jogador, o que significa obtenção de pontos e sendo o laser o disparo mais forte nenhum outro disparo o deterá. 
--
-- > b - Utilização do canhão
--
-- No que toca ao disparo de canhão que tem como grande vantagem ser infinito , a nosso ver é a 2ª melhor arma do jogo pois para além do que foi dito , também possiblita o 
--jogador de tirar vidas ao adversário e destruir paredes destrutiveis , assim sendo este disparo irá ser usado se algum jogador se encontrar a até 4 posições de distância
--da linha de disparo do bot , isto porque , estando ele perto, é muito mais dificil de escapar à bala. Se alguma bala de canhão estiver a 4 ou menos posições do jogador ,
--isto irá permitir destruir a bala de canhão e preservar as vidas do bot , relembrando que cada uma vale 4 pontos. Por último, será usada se existir alguma parede
--destrutivel nos blocos à frente do jogador , o que para além de permitir ganhar pontos, permite um possível avanço do jogador se necessário.
--
--
-- > c - Utilização do Choque
--
-- A utilização do choque também tem a sua vantagem , e esta é o facto de imobilizar algum jogador no raio de 3 posições do jogador , o que permite que represente menos
--perigo pois há menos hipóteses de o bot ser atingido. Sendo assim , se não se justificar o uso de um disparo canhão ou um disparo laser e se o bot encontrar algum
--jogador no raio de 3 posições, irá usar o choque , isto se o contador de choques for superior a 0.
--
--
-- > Utilização dos Movimentos
--
-- > a - Movimentação na direção atual. 
--
-- A movimentação também é importante pois permite "fugir" de possíveis disparos e preservar vidas. Sendo assim, o bot irá movimentar-se para a frente se
--vier alguma bala numa direção perpendicular à sua direção atual , para este poder escapar à bala ou pelo menos tentar e também se for possível movimentar-se
--para a frente , isto é , se não existir alguma parede a impossibilitá-lo.
--
-- > b - Mudança de direção.
--
-- O bot irá mudar a sua direção se estiver impossibilitado de se mover para a frente, isto porque se tiver alguma parede indestrútivel à frente será impossível a
--a sua movimentação nesse sentido e também se vier alguma bala na sua direção ou da direção oposta à que ele está para permitir uma possível fuga e salvaguardar
--as suas vidas.
--
-- > Não jogar
--
-- A última hipótese e, a nosso ver, a com menos utilidade, é o jogador não jogar , ou seja, não fazer nada. Mas se por algum motivo não fôr possível a realização
--de alguma jogada anteriormente referida, assim o fará.

-- * Conclusão
--
-- $conc
-- Concluido , apesar de achamos que esta foi a tarefa mais desafiante , em termos de resultados obtidos foi talvez uma das menos eficazes uma vez que não foi testada
--o suficiente , mas , mesmo assim , o bot responde e executa sempre uma jogada , sendo que talvez nao seja a mais indicada para o tempo , ou seja , não é um
--bot totalmente "perfeito".

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot j (Estado m jog disp) | verificaSeJogaLaser j jog m == True = Just (Dispara Laser)
                          | verificaSeJogaCanhao j jog disp m == True = Just (Dispara Canhao)
                          | verificaSeJogaChoque j jog == True = Just (Dispara Choque)
                          | verificaSeAvanca j jog disp m == True = Just (avanca (procuraJogador (jog) j))
                          | verificaSeMudaDir j jog disp m == True = Just (mudaDir (procuraJogador (jog) j))
                          | otherwise = Nothing 

--Funções para ver o Jogador que vai jogar e remove esse jogador para ajudar nas comparações--

-- | Procura um jogador na Lista
procuraJogador :: [Jogador] -> Int -> Jogador
procuraJogador (h:t) j = if j==0 then h else procuraJogador t (j-1)

-- | Remove um Jogador da lista
removeJogador :: [Jogador] -> Int -> [Jogador]
removeJogador [] _ = []
removeJogador (h:t) j = if j==0 then t else h : removeJogador t (j-1)

--Funções de ajuda-- 


-- | Verifica se duas Posicoes sao iguais
verificaPosIgual :: Posicao -> Posicao -> Bool
verificaPosIgual (x,y) (a,b) = if x==a && y==b then True else False  


-- | Verifica se uma Posicao é igual a alguma de uma lista
verificaPosIgual1 :: Posicao -> [Posicao] -> Bool
verificaPosIgual1 (x,y) [] = False
verificaPosIgual1 (x,y) (h:t) = if verificaPosIgual (x,y) h == True then True else False 


-- | Verifica se alguma posicao das duas listas esta presente na outra
verificaPosFinal :: [Posicao] -> [Posicao] -> Bool 
verificaPosFinal [] [] = False 
verificaPosFinal _ []  = False
verificaPosFinal [] _  = False
verificaPosFinal (a:b) (h:t) = if verificaPosIgual1 a (h:t) == True then True else verificaPosFinal b (h:t)

--Funções que irão determinar os casos em que o bot usará o laser--
-- * Utilização do Laser

-- | Verifica se um Jogador não tem Lasers
verificaSeTemLaser :: Jogador -> Bool
verificaSeTemLaser (Jogador (x,y) d v l c) = if l>0 then True else False


-- | Cria a lista de Posicoes afetadas pelo Laser
posicoes1laser :: Jogador -> Mapa -> [Posicao]
posicoes1laser d [] = []
posicoes1laser (Jogador (x,y) d v l c) m | d==C = if (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) then [] else [(x,y),(x,y+1)] ++ posicoes1laser (Jogador (x-1,y) d v l c) m
                                         | d==B = if (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) then [] else [(x+1,y),(x+1,y+1)] ++ posicoes1laser (Jogador (x+1,y) d v l c) m
                                         | d==D = if (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) then [] else [(x,y+1),(x+1,y+1)] ++ posicoes1laser (Jogador (x,y+1) d v l c)  m
                                         | otherwise = if (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) then [] else [(x,y),(x+1,y)] ++ posicoes1laser (Jogador (x,y-1) d v l c) m 


-- | Cria a lista de Posicoes ocupadas por um Jogador
posicoesOcupadas1jog :: Jogador -> [Posicao]
posicoesOcupadas1jog (Jogador (x,y) d v l c) = [(x,y),(x+1,y),(x,y+1),(x+1,y+1)] 


-- | Cria a lista de Posicoes ocupadas pelos Jogadores
posicoesOcupadasJog :: [Jogador] -> [Posicao]
posicoesOcupadasJog [] = []
posicoesOcupadasJog (h:t) = posicoesOcupadas1jog h ++ posicoesOcupadasJog t


-- | Verifica se existem Blocos Destrutiveis numa Lista de Posicoes
verificaSeTemBDes :: [Posicao] -> Mapa -> Bool 
verificaSeTemBDes [] [] = False 
verificaSeTemBDes p []  = False 
verificaSeTemBDes [] m  = False 
verificaSeTemBDes (h:t) m = if encontraPosicaoMatriz h m == Bloco Destrutivel then True else verificaSeTemBDes t m 


-- | Verifica se um Jogador utilizou um Lazer
verificaSeJogaLaser :: Int -> [Jogador] -> Mapa -> Bool 
verificaSeJogaLaser j [] [] = False
verificaSeJogaLaser j l []  = False 
verificaSeJogaLaser j [] m  = False  
verificaSeJogaLaser j l m   = if verificaSeTemLaser (procuraJogador l j) == True && verificaSeJogaLaserAux j l m == True then True else False


-- | Verifica se um Laser vai atingir um Jogador
verificaSeJogaLaserAux :: Int -> [Jogador] -> Mapa -> Bool
verificaSeJogaLaserAux j [] [] = False
verificaSeJogaLaserAux j l []  = False 
verificaSeJogaLaserAux j [] m  = False  
verificaSeJogaLaserAux j l m   = if verificaSeTemBDes (posicoes1laser (procuraJogador l j) m) m == True || verificaPosFinal (posicoesOcupadasJog (removeJogador l j)) (posicoes1laser (procuraJogador l j) m) == True then True else False

--Funções que irão determinar os casos em que o bot usará o canhão--
-- * Utilização do Canhão

-- | Verifica se um Jogador tem Blocos Destrutiveis à sua frente
verificaParedesFrente :: Jogador -> Mapa -> Bool
verificaParedesFrente j [] = False 
verificaParedesFrente (Jogador (x,y) d v l c) m | d==C && (encontraPosicaoMatriz (x-1,y) m == Bloco Destrutivel || encontraPosicaoMatriz (x-1,y+1) m == Bloco Destrutivel) = True
                                                | d==B && (encontraPosicaoMatriz (x+2,y+1) m == Bloco Destrutivel || encontraPosicaoMatriz (x+2,y) m == Bloco Destrutivel) = True
                                                | d==D && (encontraPosicaoMatriz (x,y+2) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y+2) m == Bloco Destrutivel) = True
                                                | d==E && (encontraPosicaoMatriz (x,y-1) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y-1) m == Bloco Destrutivel) = True                         
                                                | otherwise = False


-- | Verifica se um Disparo é um Disparo Canhao
verificaSeECanhao :: Disparo -> Bool 
verificaSeECanhao (DisparoChoque j t) = False
verificaSeECanhao (DisparoLaser j (x,y) d)  = False
verificaSeECanhao (DisparoCanhao j (x,y) d) = True 


-- | Retira da lista de Disparos os que nao sao Canhao
soCanhoes :: [Disparo] -> [Disparo]
soCanhoes [] = []
soCanhoes (h:t) = if verificaSeECanhao h == True then h : soCanhoes t else soCanhoes t


-- | Retira a Posicao do Disparo
canhaoToPos :: Disparo -> Posicao 
canhaoToPos (DisparoCanhao j (x,y) d) = (x,y)


-- | Retira a Posicao de varios Disparos Canhao
posCanhoes :: [Disparo] -> [Posicao]
posCanhoes [] = []
posCanhoes (h:t) = canhaoToPos h : posCanhoes t 


-- | Cria a lista de Posicoes de Perigo
posicoesdeperigoCanhao :: Jogador -> [Posicao]
posicoesdeperigoCanhao (Jogador (x,y) d v l c) | d==C = [(x-2,y),(x-3,y),(x-4,y)]
                                               | d==B = [(x+2,y),(x+3,y),(x+4,y)]
                                               | d==D = [(x,y+2),(x,y+3),(x,y+4)]
                                               | otherwise = [(x,y-2),(x,y-3),(x,y-4)]

-- | Cria a lista de Posicoes onde um Disparo Canhao vai afetar o Jogador
posicoesdeperigoTanques :: Jogador -> [Posicao]
posicoesdeperigoTanques (Jogador (x,y) d v l c) | d==C = [(x-1,y),(x-2,y),(x-3,y),(x-4,y),(x-1,y+1),(x-2,y+1),(x-3,y+1),(x-4,y+1)]
                                                | d==B = [(x+2,y),(x+3,y),(x+4,y),(x+5,y),(x+2,y+1),(x+3,y+1),(x+4,y+1),(x+5,y+1)]
                                                | d==D = [(x,y+2),(x,y+3),(x,y+4),(x,y+5),(x+1,y+2),(x+1,y+3),(x+1,y+4),(x+1,y+5)]
                                                | otherwise = [(x,y-1),(x,y-2),(x,y-3),(x,y-4),(x+1,y-1),(x+1,y-2),(x+1,y-3),(x+1,y-4)]


-- | Verifica se um Jogador vai ser atingido um um Disoari
verificaSeJogaCanhao :: Int -> [Jogador] -> [Disparo] -> Mapa -> Bool 
verificaSeJogaCanhao j l [] m = False 
verificaSeJogaCanhao j [] d m = False 
verificaSeJogaCanhao j l d [] = False 
verificaSeJogaCanhao j l d m = if verificaParedesFrente (procuraJogador l j) m == True || verificaPosFinal (posicoesdeperigoTanques (procuraJogador l j)) (posicoesOcupadasJog (removeJogador l j)) == True || verificaPosFinal (posicoesdeperigoCanhao (procuraJogador l j)) (posCanhoes (soCanhoes d)) == True then True else False

--Funções que irão determinar os casos em que o bot usará o choque--
-- * Utilização do Choque

-- | Verifica se o Jogador tem algum Disparo Choque
verificaSeTemChoque :: Jogador -> Bool
verificaSeTemChoque (Jogador (x,y) d v l c) = if c>0 then True else False


-- | Cria a lista de Posicoes afetadas pelo Disparo Choque
posicaoToListadePosicoesChoque :: Jogador -> [Posicao]
posicaoToListadePosicoesChoque (Jogador (x,y) d v l c) = [(x-2,y-2),(x-2,y-1),(x-2,y),(x-2,y+1),(x-2,y+2),(x-2,y+3),(x-1,y-2),(x-1,y-1),(x-1,y),(x-1,y+1),(x-1,y+2),(x-1,y+3),(x,y-2),(x,y-1),(x,y),(x,y+1),(x,y+2),(x,y+3),(x+1,y-2),(x+1,y-1),(x+1,y),(x+1,y+1),(x+1,y+2),(x+1,y+3),(x+2,y-2),(x+2,y-1),(x+2,y),(x+2,y+1),(x+2,y+2),(x+2,y+3),(x+3,y-2),(x+3,y-1),(x+3,y),(x+3,y+1),(x+3,y+2),(x+3,y+3)]


-- | Verifica se vai utilizar um Disparo Choque
verificaSeJogaChoque :: Int -> [Jogador] ->Bool 
verificaSeJogaChoque j [] = False 
verificaSeJogaChoque j l  = if verificaSeTemChoque (procuraJogador l j) == True && verificaPosFinal (posicaoToListadePosicoesChoque (procuraJogador l j)) (posicoesOcupadasJog (removeJogador l j)) == True then True else False 

--Funções que irão determinar os casos em que o bot avança de posição , isto é , movimenta-se na mesma direção em que se encontra--
-- * Possibilidade de Avançar

-- | Verifica se o Jogador pode avançar na sua Direcao
verificaSePodeAvancar :: Jogador -> Mapa -> Bool
verificaSePodeAvancar j [] = False 
verificaSePodeAvancar (Jogador (x,y) d v l c) m | d==C && (encontraPosicaoMatriz (x-1,y) m == Vazia && encontraPosicaoMatriz (x-1,y+1) m == Vazia) = True
                                                | d==B && (encontraPosicaoMatriz (x+2,y+1) m == Vazia && encontraPosicaoMatriz (x+2,y) m == Vazia) = True
                                                | d==D && (encontraPosicaoMatriz (x,y+2) m == Vazia && encontraPosicaoMatriz (x+1,y+2) m == Vazia) = True
                                                | d==E && (encontraPosicaoMatriz (x,y-1) m == Vazia && encontraPosicaoMatriz (x+1,y-1) m == Vazia) = True                         
                                                | otherwise = False


-- | Verifica se algo esta na Direcao do Jogador
verificaCanhoesDirOposta :: Jogador -> Posicao -> Bool 
verificaCanhoesDirOposta (Jogador (x,y) d v l c) (a,b) | d==C = if a==x || a==x+1 || a==x-1 then True else False 
                                                       | d==B = if a==x || a==x+1 || a==x-1 then True else False
                                                       | d==D = if b==y || b==y+1 || b==y-1 then True else False
                                                       | otherwise = if b==y || b==y+1 || b==y-1 then True else False

-- | Verifica se ha algo na Direcao do Jogador
verificaCanhoesDirOpostaFinal :: Jogador -> [Posicao] -> Bool 
verificaCanhoesDirOpostaFinal j [] = False
verificaCanhoesDirOpostaFinal j (h:t) = if verificaCanhoesDirOposta j h == True then True else verificaCanhoesDirOpostaFinal j t 


-- | Movimenta o Jogador na sua Direcao
avanca :: Jogador -> Jogada 
avanca (Jogador (x,y) d v l c) = Movimenta d 


-- | Verifica se deve ou nao avancar
verificaSeAvanca :: Int -> [Jogador] -> [Disparo] -> Mapa -> Bool 
verificaSeAvanca j l [] m = False 
verificaSeAvanca j [] d m = False 
verificaSeAvanca j l d [] = False 
verificaSeAvanca j l d m  = if verificaSePodeAvancar (procuraJogador l j) m == True || verificaCanhoesDirOpostaFinal (procuraJogador l j) (posCanhoes (soCanhoes d)) == True then True else False

--Funções que irão determinar os casos em que o bot muda de diração , isto é , movimenta-se noutra direção comparativamente à que se encontra--
-- * Possibilidade de Mudar de Direção

-- | Muda a Direcao do Jogador
mudaDir :: Jogador -> Jogada 
mudaDir (Jogador (x,y) d v l c) | d==C = Movimenta D
                                | d==B = Movimenta E
                                | d==D = Movimenta B
                                | otherwise = Movimenta C

-- | Verifica se algo esta na Direcao do Jogador
verificaCanhoesMesmaDir:: Jogador -> Posicao -> Bool 
verificaCanhoesMesmaDir (Jogador (x,y) d v l c) (a,b) | d==C = if b==y || b==y+1 || b==y-1 then True else False 
                                                      | d==B = if b==y || b==y+1 || b==y-1 then True else False
                                                      | d==D = if a==x || a==x+1 || a==x-1 then True else False
                                                      | otherwise = if a==x || a==x+1 || a==x-1 then True else False

-- | Verifica se ha algo na Direcao do Jogador
verificaCanhoesMesmaDirFinal :: Jogador -> [Posicao] -> Bool 
verificaCanhoesMesmaDirFinal j [] = False
verificaCanhoesMesmaDirFinal j (h:t) = if verificaCanhoesMesmaDir j h == True then True else verificaCanhoesMesmaDirFinal j t 


-- | Verifica se deve ou nao mudar de Direcao
verificaSeMudaDir :: Int -> [Jogador] -> [Disparo] -> Mapa -> Bool 
verificaSeMudaDir j l [] m = False 
verificaSeMudaDir j [] d m = False 
verificaSeMudaDir j l d [] = False 
verificaSeMudaDir j l d m  = if verificaSePodeAvancar (procuraJogador l j) m == False || verificaCanhoesMesmaDirFinal (procuraJogador l j) (posCanhoes (soCanhoes d)) == True then True else False

