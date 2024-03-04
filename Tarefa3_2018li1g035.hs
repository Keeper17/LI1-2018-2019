-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g035 where

import LI11819
import Data.Char

-- * Relatório
--
-- * Introdução 
--
-- $intro
-- Começando, iremos explicar todo o procedimento da tarefa 3 , bem como o método usado por nós para a conclusão da mesma ,e o quão útil esta é para o jogo. Iremos
--também interpretar os resultados obtidos após a compressão/descompressão do estado do jogo , e também o porquê desses mesmos resultados. Sendo assim , o principal
--desafio desta tarefa foi criar uma maneira o mais eficiente possível de comprimir e logo após descomprimir o estado do jogo recorrendo a funções que permitissem 
--tal coisa.
--
-- * Objetivos
--
-- $obs
-- O principal objetivo desta tarefa , como foi anteriormente referido , é criar uma fórmula que permita comprimir e descomprimir o estado do jogo da maneira mais eficaz
--possível , de forma a reduzir o espaço utilizado na gravação do estado atual do jogo. Sendo assim , e como nos foi pedido , iniciamos por comprimir os estado do jogo 
--para só depois descomprimir usando o mesmo método de pensamento.
--
-- * Procedimento
--
-- O método de pensamento usado foi dividir a estratégia de compressão por 3 partes : a compressão do mapa , a compressão da lista dos jogadores e compressão da lista de 
--disparos.
-- Assim sendo , a estratégia usada foi a seguinte :
--
-- > Compressão do estado 
--
-- > a - Compressão do mapa
--
-- No âmbito da compressão do mapa , começamos por remover do mapa as paredes laterais , ou seja , as chamadas bordas para reduzir ao número de caratéres a usar na compressão, isto
--recorrendo ao uso de 2 auxiliares. De seguida, usamos o 'v' para identificar a peça 'Vazia', o 'd' para a peça 'Bloco Destrutivel' e o 'i' para a peça 'Bloco Indestrutivel'. Por fim
--usamos o caratér '.' no final de cada linha para identificar as diferentes linhas do mapa. Exmplificando :
--
-- Input - listaMapa (novoMapa2 (mapaAux3 [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel]]))
--
-- 
-- Output - "vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv."
--
-- > b - Compressão da lista de jogadores
--
-- No que toca à compressão da lista de jogadores , o estratagema usado foi primeiramente iniciar a lista de jogadores com o caratér 'j' para destinguir a mesma do mapa.
--Depois , usou-se o caratér '.' para distinguir os jogadores e todos os inteiros foram separados por virgúlas para podermos converter em String usando a função Show, e por
--último usou-se o caratér '*' para distinguir a direção do jogador dos inteiros. Sendo que se converteu jogador a jogador e string e tudo foi concatenado no fim. Assim sendo,
--recorremos ao seguinte exemplo para mostrar o funcionamento deste método:
--
-- Input - listaJog [(Jogador (1,2) B 1 1 1),(Jogador (6,6) C 1 1 1)]
--
-- Output - "1,2,1,1,1,*B.6,6,1,1,1,*C."
--
-- Nota : o caratér 'j' apenas foi adicionado na concatenação das 3 strings comprimidas , daí não aparecer no output.
--
-- > c - Compressão da lista de disparos
--
-- Quanto à lista de disparos a estratégia usada foi semelhante à estretégia usada na lista de jogadores , começamos assim por separar esta string da string da lista de 
--jogadores recorrendo ao caratér 't' sendo que também foi apenas adicionado na concatenação final das strings. Quanto a identificação do tipo de disparo , usou-se o caratér
--'c' para o disparo de canhão , o caratér 'C' para o disparo de choque e o disparo de laser foi identificado pelo caratér 'L'. A restante estratégia , isto é , a estratégia
--usada para a separação dos inteiros , ou seja , o identificador do jogador , a posição e o tempo , foi a mesma usada na lista de jogadores , assim como o identificador da
--direçao. Como exemplo temos :
--
-- Input - listaDisp [(DisparoCanhao 0 (2,2) B),(DisparoChoque 2 2),(DisparoLaser 1 (3,1) D)]
--
-- Output - "c0,2,2,*B.C2,2*.L1,3,1,*D."
--
--
-- > Descompressão do estado 
--
-- No âmbito da descompressão a estratégia usada foi a mesma tanto na descompressão do mapa , como na das listas de jogadores e de disparos , foi recorrer ao take e drop while 
--usando os caratéres de distinção falados anteriormente como pontos de "paragem" e usando o descString para passar as strings que representam inteiros para inteiros.No fim ,
--concatenou-se tudo num só estado , sendo que as 3 listas apesar de comprimidas com igual método , foram descomprimidas de forma separada. Assim sendo , apresentaremos agora
--os exemplos de descompressão, tanto do mapa,como dos jogadores e como dos dosparos.
--
-- > a - Descompressão do mapa 
--
-- Input - descMapaFinal "vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.j1,2,1,1,1,*B.6,6,1,1,1,*C.tc0,2,2,*B.C2,2*.L1,3,1,*D."
--
-- Output - [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Vazia,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
--                                          ,Vazia,Bloco Indestrutivel]
--                                          ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
--                                          ,Bloco Indestrutivel]]
--
--
--
-- > b - Descompressão da lista de jogadores
--
-- Input - descJogFinal "vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.j1,2,1,1,1,*B.6,6,1,1,1,*C.tc0,2,2,*B.C2,2*.L1,3,1,*D."
--
-- Output - [Jogador {posicaoJogador = (1,2), direcaoJogador = B, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1},Jogador {posicaoJogador = (6,6), 
--           direcaoJogador = C, vidasJogador = 1, lasersJogador = 1, choquesJogador = 1}]
--
-- > c - Descompressão da lista de disparos 
-- 
-- Input - descTirosFinal "vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.vvvvvvvv.j1,2,1,1,1,*B.6,6,1,1,1,*C.tc0,2,2,*B.C2,2*.L1,3,1,*D."
--
-- Output - [DisparoCanhao {jogadorDisparo = 0, posicaoDisparo = (2,2), direcaoDisparo = B},DisparoChoque {jogadorDisparo = 2, tempoDisparo = 2},
--          DisparoLaser {jogadorDisparo = 1, posicaoDisparo = (3,1), direcaoDisparo = D}]
--
--
-- * Conclusão
--
-- $conc
-- Em suma , os resultados obtidos foram positivos sendo que todos os testes feitos não apresentaram erros , tendo obtido bons resultados no site de testes 
--disponiblizado pelos professores , mas no que toca a termos de avaliação não se obteu o total de nota da tarefa , o que prova que se poderia ter testado 
--mais vezes e de maneiras diferentes porque eventualmente não esteve tão eficaz como desejado, mas ,ainda assim , em termos de percentagem de avaliação foi 
--uma avaliação positiva sendo que tivemos uma boa classificação.

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [estado]

-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado m jog disp) = listaMapa (novoMapa2 (mapaAux3 m)) ++ "j" ++ listaJog (jog) ++ "t" ++ listaDisp (disp) 

-- | Funcao Auxiliar para a comprime --
dirToChar :: Direcao -> Char
dirToChar x | x==C = 'C'
            | x==B = 'B'
            | x==E = 'E'
            | otherwise = 'D'

-- Funcoes para comprimir o mapa --

-- | Funcao auxiliar para comprimir o Mapa
mapaAux1 :: [a] -> [a]
mapaAux1 [] = []
mapaAux1 l  = tail l

-- | Funcao auxiliar para comprimir o Mapa
mapaAux2 :: [a] -> [a]
mapaAux2 [] = []
mapaAux2 [a] = [] 
mapaAux2 (h:t) = h : mapaAux2 t   

-- | Funcao auxiliar para comprimir o Mapa
mapaAux3 :: Mapa -> Mapa 
mapaAux3 [] = []
mapaAux3 (h:t) = mapaAux2 (mapaAux1 h) : mapaAux3 t 
 
-- | Funcao auxiliar para comprimir o Mapa
novoMapa :: Mapa -> Mapa 
novoMapa [] = []
novoMapa l = tail l 

-- | Funcao auxiliar para comprimir o Mapa
removeUltima :: Mapa -> Mapa
removeUltima [] = []
removeUltima [a] = []
removeUltima (h:t) = h : removeUltima t

-- | Funcao auxiliar para comprimir o Mapa
novoMapa2 :: Mapa -> Mapa
novoMapa2 [] = []
novoMapa2 l = novoMapa (removeUltima (l))

-- | Comprime uma Peca para uma String
pecaToString :: Peca -> String 
pecaToString (Vazia) = 'v' : []
pecaToString (Bloco Destrutivel) = 'd' : []
pecaToString (Bloco Indestrutivel) = 'i' : []

-- | Comprime uma lista de Pecas para uma String
pecasToString :: [Peca] -> String 
pecasToString [] = ""
pecasToString (h:t) = pecaToString (h) ++ pecasToString (t)

-- | Termina a String com "."
pecasToString1 :: [Peca] -> String
pecasToString1 [] = ""
pecasToString1 l = pecasToString l ++ "."

-- | Comprime um Mapa para uma String
mapaToString :: Mapa -> String 
mapaToString [] = ""
mapaToString (h:t) = pecasToString1 h ++ mapaToString t 

-- | Termina a String do Mapa
listaMapa :: Mapa -> String 
listaMapa [] = ""
listaMapa m  = mapaToString m 

-- Funcoes para comprimir a lista de Jogadores --

-- | Comprime um jogador
jogToString :: Jogador -> String
jogToString (Jogador (x,y) d v l c) = show (x) ++ "," ++ show (y) ++ "," ++ show (v) ++ "," ++ show (l) ++ "," ++ show (c) ++ "," ++ "*" ++ [dirToChar (d)] ++ "."

-- | Comprime uma lista de Jogadores
listaJog :: [Jogador] -> String 
listaJog [] = ""
listaJog (h:t) = jogToString h ++ listaJog t 

-- Funcoes para comprimir a lista de Disparos --

-- | Comprime um Disparo
dispToString :: Disparo -> String 
dispToString (DisparoChoque j t) = "C" ++ show j ++ "," ++ show t ++ "*" ++ "."  
dispToString (DisparoLaser j (x,y) d)  = "L" ++ show j ++ "," ++ show x ++ "," ++ show y ++ "," ++ "*" ++ [dirToChar (d)] ++ "."
dispToString (DisparoCanhao j (x,y) d) = "c" ++ show j ++ "," ++ show x ++ "," ++ show y ++ "," ++ "*" ++ [dirToChar (d)] ++ "."

-- | Comprime uma lista de Disparos
listadToString :: [Disparo] -> String
listadToString [] = ""
listadToString (h:t) = dispToString h ++ listadToString t 

-- | Termina a String da Lista de Disparos
listaDisp :: [Disparo] -> String 
listaDisp [] = ""
listaDisp l  = listadToString l 

-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime l = Estado (descMapaFinal l) (descJogFinal l) (descTirosFinal l)

-- Funcoes auxiliares para a descomprime --
-- | Descomprime para inteiro
descString :: String -> Int
descString x = read x::Int

-- | Descomprime uma String para uma lista de Ints
descSInt :: String -> [Int]
descSInt "" = []
descSInt s = descString (takeWhile (/=',') s) : descSInt (tail (dropWhile (/=',') s))

-- | Transforma uma String numa Direcao
stringToDir :: String -> Direcao
stringToDir "C" = C
stringToDir "D" = D
stringToDir "E" = E
stringToDir "B" = B

-- Descompressão do mapa --
-- | Transforma um Char num tipo de Pecas
descPeca :: Char -> Peca 
descPeca x | x == 'v' = Vazia 
           | x == 'd' = Bloco Destrutivel
           | otherwise = Bloco Indestrutivel

-- | Transforma uma String numa Lista de Pecas
descListaPeca :: String -> [Peca]
descListaPeca "" = []
descListaPeca (h:t) = descPeca h : descListaPeca t

-- | Descomprime o Mapa sem as bordas
descMapa :: String -> Mapa 
descMapa "" = []
descMapa s = descListaPeca (takeWhile (/='.') s) : descMapa (tail (dropWhile (/='.') s))

-- | Adiciona as bordas laterais a uma linha
descMapa0 :: [Peca] -> [Peca]
descMapa0 [] = []
descMapa0 p  = [Bloco Indestrutivel] ++ p ++ [Bloco Indestrutivel]

-- | Adiciona as bordas laterais ao Mapa
descMapa1 :: Mapa -> Mapa
descMapa1 [] = []
descMapa1 (h:t) = descMapa0 h : descMapa1 t 

-- | Cria uma linha de Blocos Indestrutiveis
descMapa2 :: Int -> [Peca]
descMapa2 0 = []
descMapa2 x = Bloco Indestrutivel : descMapa2 (x-1)

-- | Adiciona a primeira e ultima linha de blocos Indestrutiveis ao mapa
descMapa4 :: Mapa -> Mapa 
descMapa4 [] = []
descMapa4 l = [descMapa2 (length (head (descMapa1 l)))] ++ (descMapa1 l) ++ [descMapa2 (length (head (descMapa1 l)))]       

-- | Descomprime o Mapa
descMapaFinal :: String -> Mapa 
descMapaFinal s = descMapa4 (descMapa (takeWhile (/='j') s))

-- Descompressão da lista de Jogadores -- 

-- | Descomprime a String de um Jogador
descToPlayer :: [Int] -> String -> Jogador 
descToPlayer (a:b:t) s = Jogador (a,b) (stringToDir (takeWhile (/='.') (tail (dropWhile (/='*') s)))) (head (t)) (head (tail t)) (head (tail (tail t)))

-- | Reduz a String para a String de um Jogador
descJog :: String -> Jogador 
descJog l = descToPlayer (descSInt (takeWhile (/='*') l)) l

-- | Descomprime uma Lista de Jogadores
descJog1 :: String -> [Jogador]
descJog1 "" = []
descJog1 l = descJog (takeWhile (/='.') l) : descJog1 (tail (dropWhile (/='.') l)) 

-- | Reduz uma String para uma Lista de Jogadores
descJogFinal :: String -> [Jogador]
descJogFinal "" = []
descJogFinal l = descJog1 (tail (dropWhile (/='j') (takeWhile (/='t') l))) 

-- Descompressão da lista de disparos -- 

-- | Descomprime um Disparo
tiroFromString :: [Int] -> String -> Disparo
tiroFromString (a:t) l | head l=='c' = DisparoCanhao (a) (head t,head (tail t)) (stringToDir (takeWhile (/='.') (tail (dropWhile (/='*') l))))
                       | head l=='L' = DisparoLaser  (a) (head t,head (tail t)) (stringToDir (takeWhile (/='.') (tail (dropWhile (/='*') l))))
                       | otherwise = DisparoChoque (a) (head t) 

-- | Reduz uma String para um Disparo
tiroFromString1 :: String -> Disparo 
tiroFromString1 l = tiroFromString (descSInt (tail (takeWhile (/='*') l))) l 

-- | Descomprime uma lista de Disparos
descTiros :: String -> [Disparo]
descTiros "" = []
descTiros l = tiroFromString1 (takeWhile (/='.') l) : descTiros (tail (dropWhile (/='.') l))

-- | Reduz uma Lista de Resultados
descTirosFinal :: String -> [Disparo]
descTirosFinal "" = []
descTirosFinal l  = descTiros (tail (dropWhile (/='t') l))

-- | Exemplo de Jogador1
jog1 :: Jogador
jog1 = Jogador (1,2) B 1 1 1

-- | Exemplo de Jogador2
jog2 :: Jogador
jog2 = Jogador (2,3) E 1 1 1

-- | Exemplo de Jogador3
jog3 :: Jogador
jog3 = Jogador (3,5) B 1 1 1

-- | Exemplo de Jogador4
jog4 :: Jogador
jog4 = Jogador (6,6) C 1 1 1

-- | Exemplo de Disparo Canhao
dis1 :: Disparo
dis1 = DisparoCanhao 0 (2,2) B

-- | Exemplo de Disparo Choque
dis2 :: Disparo
dis2 = DisparoChoque 1 3

-- | Exemplo de Disparo Choque
dis3 :: Disparo
dis3 = DisparoChoque 2 2

-- | Exemplo de Disparo Lase
dis4 :: Disparo
dis4 = DisparoLaser 1 (3,1) D

-- | Exemplo de Estado
estado :: Estado
estado = Estado m [jog1, jog4] [dis1, dis3, dis4]

-- | Mapa exemplo
m :: Mapa 
m = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]]
