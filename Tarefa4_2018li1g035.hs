-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g035 where

import LI11819
import Tarefa0_2018li1g035

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [estado,estado2,estado3]

-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- * Lasers


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado m jog disp) = Estado (atualizaMapa (posicoesLasers (removeSemSerLaser disp) m) m) (atualizaVidasLaser jog disp m) (removeselaser (removeCanhaoSeLaser disp m))


-- | Efeito dos lasers no mapa. 


-- | Verifica se um Disparo é Laser, returnando False caso contrário
verificaSeELaser :: Disparo -> Bool 
verificaSeELaser (DisparoChoque j t) = False
verificaSeELaser (DisparoLaser j (x,y) d)  = True
verificaSeELaser (DisparoCanhao j (x,y) d) = False


-- | Retira da lista de Disparo os que não são Laser
removeSemSerLaser :: [Disparo] -> [Disparo]
removeSemSerLaser [] = []
removeSemSerLaser (h:t) = if verificaSeELaser h == False then removeSemSerLaser t else h : removeSemSerLaser t 


-- | Cria a Lista de Posicoes afetadas por um Lasers
posicoes1laser :: Disparo -> Mapa -> [Posicao]
posicoes1laser d [] = []
posicoes1laser (DisparoLaser j (x,y) d) m | d==C = if (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) then [] else [(x,y),(x,y+1)] ++ posicoes1laser (DisparoLaser j (x-1,y) d) m
                                          | d==B = if (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) then [] else [(x+1,y),(x+1,y+1)] ++ posicoes1laser (DisparoLaser j (x+1,y) d) m
                                          | d==D = if (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) then [] else [(x,y+1),(x+1,y+1)] ++ posicoes1laser (DisparoLaser j (x,y+1) d) m
                                          | otherwise = if (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) then [] else [(x,y),(x+1,y)] ++ posicoes1laser (DisparoLaser j (x,y-1) d) m 


-- | Cria uma Lista das Posicoes afetadas por todos os Disparos Laser
posicoesLasers :: [Disparo] -> Mapa -> [Posicao]
posicoesLasers [] [] = []
posicoesLasers l []  = []
posicoesLasers [] l  = []
posicoesLasers (h:t) m = posicoes1laser h m ++ posicoesLasers t m 

-- | Efeito dos lasers nos jogadores.

-- | Cria a lista de Posicoes ocupadas por um Jogador
posicoesOcupadas1jog :: Jogador -> [Posicao]
posicoesOcupadas1jog (Jogador (x,y) d v l c) = [(x,y),(x+1,y),(x,y+1),(x+1,y+1)] 


-- | Diminui a vida dos Jogadores que são atingidos por um Laser
atualizaVidasLaser :: [Jogador] -> [Disparo] -> Mapa -> [Jogador]
atualizaVidasLaser [] []   [] = []
atualizaVidasLaser [] disp m = []
atualizaVidasLaser j  []   m = j 
atualizaVidasLaser j  disp []= j 
atualizaVidasLaser ((Jogador (x,y) d v l c):t) disp m = if verificaPosFinal (posicoesOcupadas1jog (Jogador (x,y) d v l c)) (posicoesLasers (removeSemSerLaser disp) m) == True then (Jogador (x,y) d (v-1) l c) : atualizaVidasLaser t disp m else (Jogador (x,y) d v l c) : atualizaVidasLaser t disp m 

-- | Efeito dos lasers nos disparos e sua atualização.

-- | Cria a lista de Posicoes afetadas pelo Laser, parando no primeiro Bloco Indestrutivel que atingir
posicoesexatas1laser :: Disparo -> Mapa -> [Posicao]
posicoesexatas1laser d [] = []
posicoesexatas1laser (DisparoLaser j (x,y) d) m | d==C = if (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) then [] else [(x,y)] ++ posicoesexatas1laser (DisparoLaser j (x-1,y) d) m
                                                | d==B = if (encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) then [] else [(x,y)] ++ posicoesexatas1laser (DisparoLaser j (x+1,y) d) m
                                                | d==D = if (encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) then [] else [(x,y)] ++ posicoesexatas1laser (DisparoLaser j (x,y+1) d) m
                                                | otherwise = if (encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) then [] else [(x,y)] ++ posicoesexatas1laser (DisparoLaser j (x,y-1) d) m 


-- | Cria a lista de Posicoes afetadas por todos os Lasers ativos
posicoesLasers2 :: [Disparo] -> Mapa -> [Posicao]
posicoesLasers2 [] [] = []
posicoesLasers2 l []  = []
posicoesLasers2 [] l  = []
posicoesLasers2 (h:t) m = posicoesexatas1laser h m ++ posicoesLasers2 t m 

-- | Compara as Posicoes de um Disparo Canhao
comparaCanhaocomLasers :: Disparo -> [Posicao] -> Bool
comparaCanhaocomLasers c [] = False
comparaCanhaocomLasers (DisparoCanhao j (x,y) d) l = if verificaPosIgual1 (x,y) l == True then True else False
comparaCanhaocomLasers x l = False 


-- | Remove um Disparo Canhao caso seja atingido por um Laser
removeCanhaoSeLaser :: [Disparo] -> Mapa -> [Disparo]
removeCanhaoSeLaser [] [] = []
removeCanhaoSeLaser [] m = []
removeCanhaoSeLaser d [] = []
removeCanhaoSeLaser (h:t) m = if verificaSeECanhao h == True && comparaCanhaocomLasers h (posicoesLasers2 (removeSemSerLaser (h:t)) m) == True then removeCanhaoSeLaser t m else h : removeCanhaoSeLaser t m 


-- | Retira um Disparo Laser da Lista de Disparos
removeselaser :: [Disparo] -> [Disparo]
removeselaser [] = []
removeselaser (h:t) = if verificaSeELaser h == True then removeselaser t else h : removeselaser t 

-- * Canhões


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado m jog disp) = Estado (atualizaMapa (disparosPos (soCanhoes disp)) m) (atualizaVidasJog jog disp) (atualizaPosicoesCanhao2 (atualizaverificando (listaColiEntCanhoes disp disp) jog m))

-- | Funções de ajuda 

-- | Verifica se duas Posicoes são iguais
verificaPosIgual :: Posicao -> Posicao -> Bool
verificaPosIgual (x,y) (a,b) = if x==a && y==b then True else False  

-- | Verifica se uma Posicao esta presente numa Lista de Posicoes
verificaPosIgual1 :: Posicao -> [Posicao] -> Bool
verificaPosIgual1 (x,y) [] = False
verificaPosIgual1 (x,y) (h:t) = if verificaPosIgual (x,y) h == True then True else False 


-- | Verifica se alguma das Posicoes da primeira lista esta presente na segunda
verificaPosFinal :: [Posicao] -> [Posicao] -> Bool 
verificaPosFinal [] [] = False 
verificaPosFinal _ []  = False
verificaPosFinal [] _  = False
verificaPosFinal (a:b) (h:t) = if verificaPosIgual1 a (h:t) == True then True else verificaPosFinal b (h:t)

-- | Retirar vidas aos jogadores se houver choque contra algum canhão. 

-- | Verifica se um Disparo é Canhao
verificaSeECanhao :: Disparo -> Bool 
verificaSeECanhao (DisparoChoque j t) = False
verificaSeECanhao (DisparoLaser j (x,y) d)  = False
verificaSeECanhao (DisparoCanhao j (x,y) d) = True 


-- | Retira a Posicao do Canhao
canhaoToPos :: Disparo -> Posicao 
canhaoToPos (DisparoCanhao j (x,y) d) = (x,y)


-- | Retira da Lista de Disparos todos os que não são Disparos Canhao
soCanhoes :: [Disparo] -> [Disparo]
soCanhoes [] = []
soCanhoes (h:t) = if verificaSeECanhao h == True then h : soCanhoes t else soCanhoes t 


-- | Cria a Lista de Posicoes afetadas pelos Canhoes
posCanhoes :: [Disparo] -> [Posicao]
posCanhoes [] = []
posCanhoes (h:t) = canhaoToPos h : posCanhoes t 


-- | A partir de um Jogador cria uma lista de Posicoes proibidas
posicoesProibidas :: Jogador -> [Posicao]
posicoesProibidas (Jogador (x,y) d v l c) = [(x,y-1),(x+1,y-1),(x-1,y-1),(x+1,y),(x-1,y),(x,y+1),(x+1,y+1),(x-1,y+1)]


-- | Retira uma vida a um Jogador que é atingido por um Disparo Canhao
atualizaVidaDe1Jog :: Jogador -> [Disparo] -> Jogador 
atualizaVidaDe1Jog (Jogador (x,y) d v l c) []   = (Jogador (x,y) d v l c)
atualizaVidaDe1Jog (Jogador (x,y) d v l c) disp = if verificaPosFinal (posCanhoes(soCanhoes (disp))) (posicoesProibidas (Jogador (x,y) d v l c)) == True && v>0 then (Jogador (x,y) d (v-1) l c) else (Jogador (x,y) d v l c)


-- | Retira uma vida aos Jogadores que forem atingidos por Disparos Canhao
atualizaVidasJog :: [Jogador] -> [Disparo] -> [Jogador]
atualizaVidasJog [] []      = []
atualizaVidasJog jog []     = jog 
atualizaVidasJog [] disp    = []
atualizaVidasJog (h:t) disp = atualizaVidaDe1Jog h disp : atualizaVidasJog t disp

-- | Atualizar o mapa se houver choque de algum canhão.

-- | Cria a Lista de Posicoes afetadas pelo Disparo Canhao num dado local
disparoPos :: Disparo -> [Posicao] 
disparoPos (DisparoCanhao j (x,y) d) | d==B = [(x+1,y+1),(x+1,y)]
                                     | d==C = [(x,y),(x,y+1)]
                                     | d==D = [(x+1,y),(x+1,y+1)]
                                     | otherwise = [(x,y),(x+1,y)]


-- | Cria a Lista de Posicoes afetadas por vários Disparos Canhao
disparosPos :: [Disparo] -> [Posicao]
disparosPos [] = []                                    
disparosPos (h:t) = disparoPos h ++ disparosPos t 

-- | Remove as Pecas Bloco Destrutiveis quando são atingidas, e substitui-as por Pecas Vazias
atualizaMapa :: [Posicao] -> Mapa -> Mapa 
atualizaMapa [] [] = []
atualizaMapa [] m  = m
atualizaMapa l  [] = []
atualizaMapa (h:t) m = if encontraPosicaoMatriz h m == Bloco Destrutivel then atualizaMapa t (atualizaPosicaoMatriz h (Vazia) m) else atualizaMapa t m 

-- | Explosões dos tiros de canhão.

-- | Ve se 2 Disparos Canhao estao na mesma PosicaoGrelha
comparaCanhoes :: Disparo -> Disparo -> Bool 
comparaCanhoes (DisparoCanhao j1 (x,y) d1) (DisparoCanhao j2 (a,b) d2) = if ((x,y) == (a,b) && j1/=j2) || ((x,y) == (a,b) && d1/=d2) then True else False 
comparaCanhoes x y = False 


-- | Ve se um Disparo canhao esta na mesma posicao de outro da lista
comparaCanhoes2 :: Disparo -> [Disparo] -> Bool 
comparaCanhoes2 x [] = False 
comparaCanhoes2 x (h:t) = if comparaCanhoes x h == True then True else comparaCanhoes2 x t 

-- | Compara duas listas de Disparos Canhao
listaColiEntCanhoes :: [Disparo] -> [Disparo] -> [Disparo]
listaColiEntCanhoes [] [] = []
listaColiEntCanhoes l []  = []
listaColiEntCanhoes [] l  = []
listaColiEntCanhoes (h:t) l = if comparaCanhoes2 h l == False then h : listaColiEntCanhoes t l else listaColiEntCanhoes t l


-- | Verifica se o Disparo Canhao esta a colidir com algum Jogador
verificaColiTanques :: Disparo -> Jogador -> Bool
verificaColiTanques (DisparoCanhao j (a,b) dir) (Jogador (x,y) d v l c) | dir==C && ((x,y)==(a-1,b) || (x,y)==(a-1,b+1) || (x,y)==(a-1,b-1)) = True 
                                                                        | dir==B && ((x,y)==(a+1,b) || (x,y)==(a+1,b+1) || (x,y)==(a+1,b-1)) = True 
                                                                        | dir==D && ((x,y)==(a,b+1) || (x,y)==(a-1,b+1) || (x,y)==(a+1,b+1)) = True
                                                                        | dir==E && ((x,y)==(a,b-1) || (x,y)==(a-1,b-1) || (x,y)==(a+1,b-1)) = True
                                                                        | otherwise = False 
verificaColiTanques x y = False


-- | Verifica se o Disparo Canhao esta a colidir com algum dos Jogadores da Lista
verificaColiTanques2 :: Disparo -> [Jogador] -> Bool 
verificaColiTanques2 d [] = False 
verificaColiTanques2 d (h:t) = if verificaColiTanques d h == True then True else verificaColiTanques2 d t 


-- | Verifica se o Disparo Canhao esta a colidir com algum Bloco Indestrutivel ou Bloco Destrutivel
verificaColiParedes :: Disparo -> Mapa -> Bool
verificaColiParedes t [] = False 
verificaColiParedes (DisparoCanhao j (x,y) d) m | d==C && (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel || encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x,y+1) m == Bloco Destrutivel || encontraPosicaoMatriz (x,y+1) m == Bloco Indestrutivel) = True
                                                | d==B && (encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) = True
                                                | d==D && (encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y+1) m == Bloco Indestrutivel) = True
                                                | d==E && (encontraPosicaoMatriz (x,y) m == Bloco Destrutivel || encontraPosicaoMatriz (x,y) m == Bloco Indestrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Destrutivel || encontraPosicaoMatriz (x+1,y) m == Bloco Indestrutivel) = True                         
                                                | otherwise = False
verificaColiParedes t m = False

-- | Atualiza  a lista de Disparos, verificando atraves das funcoes anteriores
atualizaverificando :: [Disparo] -> [Jogador] -> Mapa -> [Disparo]
atualizaverificando [] _ _ = []
atualizaverificando (h:t) jog m = if (verificaColiTanques2 h jog == True || verificaColiParedes h m == True) then atualizaverificando t jog m else h : atualizaverificando t jog m 

-- | Atualiza a PosicaoGrelha do Disparo Canhao
atualizaPosicoesCanhao :: Disparo -> Disparo 
atualizaPosicoesCanhao (DisparoCanhao j (x,y) d) | d==C = (DisparoCanhao j (x-1,y) d)
                                                 | d==B = (DisparoCanhao j (x+1,y) d)
                                                 | d==D = (DisparoCanhao j (x,y+1) d)
                                                 | otherwise = (DisparoCanhao j (x,y-1) d)
atualizaPosicoesCanhao x = x                                                  


-- | Atualiza a PosicaoGrelha da lista de Disparos
atualizaPosicoesCanhao2 :: [Disparo] -> [Disparo]
atualizaPosicoesCanhao2 [] = []
atualizaPosicoesCanhao2 (h:t) = atualizaPosicoesCanhao h : atualizaPosicoesCanhao2 t 


-- * Choques

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado m jog disp) = Estado m jog (atualizaChoques (removeTicks0 disp))

-- | Funçoes que removem os choques que estejam com a contagem dos ticks a 0.

-- | Verifica se o t de um Disparo Choque esta a 0
verificaSe0 :: Disparo -> Bool 
verificaSe0 (DisparoChoque j t) = if t==0 then True else False
verificaSe0 (DisparoLaser j (x,y) d)  = False
verificaSe0 (DisparoCanhao j (x,y) d) = False 

-- | Remove um Disparo Choque da lista de Disparos, se este tiver t==0
removeTicks0 :: [Disparo] -> [Disparo]
removeTicks0 [] = []
removeTicks0 (h:t) = if verificaSe0 h == True then removeTicks0 t else h:removeTicks0 t

-- | Funções que atualizam o tempo dos choques 

-- | Decrementa o t de um Disparo, caso seja Laser
atualizaSeChoque :: Disparo -> Disparo
atualizaSeChoque (DisparoChoque j t) = if t>0 then (DisparoChoque j (t-1)) else (DisparoChoque j t)
atualizaSeChoque (DisparoLaser j (x,y) d) = (DisparoLaser j (x,y) d)
atualizaSeChoque (DisparoCanhao j (x,y) d) = (DisparoCanhao j (x,y) d)


-- | Atualiza os Disparos Choques da lista de Disparos
atualizaChoques :: [Disparo] -> [Disparo]
atualizaChoques [] = []
atualizaChoques (h:t) = atualizaSeChoque h : atualizaChoques t 

-- * Exemplos

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

-- | Exemplo de Disparo Laser
dis4 :: Disparo
dis4 = DisparoLaser 1 (3,1) D

-- | Exemplo de Estado1
estado :: Estado
estado = Estado m2 [jog1,jog3,jog4] [dis1,dis3,dis4]

-- | Exemplo de Estado2
estado2 :: Estado
estado2 = Estado m1 [jog2,jog4] [dis1,dis2,dis3,dis4]

-- | Exemplo de Estado3
estado3 :: Estado
estado3 = Estado m3 [jog1,jog2,jog3,jog4] [dis3, dis4]

-- | Exemplo de Mapa1
m1 :: Mapa 
m1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]]

-- | Exemplo de Mapa2
m2 :: Mapa 
m2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]]

-- | Exemplo de Mapa3
m3 :: Mapa 
m3 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
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
