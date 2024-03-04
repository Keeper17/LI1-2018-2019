-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g035 where

import LI11819
import Tarefa0_2018li1g035

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = []

-- Função para ver o Jogador que vai jogar e repô-lo na lista --

-- | Substitui um jogador na Lista
procuraJogador :: [Jogador] -> Int -> Jogador
procuraJogador (h:t) j = if j==0 then h else procuraJogador t (j-1)

-- | Substitui um jogador na Lista
repoeJogador :: [Jogador] -> Jogador -> Int -> [Jogador]
repoeJogador [] _ _ = []
repoeJogador (h:t) j iden = if iden==0 then j : t else h : repoeJogador t j (iden-1)

-- | Remove um Jogador da lista
removeJogador :: [Jogador] -> Int -> [Jogador]
removeJogador [] _ = []
removeJogador (h:t) j = if j==0 then t else h : removeJogador t (j-1)

-- Funções relativas à atualização da lista de disparos --  

-- | Coloca o Tiro na Grelha
comecoDoTiro :: PosicaoGrelha -> Direcao -> PosicaoGrelha 
comecoDoTiro (x,y) d | d==C = (x-1,y)
                     | d==B = (x+1,y)
                     | d==E = (x,y-1)
                     | otherwise = (x,y+1)

-- | Atualiza o Disparo conforme o tipo de Disparo
atualizaListaDisparos :: [Disparo] -> Jogador -> Int -> Jogada -> [Disparo] 
atualizaListaDisparos [] (Jogador (x,y) d v l c) p j | j == Dispara Canhao        && v>0 = [DisparoCanhao p (comecoDoTiro (x,y) d) d]
                                                     | j == Dispara Laser  && l>0 && v>0 = [DisparoLaser  p (comecoDoTiro (x,y) d) d]
                                                     | j == Dispara Choque && c>0 && v>0 = [DisparoChoque p 5]
                                                     | otherwise = []                                                                                        
atualizaListaDisparos (h:t) jog p j = h : atualizaListaDisparos t jog p j

-- Funções relativas à atualização da lista de jogadores --
-- | Atualiza a Posicao de um Jogador
atualizaPosicao :: Jogador -> Jogada -> Jogador 
atualizaPosicao (Jogador (x,y) d v l c) jog | d==C && jog==Movimenta C = (Jogador (x-1,y) d v l c) 
                                            | d==B && jog==Movimenta B = (Jogador (x+1,y) d v l c) 
                                            | d==E && jog==Movimenta E = (Jogador (x,y-1) d v l c) 
                                            | d==D && jog==Movimenta D = (Jogador (x,y+1) d v l c) 
                                            | otherwise = (Jogador (x,y) d v l c) 


-- | Atualiza a Direcao de um Jogador
atualizaDirecao :: Jogador -> Jogada -> Jogador
atualizaDirecao (Jogador (x,y) d v l c) jog | jog==Movimenta C = (Jogador (x,y) C v l c) 
                                            | jog==Movimenta B = (Jogador (x,y) B v l c) 
                                            | jog==Movimenta E = (Jogador (x,y) E v l c) 
                                            | jog==Movimenta D = (Jogador (x,y) D v l c) 
                                            | otherwise = (Jogador (x,y) d v l c)

-- | Atualiza o numero de Municoes de Laser                                            
atualizaLaser :: Jogador -> Jogada -> Jogador 
atualizaLaser (Jogador (x,y) d v l c) jog | jog==Dispara Laser && v>0 && l>0 = (Jogador (x,y) d v (l-1) c)
                                          | otherwise = (Jogador (x,y) d v l c)

-- | Atualiza o numero de Municoes de Choques
atualizaChoque :: Jogador -> Jogada -> Jogador
atualizaChoque (Jogador (x,y) d v l c) jog | jog==Dispara Choque && v>0 && c>0 = (Jogador (x,y) d v l (c-1))
                                           | otherwise = (Jogador (x,y) d v l c)
 
-- Condições para ser possível para ser possível atualizar a posicao --

-- Verifica se aparece algum tanque na sua direcao --

-- | Atualiza a Posicao quando o jogador esta na mesma direcao
atualizaPosJogSePos :: Jogador -> Jogada -> Jogador 
atualizaPosJogSePos (Jogador (x,y) d v l c) jog | d==C && jog==Movimenta C = (Jogador (x-1,y) d v l c)
                                                | d==B && jog==Movimenta B = (Jogador (x+1,y) d v l c)
                                                | d==E && jog==Movimenta E = (Jogador (x,y-1) d v l c)
                                                | d==D && jog==Movimenta D = (Jogador (x,y+1) d v l c)
                                                | otherwise = (Jogador (x,y) d v l c)

-- | Cria uma lista com as posicoes ocupadas pelo Jogador                                             
posicoesOcupadas :: Jogador -> [Posicao] 
posicoesOcupadas (Jogador (x,y) d v l c) = [(x,y),(x+1,y),(x,y+1),(x+1,y+1)]

-- | Usa a posicoesOcupadas para vários Jogadore
posicoesJogadores :: [Jogador] -> [[Posicao]]
posicoesJogadores [] = []
posicoesJogadores (h:t) = posicoesOcupadas h : posicoesJogadores t  

-- | Compara duas posicoes e verifica se sao iguais
comparaPosicao :: Posicao -> Posicao -> Bool 
comparaPosicao a b = if a==b then False else True

-- | Compara uma Posicao com uma lista de Posicoes e verifica se sao iguais
comparaPosicaoComLista :: Posicao -> [Posicao] -> Bool
comparaPosicaoComLista a [] = True 
comparaPosicaoComLista a (h:t) = if comparaPosicao a h == False then False else comparaPosicaoComLista a t

-- | Compara uma Posicao com as de uma Matriz e verifica se sao iguais
comparaPosicaocomMatriz :: Posicao -> [[Posicao]] -> Bool
comparaPosicaocomMatriz a [] = True 
comparaPosicaocomMatriz a (h:t) = if comparaPosicaoComLista a h == False then False else comparaPosicaocomMatriz a t 

-- | Verifica se alguma das Posicoes da lista estao na Matriz
verificaPosicoes :: [Posicao] -> [[Posicao]] -> Bool
verificaPosicoes [] [] = True 
verificaPosicoes [] a  = True
verificaPosicoes a []  = True
verificaPosicoes (h:t) a = if comparaPosicaocomMatriz h a == False then False else verificaPosicoes t a 

-- Verifica se esta algum choque ativo --

-- | Cria uma Lista com o jogadores ativou o choque
veSeHaChoques1 :: Disparo -> [Int]
veSeHaChoques1 (DisparoChoque j t) = [j]
veSeHaChoques1 (DisparoLaser j (x,y) d)  = []
veSeHaChoques1 (DisparoCanhao j (x,y) d) = []

-- | Cria uma lista com os jogadores que ativaram os choques
veSeHaChoques2 :: [Disparo] -> [Int]
veSeHaChoques2 [] = []
veSeHaChoques2 (h:t) = veSeHaChoques1 h ++ veSeHaChoques2 t

-- | Tira um jogador da lista de Jogadores
veOJogador :: [Jogador] -> Int -> Jogador 
veOJogador ((Jogador (x,y) d v l c):t) j = if j==0 then (Jogador (x,y) d v l c) else veOJogador t (j-1)

-- | Cria uma lista com os jogadores que foram retirados 
veOJogador2 :: [Jogador] -> [Int] -> [Jogador]
veOJogador2 [] [] = []
veOJogador2 a [] = []
veOJogador2 [] a = []
veOJogador2 l (h:t) = veOJogador l h : veOJogador2 l t 

-- | Cria uma Lista das Posicoes onde o Choque e efetivo
posicaoToListadePosicoesChoque :: Jogador -> [Posicao]
posicaoToListadePosicoesChoque (Jogador (x,y) d v l c) = [(x-2,y-2),(x-2,y-1),(x-2,y),(x-2,y+1),(x-2,y+2),(x-2,y+3),(x-1,y-2),(x-1,y-1),(x-1,y),(x-1,y+1),(x-1,y+2),(x-1,y+3),(x,y-2),(x,y-1),(x,y),(x,y+1),(x,y+2),(x,y+3),(x+1,y-2),(x+1,y-1),(x+1,y),(x+1,y+1),(x+1,y+2),(x+1,y+3),(x+2,y-2),(x+2,y-1),(x+2,y),(x+2,y+1),(x+2,y+2),(x+2,y+3),(x+3,y-2),(x+3,y-1),(x+3,y),(x+3,y+1),(x+3,y+2),(x+3,y+3)]

-- | Cria uma lista de Posicoes onde o Choque e efetivo por cada jogador
posicaoToListadePosicoesChoque2 :: [Jogador] -> [[Posicao]]
posicaoToListadePosicoesChoque2 [] = []
posicaoToListadePosicoesChoque2 (h:t) = posicaoToListadePosicoesChoque h : posicaoToListadePosicoesChoque2 t 

-- Verifica se tem Paredes em volta --

-- | Verifica se tem paredes 
verificaMapa :: Mapa -> Jogador -> Jogada -> Bool 
verificaMapa [] j play = True 
verificaMapa m (Jogador (x,y) d v l c) play | d==C && play==Movimenta C && (encontraPosicaoMatriz (x-1,y) m == Vazia) && (encontraPosicaoMatriz (x-1,y+1) m == Vazia) = True
                                            | d==B && play==Movimenta B && (encontraPosicaoMatriz (x+2,y) m == Vazia) && (encontraPosicaoMatriz (x+2,y+1) m == Vazia) = True
                                            | d==E && play==Movimenta E && (encontraPosicaoMatriz (x,y-1) m == Vazia) && (encontraPosicaoMatriz (x+1,y-1) m == Vazia) = True
                                            | d==D && play==Movimenta D && (encontraPosicaoMatriz (x,y+2) m == Vazia) && (encontraPosicaoMatriz (x+1,y+2) m == Vazia) = True
                                            | otherwise = False 

-- Verifica se pode atualizar a posicao --
-- | Verifica se pode ou nao mover-se na Direcao
verificaFinal :: Jogador -> [Jogador] -> [Disparo] -> Mapa -> Jogada -> Jogador
verificaFinal (Jogador (x,y) d v l c) players disp m play = if comparaPosicaocomMatriz (x,y) (posicaoToListadePosicoesChoque2 (players)) == True && verificaMapa (m) (Jogador (x,y) d v l c) play == True && verificaPosicoes (posicoesOcupadas (Jogador (x,y) d v l c)) (posicoesJogadores (players)) == True then atualizaPosicao (Jogador (x,y) d v l c) (play) else (Jogador (x,y) d v l c)

-- | Efetua uma jogada
jogada :: Int -> Jogada -> Estado -> Estado 
jogada j play (Estado m jog disp) = Estado (m) (repoeJogador (jog) (atualizaDirecao (verificaFinal (atualizaLaser (atualizaChoque (procuraJogador (jog) (j)) play) play) (removeJogador (jog) (j)) (disp) (m) (play)) play) (j)) (atualizaListaDisparos (disp) (procuraJogador (jog) (j)) j play)

