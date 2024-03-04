-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g035 where
import Tarefa0_2018li1g035
import LI11819

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[Move D, Move D, Move D] , [MudaParede,Desenha,Move C,Move E ,MudaParede , Roda, Desenha] , [Move D, Roda, Desenha],[],[Desenha,Desenha],[MudaTetromino,MudaTetromino,Roda,Move D,Desenha]]


-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao ins (Editor (x,y) d t pr m)  | ins ==      Roda     = ( Editor (x,y)  (rodaDir d)    t            pr        m )
                                       | ins == MudaTetromino = ( Editor (x,y)        d   (mudaTetro t)     pr        m )
                                       | ins ==   MudaParede  = ( Editor (x,y)        d        t       (mudaPar pr)   m )
                                       | ins ==    Desenha    = desMapa(Editor (x,y) d t pr m) (0,0) 
                                       | ins ==     Move C    = (Editor (x-1,y)       d        t            pr        m )
                                       | ins ==     Move D    = (Editor (x,y+1)       d        t            pr        m )
                                       | ins ==     Move B    = (Editor (x+1,y)       d        t            pr        m )
                                       | ins ==     Move E    = (Editor (x,y-1)       d        t            pr        m ) 
                                       | otherwise = (Editor (x,y) d t pr m)


-- | Muda o tipo de Tetromino
mudaTetro :: Tetromino -> Tetromino
mudaTetro t | t == I =  J
            | t == J =  L
            | t == L =  O
            | t == O =  S
            | t == S =  T
            | t == T =  Z
            | t == Z =  I

-- | Muda o tipo de Parede
mudaPar :: Parede -> Parede
mudaPar p | p ==  Indestrutivel = Destrutivel
          | otherwise = Indestrutivel

-- | Muda a Direcao para a proxima
rodaDir :: Direcao -> Direcao
rodaDir d | d == C = D 
          | d == D = B
          | d == B = E 
          | d == E = D

--------------------------------------------------------DESENHA

-- | Dá o tamanho do Tetromino 
tamanhoTetro :: Tetromino -> Int
tamanhoTetro t | t == I = 3
               | t == O = 1
               | otherwise = 2

-- | Poe o Tetromino na Direcao correta
dirMat :: Tetromino -> Direcao -> Matriz Bool
dirMat t d | d == C = tetrominoParaMatriz t
           | d == D = rodaMatriz ( tetrominoParaMatriz t)
           | d == B = rodaMatriz (rodaMatriz ( tetrominoParaMatriz t))
           | d == E = rodaMatriz (rodaMatriz (rodaMatriz( tetrominoParaMatriz t)))



-- | Recursiva da fazPecaL para as linhas
desMapa :: Editor -> (Int,Int) -> Editor
desMapa (Editor (x,y) d t p m) (a,b) | ((tamanhoTetro t) + x) == a = fazPecaL (dirMat t d) (Editor (x,y) d t p m) (a,b)
                                     | otherwise = desMapa (fazPecaL (dirMat t d) (Editor (x,y) d t p m) (a,b)) ((a+1),b)

-- | Recursiva da fazPeca para uma linha
fazPecaL :: Matriz Bool -> Editor -> (Int,Int) -> Editor
fazPecaL mb (Editor (x,y) d t p m) (a,b) | ((tamanhoTetro t) + y) == b = fazPeca mb (Editor (x,y) d t p m) (a,b)
                                         | otherwise = fazPecaL mb (fazPeca mb (Editor (x,y) d t p m ) (a,b) ) (a,(b+1))

-- | Muda uma Peca
fazPeca :: Matriz Bool -> Editor -> (Int,Int) -> Editor
fazPeca mb (Editor (x,y) d t p m) (a,b) | posiMat mb (a,b) == True = ( Editor (x,y) d t p (atualizaPosicaoMatriz (x+a,y+b) (Bloco p) m))
                                        | otherwise = (Editor (x,y) d t p m)

-- | Ve onde vai comecar o Tetromino
posiMat :: Matriz Bool -> Posicao -> Bool
posiMat ((x:xs):ys) (a,b) | (eIndiceListaValido b (x:xs) == False || eIndiceListaValido a ((x:xs):ys) == False ) = False
                          | b == 0 && a == 0 = x
                          | b /= 0 && a == 0  = posiMat (xs:ys) (a,(b-1))
                          | otherwise = posiMat ys ((a-1),b)




-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] e = e
instrucoes (h:t) e = instrucoes  t (instrucao h e)


-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
newdimensao :: Dimensao -> Dimensao 
newdimensao (x,y) = ((x-1),(y-1))

-- | Cria o numero de Colunas da Matriz
dimensaoToPosicoesLinha :: Dimensao -> [(Int,Int)]
dimensaoToPosicoesLinha (x,y) = if y<=0 then [] else dimensaoToPosicoesLinha (x,(y-1)) ++ [newdimensao (x,y)]

-- | Cria o numero de Linhas da Matriz
dimensaoToPosicoesMatriz :: Dimensao -> [[(Int,Int)]]
dimensaoToPosicoesMatriz (x,y) = if x<=0 then [] else dimensaoToPosicoesMatriz ((x-1),y) ++ [dimensaoToPosicoesLinha (x,y)]

-- | Preenche as linhas com Peca
linhaPosParaPecas :: Dimensao -> [(Int,Int)] -> [Peca]
linhaPosParaPecas (a,b) []    = []
linhaPosParaPecas (a,b) ((x,y):t) = if x==0 || y==0 || x==fst (newdimensao (a,b)) || y==snd (newdimensao (a,b)) then Bloco Indestrutivel : linhaPosParaPecas (a,b) t else Vazia : (linhaPosParaPecas (a,b) t)

-- | Preenche a Matriz com as varias Linhas
matrizPosParaPecas :: Dimensao -> [[(Int,Int)]] -> Mapa
matrizPosParaPecas (a,b) [] = []
matrizPosParaPecas (a,b) (h:t) = linhaPosParaPecas (a,b) h : matrizPosParaPecas (a,b) t

-- | Cria a Matriz a partir da Dimensao
mapaInicial :: Dimensao -> Mapa
mapaInicial (x,y) = matrizPosParaPecas (x,y) (dimensaoToPosicoesMatriz (x,y))

-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial :: Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial []    = ( Editor (posicaoInicial []) C I Indestrutivel    (mapaInicial(dimensaoInicial []))  ) 
editorInicial (h:t) | h == Roda = editorInicial t
editorInicial (h:t) | h == MudaTetromino = editorInicial t 
editorInicial (h:t) | h == MudaParede = editorInicial t
editorInicial (h:t) = (Editor (mudaPos (h:t) (posicaoInicial (h:t))) C  I  Indestrutivel  (mapaInicial(dimensaoInicial (h:t))) )

-- | Altera a Posicao apos a Instrucao Move
mudaPos :: Instrucoes -> Posicao -> Posicao
mudaPos [] (x,y) = (x,y)
mudaPos (h:t) (x,y)| h == Move C = mudaPos t ((x-1),y)
                   | h == Move B = mudaPos t ((x+1),y)
                   | h == Move D = mudaPos t (x,(y+1))
                   | h == Move E = mudaPos t (x,(y-1))
                   | otherwise = mudaPos t (x,y)

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi ins = tiraMapa ( instrucoes ins (editorInicial ins) )

-- | Retira o Mapa de um Editor
tiraMapa :: Editor -> Mapa
tiraMapa (Editor pos dir tetr par mapa ) = mapa
