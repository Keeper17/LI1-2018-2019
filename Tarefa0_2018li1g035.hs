-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g035 where

import LI11819

-- * Funções não-recursivas.

-- | Um 'Vetor' é uma 'Posicao' em relação à origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** Funções sobre vetores

-- *** Funções gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (x,y) (a,b) = (x+a,y+b)

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (x,y) (a,b) = (x-a,y-b)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor a (x,y) = (x*a,y*a)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (x,y) = (y,-x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x,y) = (x,-y)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x,y) = (-x,y)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor x | x==C = (-1,0) 
                   | x==D = (0,1)
                   | x==B = (1,0)
                   | x==E = (0,-1)

-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido a [] = False
eIndiceListaValido a l | a < 0 = False
                       | ((length l)-1) < a = False
                       | otherwise = True

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao
dimensaoMatriz [] = (0,0)
dimensaoMatriz (h:t) = ( contaLinha (h:t) , contaCol h )

contaCol :: [a] -> Int 
contaCol [] = 0
contaCol (h:t) = 1 + (contaCol t)

contaLinha :: [[a]] -> Int
contaLinha [] = 0
contaLinha (h:t) = if contaCol h == 0 then contaLinha t else 1 + (contaLinha t)

-- | Verifica se a posição pertence à matriz.

ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) [] = False
ePosicaoMatrizValida (x,y) l | x < 0 || y < 0 = False
                             | x > fst (dimensaoMatriz l) || y > snd (dimensaoMatriz l) = False
                             |otherwise = True

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz (x,y) [] = False
eBordaMatriz (x,y) m  | x==0 = True 
                      | y==0 = True
                      | x==(fst (dimensaoMatriz m))-1 = True    
                      | y==(snd (dimensaoMatriz m))-1 = True
                      | otherwise = False    

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz a | a == I = [[False,True,False,False],[False,True,False,False],[False,True,False,False],[False,True,False,False]]
                      | a == J = [[False,True,False],[False,True,False],[True,True,False]]
                      | a == L = [[False,True,False],[False,True,False],[False,True,True]]
                      | a == O = [[True,True],[True,True]]
                      | a == S = [[False,True,True],[True,True,False],[False,False,False]]
                      | a == T = [[False,False,False],[True,True,True],[False,True,False]]
                      | a == Z = [[True,True,False],[False,True,True],[False,False,False]]
-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista a [x] = x
encontraIndiceLista a (h:t) = if a == 0 then h else encontraIndiceLista (a-1) t

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista a e []    = []
atualizaIndiceLista a e (h:t) = if eIndiceListaValido a (h:t) == False then (h:t) else if a==0 then (e:t) else h:(atualizaIndiceLista (a-1) e t)
-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>


rodaMatriz :: Matriz a -> Matriz a
rodaMatriz [] = []
rodaMatriz ([]:_) = []
rodaMatriz l = (reverse (map head l)) : rodaMatriz (map tail l)

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH []    = [] 
inverteMatrizH (h:t) = reverseLista h : inverteMatrizH t

-- | Inverte uma Lista
reverseLista :: [a] -> [a]
reverseLista [] = []
reverseLista (h:t) = reverseLista t ++ [h]


-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV []    = []
inverteMatrizV (h:t) = inverteMatrizV t ++ [h]

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (x,y) e | x==0 = []
                   | x/=0 = criaMatriz (x-1,y) e ++ [criaAux (x-1,y) e]

-- | Cria uma linha com o mesmo elemento
criaAux :: Dimensao -> a -> [a]
criaAux (x,y) e | y==0 = []
                | y/=0 = criaAux (x,y-1) e ++ [e]

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoLista :: Posicao -> [a] -> a 
encontraPosicaoLista (x,y) (h:t) = if y==0 then h else encontraPosicaoLista (x,y-1) t

encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (x,y) [[a]] = a
encontraPosicaoMatriz (x,y) (h:t) = if x==0 then encontraPosicaoLista (x,y) h else encontraPosicaoMatriz (x-1,y) t 

-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.

atualLista :: Int -> a -> [a] -> [a]
atualLista x a [] = []
atualLista 0 a (h:t) = (a:t)
atualLista x a (h:t) = [h] ++ (atualLista (x-1) a t)

atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (x,y) a [[]] = [[]]
atualizaPosicaoMatriz (0,y) a (h:t) = atualLista y a h : t
atualizaPosicaoMatriz (x,y) a (h:t) = h : atualizaPosicaoMatriz (x-1,y) a t