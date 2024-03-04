{-|
Module      : Tarefa5_2018li1g035
Description : Módulo da Tarefa 5 para LI1 18/19

Módulo para a realização da Tarefa 5 de LI1 em 2018/19.
-}
module Main where

import LI11819
import Tarefa2_2018li1g035
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy
import GHC.Float

-- * Relatório
--
-- * Introdução
--
-- $int
-- Aqui iremos explicar o funcionamento e o raciocínio por de trás da Tarefa 5, observando as principais funções utilizadas e os seus outputs. 
--O principal objetivo desta Tarefa é criar uma janela onde será possível observar o decorrer do Jogo e alterar o Estado do Jogo. O Principal desafio desta 
-- Tarefa foi conseguir alinhar as várias Pictures por nos definidas nos seus respetivos referenciais ( neste caso : Posicao e PosicaoGrelha).

-- * Objetivos
--
-- $obj
-- Como referido anteriormente, o objetivo desta Tarefa é Obter uma imagem que represente o Estado atual do Jogo, representando no ecra todos os Disparos,
--Pecas, Jogadores, os seus pontos de vida e munições e todas as interações póssiveis entre estes, tentandos, igualmente, manter o aspeto do Jogo o mais
--interessante e atrativo possível.


-- * Procedimento
--
-- $prod
--  Para atingir o objetivo, criamos um type novo que contem a informação referente ao Estado do Jogo:
-- EstadoGloss :: ([Posicao],[Posicao],[Posicao],[Jogador],[Picture],[Disparo],Float,Mapa) Sendo o seu conteudo uma Lista que contém as Posicoes de todas
--as Pecas Vazias, uma Lista com as Posicoes de todas as Pecas Destrutivel, uma Lista com as Posicoes de todas as Pecas Indestrutivel, a Lista dos Jogadores,
--as Pictures utilizadas para representar o Estado, a lista de Disparo, o tempo do Jogo, e o Mapa que está a ser representado atualmente.
--
-- > 1) Desenhar o Mapa
--
-- Para desenhar o Mapa, foram usadas funções que, a partir do EstadoGloss, desenhassem cada tipo de Peca na sua Posicao. Assim, criamos funções (tiraVazM,
--tiraIndM e tiraDesM) que recebendo o mapa construissem as listas que iriam estar presentes no EstadoGloss
--
-- De seguida, usamos funções que colocam um tipo de Peca (vazias , dests, indests), que recebiam a lista de Pecas do EstadoGloss correspondente, e que a partir 
--da sua Posicao iriam posicionar a Peca na localização correta, utilizando a placePos. Por exemplo a função vazias l d0 p iria percorrer toda a lista l, que 
--contem todas as Posicoes do Mapa onde existem Pecas Vazias, e coloca-la-ia através de chamadas recursivas da função placePos.
-- Cada imagem utilizada tem um tamanho de 50x50, sendo que o tamanho destas no Display varia conforme a dimensão do Mapa
--
-- > 2) Desenhar os Jogadores
--
-- Após ter o Mapa na Posicao correta, havia a necessidade de colocar os Jogadores neste. Para isso, utilizamos a translateJog, que recebe um Jogadore e o coloca
--no seu respetivo lugar. Uma vez que a imagem do tank é duas vezes maior do que a das Pecas, e que a Posicao dos Jogadores é do tipo PosicaoGrelha, foram feitos
--os devidos ajustes para que, em conjunto com a Picture do Mapa completo, os tanks estivessem alinhados e com o mesmo tamanho das restantes Pecas. Depois, foi 
--utilizada a funcao desenhaTanks para colocar os Jogadores, chamando recursivamente a função anterior para a lista de Jogadores.
-- 
-- Com as Pictures do Mapa e dos Jogadores fui-nos possível cria a primeira função utilizada pelo main, desenhaEstado, que a partir das Pictures criadas pela
--desenhaTanks e pela desenhaMapa origina uma Picture, juntando as duas.
--
-- > 3) Movimentar Jogadores
--
-- Uma vez que esta Tarefa tem como objetivo criar uma imagem não estática, há necessidáde de movimentar os tanks de cada Jogador e de alterar a sua Direcao.
-- Para isso foi utilizada a função reageEvento, que após ser pressionada uma certa tecla, altera o Estado do Jogo. Para tal, foi necessário implementar funções
--da Tarefa 2, para averiguar se o movimento efetuado pelo Jogador é ou não válido.
--
-- > 4) Criação da main
--
-- Para que o Estado fosse representado, fui ncessário criar a função main, que para além da reageEvento e desenhaEstado, utiliza também outras funções.
-- É nesta função que se dá o load do ficheiros .bmp utilizados para desenhar o Mapa e representar os Jogadores. Defenimos o tamanho da janela, e criamos o estado
--inicial do Jogo:
--    estadoInicial p = ( tiraVazM m (0,0) , tiraDesM m (0,0) , tiraIndM m (0,0) ,  [(Jogador (19,11) C 6 3 3),(Jogador (1,1) B 6 3 3)] , p , [] , 0 , m )
--  Sendo p a lista de Pictures que este recebe na main e m um exemplo de Mapa por nós defenido.
--

-- * Conclusão
--
-- $conc
-- Concluindo, conseguimos representar os tank e o mapa de forma correta, contudo, não conseguimos utilizar a parte da Tarefa4 referente aos Disparos para 
--completar a função reageTempoGloss. Queriamos também representar os valores referentes às vida e municoes de cada Jogador, contudo, não o conseguimos executar.



-- | Função principal da Tarefa 5, que invoca o Jogo
main :: IO()
main = do
        vaz <- loadJuicy "vaz.bmp"
        des <- loadJuicy "des.bmp"
        ind <- loadJuicy "ind.bmp"
        t0  <- loadJuicy "tank0.bmp"
        t1  <- loadJuicy "tank1.bmp"
        play window
             (greyN 0.75)
             fr
             (estadoInicial ( toPicture [vaz,des,ind,t0,t1] ))
             desenhaEstado
             reageEvento
             reageTempoGloss

-- | Estado utilizado para trabalhar com o Gloss
type EstadoGloss = ([Posicao],[Posicao],[Posicao],[Jogador],[Picture],[Disparo],Float,Mapa)
---(Vazia, Bloco Destrutivel, Bloco Indestrutivel, Jogador, Picture,Disparos, tempo)


-- | Define as propriedades do Display
window :: Display
window = InWindow "Tanks" 
                  (1000, 600)
                  (200,50) 

-- | Frame rate a que o jogo corre              
fr :: Int 
fr = 50

-- | Estado inicial do jogo
estadoInicial :: [Picture] -> EstadoGloss
estadoInicial p = ( tiraVazM m (0,0) , tiraDesM m (0,0) , tiraIndM m (0,0) ,  [(Jogador (19,11) C 6 3 3),(Jogador (1,1) B 6 3 3)] , p , [] , 0 , m )





-- * Funções para Desenhar o EstadoGloss

-- | Função que desenha o Estado do Jogo
desenhaEstado :: EstadoGloss -> Picture
desenhaEstado e = Pictures [ desenhaMapa e , desenhaTanks e ]


-- | Cria uma lista das imagens a partir da lista de posições das Pecas Vazias do mapa
vazias :: [Posicao] -> Dimensao -> Picture -> [Picture] 
vazias [] d p = []
vazias (h:t) d vaz  = (placePos h d vaz) : (vazias t d vaz)

-- | Cria uma lista das imagens a partir da lista de posições das Pecas Bloco Destrutivel do mapa
dests :: [Posicao] -> Dimensao -> Picture -> [Picture]
dests [] d p = []
dests (h:t) d des = (placePos h d des) : (dests t d des)


-- | Cria uma lista das imagens a partir da lista de posições das Pecas Bloco Indestrutivel do mapa
indests :: [Posicao] -> Dimensao -> Picture -> [Picture]
indests [] d p = []
indests (h:t) d ind = (placePos h d ind) : (indests t d ind)


-- | Cria uma Picture que junta os 3 tipos de Pecas
desenhaMapa :: EstadoGloss -> Picture
desenhaMapa (v,d,i,j,[vaz,des,ind,t0,t1],l,n,m) = Pictures ( (vazias v d0 vaz) ++ (dests d d0 des) ++ (indests i d0 ind) ) 


-- | Cria a Picture associada aos Tanks
desenhaTanks :: EstadoGloss  -> Picture
desenhaTanks (v,d,i, [j0,j1], [vaz,des,ind,t0,t1] ,l , n ,m ) = Pictures  [ ( translateJog j0 t0 ) , ( translateJog j1 t1 ) ]  



-- | Coloca o Jogador na Posicao e roda a Picture conforme a Direcao do Jogador
translateJog :: Jogador -> Picture -> Picture
translateJog (Jogador (x,y) a b c d) p | a == C = Translate  ( (fromIntegral y)*(mapSide d0)-350 + ((mapSide d0) / 2) ) (250 - (fromIntegral x)*(mapSide d0) - ((mapSide d0)/2) ) (resized p d0)
                                       | a == B = Translate  ( (fromIntegral y)*(mapSide d0)-350 + ((mapSide d0) / 2) ) (250 - (fromIntegral x)*(mapSide d0) - ((mapSide d0)/2) ) ((resized (Rotate 180 p) d0 ) )
                                       | a == E = Translate  ( (fromIntegral y)*(mapSide d0)-350 + ((mapSide d0) / 2) ) (250 - (fromIntegral x)*(mapSide d0) - ((mapSide d0)/2) ) ((resized (Rotate 270 p) d0 ) )
                                       | a == D = Translate  ( (fromIntegral y)*(mapSide d0)-350 + ((mapSide d0) / 2) ) (250 - (fromIntegral x)*(mapSide d0) - ((mapSide d0)/2) ) ((resized (Rotate 90  p) d0 ) )

-- * Funções para Alterar o EstadoGloss

-- | Função que altera o Estado do Jogo quando acontece um Event
reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, ((moveJC j0 (j1:t) l m):j1:t) ,p ,l ,n ,m )
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, ((moveJB j0 (j1:t) l m):j1:t) ,p ,l ,n ,m )
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, ((moveJE j0 (j1:t) l m):j1:t) ,p ,l ,n ,m )
reageEvento (EventKey (SpecialKey KeyRight) Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, ((moveJD j0 (j1:t) l m):j1:t) ,p ,l ,n ,m ) 
reageEvento (EventKey (Char 'w')            Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, (j0:(moveJC j1 (j0:t) l m):t) ,p ,l ,n ,m )  
reageEvento (EventKey (Char 's')            Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, (j0:(moveJB j1 (j0:t) l m):t) ,p ,l ,n ,m )
reageEvento (EventKey (Char 'a')            Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, (j0:(moveJE j1 (j0:t) l m):t) ,p ,l ,n ,m )
reageEvento (EventKey (Char 'd')            Down _ _)  ( v,d,u, (j0:j1:t) ,p ,l , n ,m ) = ( v,d,u, (j0:(moveJD j1 (j0:t) l m):t) ,p ,l ,n ,m )
reageEvento _ s = s -- ignora qualquer outro evento


-- | Verifica se pode ou nao mover-se na Direcao C
moveJC :: Jogador -> [Jogador] -> [Disparo] -> Mapa -> Jogador
moveJC (Jogador (x,y) d v l c) jogs dis m = if comparaPosicaocomMatriz (x,y) (posicaoToListadePosicoesChoque2 (jogs)) == True && verificaMapa m (Jogador (x,y) d v l c) (Movimenta C) == True && verificaPosicoes (posicoesOcupadas (Jogador (x,y) d v l c)) (posicoesJogadores (jogs)) == True then moveJogC (Jogador (x,y) d v l c) else (Jogador (x,y) C v l c)

-- | Verifica se pode ou nao mover-se na Direcao B
moveJB :: Jogador -> [Jogador] -> [Disparo] -> Mapa -> Jogador
moveJB (Jogador (x,y) d v l c) jogs dis m = if comparaPosicaocomMatriz (x,y) (posicaoToListadePosicoesChoque2 (jogs)) == True && verificaMapa m (Jogador (x,y) d v l c) (Movimenta B) == True && verificaPosicoes (posicoesOcupadas (Jogador (x,y) d v l c)) (posicoesJogadores (jogs)) == True then moveJogB (Jogador (x,y) d v l c) else (Jogador (x,y) B v l c)

-- | Verifica se pode ou nao mover-se na Direcao E
moveJE :: Jogador -> [Jogador] -> [Disparo] -> Mapa -> Jogador
moveJE (Jogador (x,y) d v l c) jogs dis m = if comparaPosicaocomMatriz (x,y) (posicaoToListadePosicoesChoque2 (jogs)) == True && verificaMapa m (Jogador (x,y) d v l c) (Movimenta E) == True && verificaPosicoes (posicoesOcupadas (Jogador (x,y) d v l c)) (posicoesJogadores (jogs)) == True then moveJogE (Jogador (x,y) d v l c) else (Jogador (x,y) E v l c)

-- | Verifica se pode ou nao mover-se na Direcao D
moveJD :: Jogador -> [Jogador] -> [Disparo] -> Mapa -> Jogador
moveJD (Jogador (x,y) d v l c) jogs dis m = if comparaPosicaocomMatriz (x,y) (posicaoToListadePosicoesChoque2 (jogs)) == True && verificaMapa m (Jogador (x,y) d v l c) (Movimenta D) == True && verificaPosicoes (posicoesOcupadas (Jogador (x,y) d v l c)) (posicoesJogadores (jogs)) == True then moveJogD (Jogador (x,y) d v l c) else (Jogador (x,y) D v l c)


-- | Altera a Posicao do Jogador ou muda a Direcao para Cima, se necessário
moveJogC :: Jogador -> Jogador
moveJogC (Jogador (x,y) d v l a )  = if d == C then Jogador ( (x-1),y ) C v l a else Jogador (x,y) C v l a 


-- | Altera a Posicao do Jogador ou muda a Direcao para Baixo, se necessário
moveJogB :: Jogador ->  Jogador
moveJogB (Jogador (x,y) d v l a )  = if d == B then Jogador ( (x+1),y ) B v l a else Jogador (x,y) B v l a 


-- | Altera a Posicao do Jogador ou muda a Direcao para Esquerda, se necessário
moveJogE :: Jogador ->  Jogador
moveJogE (Jogador (x,y) d v l a )  = if d == E then Jogador ( x,(y-1) ) E v l a else Jogador (x,y) E v l a 


-- | Altera a Posicao do Jogador ou muda a Direcao para Direita, se necessário
moveJogD :: Jogador ->  Jogador
moveJogD (Jogador (x,y) d v l a )  = if d == D then Jogador ( x,(y+1) ) D v l a else Jogador (x,y) D v l a 


-- | Função que altera o Estado do jogo conforme a passagem do tempo
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (v,d,i,j,p,f,n,m) = (v,d,i,j,p,f,(n+1),m)


-- * Funções que retiram as Pecas do Mapa

-- | Funçao que coloca uma Picture na Posicao certa
placePos :: Posicao -> Dimensao -> Picture -> Picture
placePos (x,y) d p = Translate ( ( (fromIntegral y)* dim )- 350  ) (250 -  (fromIntegral x)*dim  ) (resized p d)
                  where dim = mapSide d


-- | Cria uma lista de Posicao das Pecas Vazias de uma linha 
tiraVazL :: [Peca] -> Posicao -> [Posicao]
tiraVazL [] p = []
tiraVazL (h:t) (x,y) | h == Vazia = (x,y) : ( tiraVazL t (x,(y+1)) )
                     | otherwise  = tiraVazL t (x,(y+1))


-- | Cria uma lista de Posicao das Pecas Vazias de um Mapa
tiraVazM :: Mapa -> Posicao -> [Posicao]
tiraVazM [] p = []
tiraVazM (h:t) (x,y) = ( tiraVazL h (x,y) ) ++ ( tiraVazM t ((x+1),y) )


-- | Cria uma lista de Posicao das Pecas Bloco Destrutivel de uma linha 
tiraDesL :: [Peca] -> Posicao -> [Posicao]
tiraDesL [] p = []
tiraDesL (h:t) (x,y) | h == Bloco Destrutivel = (x,y) : ( tiraDesL t (x,(y+1)) )
                     | otherwise = tiraDesL t (x,(y+1))


-- | Cria uma lista de Posicao das Pecas Bloco Destrutivel de um Mapa 
tiraDesM :: Mapa -> Posicao -> [Posicao]
tiraDesM [] p = []
tiraDesM (h:t) (x,y) = ( tiraDesL h (x,y) ) ++ ( tiraDesM t ((x+1),y) )


-- | Cria uma lista de Posicao das Pecas Bloco Indestrutivel de uma linha 
tiraIndL :: [Peca] -> Posicao -> [Posicao]
tiraIndL [] p = []
tiraIndL (h:t) (x,y) | h == Bloco Indestrutivel = (x,y) : ( tiraIndL t (x,(y+1)) )
                     | otherwise = tiraIndL t (x,(y+1))

-- | Cria uma lista de Posicao das Pecas Bloco Indestrutivel de um Mapa
tiraIndM :: Mapa -> Posicao -> [Posicao]
tiraIndM [] p = []
tiraIndM (h:t) (x,y) = ( tiraIndL h (x,y) ) ++ ( tiraIndM t ((x+1),y) )



-- * Funções Auxiliares

-- | Dada uma lista de Maybe Picture, transforma na lista de Picture correspondente
toPicture :: [Maybe Picture] -> [Picture]
toPicture [] = []
toPicture ((Just p):ps) = p:(toPicture ps)
toPicture ((Nothing):ps) = toPicture ps



-- * Funções para alterar o tamanho das Pictures

-- | Calcula o maior valor da Dimensao
ladoMaior :: Dimensao -> Int
ladoMaior (x,y) =   max x y


-- | Calcula o tamanho de cada Peca atravez da Dimensao do Mapa
mapSide :: Dimensao -> Float
mapSide d = ( 500 / (fromIntegral (ladoMaior d)) )


-- | Calcula a razao a partir das Pecas ( que têm 50x50 de tamanho)
r :: Dimensao -> Float
r d  = ( mapSide d ) / 50


-- | Altera o tamanho da Picture
resized :: Picture -> Dimensao -> Picture
resized p d = Scale (r d) (r d) p

-- * Exemplos

-- | Exemplo de Mapa
m :: Mapa 
m = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
      Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
      Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,
      Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,
      Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,
      Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
      [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
      Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,
      Bloco Indestrutivel,Bloco Indestrutivel]]

-- | Dimensao do Mapa exemplo
d0 :: Posicao
d0 = (14,22)

