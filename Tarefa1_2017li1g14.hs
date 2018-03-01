module Tarefa1_2017li1g14 where

import LI11718


testesT1 :: Caminho
testesT1 = [Avanca,CurvaDir,Sobe,CurvaDir,Avanca,CurvaDir,Desce,CurvaDir]


----------------------------------------------------------------------------------------------------------------------------
--Construcao do tabuleiro so com a Peca Lava 0
tabLava :: Dimensao -> Tabuleiro
tabLava (x,y) = replicate y (replicate x ((Peca Lava 0)))

--Cria lista de tuplos (Peca,Posicao no Tabuleiro)
listaPecas :: Caminho -> Posicao -> Orientacao -> Altura -> [(Peca,Posicao)]
listaPecas [] (x,y) o a = []
listaPecas (h:t) (x,y) o a | h == Avanca = [(Peca Recta a,(x,y))] ++ (listaPecas t (proxPosicao (x,y) o) (poxOrientacao o h) a)
                           | h == Sobe = [(Peca (Rampa o) a,(x,y))] ++ (listaPecas t (proxPosicao (x,y) o) (poxOrientacao o h) (a+1))
                           | h == Desce = [(Peca (Rampa o) (a-1),(x,y))] ++ (listaPecas t (proxPosicao (x,y) o) (poxOrientacao o h) (a-1))
                           | h == CurvaEsq = [(Peca (Curva o) a,(x,y))]++ (listaPecas t (proxPosicao (x,y) (poxOrientacao o h)) (poxOrientacao o h) a)
                           | h == CurvaDir = [(Peca (Curva o) a,(x,y))] ++ (listaPecas t (proxPosicao (x,y) (poxOrientacao o h)) (poxOrientacao o h) a)


                                                              
-- Recebe a Orientacao de uma Peca e devolve a Orientacao seguinte
poxOrientacao :: Orientacao -> Passo -> Orientacao
poxOrientacao orientacao passo | orientacao == Este && passo == CurvaDir = Sul
                               | orientacao == Este && passo == CurvaEsq = Norte
                               | orientacao == Oeste && passo == CurvaDir = Norte
                               | orientacao == Oeste && passo == CurvaEsq = Sul
                               | orientacao == Norte && passo == CurvaEsq = Oeste
                               | orientacao == Norte && passo == CurvaDir = Este
                               | orientacao == Sul && passo == CurvaDir = Oeste
                               | orientacao == Sul && passo == CurvaEsq = Este
                               | orientacao == Este && passo == Avanca = Este
                               | orientacao == Oeste && passo == Avanca = Oeste
                               | orientacao == Norte && passo == Avanca = Norte
                               | orientacao == Sul && passo == Avanca = Sul
                               | orientacao == Este && (passo == Sobe || passo == Desce) = Este 
                               | orientacao == Oeste && (passo == Sobe || passo == Desce) = Oeste
                               | orientacao == Norte && (passo == Sobe || passo == Desce) = Norte
                               | orientacao == Sul && (passo == Sobe || passo == Desce) = Sul


-- Recebe a Orientacao de uma Peca e devolve a Orientacao seguinte
proxPosicao :: Posicao -> Orientacao -> Posicao
proxPosicao (x,y) o | (o == Este) = (x+1,y)
                    | (o == Oeste) = (x-1,y)
                    | (o == Norte) = (x,y-1)
                    | (o == Sul) = (x,y+1) 

-- Substitui Peca na Posicao
updateMatrix :: Tabuleiro -> (Peca,Posicao) -> Tabuleiro
updateMatrix [] _ = []
updateMatrix (h:t) (peca,(c,0)) = updateMatrixLine h peca c : t
updateMatrix (h:t) (peca,(c,l)) = h : updateMatrix t (peca,(c,l-1))

updateMatrixLine :: [Peca] -> Peca -> Int -> [Peca]
updateMatrixLine [] _ _ = []
updateMatrixLine (h:t) peca 0 = peca : t
updateMatrixLine (h:t) peca x = h : updateMatrixLine t peca (x-1)

criaTab :: Tabuleiro -> [(Peca,Posicao)] -> Tabuleiro
criaTab [] _ = []
criaTab t [] = t
criaTab t ((peca,posicao):y) = criaTab (updateMatrix t (peca,posicao)) y


--Construir Tabuleiro Final-----------------
lista :: Caminho -> [(Peca,Posicao)]
lista c = listaPecas c (partida c) dirInit altInit

tabInit :: Caminho -> Tabuleiro
tabInit c = tabLava (dimensao c)

tabuleiro :: Caminho -> Tabuleiro 
tabuleiro c = criaTab (tabInit c) (lista c)

-------------------------------
--Conclusao da Tarefa 1
--Tarefa 1 
constroi :: Caminho -> Mapa
constroi c = Mapa ((partida c),dirInit) (tabuleiro c)


