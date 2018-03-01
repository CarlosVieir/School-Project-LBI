module Tarefa2_2017li1g14 where

import LI11718


testesT2 :: [Tabuleiro]
testesT2 = []

validaLinha :: Tabuleiro -> Bool
validaLinha [] = True
validaLinha (h:t) | head h == Peca Lava 0 = validaLinha (tail h:t)
                  | otherwise = False

validaColuna :: Tabuleiro -> Bool
validaColuna [] = True
validaColuna (h:t) | (head h == Peca Lava 0) && (last h == Peca Lava 0) = validaColuna t
                   | otherwise = False 


validaLC :: Tabuleiro -> Bool
validaLC t | validaLinha t == True && validaColuna t == True = True
           | otherwise = False



passo :: Peca -> (Posicao,Orientacao,Altura) -> Maybe (Posicao,Orientacao,Altura)
passo (Peca tipo a) ((x,xs),ori,alt) | (ori == Este) && (tipo == Curva Este) && (a == alt) = Just ((x,xs+1),Sul,a)
                                     | (ori == Este) && (tipo == Curva Sul) && (a == alt) = Just ((x,xs-1),Norte,a)
                                     | (ori == Este) && (tipo == Rampa Este) && (alt == (a + 1) || alt == (a -1)) = Just ((x+1,xs),Este,a)
                                     | (ori == Este) && (tipo == Recta) && (a == alt) = Just ((x+1,xs),Este,a)
                                     | (ori == Oeste) && (tipo == Curva Oeste) && (a == alt) = Just ((x,xs-1),Norte,alt)
                                     | (ori == Oeste) && (tipo == Curva Norte) && (a == alt) = Just ((x,xs+1),Sul,alt)
                                     | (ori == Oeste) && (tipo == Rampa Oeste) && (alt == (a + 1) || alt == (a -1)) = Just ((x-1,xs),Oeste,a)
                                     | (ori == Oeste) && (tipo == Recta) && (alt == a) = Just ((x-1,xs),Oeste,a)
                                     | (ori == Sul) && (tipo == Curva Oeste) && (alt == a) = Just ((x+1,xs),Este,a)
                                     | (ori == Sul) && (tipo == Curva Sul) && (alt == a) = Just ((x-1,xs),Oeste,a)
                                     | (ori == Sul) && (tipo == Rampa Sul) && (alt == (a + 1) || alt == (a -1)) = Just ((x,xs+1),Sul,a)
                                     | (ori == Sul) && (tipo == Recta) && (alt == a) = Just ((x,xs+1),Sul,a)
                                     | (ori == Norte) && (tipo == Curva Norte) && (alt == a) = Just ((x+1,xs),Este,a)
                                     | (ori == Norte) && (tipo == Curva Este) && (alt == a) = Just ((x-1,xs),Oeste,a)
                                     | (ori == Norte) && (tipo == Rampa Norte) && (alt == (a + 1) || alt == (a -1)) = Just ((x,xs-1),Norte,a)
                                     | (ori == Norte) && (tipo == Recta) && (alt == a) = Just ((x,xs-1),Norte,a)
  

passos :: Tabuleiro -> (Posicao,Orientacao,Altura) -> Maybe (Posicao,Orientacao,Altura) -> [Posicao] -> Maybe [Posicao]                                
passos t _ Nothing _ = Nothing
passos t (pi,oi,ai) (Just (pa,oa,aa)) ac | (pi == pa) && (oi == oa) && (ai == aa) = Just ac
                                         | otherwise = let p = devolvePeca t pa 
                                                        in passos t (pi,oi,ai) (passo p (pa,oa,aa)) ac                            
                                  

devolvePeca :: Tabuleiro -> Posicao -> Peca
devolvePeca (h:t) (x,0) = pecaColuna h x 
devolvePeca (h:t) (x,y) = devolvePeca t (x,y-1)

pecaColuna :: [Peca] -> Int -> Peca
pecaColuna (h:t) 0 = h
pecaColuna (h:t) x = pecaColuna t (x-1)


valida :: Mapa -> Bool
valida m = undefined
