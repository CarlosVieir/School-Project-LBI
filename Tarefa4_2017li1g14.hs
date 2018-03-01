{-|
Module      : Tarefa4_2017li1g14
Description : Módulo da Tarefa 4 para LI1 17/18

Módulo para a realização da Tarefa 4 de LI1 em 2017/18.
-}
module Tarefa4_2017li1g14 where

import LI11718

-- | Estado do jogo:
--
-- * Dimensões do mapa
-- * Coordenadas da bcarro no mapa
-- * Tempo passado desde o início do jogo

atrito :: Velocidade -> Tempo -> Double -> Velocidade
atrito (xi,yi) t k_atrito = ((xi*t*k_atrito),(yi*t*k_atrito))  

acelaracao :: Velocidade -> Tempo -> Double -> Velocidade
acelaracao (xi,yi) t k_acel = ((xi*t*k_acel),(yi*t*k_acel))

--------------------------------------
posi :: Tabuleiro -> (Double, Double) -> Peca
posi t (x,y) = devolvePeca t (floor x,floor y)

devolvePeca :: Tabuleiro -> Posicao -> Peca
devolvePeca (h:t) (x,0) = pecaColuna h x 
devolvePeca (h:t) (x,y) = devolvePeca t (x,y-1)

pecaColuna :: [Peca] -> Int -> Peca
pecaColuna (h:t) 0 = h
pecaColuna (h:t) x = pecaColuna t (x-1)

tp :: Peca -> Tipo
tp (Peca tipo _) = tipo

angulo :: Peca -> Double -> Velocidade
angulo (Peca (Rampa Norte) _) k_peso = (k_peso*cos(90),k_peso*sin(90))
angulo (Peca (Rampa Sul) _) k_peso = (k_peso*cos(270),k_peso*sin(270))
angulo (Peca (Rampa Este) _) k_peso = (k_peso*cos(180),k_peso*sin(180))
angulo (Peca (Rampa Oeste) _) k_peso = (k_peso*cos(0),k_peso*sin(0))
angulo _ _ = (0,0)

gravidade :: (Double,Double) -> Angulo -> Velocidade -> Tempo -> Double -> Tabuleiro -> Velocidade
gravidade (x,y) d (vx,vy) t k_peso tab | tp (posi tab (x,y)) == Rampa Norte && (d >= 0 || d <= 180) = (vx - (fst (angulo (posi tab (x,y)) k_peso)) * t, vy - (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Norte && (d > 180   || d < 360) = (vx + (fst (angulo (posi tab (x,y)) k_peso)) * t, vy + (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Sul && (d >= 180 || d <= 360) = (vx - (fst (angulo (posi tab (x,y)) k_peso)) * t, vy - (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Sul &&  (d >= 0 || d <= 180) = (vx + (fst (angulo (posi tab (x,y)) k_peso)) * t, vy + (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Este && (d >= 0 || d <= 90 || d >= 270 || d <=360) = (vx - (fst (angulo (posi tab (x,y)) k_peso)) * t, vy - (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Este && (d > 90 || d < 180) = (vx + (fst (angulo (posi tab (x,y)) k_peso)) * t, vy + (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Oeste && (d >= 90 || d <= 270) = (vx - (fst (angulo (posi tab (x,y)) k_peso)) * t, vy - (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | tp (posi tab (x,y)) == Rampa Oeste && ((d >= 0 || d <= 90 || d >= 270 || d <=360)) = (vx + (fst (angulo (posi tab (x,y)) k_peso)) * t, vy + (snd (angulo (posi tab (x,y)) k_peso)) * t)
                                       | otherwise = (vx,vy)


pneus :: Angulo -> Velocidade -> Tempo -> Double -> Velocidade
pneus angulo (xi,yi) t k_pneus = ((xi*t*(sin(angulo)*k_pneus)),(yi*t*(sin(angulo)*k_pneus)))

actuaVector :: Tempo -> Acao -> Jogo -> Velocidade
actuaVector temp ((Acao ace trav _ _ _)) (Jogo (Mapa po tab) (Propriedades k_atrito k_pneus k_acel k_peso k_nitro k_roda) [(Carro p d v)] _ _) | ace == True = ((fst (gravidade p d v temp k_peso tab)) - (fst (atrito v temp k_atrito)) + (fst (acelaracao v temp k_acel)) - (fst (pneus d v temp k_pneus)),(snd (gravidade p d v temp k_peso tab)) - (snd (atrito v temp k_atrito)) + (snd (acelaracao v temp k_acel)) - (snd (pneus d v temp k_pneus)))
                                                                                                                                               | trav == True = ((fst (gravidade p d v temp k_peso tab)) - (fst (atrito v temp k_atrito)) - (fst (acelaracao v temp k_acel)) - (fst (pneus d v temp k_pneus)),(snd (gravidade p d v temp k_peso tab)) - (snd (atrito v temp k_atrito)) - (snd (acelaracao v temp k_acel)) - (snd (pneus d v temp k_pneus)))


{-- Velocidade factualizada - (vi + grav - atri + (ace/tra) + pneu) 
--}
---------------------------------------------------------------------


atuadirecao :: Carro -> Tempo -> Acao -> Propriedades -> Carro
atuadirecao (Carro p dir vel) t (Acao _ _ esq dirt _) (Propriedades _ _ _ _ _ k_roda) | (esq == True) = (Carro p (dir * k_roda * t) vel)
                                                                                      | (dirt == True) = (Carro p (dir * k_roda * t) vel)
                                                                                  	  | otherwise = (Carro p dir vel)


nitro :: Carro -> Tempo -> Acao -> Propriedades -> Carro 
nitro (Carro p dir (xi,yi)) t (Acao _ _ _ _ nitro) (Propriedades _ _ _ _ k_nitro _) | (nitro == Just 1) = (Carro p dir ((xi*t*k_nitro),(yi*t*k_nitro))) 
																				                                            | otherwise = (Carro p dir (xi,yi)) 



{--
O testes a serem considerados pelo sistema de /feedback/
para a função 'atualiza'.--}

testesT4 :: [(Tempo,Jogo,Acao)]
testesT4 = []


{--
Função usada para atualizar o estado do jogo dadas as
ações de um jogador num determinado período de tempo.--}
--atualiza :: Tempo -> Jogo -> Int -> Acao -> Jogo

atualiza :: Tempo -- ^ a duração da ação
         -> Jogo  -- ^ o estado do jogo
         -> Int   -- ^ o identificador do jogador
         -> Acao  -- ^ a ação tomada pelo jogador
         -> Jogo  -- ^ o estado atualizado do jogo
{--
atualiza t e j a = undefined
--}
actualiza tmp (Jogo mapa pista [c1,c2,c3,c4] [n1,n2,n3,n4] [(x1,x2),(y1,y2),(k1,k2),(j1,j1)] 1 (Acao ace trav esq dir nit) = Jogo (mapa pista c1 n1 (x1,x2))
actualiza tmp (Jogo mapa pista [c1,c2,c3,c4] [n1,n2,n3,n4] [(x1,x2),(y1,y2),(k1,k2),(j1,j1)] 2 (Acao ace trav esq dir nit) = Jogo (mapa pista c1 n2 (y1,y2))
actualiza tmp (Jogo mapa pista [c1,c2,c3,c4] [n1,n2,n3,n4] [(x1,x2),(y1,y2),(k1,k2),(j1,j1)] 3 (Acao ace trav esq dir nit) = Jogo (mapa pista c1 n3 (k1,k2))
actualiza tmp (Jogo mapa pista [c1,c2,c3,c4] [n1,n2,n3,n4] [(x1,x2),(y1,y2),(k1,k2),(j1,j1)] 4 (Acao ace trav esq dir nit) = Jogo (mapa pista c1 n4 (j1,j2))
