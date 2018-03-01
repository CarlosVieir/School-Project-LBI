module Main where


import Graphics.Gloss
import Graphics.Gloss.Data.Picture          -- importar o tipo Picture
import Graphics.Gloss.Interface.Pure.Game   -- importar o tipo Event
import System.Random

import LI11718
import Mapas

type Estado = (Mapa,Bool,Bool,Picture,Picture,Int,Float)


{-- 1 - Ecra intro; 2 - ecra numero de jogadores; 3 - Picture intro; 4 - Picture numero de jogadores; 5 - Numeor de jogadoes
6 - Time  
--}

estadoInicial :: Picture -> Picture -> Int-> Estado -- Estado Inicial do Jogo
estadoInicial intro numjo n = ((Mapa ((0,0),Este) []),True,False,intro,numjo,n,0)

escolheNumber :: Picture -> Picture -> Int -> Estado -- Ecolher a quantidade de jogadores
escolheNumber intro numjo n = ((Mapa ((0,0),Este) []),False,True,intro,numjo,n,0)

desenhaMapa :: Mapa -> Picture
desenhaMapa (Mapa ((x,y),ori) tab) = Blank


desenhaEstado :: Estado -> Picture --Desenha no ecra o estado do jogo
desenhaEstado (mapa,True,False,intro,numjo,n,tmp) = intro
desenhaEstado (mapa,False,True,intro,numjo,n,tmp) = numjo
desenhaEstado (mapa,False,False,intro,numjo,n,tmp) = Blank
--desenhaEstado (False,Faintro,numjo,n,tmp) = Blank 

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (Char _) Down _ _) (mapa,True,False,intro,numjo,n,tmp) = escolheNumber intro numjo n
reageEvento (EventKey (SpecialKey _) Down _ _) (mapa,True,boll,intro,numjo,n,tmp) = escolheNumber intro numjo n
reageEvento (EventKey (Char '1') Down _ _) (mapa,False,True,intro,numjo,n,tmp) = escolheNumber intro numjo 1
reageEvento (EventKey (Char '2') Down _ _) (mapa,False,True,intro,numjo,n,tmp) = escolheNumber intro numjo 2
reageEvento (EventKey (Char '3') Down _ _) (mapa,False,True,intro,numjo,n,tmp) = escolheNumber intro numjo 3
reageEvento (EventKey (Char '4') Down _ _) (mapa,False,True,intro,numjo,n,tmp) = escolheNumber intro numjo 4
reageEvento e (mapa,bool,bool2,intro,numjo,n,tmp) = (mapa,bool,bool2,intro,numjo,n,tmp)

reageTempo :: Float -> Estado -> Estado
reageTempo fr (mapa,False,False,intro,numjo,n,tmp) = (mapa,False,False,intro,numjo,n,tmp + fr)
reageTempo fr e = e


--------
window :: Display
window = InWindow "Game" (800,550) (350,50)

background :: Color
background = greyN 0.5

fr :: Int 
fr = 50
--------

main :: IO ()
main = do intro <- loadBMP "./img/intro.bmp"
          numjo <- loadBMP "./img/number.bmp"
          n <- randomRIO (0,99)

          play window
               background
               fr
               (estadoInicial intro numjo n)
               desenhaEstado                 -- desenhaEstado
               reageEvento                   -- modifica estado de jogo
               reageTempo                    -- reageTempo



