module Lib where
import Text.Show.Functions

type Sabor = String

data Postre= UnPostre{
    nombrePostre :: String,
    sabores :: [Sabor],
    peso :: Float,
    temperatura :: Float
}deriving Show

bizcochoBorracho ::Postre
bizcochoBorracho = UnPostre "Bizcocho borracho de fruta y crema" ["bizocho","crema","fruta"] 100 25 

tartaMelaza :: Postre
tartaMelaza = UnPostre "Tarta de Melaza" ["melaza"] 50 0

type Hechizo = Postre -> Postre

incendio :: Hechizo
incendio postre = postre{temperatura=temperatura postre -1 , peso = peso postre - (peso postre) * 0.05}

immobulus :: Hechizo
immobulus postre = postre{temperatura=0}

wingardiumLeviosa :: Hechizo
wingardiumLeviosa postre = postre{sabores = sabores postre ++ ["concentrado"], peso= peso postre - (peso postre) * 0.1}

diffindo :: Float -> Hechizo
diffindo porcentaje postre = postre{peso= peso postre - (peso postre) * porcentaje} 

riddikulus :: Sabor -> Hechizo
riddikulus sabor postre = postre{sabores=  (sabores postre ++ [reverse sabor])}

avadaKedavra:: Hechizo
avadaKedavra postre=immobulus postre{sabores=[]}

saberPostreListo :: [Postre]-> Hechizo -> Bool
saberPostreListo postres hechizo = length (filter(\postre-> peso(hechizo postre) == 0  || temperatura (hechizo postre) <=0)postres) ==0

saberPromedioPostresListos :: [Postre] -> Float
saberPromedioPostresListos postres
    |  saberPostreListo postres (diffindo 0) = foldl (\p postre -> p + peso postre) 0.0 postres / fromIntegral (length postres)
    |  otherwise = 0

data Mago = UnMago{
    nombreMAgo::String,
    hechizosAprendidos ::[Hechizo],
    cantidadHorocruxes :: Int
} deriving Show

unMago::Mago
unMago = UnMago "Mago" [] 0

asistirDefensa :: Hechizo -> Postre -> Mago -> Mago
asistirDefensa hechizo postre mago
    | sabores (hechizo postre) == sabores (avadaKedavra postre) = sumarHechizo hechizo mago {cantidadHorocruxes=cantidadHorocruxes mago +1}
    | otherwise = sumarHechizo hechizo mago

sumarHechizo :: Hechizo ->Mago->Mago
sumarHechizo hechizo mago = mago {hechizosAprendidos=hechizosAprendidos mago ++ [hechizo]}

--mejorHechizo :: Postre ->Mago -> Hechizo
--mejorHechizo postre mago= head(filter(\hechizo-> ) hechizosAprendidos mago)

--mejorHechizoActual :: Hechizo ->[Hechizo] -> postre -> hechizo


listaPostres :: [Postre]
listaPostres= bizcochoBorracho : tartaMelaza :listaPostres

listaMagos :: [Mago]
listaMagos = unMago :listaMagos


    