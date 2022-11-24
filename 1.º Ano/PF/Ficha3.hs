import Ficha1 
-- Ficha 3 --
--Exercicio 1--

type Etapa = (Hour,Hour)
type Viagem = [Etapa]

etapaVal :: Etapa -> Bool 
etapaVal (x,y) | (hvalid x) && (hvalid y) = befORaft x y
               | otherwise = False  

viagemVal :: Viagem -> Bool
viagemVal [] = False
viagemVal ((x1,y1):(x2,y2):xs) | etapaVal (x1,y1) && etapaVal (x2,y2) && etapaVal (y1,x2) = True
                               | otherwise = False

-- Exercicio 2 --
type Poligonal = [Ponto]

--triangula :: Poligonal -> [Figura]

--areaPolig :: Poligonal -> Double
--areaPolig l = somaTriang (triangula l) 
--  where somaTriang :: [Figura] -> Double
--        somaTriang [] = 0
--        somaTriang (f:fs) = area f + area fs 

-- Exercicio 3 --
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
            deriving (Show,Eq)

type Nome = String
type Agenda = [(Nome, [Contacto])]

ag1 :: Agenda 
ag1 = [("Ana", [Email "ana@gmail.com", Tlm 914207690, Casa 25327769]), 
       ("Pedro", [Tlm 930427276]),
       ("Rui", [Trab 220011441, Email "rui11@uminho.pt"]),
       ("Ines", []),
       ("Nunu", [Tlm 932888444, Tlm 923444888])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n s [] = [(n, [Email s])]
acrescEmail n s ((x,l):t) | n == x = if (Email s) `elem` l then ((x,l):t) else (x,(Email s) : l) : t
                          | otherwise = (x,l) : (acrescEmail n s t)

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails n [] = Nothing
verEmails n ((x,l):t) | n == x = Just (daEmails l)
                      | otherwise = verEmails n t 

daEmails :: [Contacto] -> [String]
daEmails [] = []
daEmails (Email s:t) = s : daEmails t 
daEmails ( _ : t) = daEmails t  

-- Exercicio 4 --

-- Exercicio 5 --
data Movimento = Credito Float | Debito Float
               deriving Show

data Data = D Int Int Int -- Dia Mês Ano
          deriving Show

data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

ext1 :: Extracto
ext1 = Ext 1000 [(D 24 6 2020, "Pagamento qualquer", Debito 250),
                 (D 1 1 2021, "Borracheira de Ano Novo", Debito 320),
                 (D 27 2 2021, "Novo emprego", Credito 1600),
                 (D 2 3 2021, "Contas EDP", Debito 105),
                 (D 27 3 2021, "Paycheck2", Credito 1650),
                 (D 15 4 2021, "Ferias", Debito 569)]

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ m) x = daMovi m x 

daMovi :: [(Data, String, Movimento)] -> Float -> [Movimento] 
daMovi [] _ = []
daMovi ((_,_,Credito y):t) x = if y >= x then (Credito y) : daMovi t x
                                         else daMovi t x
daMovi ((_,_,Debito y):t) x = if y >= x then (Debito y) : daMovi t x
                                        else daMovi t x


filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ m) l = auxF m l 

auxF :: [(Data, String, Movimento)] -> [String] -> [(Data,Movimento)]
auxF [] _ = []
auxF ((a,b,c):t) l = if b `elem` l then ((a,c) : auxF t l) 
                                   else auxF t l


creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ m) = cdAux m

cdAux :: [(Data, String, Movimento)] -> (Float,Float)
cdAux [] = (0,0)
cdAux ((_,_,Credito x):t) = (a+x,b)
   where (a,b) = cdAux t 
cdAux ((_,_,Debito x):t) = (a,b+x)
   where (a,b) = cdAux t 

cdAux' :: [(Data, String, Movimento)] -> (Float,Float)
cdAux' ((_,_,m):t) = case m of 
                   Credito x -> (a+x,b)
                   Debito x -> (a,b+x)
   where (a,b) = cdAux' t 


saldo :: Extracto -> Float
saldo (Ext s m) = let (c,d) = creDeb (Ext s m)
                  in s + c - d 








































