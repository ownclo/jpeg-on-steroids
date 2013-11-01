----------------------------------------------
-- Auxiliary functions:
----------------------------------------------

infixr 9 `o`
o :: (c->d) -> (a->b->c) -> (a->b->d)
(g `o` f) x y = g (f x y)

ap     :: (a->b) -> a -> b
ap f x  = f x

ap'    :: a -> (a->b) -> b
ap' x f = f x

type Table a  =  Int -> a

subst :: Eq a => a -> b -> (a->b) -> (a->b)
subst i e t j  | i==j      =  e
               | otherwise =  t j

multi  :: Int -> [a] -> [a]
multi n = concat . map (copy n)

ceilDiv    :: Int -> Int -> Int
ceilDiv n d = (n+d-1)/d

----------------------------------------------
-- Matrix manipulation
----------------------------------------------

type Dim   = (Int,Int)
type Mat a = [[a]]

matapply    :: Num a  =>  Mat a -> [a] -> [a]
matapply m v = map (inprod v) m

inprod :: Num a  =>  [a] -> [a] -> a
inprod  = sum `o` zipWith (*)

matmap :: (a->b) -> Mat a -> Mat b
matmap  = map . map

matconcat :: Mat (Mat a) -> Mat a
matconcat  = concat . map (map concat . transpose)

matzip :: [Mat a] -> Mat [a]
matzip  = map transpose . transpose


----------------------------------------------
-- Bit Streams
----------------------------------------------

type Bits = [Bool]

byte2bits  :: Int -> Bits
byte2bits x = zipWith (>=) (map (rem x) powers) (tail powers)
      where powers = [256,128,64,32,16,8,4,2,1]

string2bits :: String -> Bits
string2bits  = concat . map (byte2bits.ord)

byte2nibs  :: Int -> (Int,Int)
byte2nibs x = (x/16, x`rem`16)


----------------------------------------------
-- Binary Trees
----------------------------------------------

data Tree a  =  Nil
             |  Tip a
             |  Bin (Tree a) (Tree a)

instance Functor Tree where
    map f Nil       =  Nil
    map f (Tip a)   =  Tip (f a)
    map f (Bin x y) =  Bin (map f x) (map f y)


----------------------------------------------
-- State Function (StFun) Monad
----------------------------------------------

data StFun s r = SF (s -> (r,s))

instance Functor (StFun s) where
    map h (SF f)    = SF g 
                where g s = (h x,s')
                       where (x,s') = f s 

instance Monad (StFun s) where
    result x        = SF g  
                where g s = (x,s)
    SF f `bind` sfh = SF g 
                where g s = h s'
                        where (x,s') = f s
                              SF h   = sfh x

st'apply :: StFun a b -> a -> b
st'apply (SF f) s   = x 
                where (x,_) = f s


----------------------------------------------
-- Primitive State Functions
----------------------------------------------

empty  ::  StFun [a] Bool
empty   =  SF f
    where  f [] = (True,  [])
           f xs = (False, xs)

item   ::  StFun [a] a
item    =  SF f
    where  f (x:xs) = (x,xs)

peekitem   ::  StFun [a] a
peekitem    =  SF f
        where  f ys@(x:xs) = (x, ys)

entropy :: StFun String String
entropy =  SF f
    where  f ys@('\xFF':'\x00':xs)  = let (as,bs) = f xs in ('\xFF':as,bs) 
           f ys@('\xFF': _       )  = ([],ys)
           f    ( x           :xs)  = let (as,bs) = f xs in (x:as,bs) 


----------------------------------------------
-- Auxiliary State Functions
----------------------------------------------

byte :: StFun String Int
byte  = [ ord c | c <- item ]

word :: StFun String Int
word  = [ a*256+b | a<-byte, b<-byte ]

nibbles :: StFun String (Int,Int)
nibbles  = [ byte2nibs a | a <- byte ]


----------------------------------------------
-- State Function Combinators
----------------------------------------------

-- list    ::            [StFun s r] -> StFun s [r]
list       :: Monad m => [m       a] -> m       [a]
list []     = result []
list (f:fs) = [ x:xs | x<-f, xs<-list fs]

exactly         :: Monad m => Int -> m a -> m [a]
exactly 0     f  = result []
exactly (n+1) f  = [ x:xs | x<-f, xs<-exactly n f ]

matrix      :: Monad m => Dim -> m a -> m (Mat a)
matrix (y,x) = exactly y . exactly x

many   :: Monad (StFun [a]) => StFun [a] b -> StFun [a] [b]
many f  = [ if b then [] else y:ys
          | b  <- empty
          , y  <- f
          , ys <- many f
          ]

sf'uncur  :: (b -> StFun a (b,c)) -> StFun (a,b) c
sf'uncur f = SF h
  where h (a,b) = (c, (a',b'))
            where SF g         = f b
                  ((b',c),a')  = g a

sf'curry       :: StFun (a,b) c -> b -> StFun a (b,c)
sf'curry (SF h) = f
          where f b = SF g
                 where g a = ((b',c),a') 
                        where (c,(a',b')) = h (a,b)


----------------------------------------------
-- Huffman Trees
----------------------------------------------

build :: Monad (StFun [(a,Int)]) => Int -> StFun [(a,Int)] (Tree a)
build n = [ if b then Nil else t
          | b     <- empty
          , (_,s) <- peekitem
          , t     <- if   n==s
                     then [Tip v   | (v,_) <- item]
                     else [Bin x y | x <- build (n+1), y <- build (n+1)]
          ]

{-
build :: Monad (StFun [(a,Int)]) => Int -> StFun [(a,Int)] (Tree a)
build n = [ res
          | b     <- empty
          , res   <- if b then result Nil else
                     [ t
                     |  (_,s) <- peekitem
                     , t     <- if   n==s
                                then [Tip v   | (v,_) <- item]
                                else [Bin x y | x <- build (n+1), y <- build (n+1)]
                     ]
          ]
-}


huffmanTree ::  Monad (StFun [(a,Int)]) => [[a]] -> Tree a
huffmanTree  =  st'apply (build 0) . concat . zipWith f [1..16]
         where  f s = map (\v->(v,s))


lookup              :: Tree a -> StFun Bits a
lookup (Tip x)       = result x
lookup (Bin lef rit) = [ x
                       | b <- item
                       , x <- lookup (if b then rit else lef)
                       ]

receive      :: Int -> StFun Bits Int
receive 0     = result 0
receive (k+1) = [ 2*n + (if b then 1 else 0)
                | n <- receive k
                , b <- item
                ]

dcdecode  :: Tree Int -> StFun Bits Int
dcdecode t = [ extend v s
             | s <- lookup t
             , v <- receive s
             ]

extend v t | t==0      =  0
           | v>=vt     =  v
           | otherwise =  v + 1 - 2*vt
                   where  vt = 2^(t-1)

acdecode :: Tree (Int,Int) -> Int -> StFun Bits [Int]
acdecode t k 
  = [ x
    | (r,s) <- lookup t
    , x  <- let  k' =  k + r + 1
            in   if   r==0&&s==0 
                 then [copy (64-k) 0]
                 else [ copy r 0 ++ (extend x s:xs)
                      | x <-  receive s
                      , xs <- if k'>=64 then [[]] else acdecode t k'
                      ]
    ]


----------------------------------------------
-- Discrete Cosine Transform
----------------------------------------------

idct1 :: [Float] -> [Float]
idct1  = matapply cosinuses

idct2 :: Mat Float -> Mat Float
idct2  = transpose . map idct1 . transpose . map idct1

cosinuses :: Mat Float
cosinuses  = map f [1,3..15]
     where f x = map g [0..7]
             where g 0 = 0.5 / sqrt 2.0
                   g u = 0.5 * cos(fromInteger(x*u)*(pi/16.0))


----------------------------------------------
-- Dequantization and Upsampling
----------------------------------------------

type QuaTab = [Int]

dequant :: QuaTab -> [Int] -> Mat Int
dequant  =  matmap truncate `o` idct2 `o` zigzag `o` map fromInteger `o` zipWith (*) 

upsamp      :: Dim -> Mat a -> Mat a
upsamp (1,1) = id
upsamp (x,y) = multi y . map (multi x)

zigzag xs = matmap (xs!!) [[ 0, 1, 5, 6,14,15,27,28]
                          ,[ 2, 4, 7,13,16,26,29,42]
                          ,[ 3, 8,12,17,25,30,41,43]
                          ,[ 9,11,18,24,31,40,44,53]
                          ,[10,19,23,32,39,45,52,54]
                          ,[20,22,33,38,46,51,55,60]
                          ,[21,34,37,47,50,56,59,61]
                          ,[35,36,48,49,57,58,62,63]
                          ]


-- alternative, cheaper in time but more expensive in memory:

zigzag' xs =  (transpose . map concat . transpose . fst . foldr f e) [1..15]
      where e = ([],reverse xs)
            f n (rss,xs) = (bs:rss, ys)
              where (as,ys) = splitAt (min n (16-n)) xs
                    rev = if even n then id else reverse
                    bs =    copy (max (n-8) 0) [] 
                         ++ map (:[]) (rev as) 
                         ++ copy (max (8-n) 0) []

----------------------------------------------
-- Data decoding
----------------------------------------------

type DataUnit =  Mat Int
type Picture  =  Mat [Int]

type DataSpec =  (Dim, QuaTab, Tree Int, Tree (Int,Int))
type MCUSpec  =  [(Dim, DataSpec)]

dataunit ::  DataSpec -> Int -> StFun Bits (Int,DataUnit)
dataunit (u,q,dc,ac) x = [ let y=x+dx 
                           in  (y,upsamp u (dequant q (y:xs))) 
                         | dx <- dcdecode dc
                         , xs <- acdecode ac 1
                         ]

units    :: Dim -> DataSpec -> StFun (Bits,Int) DataUnit
units dim = map matconcat . matrix dim . sf'uncur . dataunit

units'  :: (Dim,DataSpec) -> Int -> StFun Bits (Int,DataUnit)
units'   =  sf'curry . uncurry units

mcu     :: MCUSpec -> [ Int -> StFun Bits (Int,DataUnit) ]
mcu      = map units'

mcu'    :: MCUSpec -> [Int] -> [ StFun Bits (Int,DataUnit) ]
mcu'     = zipWith ap . mcu

mcu''   :: MCUSpec -> [Int] -> StFun Bits ([Int],[DataUnit])
mcu''    = map unzip `o` list `o` mcu'

mcu'''  :: MCUSpec -> StFun (Bits,[Int]) Picture
mcu'''   = map matzip . sf'uncur . mcu''

picture :: Dim -> MCUSpec -> StFun (Bits,[Int]) Picture
picture dim  = map matconcat . matrix dim . mcu'''

-- if you prefer one-liners over auxiliary definitions:

pict dim  =     map matconcat 
             .  matrix dim 
             .  map matzip 
             .  sf'uncur 
             .  map unzip
            `o` list
            `o` zipWith ap
             .  map (sf'curry . uncurry units)



----------------------------------------------
-- JPEG Header structure
----------------------------------------------

type FrameCompo = (Int,Dim,Int)
type ScanCompo  = (Int,Int,Int)
type QtabCompo  = (Int,[Int])

type SOF = (Dim,[FrameCompo])
type DHT = (Int,Int,Tree Int)
type SOS = ([ScanCompo],Bits)
type DQT = [QtabCompo]
type XXX = (Char,String)

frameCompo = [ (c,dim,tq) 
             | c <- byte, dim <- nibbles, tq <- byte
             ]

scanCompo  = [ (cs,td,ta)
             | cs <- byte, (td,ta) <- nibbles
             ]

qtabCompo  = [ (id,qt)
             | (p,id) <- nibbles
             , qt <- exactly 64 (if p==0 then byte else word)
             ]

sofSeg = [ ((y,x), fcs)
         | _ <- word, _ <- byte, y <- word, x <- word
         , n <- byte, fcs <- exactly n frameCompo
         ]
dhtSeg = [ (tc, th, huffmanTree v)
         | _ <- word, (tc,th) <- nibbles, ns <- exactly 16 byte
         , v <- list (map (flip exactly byte) ns)
         ]
dqtSeg = [ qts
         | len <- word
         , qts <- exactly ((len-2)`rem`64) qtabCompo
         ]
sosSeg = [ (scs, string2bits ent)
         | _ <- word, n <- byte, scs <- exactly n scanCompo
         , _ <- byte, _ <- byte, _   <- nibbles, ent <- entropy
         ]

segment :: (SOF->a, DHT->a, DQT->a, SOS->a, XXX->a) -> StFun String a
segment (sof,dht,dqt,sos,xxx) =
  [ s
  | _ <- item
  , c <- item
  , s <- case c of
         '\xC0' -> map sof sofSeg
         '\xC4' -> map dht dhtSeg
         '\xDB' -> map dqt dqtSeg
         '\xDA' -> map sos sosSeg
         '\xD8' -> [ xxx (c,[]) ]
         '\xD9' -> [ xxx (c,[]) ]
         _      -> [ xxx (c,xs) | n <- word, xs <- exactly (n-2) item ]
  ]

----------------------------------------------
-- JPEG Decoder
----------------------------------------------

type Huf   =  (Table(Tree Int), Table(Tree (Int,Int)))
type Sof   =  (Dim, Table(Dim,QuaTab))
type Qua   =  Table QuaTab
type State =  (Sof,Huf,Qua,Picture)

segments :: StFun String [State->State]
segments = many (segment (sof,dht,dqt,sos,xxx))
     where sof x s@(a,b,c,d) = (evalSOF x s, b, c, d)
           dht x s@(a,b,c,d) = (a, evalDHT x s, c, d)
           dqt x s@(a,b,c,d) = (a, b, evalDQT x s, d)
           sos x s@(a,b,c,d) = (a, b, c, evalSOS x s)
           xxx _ s           = s

errRes  :: State
errRes   = (error"SOF", error"DHT", error"DQT", error"SOS")

evalSOF :: SOF -> State -> Sof
evalSOF (dim,xs) (~(_,sof),_,qua,_)  =  (dim, foldr f sof xs)
                                 where  f (i,d,q) = subst i (d,qua q)

evalDHT :: DHT -> State -> Huf
evalDHT (0,i,tree) (_,~(hdc,hac),_,_) = (subst i tree hdc, hac)
evalDHT (1,i,tree) (_,~(hdc,hac),_,_) = (hdc, subst i (map byte2nibs tree) hac)

evalDQT :: DQT -> State -> Qua
evalDQT xs (_,_,qua,_) =  foldr f qua xs
                   where  f (i,q) = subst i q 

evalSOS :: SOS -> State -> Picture
evalSOS (cs,xs) (((y,x),sof),(h0,h1),_,_) 
                                 =  st'apply thePicture (xs,[0,0,0])
            where thePicture     =  picture repCount mcuSpec
                  mcuSpec        =  map f cs
                  f (id,dc,ac)   =  (d, (upsCount d, qt, h0 dc, h1 ac))
                             where  (d,qt) = sof id
                  repCount       =  ( ceilDiv y (8*maxy), ceilDiv x (8*maxx) ) 
                  upsCount (h,w) =  ( maxy/h, maxx/w )
                  maxy           =  maximum ( map (fst.fst) mcuSpec )
                  maxx           =  maximum ( map (snd.fst) mcuSpec )

jpegDecode :: String -> Picture
jpegDecode  = pi4 . foldl ap' errRes . st'apply segments 
        where pi4 (_,_,_,x) = x



----------------------------------------------
-- Main driver
----------------------------------------------

yCbCr2rgb  =  matmap f 
       where  f =  map ((+128).(/15)) . matapply [ [15,  0, 24]
                                                 , [15, -5,-12]
                                                 , [15, 30,  0]
                                                 ] 
dst << src  =  readFile src abort 
               (\input -> let output = (ppm . yCbCr2rgb . jpegDecode) input
                          in  writeFile dst output abort done
               )

main  =  "example.ppm" << "example.jpg"

----------------------------------------------
-- PPM Creation
----------------------------------------------

ppm xss  =  "P6\n# Creator: Gofer JPEG decoder\n" 
            ++ w ++ " " ++ h ++ "\n255\n"
            ++ (map (chr.sane) . concat . concat) xss
     where  w = "384"
            h = "256"
            --w = show (length (head xss))
            --h = show (length xss)

sane x = (0 `max` x) `min` 255

----------------------------------------------
-- XPM Creation
----------------------------------------------

xpm  xss = xpmhead xss
           ++ concat (map xpmpal [0..255]) 
           ++ concat (map xpmline xss)
           ++ xpmtail

xpmhead xss = "/* XPM */\nstatic char *a[] = { \"" ++ w ++ " " ++ h ++ " 256 2\"\n"
        where w = "160"
              --w = show (length (head xss))
              --h = show (length xss)
              h = "80"

xpmtail = "};\n"

xpmpal x =  ",\"" ++ s ++ " c #" ++ s ++ s ++ s ++ "\"\n"
     where s = byte2hex x

xpmline xs = ",\"" ++ concat(map byte2hex xs) ++ "\"\n"


nib2hex x | x<10 = chr (x+48)
          | otherwise = chr (x+55)

byte2hex x = [ nib2hex h, nib2hex l ]
       where (h,l) = byte2nibs x

----------------------------------------------
-- BMP Creation
----------------------------------------------

bmp xss = bmphead xss
          ++ concat (map bmpline xss)

bmphead :: [[a]] -> String
bmphead xss = (concat . map wor )
              ([ 16793, len, 0, 0, 0 ,54, 0, 40
               , 0    , w  , 0, h, 0 , 1, 24, 0 ] ++ copy 11 0)
        where w = length (head xss)
              h = length xss
              len = w*h*3 + 54

bmpline :: [[Int]] -> String
bmpline = concat . map (map chr)

wor x = [chr (x/256), chr (x`rem`256) ]

