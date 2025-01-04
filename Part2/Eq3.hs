module Eq3 (Eq3, (===)) where

import MaybeNull
import Bool3

class Eq3 a where
   (===) :: a -> a -> Bool3

instance (Eq3 a) => Eq3 (MaybeNull a) where  ------- => tarkoittaa, että pitää olla osa Eq3 luokkaa!
  _ === Null = Unk3
  Null === _ = Unk3
  JustVal x === JustVal y = x === y
  
instance Eq3 Bool3 where
   Unk3 === _ = Unk3
   _ === Unk3 = Unk3
   True3 === True3 = True3
   False3 === False3 = True3
   False3 === True3 = False3
   True3 === False3 = False3