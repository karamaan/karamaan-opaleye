{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Karamaan.Opaleye.Pack where

import Control.Arrow ((&&&), first)

import Karamaan.Opaleye.Wire (Wire(Wire), unwire)

class Pack a where
  pack'  :: [String] -> (a, [String])
  unpack :: a -> [String]

instance Pack (Wire a) where
  pack'  = (Wire . head) &&& tail
  unpack = return . unwire

instance (Pack a, Pack b) => Pack (a, b) where
  pack' s = let (a, rest)  = pack' s
                (b, rest') = pack' rest
            in ((a, b), rest')
  unpack (a, b) = unpack a ++ unpack b

flatten3 (a, (b, c)) = (a, b, c)
unflatten3 (a, b, c) = (a, (b, c))

flatten4 (a, (b, (c, a4))) = (a, b, c, a4)
unflatten4 (a, b, c, a4) = (a, (b, (c, a4)))

flatten5 (a, (b, (c, (a4, a5)))) = (a, b, c, a4, a5)
unflatten5 (a, b, c, a4, a5) = (a, (b, (c, (a4, a5))))

flatten6 (a, (b, (c, (a4, (a5, a6))))) = (a, b, c, a4, a5, a6)
unflatten6 (a, b, c, a4, a5, a6) = (a, (b, (c, (a4, (a5, a6)))))

flatten7 (a, (b, (c, (a4, (a5, (a6, a7)))))) = (a, b, c, a4, a5, a6, a7)
unflatten7 (a, b, c, a4, a5, a6, a7) = (a, (b, (c, (a4, (a5, (a6, a7))))))

flatten8 (a, (b, (c, (a4, (a5, (a6, (a7, a8)))))))
  = (a, b, c, a4, a5, a6, a7, a8)
unflatten8 (a, b, c, a4, a5, a6, a7, a8)
  = (a, (b, (c, (a4, (a5, (a6, (a7, a8)))))))

flatten9 (a, (b, (c, (a4, (a5, (a6, (a7, (a8, a9))))))))
  = (a, b, c, a4, a5, a6, a7, a8, a9)
unflatten9 (a, b, c, a4, a5, a6, a7, a8, a9)
  = (a, (b, (c, (a4, (a5, (a6, (a7, (a8, a9))))))))

flatten10 (a, (b, (c, (a4, (a5, (a6, (a7, (a8, (a9, a10)))))))))
  = (a, b, c, a4, a5, a6, a7, a8, a9, a10)
unflatten10 (a, b, c, a4, a5, a6, a7, a8, a9, a10)
  = (a, (b, (c, (a4, (a5, (a6, (a7, (a8, (a9, a10)))))))))

instance (Pack a, Pack b, Pack c) => Pack (a, b, c) where
  pack' = first flatten3 . pack'
  unpack = unpack . unflatten3

instance (Pack a, Pack b, Pack c, Pack d) => Pack (a, b, c, d) where
  pack' = first flatten4 . pack'
  unpack = unpack . unflatten4

instance (Pack a, Pack b, Pack c, Pack d, Pack e) => Pack (a, b, c, d, e) where
  pack' = first flatten5 . pack'
  unpack = unpack . unflatten5

instance (Pack a, Pack b, Pack c, Pack d, Pack e, Pack f)
         => Pack (a, b, c, d, e, f) where
  pack' = first flatten6 . pack'
  unpack = unpack . unflatten6

instance (Pack a, Pack b, Pack c, Pack d, Pack e, Pack f, Pack a7)
         => Pack (a, b, c, d, e, f, a7) where
  pack' = first flatten7 . pack'
  unpack = unpack . unflatten7

instance (Pack a, Pack b, Pack c, Pack d, Pack e, Pack f, Pack a7, Pack a8)
         => Pack (a, b, c, d, e, f, a7, a8) where
  pack' = first flatten8 . pack'
  unpack = unpack . unflatten8

instance (Pack a, Pack b, Pack c, Pack d, Pack e, Pack f, Pack a7, Pack a8,
          Pack a9, Pack a10)
         => Pack (a, b, c, d, e, f, a7, a8, a9, a10) where
  pack' = first flatten10 . pack'
  unpack = unpack . unflatten10

pack :: Pack a => [String] -> a
pack = fst . pack'

packMap :: Pack a => (String -> String) -> a -> a
packMap f = pack . map f . unpack
