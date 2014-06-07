
-- Christopher preferred this API for aggregation
(>:) :: Aggregator a b -> (x -> a) -> Aggregator x b
(>:) = flip lmap

pa3 :: ProductProfunctor p => (p a b1, p a b2, p a b3) -> p a (b1, b2, b3)
pa3 = lmap (\x -> (x,x,x)) . p3

example :: Aggregator (Wire String, Wire Int)
                      (Wire String, Wire Int, Wire Int)
example = pa3 (groupBy >: fst, sum >: snd, avg >: snd)
