import PHashBench
import SHashBench
import Criterion.Main

main :: IO ()
main = defaultMain benches
    where benches = [ bgroup "parallel hash" [bench "10000" $ nfIO $ pHashDocs 8]
                    , bgroup "serial hash"   [bench "10000" $ nfIO $ sHashDocs 8]
                    ]
