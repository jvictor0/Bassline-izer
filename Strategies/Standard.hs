module Strategies.Standard where

import Strategy
import Rand

strategy :: Strategy
strategy mode n = select [(1,arpeggioRandom (0.4,0.3,0.3,0.1)),
                          (1,arpeggioUp),
                          (1,arpeggioDown),
                          (1,scaleUp 0.3),
                          (1,scaleDown 0.3),
                          (1,hang)] 
                  >>= (\f -> f mode n)
           
strategyAdapter mode targ notes = select 
                                  [(2,identityStrategyAdapter),
                                   (1,resolveQuarter),
                                   (0.8,resolveEighth),
                                   (1,addPassingTone 0.1 0.8),
                                   (1,addDouble 0.2)]
                                  >>= (\f -> f mode targ notes)
                                   
                                  
