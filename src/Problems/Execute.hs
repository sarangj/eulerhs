module Problems.Execute 
  ( execute
  , Problem(..)
  ) where

import qualified Problems.Problem1 as Problem1
import qualified Problems.Problem2 as Problem2
import qualified Problems.Problem3 as Problem3
import qualified Problems.Problem4 as Problem4
import qualified Problems.Problem5 as Problem5
import qualified Problems.Problem6 as Problem6
import qualified Problems.Problem7 as Problem7
import qualified Problems.Problem8 as Problem8
import qualified Problems.Problem9 as Problem9
import qualified Problems.Problem10 as Problem10

data Problem 
  = Problem1
  | Problem2
  | Problem3
  | Problem4
  | Problem5
  | Problem6
  | Problem7
  | Problem8
  | Problem9
  | Problem10
  deriving (Eq, Show)

-- | Execute one of my solved problems.  For now, assume a solution is an
-- Int which may need to be changed in the future
execute :: Problem -> Int
execute Problem1 = Problem1.getSolution
execute Problem2 = Problem2.getSolution
execute Problem3 = Problem3.getSolution
execute Problem4 = Problem4.getSolution
execute Problem5 = Problem5.getSolution
execute Problem6 = Problem6.getSolution
execute Problem7 = Problem7.getSolution
execute Problem8 = Problem8.getSolution
execute Problem9 = Problem9.getSolution
execute Problem10 = Problem10.getSolution
