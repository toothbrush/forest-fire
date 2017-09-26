module TreeUtils where

import Data.Tree
import Types

-- dependencyToTree :: Dependency -> Tree String
-- dependencyToTree (Dependency (StackName name) deps) = Node name (map dependencyToTree deps)

dependencyToTree = undefined

postorder :: Tree a -> [a]
postorder (Node label kids) = concatMap postorder kids ++ [label]
