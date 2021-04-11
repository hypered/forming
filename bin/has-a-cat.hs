module Main where

import Forming


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMainOne $
  Computation
    "has-a-cat"
    "A form to obtain a cat's name, if there is a cat."
    "form"
    form_1


--------------------------------------------------------------------------------

-- A example form.
-- The second value must be provided if the first one is set to True.
--   $ runghc script.hs --set "has a cat" True --set "a cat's name" Tom form
--   Result (Object [("has a cat",Bool True),("a cat's name",Int 1)])
--
form_1 =
  [ Rule "has a cat" Unset
  , Rule "a cat's name" Unset
  , Rule "form" (Exp (Cond
      (Name "has a cat")
      (Names ["has a cat", "a cat's name"])
      (Names ["has a cat"])))
  ]
