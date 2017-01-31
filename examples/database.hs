{-# LANGUAGE StaticPointers,
             FlexibleInstances,
             TypeFamilies,
             FlexibleContexts,
             OverloadedStrings,
             MultiParamTypeClasses,
             Arrows #-}
import Haste.App
import Data.List (intercalate)

-- Opaleye boilerplate: declare the database table and a run function
import Database.SQLite.Simple (Connection, withConnection)
import Opaleye.SQLite
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Control.Arrow

ageTable :: Table (Column PGText, Column PGInt4)
                  (Column PGText, Column PGInt4)
ageTable = Table "ages" $ p2 (required "name", required "age")

runQ :: Default QueryRunner a b => env -> Query a -> CIO [b]
runQ _ q = liftIO $ withConnection "mydb.sqlite" $ \c -> runQuery c q


-- Set up Query as a new node
instance Node Query where
  getEnv = pure ()

instance Mapping Query (Column PGText) where
  type Hask Query (Column PGText) = [String] ; invoke = runQ


-- Run the thing; query is inline
main = runApp [start (Proxy :: Proxy Query)] $ do
  old <- maybe 0 id . fromJSString <$> prompt "How old is \"old\" to you?"
  olds <- using old $ static (remote $ \old -> proc() -> do
      (name, age) <- queryTable ageTable -< ()
      restrict -< age .>= fromIntegral (old :: Int)
      returnA -< name
    )
  alert (toJSString $ "Then, these people are old: " ++ intercalate ", " olds)
