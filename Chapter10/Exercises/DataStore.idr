module DataStore

import Data.Vect

infixr 5 .+.

public export
data Schema = SString | SInt | (.+.) Schema Schema

public export
SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

export
record DataStore (schema : Schema) where
  constructor MkData
  size : Nat
  items : Vect size (SchemaType schema)

export
empty : DataStore schema
empty = MkData 0 []

export
addToStore : (entry : SchemaType schema) ->
             (store : DataStore schema) ->
             DataStore schema
addToStore entry (MkData _ items)
     = MkData _ (entry :: items)

public export
data StoreView : DataStore schema -> Type where
     SNil : StoreView empty
     SAdd : (rec : StoreView store) -> StoreView (addToStore entry store)

storeViewHelp : (items : Vect size (SchemaType schema)) ->
                StoreView (MkData size items)
storeViewHelp [] = SNil
storeViewHelp (entry :: xs) = SAdd (storeViewHelp xs)

export
storeView : (store : DataStore schema) -> StoreView store
storeView (MkData size items)
    = storeViewHelp items
