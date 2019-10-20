module ListExtras exposing (getAt, insertAt, updateAt, deleteAt, reinsert, zip)

getAt : Int -> (List a) -> Maybe a
getAt idx list =
  List.drop idx list |> List.head


insertAt : Int -> a -> (List a) -> (List a)
insertAt idx value list =
  (List.take idx list) ++ (value :: (List.drop idx list))


updateAt : Int -> (a -> a) -> (List a) -> (List a)
updateAt idx update list =
  let maybeUpdate i v = if i == idx then update v else v
  in List.indexedMap maybeUpdate list


deleteAt : Int -> (List a) -> (List a)
deleteAt idx list =
  (List.take idx list) ++ (List.drop (idx + 1) list)


reinsert : Int -> Int -> (List a) -> (List a)
reinsert from to list =
  getAt from list
  |> Maybe.andThen (\v -> Just (deleteAt from list |> insertAt to v))
  |> Maybe.withDefault list


zip : (List a) -> (List b) -> (List (a, b))
zip = List.map2 Tuple.pair
