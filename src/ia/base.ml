let rec searchNullWeight tree =
  match tree with
  | [] -> false
  | t::q -> t==0 || searchNullWeight q

let rec move tree= 
  if searchNullWeight tree then
    true
  else 
    false