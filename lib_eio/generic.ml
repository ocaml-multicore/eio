type 'a ty = ..

class type t = object
  method probe : 'a. 'a ty -> 'a option
end

let probe (t : #t) ty = t#probe ty
