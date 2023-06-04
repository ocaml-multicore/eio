type 'a ty = ..

class type t = object
  method probe : 'a. 'a ty -> 'a option
end

let probe (t : #t) ty = t#probe ty

class type close = object
  method close : unit
end

let close (t : #close) = t#close
