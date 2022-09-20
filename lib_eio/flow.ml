type shutdown_command = [ `Receive | `Send | `All ]

type read_method = ..
type read_method += Read_source_buffer of ((Cstruct.t list -> int) -> unit)

class type close = object
  method close : unit
end

let close (t : #close) = t#close

class virtual source = object (_ : <Generic.t; ..>)
  method probe _ = None
  method read_methods : read_method list = []
  method virtual read_into : Cstruct.t -> int
end

let read (t : #source) buf =
  let got = t#read_into buf in
  assert (got > 0 && got <= Cstruct.length buf);
  got

let read_methods (t : #source) = t#read_methods

let rec read_exact t buf =
  if Cstruct.length buf > 0 then (
    let got = read t buf in
    read_exact t (Cstruct.shift buf got)
  )

let cstruct_source data : source =
  object (self)
    val mutable data = data

    inherit source

    method private read_source_buffer fn =
      let rec aux () =
        match data with
        | [] -> raise End_of_file
        | x :: xs when Cstruct.length x = 0 -> data <- xs; aux ()
        | xs ->
          let n = fn xs in
          data <- Cstruct.shiftv xs n 
      in
      aux ()

    method! read_methods =
      [ Read_source_buffer self#read_source_buffer ]

    method read_into dst =
      let avail, src = Cstruct.fillv ~dst ~src:data in
      if avail = 0 then raise End_of_file;
      data <- src;
      avail
  end

let string_source s : source =
  object
    val mutable offset = 0

    inherit source

    method read_into dst =
      if offset = String.length s then raise End_of_file;

      let len = min (Cstruct.length dst) (String.length s - offset) in
      Cstruct.blit_from_string s offset dst 0 len;
      offset <- offset + len;
      len
  end

class virtual sink = object (_ : <Generic.t; ..>)
  method probe _ = None
  method virtual copy : 'a. (#source as 'a) -> unit
end

let copy (src : #source) (dst : #sink) = dst#copy src

let copy_string s = copy (string_source s)

let buffer_sink b =
  object
    inherit sink

    method copy src =
      let buf = Cstruct.create 4096 in
      try
        while true do
          let got = src#read_into buf in
          Buffer.add_string b (Cstruct.to_string ~len:got buf)
        done
      with End_of_file -> ()
  end

class virtual two_way = object (_ : <source; sink; ..>)
  method probe _ = None
  method read_methods = []

  method virtual shutdown : shutdown_command -> unit
end

let shutdown (t : #two_way) = t#shutdown
