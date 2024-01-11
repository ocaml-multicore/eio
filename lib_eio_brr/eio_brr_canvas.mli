(** Canvas element. *)
module Canvas : sig
  val to_blob :
    ?encode:Brr_canvas.Canvas.image_encode ->
    Brr_canvas.Canvas.t ->
    Brr.Blob.t option
  (** [to_blob ~encode t] is the canvas's image a blob object. [None]
      is returned either if the canvas has no pixels or if an error
      occurs during image serialisation. *)
end
