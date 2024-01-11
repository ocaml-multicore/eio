module XmlHttpRequest = struct
  include Js_of_ocaml.XmlHttpRequest
  include XmlHttpRequest
end

module File = struct
  include Js_of_ocaml.File
  include File
end

module Eio_js = Eio_js
module Eio_js_events = Js_events
