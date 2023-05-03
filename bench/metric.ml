let create name value units description : Yojson.Safe.t =
  `Assoc [
    "name", `String name;
    "value", (value :> Yojson.Safe.t);
    "units", `String units;
    "description", `String description;
  ]
