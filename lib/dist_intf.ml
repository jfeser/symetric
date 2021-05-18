module type S = sig
  type value

  type op

  val value : value -> value -> float

  val program : op Program.t -> op Program.t -> float
end
