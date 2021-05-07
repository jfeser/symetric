module type S = sig
  type value

  type params

  type op

  val value : params -> value -> value -> float

  val program : op Program.t -> op Program.t -> float
end
