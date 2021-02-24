open Grammar

type ('v, 'b) t =
  | Pred of { deps : Bind.t list; func : 'v Map.M(Bind).t -> 'b }
  | Prop of { deps : Bind.t list; out : int; func : 'v Map.M(Bind).t -> 'v }
