type ('v, 'b) t =
  | Pred of { deps : int list; func : 'v list -> 'b }
  | Prop of { deps : int list; out : int; func : 'v list -> 'v }
