open! Core
open Symetric

let () =
  let bar = Dumb_progress.basic_bar 100 in
  let bar' = Dumb_progress.basic_bar 100 in
  Dumb_progress.add bar;
  Dumb_progress.add bar';
  for i = 0 to 100 do
    Dumb_progress.update bar i;
    for j = 0 to 100 do
      Dumb_progress.update bar' j;
      (Unix.nanosleep 0.01 : float) |> ignore
    done
  done
