open Core.Std
open Format
open Slap.Common
open Slap.Size
open Slap.Io
open Slap.D

let sum v =
  Vec.fold_left (fun a b -> a +. b) 0. v

let sigf a =
  if a > 0. then 1 else 0

let and_gate x1 x2 =
  let x = Vec.of_list_dyn two [x1; x2] in
  let w = Vec.of_list_dyn two [0.5; 0.5] in
  let b = -0.2 in
  Vec.mul x w
  |> Vec.add (Vec.of_list_dyn two [b; b])
  |> sum
  |> sigf

let () =
  let a = and_gate 1. 1. in
  printf "%d\n" a
