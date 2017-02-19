open Core.Std
open Format
open Slap.Common
open Slap.Size
open Slap.Io
open Slap.D

let sum v =
  Vec.fold_left (fun a b -> a +. b) 0. v

let sigf a =
  if a > 0. then 1. else 0.

let gand x1 x2 =
  let x = Vec.of_list_dyn two [x1; x2] in
  let w = Vec.of_list_dyn two [0.5; 0.5] in
  let b = -0.7 in
  Vec.mul x w
  |> sum
  |> (+.) b
  |> sigf

let nand x1 x2 =
  let x = Vec.of_list_dyn two [x1; x2] in
  let w = Vec.of_list_dyn two [-0.5; -0.5] in
  let b = 0.7 in
  Vec.mul x w
  |> sum
  |> (+.) b
  |> sigf

let gor x1 x2 =
  let x = Vec.of_list_dyn two [x1; x2] in
  let w = Vec.of_list_dyn two [0.5; 0.5] in
  let b = -0.2 in
  Vec.mul x w
  |> sum
  |> (+.) b
  |> sigf

let xor x1 x2 =
  let s1 = nand x1 x2 in
  let s2 = gor x1 x2 in
  gand s1 s2

let () =
  assert ((gand 0. 0.) = 0.);
  assert ((gand 1. 0.) = 0.);
  assert ((gand 0. 1.) = 0.);
  assert ((gand 1. 1.) = 1.);

  assert ((nand 0. 0.) = 1.);
  assert ((nand 1. 0.) = 1.);
  assert ((nand 0. 1.) = 1.);
  assert ((nand 1. 1.) = 0.);

  assert ((gor 0. 0.) = 0.);
  assert ((gor 1. 0.) = 1.);
  assert ((gor 0. 1.) = 1.);
  assert ((gor 1. 1.) = 1.);

  assert ((xor 0. 0.) = 0.);
  assert ((xor 1. 0.) = 1.);
  assert ((xor 0. 1.) = 1.);
  assert ((xor 1. 1.) = 0.)
