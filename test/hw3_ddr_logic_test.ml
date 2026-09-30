open! Core
open Ddr_logic_library
open Hw2_ddr_logic

let sample_notes : note list =
  [ { timestamp = 1.0; direction = Left }
  ; { timestamp = 2.0; direction = Up }
  ; { timestamp = 3.0; direction = Down }
  ]
;;

let initial_state : game_state =
  { upcoming_notes = sample_notes
  ; life_bar = 50.0
  ; score = 0
  ; combo = 0
  ; song_elapsed = 0.0
  ; decision = In_progress
  }
;;

let print_state (state : game_state) = print_s [%sexp (state : game_state)]

let make_move_and_print state move =
  let new_state = make_move state move in
  print_state new_state
;;

let update_time_and_print state time =
  let new_state = update_time state time in
  print_state new_state
;;

let%test "evaluate_judgement exact thresholds" = 
equal_judgement (evaluate_judgement 0.01) Marvelous
  && equal_judgement (evaluate_judgement 0.04) Perfect
  && equal_judgement (evaluate_judgement 0.08) Great
  && equal_judgement (evaluate_judgement 0.11) Good
  && equal_judgement (evaluate_judgement 0.15) Miss
;;

let%test "make_move equality check" = 
let move : move = { key_pressed = Left; press_time = 1.0 } in
  let actual_state = make_move initial_state move in
  let expected_state : game_state =
    { upcoming_notes =
        [ { timestamp = 2.0; direction = Up }
        ; { timestamp = 3.0; direction = Down }
        ]
    ; life_bar = 55.0
    ; score = 100
    ; combo = 1
    ; song_elapsed = 1.0
    ; decision = In_progress
    }
  in
  equal_game_state actual_state expected_state
;;

let%expect_test "evaluate_judgement across all timing windows" = 
let test_times = [ 0.00; 0.02; 0.03; 0.05; 0.07; 0.09; 0.10; 0.12; 0.13; -0.01 ] in
  List.iter test_times ~f:(fun delta ->
    let j = evaluate_judgement delta in
    print_s [%sexp { delta : float; judgement = (j : judgement) }]);
  [%expect
    {|
    ((delta 0) (judgement Marvelous))
    ((delta 0.02) (judgement Marvelous))
    ((delta 0.03) (judgement Perfect))
    ((delta 0.05) (judgement Perfect))
    ((delta 0.07) (judgement Great))
    ((delta 0.09) (judgement Great))
    ((delta 0.1) (judgement Good))
    ((delta 0.12) (judgement Good))
    ((delta 0.13) (judgement Miss))
    ((delta -0.01) (judgement Marvelous))
    |}]
;;

let%expect_test "make_move: perfect hit increases score, combo, and life" =
  make_move_and_print initial_state { key_pressed = Left; press_time = 1.0 };
  [%expect
    {|
    ((upcoming_notes
      (((timestamp 2) (direction Up)) ((timestamp 3) (direction Down))))
     (life_bar 55) (score 100) (combo 1) (song_elapsed 1) (decision In_progress))
    |}]
;;

let%expect_test "make_move: wrong arrow key causes Miss and resets combo" =
  let state_after_1_hit =
    make_move initial_state { key_pressed = Left; press_time = 1.0 }
  in
  make_move_and_print state_after_1_hit { key_pressed = Right; press_time = 2.0 };
  [%expect
    {|
    ((upcoming_notes (((timestamp 3) (direction Down)))) (life_bar 45)
     (score 100) (combo 0) (song_elapsed 2) (decision In_progress))
    |}]
;;

let%expect_test "make_move: early key press (> 0.12s before note) is ignored" =
  make_move_and_print initial_state { key_pressed = Left; press_time = 0.5 };
  [%expect
    {|
    ((upcoming_notes
      (((timestamp 1) (direction Left)) ((timestamp 2) (direction Up))
       ((timestamp 3) (direction Down))))
     (life_bar 50) (score 0) (combo 0) (song_elapsed 0.5) (decision In_progress))
    |}]
;;

let%expect_test "update_time: advancing time without missed notes only updates elapsed time" =
  update_time_and_print initial_state 0.8;
  [%expect
    {|
    ((upcoming_notes
      (((timestamp 1) (direction Left)) ((timestamp 2) (direction Up))
       ((timestamp 3) (direction Down))))
     (life_bar 50) (score 0) (combo 0) (song_elapsed 0.8) (decision In_progress))
    |}]
;;

let%expect_test "update_time: time passing past note timestamp identifies note as Miss" =
  update_time_and_print initial_state 1.20;
  [%expect
    {|
    ((upcoming_notes
      (((timestamp 2) (direction Up)) ((timestamp 3) (direction Down))))
     (life_bar 40) (score 0) (combo 0) (song_elapsed 1.2) (decision In_progress))
    |}]
;;

let%expect_test "Full stage clear sequence" =
  let s1 = make_move initial_state { key_pressed = Left; press_time = 1.0 } in
  let s2 = make_move s1 { key_pressed = Up; press_time = 2.0 } in
  make_move_and_print s2 { key_pressed = Down; press_time = 3.0 };
  [%expect
    {|
    ((upcoming_notes ()) (life_bar 65) (score 300) (combo 3) (song_elapsed 3)
     (decision (Stage_cleared (final_score 300))))
    |}]
;;

let%expect_test "Stage_failed when life reaches 0 mid game" =
  let low_life_state = { initial_state with life_bar = 5.0 } in
  make_move_and_print low_life_state { key_pressed = Right; press_time = 1.0 };
  [%expect
    {|
    ((upcoming_notes
      (((timestamp 2) (direction Up)) ((timestamp 3) (direction Down))))
     (life_bar 0) (score 0) (combo 0) (song_elapsed 1) (decision Stage_failed))
    |}]
;;

let%expect_test "Cannot make move in terminal state" =
  let failed_state = { initial_state with decision = Stage_failed } in
  make_move_and_print failed_state { key_pressed = Left; press_time = 1.0 };
  [%expect
    {|
    ((upcoming_notes
      (((timestamp 1) (direction Left)) ((timestamp 2) (direction Up))
       ((timestamp 3) (direction Down))))
     (life_bar 50) (score 0) (combo 0) (song_elapsed 0) (decision Stage_failed))
    |}]
;;

let random_walk (start_state : game_state) ~random_seed =
  Core.Random.init random_seed;
  let directions = [ Left; Down; Up; Right ] in
  let rec loop state time =
    match state.decision with
    | Stage_cleared _ | Stage_failed -> state
    | In_progress ->
      let time_step = Core.Random.float_range 0.2 0.8 in
      let next_time = time +. time_step in
      let should_press_key = Core.Random.bool () in
      let next_state =
        if should_press_key then
          let key = List.random_element_exn directions in
          make_move state { key_pressed = key; press_time = next_time }
        else
          update_time state next_time
      in
      if Float.(next_time > 5.0) then next_state else loop next_state next_time
  in
  print_state (loop start_state 0.0)
;;

let%expect_test "DDR random walk simulation" =
  random_walk initial_state ~random_seed:42;
  [%expect
    {|
    ((upcoming_notes ()) (life_bar 20) (score 0) (combo 0)
     (song_elapsed 3.2450589361678266)
     (decision (Stage_cleared (final_score 0))))
    |}];
  random_walk initial_state ~random_seed:7;
  [%expect
    {|
    ((upcoming_notes ()) (life_bar 20) (score 0) (combo 0)
     (song_elapsed 3.5218993744057094)
     (decision (Stage_cleared (final_score 0))))
    |}]
;;