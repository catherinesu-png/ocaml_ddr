open! Core
open Ddr_logic_library
open Hw2_ddr_logic

val sample_notes : note list
val initial_state : game_state
val print_state : game_state -> unit
val make_move_and_print : game_state -> move -> unit
val update_time_and_print : game_state -> float -> unit
val random_walk : game_state -> random_seed:int -> unit