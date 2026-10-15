(** A (partial) fill of one child order, as the caller reports it to the scheduler. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size

module Stable : sig
  module V1 : sig
    type t =
      { child : int
      ; size : Size.Stable.V1.t
      ; price : Price.Stable.V1.t
      ; fee : float
      ; maker : bool
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { child : int (** the child's id *)
  ; size : Size.t (** positive *)
  ; price : Price.t
  ; fee : float (** USD paid, negative for a rebate *)
  ; maker : bool (** the child was resting when it filled *)
  }
[@@deriving bin_io, sexp, compare, equal]
