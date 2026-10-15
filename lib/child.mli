(** Child orders and the actions a {!Scheduler} asks its caller to send. Child ids are
    small integers, unique within one parent and assigned in order from 0; the caller maps
    them to venue order ids (for example client order ids derived from the parent's). *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Stable : sig
  module V1 : sig
    type t =
      { id : int
      ; side : Side.Stable.V1.t
      ; size : Size.Stable.V1.t
      ; limit_px : Price.Stable.V1.t
      ; post_only : bool
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { id : int
  ; side : Side.t
  ; size : Size.t (** positive, a multiple of the parent's lot *)
  ; limit_px : Price.t (** never worse than the parent's limit *)
  ; post_only : bool
  (** [true]: rest on the book and never take (add-liquidity-only); [false]:
      immediate-or-cancel *)
  }
[@@deriving bin_io, sexp, compare, equal]

module Action : sig
  module Stable : sig
    module V1 : sig
      type t =
        | Place of Stable.V1.t
        | Cancel of int
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  type nonrec t = Stable.V1.t =
    | Place of t
    | Cancel of int (** cancel the resting child with this id *)
  [@@deriving bin_io, sexp, compare, equal]
end
