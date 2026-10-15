(** A parent order: the position change to execute, its worst acceptable price and the
    venue's size grid. *)

open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Stable : sig
  module V1 : sig
    type t =
      { side : Side.Stable.V1.t
      ; size : Size.Stable.V1.t
      ; limit_px : Price.Stable.V1.t
      ; reference_px : Price.Stable.V1.t
      ; lot : Size.Stable.V1.t
      ; min_size : Size.Stable.V1.t
      ; equity : float
      }
    [@@deriving bin_io, sexp, compare, equal]
  end
end

type t = Stable.V1.t =
  { side : Side.t
  ; size : Size.t (** positive, a multiple of [lot] *)
  ; limit_px : Price.t
  (** worst price any child may trade at (the slippage bound); children never cross it *)
  ; reference_px : Price.t
  (** the price when the parent was decided (the copy engine's mark); the arrival price
      when the first book has no mid *)
  ; lot : Size.t (** child sizes are multiples of it; positive *)
  ; min_size : Size.t
  (** smallest child the venue accepts (for example its minimum notional over the
      price, rounded up to the lot); a smaller remainder is not sent *)
  ; equity : float
  (** USD equity of the account the parent belongs to, for the adaptive algorithm's
      tracking bound; 0 when unknown (the bound is then ignored) *)
  }
[@@deriving bin_io, sexp, compare, equal]

(** Checks a positive size and lot, the size on the lot grid and a non-negative
    [min_size]. *)
val create
  :  ?min_size:Size.t
  -> ?equity:float
  -> side:Side.t
  -> size:Size.t
  -> limit_px:Price.t
  -> reference_px:Price.t
  -> lot:Size.t
  -> unit
  -> t

(** [size] truncated toward zero to a multiple of [lot]. *)
val round_lot : lot:Size.t -> Size.t -> Size.t
[@@zero_alloc]
