open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Stable = struct
  module V1 = struct
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

include Stable.V1

let[@zero_alloc] round_lot ~lot size =
  let l = Size.to_units lot in
  if l <= 0 then size else Size.of_units (Size.to_units size / l * l)
;;

let create
  ?(min_size = Size.zero)
  ?(equity = 0.)
  ~side
  ~size
  ~limit_px
  ~reference_px
  ~lot
  ()
  =
  if not (Size.is_positive lot)
  then raise_s [%message "Parent.create: lot must be positive" (lot : Size.t)];
  if not (Size.is_positive size)
  then raise_s [%message "Parent.create: size must be positive" (size : Size.t)];
  if not (Size.equal (round_lot ~lot size) size)
  then raise_s [%message "Parent.create: size not on the lot" (size : Size.t) (lot : Size.t)];
  if Size.is_negative min_size
  then raise_s [%message "Parent.create: negative min_size" (min_size : Size.t)];
  { side; size; limit_px; reference_px; lot; min_size; equity }
;;
