open! Core
module Price = Market_data.Price
module Size = Market_data.Size

module Stable = struct
  module V1 = struct
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

include Stable.V1
