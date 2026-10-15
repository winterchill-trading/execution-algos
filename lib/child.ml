open! Core
module Price = Market_data.Price
module Size = Market_data.Size
module Side = Market_data.Side

module Stable = struct
  module V1 = struct
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

include Stable.V1

module Action = struct
  module Stable = struct
    module V1 = struct
      type t =
        | Place of Stable.V1.t
        | Cancel of int
      [@@deriving bin_io, sexp, compare, equal]
    end
  end

  include Stable.V1
end
