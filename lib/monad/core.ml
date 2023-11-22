module type TYPE = sig
  type t
end

module Monad = struct
  module type BASE = sig
    type 'a m

    val pure : 'a -> 'a m
    val bind : 'a m -> ('a -> 'b m) -> 'b m
  end

  module type A = sig
    type 'a m

    val apply : ('a -> 'b) m -> 'a m -> 'b m
  end

  module type F = sig
    type 'a m

    val fmap : ('a -> 'b) -> 'a m -> 'b m
  end

  module type S = sig
    include BASE
    include A with type 'a m := 'a m
    include F with type 'a m := 'a m

    module Syntax : sig
      val pure : 'a -> 'a m
      val fmap : ('a -> 'b) -> 'a m -> 'b m
      val ( let* ) : 'a m -> ('a -> 'b m) -> 'b m
      val ( let+ ) : 'a m -> ('a -> 'b) -> 'b m
      val ( and+ ) : 'a m -> 'b m -> ('a * 'b) m
      val ( >>= ) : 'a m -> ('a -> 'b m) -> 'b m
      val ( <$> ) : ('a -> 'b) -> 'a m -> 'b m
      val ( <*> ) : ('a -> 'b) m -> 'a m -> 'b m
      val ( <* ) : 'a m -> 'b m -> 'a m
      val ( *> ) : 'a m -> 'b m -> 'b m
    end
  end

  module Make (B : BASE) : S with type 'a m = 'a B.m = struct
    include B

    let apply mf ma =
      let ( let* ) = B.bind in
      let* f = mf in
      let* a = ma in
      B.pure @@ f a
    ;;

    let fmap f ma =
      let ( let* ) = B.bind in
      let* a = ma in
      B.pure @@ f a
    ;;

    module Syntax = struct
      let ( let* ) = B.bind
      let ( >>= ) = B.bind
      let pure = pure
      let fmap = fmap
      let ( <$> ) = fmap
      let ( <*> ) = apply
      let ( <* ) xp yp = pure (fun x _ -> x) <*> xp <*> yp
      let ( *> ) xp yp = pure (fun _ y -> y) <*> xp <*> yp
      let product xp yp = pure (fun x y -> x, y) <*> xp <*> yp
      let ( let+ ) x f = f <$> x
      let ( and+ ) = product
    end
  end
end

module Identity = struct
  module M = Monad.Make (struct
      type 'a m = 'a

      let pure x = x
      let bind m f = f m
    end)

  include M

  let run m = m
end

module OptionT = struct
  module Make (B : Monad.S) = struct
    module M_Base = struct
      type 'a m = 'a option B.m

      let pure x = B.pure (Some x)

      let bind m f =
        let open B.Syntax in
        let* o = m in
        match o with
        | Some v -> f v
        | None -> B.pure None
      ;;
    end

    module M = Monad.Make (M_Base)
    include M

    let lift m =
      let open B.Syntax in
      m >>= fun v -> B.pure (Some v)
    ;;

    let runOptionT (m : 'a m) : 'a option B.m = m
    let some x : 'a m = M.pure x
    let none () : 'a m = B.pure None

    let from_option o =
      match o with
      | None -> none ()
      | Some x -> some x
    ;;

    let fold m fs fn =
      let open B.Syntax in
      let* o = m in
      let v =
        match o with
        | Some s -> fs s
        | None -> fn ()
      in
      B.pure @@ Some v
    ;;

    let foldF m fs fn =
      let open B.Syntax in
      let* o = m in
      match o with
      | Some s -> fs s
      | None -> fn ()
    ;;

    (* alternative *)
    let empty = none

    let either mx my =
      let open B.Syntax in
      let* x = mx in
      match x with
      | None -> my
      | Some _ -> B.pure x
    ;;
  end
end

module EitherT = struct
  module type TYPE = sig
    type t
  end

  module Make (TYPE : TYPE) (B : Monad.S) = struct
    type ex = TYPE.t
    type 'a out = ('a, ex) result

    module M_Base = struct
      type 'a m = 'a out B.m

      let pure x = B.pure (Ok x)

      let bind m f =
        let open B.Syntax in
        let* r = m in
        match r with
        | Ok v -> f v
        | Error e -> B.pure @@ Error e
      ;;
    end

    module M = Monad.Make (M_Base)
    include M

    let lift m =
      let open B.Syntax in
      m >>= fun v -> B.pure (Ok v)
    ;;

    let runEitherT (m : 'a m) : ('a, ex) result B.m = m
    let throwError e = B.pure (Error e)

    let catchError m f =
      let open B.Syntax in
      m >>= function
      | Ok v -> B.pure (Ok v)
      | Error e -> f e
    ;;

    let fold m fr fl =
      let open B.Syntax in
      let* r = m in
      let v =
        match r with
        | Ok r -> fr r
        | Error l -> fl l
      in
      B.pure @@ Ok v
    ;;

    let foldF (m : 'a m) (fr : 'a -> 'c m) (fl : 'b -> 'c m) : 'c m =
      let open B.Syntax in
      let* r = m in
      match r with
      | Ok r -> fr r
      | Error l -> fl l
    ;;

    let bimap m fr fl =
      let open B.Syntax in
      let* r = m in
      B.pure
      @@
      match r with
      | Ok r -> Ok (fr r)
      | Error l -> Error (fl l)
    ;;
  end
end

module StateT = struct
  module Make (STATE : TYPE) (B : Monad.S) = struct
    type state = STATE.t

    module M_Base = struct
      type 'a m = state -> ('a * state) B.m

      let pure (x : 'a) : 'a m = fun s -> B.pure (x, s)

      let bind (m : 'a m) (f : 'a -> 'b m) : 'b m =
        let open B.Syntax in
        fun s -> m s >>= fun (x, s') -> f x s'
      ;;
    end

    module M = Monad.Make (M_Base)
    include M

    let lift m =
      let open B.Syntax in
      fun s -> m >>= fun x -> B.pure (x, s)
    ;;

    let runStateT (m : 'a m) (s : state) : ('a * state) B.m = m s
    let get () : 'a m = fun s -> B.pure (s, s)
    let put s _ = B.pure ((), s)
    let update (f : state -> state) : 'a m = fun s -> B.pure ((), f s)
  end
end
