structure NetServer =
struct

val stop = ref false
val quit = ref false

fun needStop () = !stop
fun needQuit () = !quit

fun run' f x = (
  Signal.signal (Posix.Signal.pipe, Signal.SIG_IGN);
  Signal.signal (Posix.Signal.term, Signal.SIG_HANDLE (fn _ => (stop := true; Thread.Thread.broadcastInterrupt ())));
  Signal.signal (Posix.Signal.quit, Signal.SIG_HANDLE (fn _ => (quit := true; Thread.Thread.broadcastInterrupt ())));
  f x
)


exception Socket of string

local
  open Foreign
  val libc = loadExecutable ()
  val setsockopt_ffi = buildCall5 ((getSymbol libc "setsockopt"), (cInt, cInt, cInt, cConstStar cInt, cInt), cInt)
in
  fun setsockopt_REUSEPORT fd =
    if setsockopt_ffi (fd, OS_Constants.SOL_SOCKET, OS_Constants.SO_REUSEPORT, 1, 4) = ~1
    then raise Socket "Cannot set SO_REUSEPORT option on socket"
    else ()
end



local
  open Thread
  open Thread Mutex ConditionVar

  val m = mutex ()
  val c = conditionVar ()
  val cnt = ref 0

  fun doFork n f x =
    let
      fun doit 0 = ()
        | doit n = (
            cnt := !cnt + 1;
            fork (fn () => (f x; lock m; signal c; cnt := !cnt - 1; unlock m), [EnableBroadcastInterrupt true]);
            doit (n - 1)
          )
    in
      lock m;
      doit n;
      unlock m
    end

  fun doWait f x  =
    while !cnt > 0 do (
      wait(c, m);
      if needStop () then () else (unlock m; doFork 1 f x)
    ) handle Interrupt => doWait f x | exc => raise exc

in
  fun runWithN logger n f x =
    if n > 0
    then (doFork n f x; doWait f x)
    else f x
end

end
