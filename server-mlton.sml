structure NetServer =
struct

val stop = ref false
val quit = ref false

fun needStop () = !stop
fun needQuit () = !quit

fun run' f x = (
  MLton.Signal.setHandler(Posix.Signal.pipe, MLton.Signal.Handler.ignore);
  f x
)



exception Socket of string

local
  val setsockopt_ffi = _import "setsockopt": int * int * int * int ref * int-> int;
in
  fun setsockopt_REUSEPORT fd =
    if setsockopt_ffi (fd, OS_Constants.SOL_SOCKET, OS_Constants.SO_REUSEPORT, (ref 1), 4) = ~1
    then raise Socket "Cannot set SO_REUSEPORT option on socket"
    else ()
end



local
  val pidToString = LargeInt.toString o SysWord.toLargeInt o Posix.Process.pidToWord
  fun myPidAsString () = pidToString (Posix.ProcEnv.getpid ())

  val main_pid = Posix.ProcEnv.getpid ()

  val child_pids = ref []


  fun sendSignalToChild signal =
    if main_pid = Posix.ProcEnv.getpid ()
    then List.app (fn pid => Posix.Process.kill (Posix.Process.K_PROC pid, signal)) (!child_pids)
    else ()


  fun setHandlersForSignals false = (
      MLton.Signal.setHandler (Posix.Signal.term, MLton.Signal.Handler.simple (fn () => stop := true));
      MLton.Signal.setHandler (Posix.Signal.quit, MLton.Signal.Handler.simple (fn () => quit := true))
    )
    | setHandlersForSignals true = ( (* send signal to group *)
      MLton.Signal.setHandler (Posix.Signal.term, MLton.Signal.Handler.simple (fn () => (
        stop := true;
        (* print ("Got TERM signal for " ^ (pidToString (Posix.ProcEnv.getpid ())) ^ ", main pid is " ^ (pidToString main_pid) ^ ".\n"); *)
        sendSignalToChild Posix.Signal.term
      )));
      MLton.Signal.setHandler (Posix.Signal.quit, MLton.Signal.Handler.simple (fn () => (
        quit := true;
        sendSignalToChild Posix.Signal.quit
      )))
    )


  fun doFork logger 0 f x = ()
    | doFork logger n f x =
        case Posix.Process.fork () of
             NONE => (
                child_pids := [];
                logger ("I am child, my PID is " ^ ( myPidAsString () ) ^ ".");
                f x;
                Posix.Process.exit 0w0
                )
           | SOME pid => (
               child_pids := pid::(!child_pids);
               doFork logger (n - 1) f x
             )


  fun wait logger f x =
    let
      val (pid, _) = Posix.Process.wait ()
    in
      (* logger ("Stoped " ^ pidToString pid); *)
      child_pids := List.filter (fn p => p <> pid) (!child_pids);
      if needStop () orelse needQuit () then () else doFork logger 1 f x;
      if null (!child_pids) then () else wait logger f x
    end

in
  fun runWithN logger n f x =
    if n > 0
    then (
      setHandlersForSignals true;
      logger ("My PID is " ^ ( myPidAsString () ) ^ ".");
      doFork logger n f x;
      wait logger f x
    )
    else (
      setHandlersForSignals false;
      f x
    )
end

end
