structure NetServer :
sig

type stream = (INetSock.inet, Socket.active Socket.stream) NetServerStream.netStream

datatype ('c, 'd) settings = Settings of {
  handler      : ('c option * 'd option) -> stream -> unit,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((unit -> 'c) * ('c -> unit)) option,
  connectHook  : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}

val run: ('c, 'd) settings -> unit

val needStop: unit -> bool

val read:  stream * (stream * string -> string) -> unit
val write: stream * string -> int
val close: stream -> unit

end
=
struct

type stream = (INetSock.inet, Socket.active Socket.stream) NetServerStream.netStream

val read  = NetServerStream.read
val write = NetServerStream.write
val close = NetServerStream.close

open NetServer

datatype ('c, 'd) settings = Settings of {
  handler      : ('c option * 'd option) -> stream -> unit,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((unit -> 'c) * ('c -> unit)) option,
  connectHook  : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}


datatype ('a, 'b) ListenSocket = ListenSocket of ('a, 'b) Socket.sock | GetListenSocket of unit -> ('a, 'b) Socket.sock



val sockToEvFD : ('a, 'b) Socket.sock -> int = fn sock => (SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o Socket.ioDesc) sock


fun run'' (settings as {host = host, port = port, reuseport = reuseport, logger = logger, ...}) =
  let

    val addr = if host = "*" then INetSock.any port else
      case NetHostDB.fromString host of NONE => INetSock.any port | SOME h => INetSock.toAddr(h, port)

    fun listen () =
      let
        val sock = INetSock.TCP.socket ()
        val fd = sockToEvFD sock
      in
        logger ("Listening on " ^ host ^ ":" ^ (Int.toString port) ^ ".");
        Socket.Ctl.setREUSEADDR (sock, true);
        if reuseport then setsockopt_REUSEPORT fd else ();
        Socket.bind (sock, addr);
        Socket.listen (sock, (#acceptQueue settings));
        sock
      end

    val maybeListenSock = if reuseport then GetListenSocket listen else ListenSocket (listen ())

    fun doListen maybeListenSock =
      let
        val listenSock = case maybeListenSock of ListenSocket sock => sock | GetListenSocket f => f ()
        val listenEvFD = sockToEvFD listenSock

        val handler = #handler settings

        val workerHook = #workerHook settings
        val workerHookData = case workerHook of NONE => NONE | SOME (init, cleanup) => SOME (init ())

        val connectHook = #connectHook settings

        val ev = Ev.evInit ()

        val streamHash = HashArrayInt.hash 1000


        fun acceptCb _ = case Socket.acceptNB listenSock of NONE => () (* Other worker was first *) | SOME (sock, _) =>
          let
            val evFD = sockToEvFD sock
            val connectHookData = case connectHook of NONE => NONE | SOME (init, cleanup) => SOME (init ())
            val _ = Socket.Ctl.setKEEPALIVE (sock, true)

            fun closeCb () = (
              case connectHook of NONE => () | SOME (init, cleanup) => cleanup (valOf connectHookData);
              HashArrayInt.delete (streamHash, evFD);
              Socket.close sock
            )

            val stream = NetServerStream.stream (ev, sock, closeCb)
            val _ = HashArrayInt.update (streamHash, evFD, stream)
          in
            handler (workerHookData, connectHookData) stream handle exc => logger ("function handler raised an exception: " ^ exnMessage exc);
            ()
          end

        val _ =  Ev.evModify ev [Ev.evAdd (listenEvFD, Ev.evRead, acceptCb)]

        val timeout = Time.fromSeconds 3 (* ToDo *)

        fun loop () =
          let
             val wait_cnt = Ev.evWait ev (SOME timeout)
          in
            if needStop () then () else loop ()
          end
      in
        loop ();
        HashArrayInt.fold (fn (evFD, stream, ()) => NetServerStream.close stream) () streamHash; (* clean *)
        Ev.evModify ev [Ev.evDelete (listenEvFD, Ev.evRead)];
        case workerHook of NONE => () | SOME (init, cleanup) => cleanup (valOf workerHookData)
      end
  in
    runWithN (#workers settings) doListen maybeListenSock;
    case maybeListenSock of ListenSocket sock => Socket.close sock | _ => ()
  end


fun run (Settings settings) = run' run'' settings handle exc => (#logger settings) ("function run raised an exception: " ^ exnMessage exc)

end
