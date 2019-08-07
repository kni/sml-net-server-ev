structure NetServer :
sig

type ev

type stream = (INetSock.inet, Socket.active Socket.stream) NetServerStream.netStream

datatype ('c, 'd) settings = Settings of {
  handler      : ev -> ('c option * 'd option) -> stream -> unit,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((ev -> 'c) * ('c -> unit)) option,
  connectHook  : ((ev -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}

val run: ('c, 'd) settings -> unit

val needStop: unit -> bool

val read:     stream * (stream * string -> string) -> unit
val write:    stream * string -> int
val shutdown: stream -> unit
val close:    stream -> unit

end
=
struct

type ev = Ev.ev

type stream = (INetSock.inet, Socket.active Socket.stream) NetServerStream.netStream

val read     = NetServerStream.read
val write    = NetServerStream.write
val shutdown = NetServerStream.shutdown
val close    = NetServerStream.close

open NetServer

datatype ('c, 'd) settings = Settings of {
  handler      : ev -> ('c option * 'd option) -> stream -> unit,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((ev -> 'c) * ('c -> unit)) option,
  connectHook  : ((ev -> 'd) * ('d -> unit)) option,
  logger       : string -> unit
}

type listenSocket = (INetSock.inet, Socket.passive Socket.stream) Socket.sock
datatype ListenSocket = ListenSocket of listenSocket | GetListenSocket of unit -> listenSocket


val evTimeout = Time.fromSeconds 1

val sockToEvFD : ('a, 'b) Socket.sock -> int = fn sock => (SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o Socket.ioDesc) sock


fun run'' (settings as {host = host, port = port, reuseport = reuseport, logger = logger, ...}) =
  let

    open Ev

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

        val ev = evInit ()

        val handler = #handler settings

        val workerHook = #workerHook settings
        val workerHookData = case workerHook of NONE => NONE | SOME (init, cleanup) => SOME (init ev)

        val connectHook = #connectHook settings

        val maxRequests = #maxRequests settings

        val streamHash = HashArrayInt.hash 1000

        val socketCnt = ref 0
        val requestCnt = ref 0

        fun acceptCb _ = case Socket.acceptNB listenSock of NONE => () (* Other worker was first *) | SOME (sock, _) =>
          let
            val evFD = sockToEvFD sock
            val connectHookData = case connectHook of NONE => NONE | SOME (init, cleanup) => SOME (init ev)
            val _ = Socket.Ctl.setKEEPALIVE (sock, true)

            fun closeCb () = (
              case connectHook of NONE => () | SOME (init, cleanup) => cleanup (valOf connectHookData);
              HashArrayInt.delete (streamHash, evFD);
              socketCnt := !socketCnt - 1;
              Socket.close sock
            )

            val stream = NetServerStream.stream (ev, sock, closeCb)
            val _ = HashArrayInt.update (streamHash, evFD, stream)
            val _ = socketCnt := !socketCnt + 1
            val _ = if maxRequests > 0 then requestCnt := !requestCnt + 1 else ()
          in
            handler ev (workerHookData, connectHookData) stream handle exc => logger ("function handler raised an exception: " ^ exnMessage exc);
            ()
          end

        val _ =  evModify ev [evAdd (listenEvFD, evRead, acceptCb)]

        fun loop () =
          let
            val wait_cnt = evWait ev (SOME evTimeout)
          in
            if needStop () then () else
            if needQuit () orelse (maxRequests > 0 andalso !requestCnt >= maxRequests)
            then (
                if !socketCnt > 0
                then loop ()
                else ()
            )
            else loop ()
          end
      in
        loop ();
        HashArrayInt.fold (fn (evFD, stream, ()) => NetServerStream.close stream) () streamHash; (* clean *)
        evModify ev [evDelete (listenEvFD, evRead)];
        case workerHook of NONE => () | SOME (init, cleanup) => cleanup (valOf workerHookData)
      end
  in
    runWithN logger (#workers settings) doListen maybeListenSock;
    case maybeListenSock of ListenSocket sock => Socket.close sock | _ => ()
  end


fun run (Settings settings) = run' run'' settings handle exc => (#logger settings) ("function run raised an exception: " ^ exnMessage exc)

end
