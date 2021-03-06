(* echo ping | nc localhost 5000 *)

fun logger msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")

fun main () =
  let

    fun handler ev (workerHookData, connectHookData) stream = (
      logger "Hello, stream.";
      case connectHookData of NONE => () | SOME data => print data;
      let
        fun rCb (stream, "")   = (logger "BY, stream (client closed socket)."; "")
          | rCb (stream, text) =
          let
            val _ = print text
          in
            NetServer.write (stream, "pong\n");
            logger "BY, stream";
            NetServer.shutdown stream;
            ""
          end
      in
        NetServer.read (stream, rCb)
      end
    )

    val settings = NetServer.Settings {
      handler        = handler,
      port           = 5000,
      host           = "*",
      acceptQueue    = 128,
      workers        = 3,    (* 0 - without workers *)
      maxRequests    = 0,    (* max requests per worker, 0 - without limit, do not use without workers *)
      reuseport      = false,
      workerHook     = SOME ( (fn ev => logger "Worker init hook."),  (fn _  => logger "Worker cleanup hook.") ),
      connectHook    = SOME ( (fn ev => (logger "Connect init hook."; "It's connect hook data.\n")), (fn _  => logger "Connect cleanup hook.") ),
      logger         = logger
    }

  in
    logger "Start.";
    NetServer.run settings
  end
