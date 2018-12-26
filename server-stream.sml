structure NetServerStream =
struct

open Ev

val chunksize       = 64 * 1024 (* ToDo *)
val maxWriteBufSize = 10 * 1024 * 1024

datatype stream_state = OpenState | ShutdownState | CloseState

datatype ('af, 'sock_type) netStream = NetStream of {
  ev:       ev,
  sock:     ('af, 'sock_type) Socket.sock,
  fd:       int, (* FD of sock *)
  rBuf:     string ref,
  rBufSize: int ref,
  rCb:      (('af, 'sock_type) netStream * string -> string) option ref, (* return tail and is read watch flag *)
  wBuf:     string list ref,
  wBufSize: int ref,
  wWatched: bool ref,
  state:    stream_state ref,
  closeCb:  unit -> unit
}


val sockToEvFD : ('a, 'b) Socket.sock -> int = fn sock => (SysWord.toInt o Posix.FileSys.fdToWord o Option.valOf o Posix.FileSys.iodToFD o Socket.ioDesc) sock


fun stream (ev, sock, closeCb) = NetStream {
    ev      = ev,
    sock    = sock,
    fd      = sockToEvFD sock,
    rBuf     = ref "",
    rBufSize = ref 0,
    rCb      = ref NONE,
    wBuf     = ref [],
    wBufSize = ref 0,
    wWatched = ref false,
    state    = ref OpenState,
    closeCb  = closeCb
  }


fun close (NetStream stream) = if !(#state stream) = CloseState then () else
  let
    val m = [evDelete (#fd stream, evRead)]
    val _ = (#rCb stream) := NONE;
    val m = if !(#wWatched stream) then ( (#wWatched stream) := false ; evDelete (#fd stream, evWrite) :: m ) else m
  in
     evModify (#ev stream) m;
     (#state stream) := CloseState;
     (#closeCb stream) ()
  end

fun shutdown (NetStream stream) =
  if !(#wBufSize stream) = 0
  then close (NetStream stream)
  else (#state stream) := ShutdownState


local

fun writeCb stream _ =
  let
    val buf     = !(#wBuf stream)
    val bufSize = !(#wBufSize stream)
    val text    = String.concat (List.rev buf)
    val data    = Word8VectorSlice.full (Byte.stringToBytes text)
    val n       = valOf (Socket.sendVecNB (#sock stream, data))
  in
    if n = bufSize
    then (
      (#wBuf     stream) := [];
      (#wBufSize stream) := 0;
      (#wWatched stream) := false;
      evModify (#ev stream) [evDelete (#fd stream, evWrite)];
      case !(#state stream) of ShutdownState => close (NetStream stream) | _ => ()
    )
    else
      let
        val text = (String.extract (text, n, NONE))
      in
        (#wBuf     stream) := [text];
        (#wBufSize stream) := bufSize - n;
        ()
      end
  end

in

fun write (NetStream stream, text) = if !(#state stream) = CloseState then 0 else
  let
    val _ = (#wBuf stream)     := text :: !(#wBuf stream)
    val _ = (#wBufSize stream) := (String.size text) + !(#wBufSize stream)
  in
    if !(#wBufSize stream) <= maxWriteBufSize then (
      if !(#wWatched stream) then 0 else (
        (#wWatched stream) := true;
        evModify (#ev stream) [evAdd (#fd stream, evWrite, writeCb stream)]
      )
    ) else (
      print "Maximum write buffer size exceeded.\n";
      close (NetStream stream);
      0
    )
  end

end


local

fun readCb stream _ =
  let
    val data = valOf (Socket.recvVecNB (#sock stream, chunksize))
    val text = Byte.bytesToString data
    val text = (!(#rBuf stream)) ^ text
    val cb   = valOf (!(#rCb stream))
    val t    = cb (NetStream stream, text)
    val _ = (#rBuf stream)     := t
    val _ = (#rBufSize stream) := 0
  in
    if Word8Vector.length data = 0 then close (NetStream stream) else ()
  end

in

fun read (NetStream stream, cb) = if !(#state stream) = CloseState then () else
  case !(#rCb stream) of
      SOME rCb =>   (#rCb stream) := SOME cb
    | NONE     => ( (#rCb stream) := SOME cb; evModify (#ev stream) [evAdd (#fd stream, evRead, readCb stream)]; () )

end

end
