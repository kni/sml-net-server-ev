structure NetServerStream =
struct

datatype ('af, 'sock_type) netStream = NetStream of {
  ev:       Ev.ev,
  sock:     ('af, 'sock_type) Socket.sock,
  fd:       int, (* FD of sock *)
  rBuf:     string ref,
  rBufSize: int ref,
  rCb:      (('af, 'sock_type) netStream * string -> string) option ref, (* return tail and is read watch flag *)
  wBuf:     string list ref,
  wBufSize: int ref,
  wWatched: bool ref,
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
    closeCb  = closeCb
  }

val chunksize = 64 * 1024 (* ToDo *)

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
      Ev.evModify (#ev stream) [Ev.evDelete (#fd stream, Ev.evWrite)];
      ()
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

fun write (NetStream stream, text) =
  let

    val _ = (#wBuf stream)     := text :: !(#wBuf stream)
    val _ = (#wBufSize stream) := (String.size text) + !(#wBufSize stream)
  in
    if !(#wWatched stream) then 0 else (
      (#wWatched stream) := true;
      Ev.evModify (#ev stream) [Ev.evAdd (#fd stream, Ev.evWrite, writeCb stream)]
    )
  end

end


fun close (NetStream stream) = (
     Ev.evModify (#ev stream) [Ev.evDelete (#fd stream, Ev.evWrite), Ev.evDelete (#fd stream, Ev.evRead)];
     (#closeCb stream) ()
  )


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

fun read (NetStream stream, cb) =
  case !(#rCb stream) of
      SOME rCb =>   (#rCb stream) := SOME cb
    | NONE     => ( (#rCb stream) := SOME cb; Ev.evModify (#ev stream) [Ev.evAdd (#fd stream, Ev.evRead, readCb stream)]; () )

end

end
