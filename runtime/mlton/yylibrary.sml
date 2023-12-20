
(* Import required modules *)
open TextIO;
open OS;
open BinIO;
open BinPrimIO;
open Process;

structure YYBasis = struct

  datatype YYValue =
    YYInt of int
  | YYDouble of real
  | YYString of string
  | YYBool of bool
  | YYVector of YYValue Vector.vector
  | YYFunc of YYValue -> YYValue
  | YYUnit 

  fun showYYValue v = 
    case v of
      YYInt i => Int.toString i
    | YYDouble d => Real.toString d
    | YYString s => "'" ^ s ^ "'"
    | YYBool b => Bool.toString b
    | YYVector v => Vector.foldl (fn (x, acc) => acc ^ showYYValue x ^ ",") "[" v ^ "]"
    | YYFunc v => "<func>"
    | YYUnit => "()"

  fun yyVectorConstruct(l) : YYValue = YYVector (Vector.fromList l)
  fun yyVectorProj(i, YYVector v) : YYValue = Vector.sub(v, i)
  fun yyGetString(YYString v) : string = v
  fun yyGetInt(YYInt v) : int = v
  fun yyGetDouble(YYDouble v) : real = v
  fun yyGetBool(YYBool v) : bool = v
  fun yyGetFunc(YYFunc v) : YYValue -> YYValue = v

  (* Define global variables *)
  val yyGlobals : (string * YYValue) list ref= ref ([])

  fun yyGlobalsGet name =
    case List.find (fn (n, _) => n = name) (!yyGlobals) of
      SOME (_, value) => value
    | NONE => raise Fail ("yyGlobalsGet: " ^ name ^ " not found")
  
  fun yyGlobalsSet (name, value) =
    yyGlobals := (name, value) :: List.filter (fn (n, _) => n <> name) (!yyGlobals)

    fun yyIntEq (YYInt i1, YYInt i2) = YYBool (i1 = i2)
    fun yyIntGt (YYInt i1, YYInt i2) = YYBool (i1 > i2)
    fun yyIntSubtract (YYInt i1, YYInt i2) = YYInt (i1 - i2)

  val yy_current_exception_handler : YYValue ref = ref (YYFunc (fn arg => 
  let val _ = TextIO.output (TextIO.stdErr, 
   "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n" ^
      "豫言运行环境(yy_runtime)：未捕捉的异常(Uncaught Exception)：\n" ^
      "尝试打印值：（可能会出现 异常）：\n" )
      val errorMsg = yyVectorProj(1, arg)
      val _ = TextIO.output (TextIO.stdErr, yyGetString errorMsg ^ "\n")
      in YYUnit end
  ))
  


end

open YYBasis

(* Define external calls structure *)
structure YYExternalCalls = struct


  fun yy_get_current_exception_handler () = !yy_current_exception_handler

  fun yy_set_current_exception_handler (new_handler) = (yy_current_exception_handler := new_handler; YYUnit)

  (* commandline.c *)
  fun yyGetCommandLineProgramName () = YYString (CommandLine.name ())

  fun yyGetCommandLineArgs () = 
  let val args = CommandLine.arguments ()
  in 
  yyVectorConstruct([YYInt (length args), 
    yyVectorConstruct (map YYString args)]
  )
  end

  (* file_system.c *)
  fun yyReadFileSync filename =
    let
      val fileContent = TextIO.inputAll (TextIO.openIn (yyGetString filename));
    in
      YYString fileContent
    end

  fun makeDirExist (dirpath : string) : unit = 
  if OS.FileSys.access (dirpath, []) then
    ()
  else
    let
      val _ = makeDirExist (#dir (OS.Path.splitDirFile dirpath))
    in
      OS.FileSys.mkDir dirpath
    end


  fun yyWriteFileSync (filePath, content) =
    let
      val strFp = yyGetString filePath
      val _ = makeDirExist (#dir (OS.Path.splitDirFile (strFp)));
      val oc = TextIO.openOut strFp;
      val _ = TextIO.output (oc, yyGetString content);
      val _ = TextIO.closeOut oc;
    in
      YYUnit
    end;

  fun yyListDirectorySync dirPath =
    let
      fun get_dir(stream) = 
        case OS.FileSys.readDir stream of 
        SOME(entry) => entry::get_dir(stream)
      | NONE => []
      val entriesList = get_dir(OS.FileSys.openDir dirPath)
    in
      (Array.fromList entriesList, length entriesList)
    end;

  fun yyIsPathDirectory path = YYBool (OS.FileSys.isDir (yyGetString path))

  fun yyIsPathRegularFile path = 
    YYBool (not (OS.FileSys.isDir (yyGetString path)))

  fun yyPathExists path = YYBool (OS.FileSys.access (yyGetString path, []));

  fun yyGetFileModifiedTime path =
    let
      val stats = OS.FileSys.modTime path;
    in
      YYInt (Int.fromLarge (Time.toSeconds stats))
    end

  fun yyGetCurrentWorkingDirectory () =
    YYString (OS.FileSys.getDir ());

  (* io.c *)
  fun yyPrintln message =
  let val _ = TextIO.output (TextIO.stdOut, yyGetString message ^ "\n")
  in YYUnit end

  fun yyPrintGeneric obj = raise Fail "yyPrintGeneric: not implemented"

  fun yyPrintlnStdErr message =
  let val _ = TextIO.output (TextIO.stdErr, yyGetString message ^ "\n")
  in YYUnit end

  fun yyPrintStr str =
  let val _ = TextIO.output (TextIO.stdOut, yyGetString str)
  in YYUnit end

  (* platform.c *)
  fun yyRunningOnWindows () = YYBool false
    (* String.isSubstring "win32" (OS.Process.platform ()); *)

  fun yyRunningOnMacOS () =  YYBool false
    (* String.isSubstring "darwin" (OS.Process.platform ()); *)

  fun yyRunningOnLinux () = YYBool true
    (* String.isSubstring "linux" (OS.Process.platform ()); *)

  fun yyGetCurrentLocalDateTimeStr () =
    YYString (Time.toString (Time.now ()));

  fun yyGetCurrentLocalDateTimeFmt fmt =
    let
    in
      YYString ( Date.fmt (yyGetString fmt) (Date.fromTimeLocal (Time.now())))
    end

  (* primop.c *)
  fun yyIntEqTest (i1, i2) =
  YYBool (yyGetInt i1 = yyGetInt i2)

  fun yyIntAdd (i1, i2) =
  YYInt (yyGetInt i1 + yyGetInt i2)

  fun yyIntSub (i1, i2) =
  YYInt (yyGetInt i1 - yyGetInt i2)

  fun yyIntMult (i1, i2) =
  YYInt (yyGetInt i1 * yyGetInt i2)

  fun yyIntDiv (i1, i2) =
  YYInt ((yyGetInt i1) div (yyGetInt i2))

  fun yyIntToString i1 =
  YYString (Int.toString (yyGetInt i1))

  fun yyDoubleAdd (i1, i2) =
  let open Real in 
  YYDouble (yyGetDouble i1 + yyGetDouble i2)
  end

  fun yyDoubleSub (i1, i2) =
  let open Real in 
  YYDouble (yyGetDouble i1 - yyGetDouble i2)
  end

  fun yyDoubleMult (i1, i2) =
  let open Real in
  YYDouble (yyGetDouble i1 * yyGetDouble i2)
  end

  fun yyDoubleDiv (i1, i2) =
  let open Real in
  YYDouble (yyGetDouble i1 / yyGetDouble i2)
  end

  fun yyDoubleToString i1 =
  YYString (Real.toString (yyGetDouble i1))

  fun yyDoubleToInt d =
  YYInt (Real.floor (yyGetDouble d))

  fun yyIntToDouble i =
  YYDouble (Real.fromInt (yyGetInt i))

  fun yyStringToInt s1 =
  YYInt (case Int.fromString (yyGetString s1) of 
  SOME i => i
  | NONE => raise Fail ("yyStringToInt: invalid int " ^ yyGetString s1)
  )

  fun yyStringToDouble s1 =
  YYDouble (case Real.fromString (yyGetString s1) of 
  SOME d => d
  | NONE => raise Fail ("yyStringToDouble: invalid double " ^ yyGetString s1)
  )

  (* process_exist.c *)
  fun yyProcessExit st =
    OS.Process.exit st;

  (* references.c *)
  fun yyNewRef value = ref value

  fun yyReadRef addr = !addr

  fun yyWriteRef (new_value, addr) = addr := new_value;

  fun yyNewRefArray (value, length) = Array.array (length, value);

  fun yyReadRefArray (addr, index) = Array.sub (addr, index);

  fun yyWriteRefArray (new_value, index, refe) = Array.update (refe, index, new_value);

  fun yyNewRefArrayGeneric length = Array.array (length, ())

  fun yy_unsafe_cast value = value

  (* strings.c *)
  fun yyIsSubstring (s1, s2) =
    String.isSubstring s1 s2;

  fun yyStringEq (s1, s2) =
    s1 = s2;

  fun yyStringByteLength s1 = String.size s1

  fun yy_string_get_byte_array s1 = s1

  fun yyStringByteArrayGetLength b = String.size b

  fun yy_string_byte_to_int (s1, idx) = String.sub (s1, idx)

  fun yy_substring_from_byte_index (b1, idx) =
    String.extract (b1, idx, NONE);

  fun yy_string_match (search, startIdx, match) =
    let
      val matchTarget = String.extract (search, startIdx, NONE)
    in
      String.isPrefix match matchTarget
    end

  fun yy_string_get_unicode_char_at_index (b, startIdx) =
  let 
    (* fun computeLength(cur) = 
      let val thisChar = String.sub (b, startIdx + cur) in

      if Word8.toInt (Word8.andb ((Word8.fromInt (Char.ord thisChar)),(Word8.fromInt 192 (*0xC0*)))) = 128 (*0x80*) then
        cur
      else
        computeLength(cur + 1)
        end
    val strL = computeLength(1) *)
    val strL = 1
  in
    String.substring (b, startIdx, strL)
  end

  fun yy_string_get_json_string (b, startIdx) =
    let
      val str = b
      val start = 0;
    in
      if String.sub (str, start) <> #"\"" then
        raise Fail "JSON字符串必须以引号开始"
      else
        let
          fun scan (endIdx, acc) =
            if String.sub (str, endIdx) = #"\"" then
              (endIdx, acc)
            else if String.sub (str, endIdx) = #"\\" then
              scan (endIdx + 2, acc@[ (case Char.fromString (String.substring (str, endIdx, 2)) of 
                SOME c => c
              | NONE => raise Fail "yy_string_get_json_string: 无效的转义字符")
              ])
            else
              scan (endIdx + 1, acc@[String.sub (str, endIdx)]);
        in
          case scan (start + 1, []) of
            (endIdx, acc) =>
              if String.sub (str, endIdx) <> #"\"" then
                raise Fail "JSON字符串必须以引号结束"
              else
                (String.implode acc, endIdx - start + 2)
          end
        end;

  fun count_utf8_code_points s =
    UTF8.size s;

  fun yyGetCodePoints str =
  let val exploded = (map (fn x => UTF8.implode [x]) (UTF8.explode str))
  in
    (Array.fromList exploded, length exploded) end

  fun yyCodePointsConcat arr =
    String.concat arr;

  (* time.c *)
  fun yyCurrentNanosecondTime () =
    let
      val time = Time.now ();
    in
      0
    end;

  (* child_processes.c *)
  fun yyRunProcessGetOutputSync (command, args) =
  raise Fail "yyRunProcessGetOutputSync: not implemented"
    (* let
    open Array
     val ccall = _import "yyRunProcessGetOutputSync" public : string * (string array) array -> string array;
     (* val ffi = _import "ffi" public: real array * int * int ref * char ref * int -> char; *)
    in
      ccall (command, args)
    end; *)

  fun yyRunProcessSync (command, args) = raise Fail "yyRunProcessSync: not implemented"
    (* Process.system (command ^ " " ^ String.concatWith " " args) = 0; *)

  fun yyRunProcessSyncPipeOutput (command, args) = raise Fail "yyRunProcessSyncPipeOutput: not implemented"
    (* Process.system (command ^ " " ^ String.concatWith " " args); *)

  (* rand.c *)
  fun yyGetRandomInt upperBound =
      Random.randRange (0, upperBound)
    (Random.rand (Int.fromLarge (Time.toSeconds (Time.now ())),
    Int.fromLarge (Time.toSeconds (Time.now ()))
    ));

  fun yyGetRandomDouble () =
    Random.randReal (
    (Random.rand (Int.fromLarge (Time.toSeconds (Time.now ())),
    Int.fromLarge (Time.toSeconds (Time.now ()))
    )));



end;

(* Define the main program *)
val _ = () (* Add your main program here *)
