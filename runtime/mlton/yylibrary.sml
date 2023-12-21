
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
  | YYRef of YYValue ref
  | YYUnit 

  fun showYYValue v = 
    case v of
      YYInt i => Int.toString i
    | YYDouble d => Real.toString d
    | YYString s => "'" ^ s ^ "'"
    | YYBool b => Bool.toString b
    | YYVector v => Vector.foldl (fn (x, acc) => acc ^ showYYValue x ^ ",") "[" v ^ "]"
    | YYRef r => "ref " ^ showYYValue (!r)
    | YYFunc v => "<func>"
    | YYUnit => "()"

  fun yyVectorConstruct(l) : YYValue = YYVector (Vector.fromList l)
  fun yyVectorRefConstruct(l: YYValue list) : YYValue = YYRef (ref (YYVector (Vector.fromList l))) 
  fun yyVectorProj(i, YYVector v) : YYValue = Vector.sub(v, i)
     | yyVectorProj(i, v) : YYValue = raise Fail ("yyVectorProj: not a vector " ^ showYYValue v)
  fun yyGetString(YYString v) : string = v
      | yyGetString(v) : string = raise Fail ("yyGetString: not a string " ^ showYYValue v)
  fun yyGetInt(YYInt v) : int = v
      | yyGetInt(v) : int = raise Fail ("yyGetInt: not an int " ^ showYYValue v)
  fun yyGetDouble(YYDouble v) : real = v
      | yyGetDouble(v) : real = raise Fail ("yyGetDouble: not a double " ^ showYYValue v)
  fun yyGetBool(YYBool v) : bool = v
      | yyGetBool(v) : bool = raise Fail ("yyGetBool: not a bool " ^ showYYValue v)
  fun yyGetFunc(YYFunc v) : YYValue -> YYValue = v
      | yyGetFunc(v) : YYValue -> YYValue = raise Fail ("yyGetFunc: not a function " ^ showYYValue v)
  fun yyGetRef(YYRef v) : YYValue ref = v
      | yyGetRef(v) : YYValue ref = raise Fail ("yyGetRef: not a ref " ^ showYYValue v)
  fun yyGetVector(YYVector v) : YYValue Vector.vector = v
      | yyGetVector(v) : YYValue Vector.vector = raise Fail ("yyGetVector: not a vector " ^ showYYValue v)

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

  val yy_current_exception_handler : YYValue ref = ref (YYVector (Vector.fromList [ (YYFunc (fn arg => 
  let val _ = TextIO.output (TextIO.stdErr, 
   "!!!!!!!!!!!!!!!!!!!!!1\n" ^
      "sml yy_runtime Uncaught Exception\n" ^
      "Trying to print its value:\n" )
      val errorMsg = yyVectorProj(1, arg)
      val _ = TextIO.output (TextIO.stdErr, yyGetString errorMsg ^ "\n")
      val _ = OS.Process.exit (OS.Process.failure)
      in YYUnit end
  )), YYUnit]))
  

end

open YYBasis

(* Define external calls structure *)
structure YYExternalCalls = struct

  (* utility funcs *)

  fun encodeList (l : YYValue list) : YYValue = 
      yyVectorRefConstruct [yyVectorRefConstruct l, YYInt (length l)]

  fun decodeList (v : YYValue) : YYValue list = 
      let val v = yyVectorProj(0, !(yyGetRef v))
      in
        Vector.foldr (fn (x, acc) => x::acc) [] (yyGetVector (!(yyGetRef v)))
      end

  fun yy_get_current_exception_handler () = !yy_current_exception_handler

  fun yy_set_current_exception_handler (new_handler) = (yy_current_exception_handler := new_handler; YYUnit)

  (* commandline.c *)
  fun yyGetCommandLineProgramName () = YYString (CommandLine.name ())

  fun yyGetCommandLineArgs () = 
  let val args = CommandLine.arguments ()
  in 
  encodeList(map YYString args)
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
      val entriesList = get_dir(OS.FileSys.openDir (yyGetString dirPath))
    in
    encodeList (map YYString entriesList)
    end;

  fun yyIsPathDirectory path = YYBool (OS.FileSys.isDir (yyGetString path))

  fun yyIsPathRegularFile path = 
    YYBool (not (OS.FileSys.isDir (yyGetString path)))

  fun yyPathExists path = YYBool (OS.FileSys.access (yyGetString path, []));

  fun yyGetFileModifiedTime path =
    let
      val stats = OS.FileSys.modTime (yyGetString path);
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
  let val requested_st = (yyGetInt st)
  in if requested_st = 0 then
    OS.Process.exit OS.Process.success
    else
    OS.Process.exit (OS.Process.failure)
  end

  (* references.c *)
  fun yyNewRef value = YYRef (ref value)

  fun yyReadRef addr = ! (yyGetRef addr)

  fun yyWriteRef (new_value, addr) = 
  let val _ = (yyGetRef addr) := new_value in 
  YYUnit end

  fun yyNewRefArray (value, length) = YYRef (ref (YYVector (Vector.tabulate (yyGetInt length, (fn _ => value)))))

  fun yyReadRefArray (addr, index) = yyVectorProj (yyGetInt index, !(yyGetRef addr));

  fun yyWriteRefArray (new_value, index, refe) = 
  let val _ = (yyGetRef refe) := YYVector (Vector.update (yyGetVector (!(yyGetRef refe)), yyGetInt index, new_value))
  in YYUnit end

  fun yyNewRefArrayGeneric length =  YYRef (ref (YYVector (Vector.tabulate (yyGetInt length, (fn _ => YYUnit)))))

  fun yy_unsafe_cast value = value

  (* strings.c *)
  fun yyIsSubstring (s1, s2) =
    YYBool (String.isSubstring (yyGetString s1) (yyGetString s2));

  fun yyStringEq (s1, s2) =
    YYBool (yyGetString s1 = yyGetString s2);

  fun yyStringByteLength s1 = YYInt (String.size (yyGetString s1))

  (* in the stdlib, byte array IS string *)
  fun yy_string_get_byte_array s1 = s1

  fun yyStringByteArrayGetLength b = YYInt (String.size (yyGetString b))

  fun yy_string_byte_to_int (s1, idx) = YYInt (Char.ord (String.sub (yyGetString s1, yyGetInt idx)))

  fun yy_substring_from_byte_index (b1, idx) =
    YYString (String.extract (yyGetString b1, yyGetInt idx, NONE))

  fun yy_string_match (search, startIdx, match) =
    let
      val matchTarget = String.extract (yyGetString search, yyGetInt startIdx, NONE)
    in
      YYBool (String.isPrefix (yyGetString match) matchTarget)
    end

  fun yy_string_get_unicode_char_at_index (b, startIdx) =
  let 
    (*TODO*)
    val strL = 1
  in
    YYString (String.substring (yyGetString b, yyGetInt startIdx, strL))
  end

  fun yy_string_get_json_string (b, startIdx) =
    let
      val str = yyGetString b
      val start = yyGetInt startIdx;
    in
      if String.sub (str, start) <> #"\"" then
        raise Fail "JSON string needs to start with a quote"
      else
        let
          fun scan (endIdx, acc) =
            if String.sub (str, endIdx) = #"\"" then
              (endIdx, acc)
            else if String.sub (str, endIdx) = #"\\" then
              scan (endIdx + 2, acc@[ (case Char.fromString (String.substring (str, endIdx, 2)) of 
                SOME c => c
              | NONE => raise Fail "yy_string_get_json_string invalid translation squence with backslash")
              ])
            else
              scan (endIdx + 1, acc@[String.sub (str, endIdx)]);
        in
          case scan (start + 1, []) of
            (endIdx, acc) =>
              if String.sub (str, endIdx) <> #"\"" then
                raise Fail "JSON string needs to end with a quote"
              else
                yyVectorRefConstruct [YYString (String.implode acc), YYInt (endIdx - start + 2)]
          end
        end;

  fun count_utf8_code_points s =
    YYInt (UTF8.size (yyGetString s));

  fun yyGetCodePoints str =
  let val exploded = (map (fn x => YYString (UTF8.implode [x])) (UTF8.explode (yyGetString str)))
  in
    encodeList exploded
  end

  fun yyCodePointsConcat arr =
    YYString (String.concat (map yyGetString (decodeList arr)));

  val refTime = Time.now ();
  (* time.c *)
  fun yyCurrentNanosecondTime () =
    let
      val time = Time.now ();
    in
      YYDouble (Real.fromLargeInt (Time.toNanoseconds time))
    end

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
    YYInt (Random.randRange (0, yyGetInt upperBound)
    (Random.rand (Int.fromLarge (Time.toSeconds (Time.now ())),
    Int.fromLarge (Time.toSeconds (Time.now ()))
    )))

  fun yyGetRandomDouble () =
    YYDouble (Random.randReal (
    (Random.rand (Int.fromLarge (Time.toSeconds (Time.now ())),
    Int.fromLarge (Time.toSeconds (Time.now ()))
    ))))



end;
