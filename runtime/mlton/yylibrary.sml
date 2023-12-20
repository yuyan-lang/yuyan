
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
  | YYVector of YYValue vector

  (* Define global variables *)
  val yyGlobals : (string * YYValue) list ref= ref ([])

  fun yyGlobalsGet name =
    case List.find (fn (n, _) => n = name) (!yyGlobals) of
      SOME (_, value) => value
    | NONE => raise Fail ("yyGlobalsGet: " ^ name ^ " not found")
  
  fun yyGlobalsSet (name, value) =
    yyGlobals := (name, value) :: List.filter (fn (n, _) => n <> name) (!yyGlobals)

end

open YYBasis

(* Define external calls structure *)
structure YYExternalCalls = struct


  (* commandline.c *)
  fun yyGetCommandLineProgramName () = CommandLine.name ()

  fun yyGetCommandLineArgs () = CommandLine.arguments ()

  (* file_system.c *)
  fun yyReadFileSync filename =
    let
      val fileContent = TextIO.inputAll (TextIO.openIn filename);
    in
      fileContent
    end;

  fun yyWriteFileSync (filePath, content) =
    let
      (* val _ = OS.FileSys.mkDir {isDir = fn _ => true, mkDir = OS.FileSys.mkDir} (FilePath.dirName filePath) *)
      val oc = TextIO.openOut filePath;
      val _ = TextIO.output (oc, content);
      val _ = TextIO.closeOut oc;
    in
      ()
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

  fun yyIsPathDirectory path = OS.FileSys.isDir path

  fun yyIsPathRegularFile path = not (OS.FileSys.isDir path)

  fun yyPathExists path = OS.FileSys.access (path, []) handle OS.SysErr _ => false;

  fun yyGetFileModifiedTime path =
    let
      val stats = OS.FileSys.modTime path;
    in
      Time.toSeconds stats
    end

  fun yyGetCurrentWorkingDirectory () =
    OS.FileSys.getDir ();

  (* io.c *)
  fun yyPrintln message =
    TextIO.output (TextIO.stdOut, message ^ "\n");

  fun yyPrintGeneric obj =
    print (Int.toString obj);

  fun yyPrintlnStdErr message =
    TextIO.output (TextIO.stdErr, message ^ "\n");

  fun yyPrintStr str =
    TextIO.output (TextIO.stdOut, str);

  (* platform.c *)
  fun yyRunningOnWindows () = false
    (* String.isSubstring "win32" (OS.Process.platform ()); *)

  fun yyRunningOnMacOS () = false
    (* String.isSubstring "darwin" (OS.Process.platform ()); *)

  fun yyRunningOnLinux () = true
    (* String.isSubstring "linux" (OS.Process.platform ()); *)

  fun yyGetCurrentLocalDateTimeStr () =
    Time.toString (Time.now ());

  fun yyGetCurrentLocalDateTimeFmt fmt =
    let
      val now = Time.now ();
    in
      Time.fmt fmt now
    end;

  (* primop.c *)
  fun yyIntEqTest (i1, i2) =
    i1 = i2;

  fun yyIntAdd (i1, i2) =
    i1 + i2;

  fun yyIntSub (i1, i2) =
    i1 - i2;

  fun yyIntMult (i1, i2) =
    i1 * i2;

  fun yyIntDiv (i1, i2) =
    i1 div i2;

  fun yyIntToString i1 =
    Int.toString i1;

  fun yyDoubleAdd (i1, i2) =
  let open Real in i1 + i2 end

  fun yyDoubleSub (i1, i2) =
  let open Real in i1 - i2 end

  fun yyDoubleMult (i1, i2) =
  let open Real in i1 * i2 end

  fun yyDoubleDiv (i1, i2) =
  let open Real in i1 / i2 end

  fun yyDoubleToString i1 =
    Real.toString i1;

  fun yyDoubleToInt d =
    Real.floor d;

  fun yyIntToDouble i =
    Real.fromInt i;

  fun yyStringToInt s1 =
    Int.fromString s1 

  fun yyStringToDouble s1 =
    Real.fromString s1

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
