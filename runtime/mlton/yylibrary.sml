
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

  (* Define global variables *)
  val yyGlobals : (string * YYValue) list ref= ref ([])

end

(* Define external calls structure *)
structure YYExternalCalls = struct

  open YYBasis

  (* commandline.c *)
  fun yyGetCommandLineProgramName () =
    OS.Process.getCmdLine () |> hd;

  fun yyGetCommandLineArgs () =
    let
      val args = tl (OS.Process.getCmdLine ());
    in
      (args, length args)
    end;

  (* file_system.c *)
  fun yyReadFileSync filename =
    let
      val fileContent = TextIO.inputAll (TextIO.openIn filename);
    in
      fileContent
    end;

  fun yyWriteFileSync (filePath, content) =
    let
      val _ = OS.FileSys.mkDir {isDir = fn _ => true, mkDir = OS.FileSys.mkDir} (FilePath.dirName filePath)
      val oc = TextIO.openOut filePath;
      val _ = TextIO.output (oc, content);
      val _ = TextIO.closeOut oc;
    in
      ()
    end;

  fun yyListDirectorySync dirPath =
    let
      val entriesList = OS.FileSys.readdir dirPath;
    in
      (entriesList, length entriesList)
    end;

  fun yyIsPathDirectory path =
    OS.FileSys.access (path, []) |> OS.FileSys.isDir;

  fun yyIsPathRegularFile path =
    OS.FileSys.access (path, []) |> OS.FileSys.isFile;

  fun yyPathExists path =
    OS.FileSys.access (path, []) handle OS.SysErr _ => false;

  fun yyGetFileModifiedTime path =
    let
      val stats = OS.FileSys.fileInfo path;
    in
      Time.toSeconds (OS.FileSys.time stats)
    end;

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
  fun yyRunningOnWindows () =
    String.isSubstring "win32" (OS.Process.platform ());

  fun yyRunningOnMacOS () =
    String.isSubstring "darwin" (OS.Process.platform ());

  fun yyRunningOnLinux () =
    String.isSubstring "linux" (OS.Process.platform ());

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
    i1 + i2;

  fun yyDoubleSub (i1, i2) =
    i1 - i2;

  fun yyDoubleMult (i1, i2) =
    i1 * i2;

  fun yyDoubleDiv (i1, i2) =
    i1 / i2;

  fun yyDoubleToString i1 =
    Real.toString i1;

  fun yyDoubleToInt d =
    Real.floor d |> Int.fromReal;

  fun yyIntToDouble i =
    Real.fromInt i;

  fun yyStringToInt s1 =
    Int.fromString s1 |> the;

  fun yyStringToDouble s1 =
    Real.fromString s1 |> the;

  (* process_exist.c *)
  fun yyProcessExit st =
    OS.Process.exit st;

  (* references.c *)
  fun yyNewRef value =
    ref (Mutable value);

  fun yyReadRef addr =
    case !addr of
      Mutable value => value
    | Immutable value => value;

  fun yyWriteRef (new_value, addr) =
    addr := Mutable new_value;

  fun yyNewRefArray (value, length) =
    Array.array (length, value);

  fun yyReadRefArray (addr, index) =
    Array.sub (!addr, index);

  fun yyWriteRefArray (new_value, index, ref) =
    Array.update (ref, index, new_value);

  fun yyNewRefArrayGeneric length =
    Array.array (length, ());

  fun yy_豫言不安全转换 value =
    value;

  (* strings.c *)
  fun yyIsSubstring (s1, s2) =
    String.isSubstring s1 s2;

  fun yyStringEq (s1, s2) =
    s1 = s2;

  fun yyStringByteLength s1 =
    size (BinPrimIO.output_string (Byte.stringToBytes s1));

  fun yy_豫言字符串获取字节数组 s1 =
    Byte.stringToBytes s1;

  fun yyStringByteArrayGetLength b =
    size b;

  fun yy_豫言字符转整数 (s1, idx) =
    let
      val buffer = Byte.stringToBytes s1;
    in
      if idx < size buffer then
        Byte.sub (buffer, idx)
      else if idx = size buffer then
        0 (* return the C null terminator *)
      else
        raise Fail "yy_豫言字符转整数 序数超限"
    end;

  fun yy_豫言子字符串从字节序数开始 (b1, idx) =
    Byte.sub (b1, idx);

  fun yy_豫言字符串匹配 (search, startIdx, match) =
    let
      val matchTarget = Byte.sub (search, startIdx) |> Byte.bytesToString;
    in
      String.isPrefix match matchTarget
    end;

  fun yy_豫言字符串获取字节序数当前字符 (b, startIdx) =
    Byte.sub (b, startIdx) |> Byte.bytesToString |> String.sub (0, 1);

  fun yy_豫言字符串获取JSON字符串 (b, startIdx) =
    let
      val str = Byte.sub (b, startIdx) |> Byte.bytesToString;
      val start = 0;
    in
      if String.sub (str, start) <> #"\"" then
        raise Fail "JSON字符串必须以引号开始"
      else
        let
          val rec scan (endIdx, acc) =
            if String.sub (str, endIdx) = #"\"" then
              (endIdx, acc)
            else if String.sub (str, endIdx) = #"\\" then
              scan (endIdx + 2, acc)
            else
              scan (endIdx + 1, acc);
        in
          case scan (start + 1, []) of
            (endIdx, acc) =>
              if String.sub (str, endIdx) <> #"\"" then
                raise Fail "JSON字符串必须以引号结束"
              else
                (String.concat acc, size (Byte.stringToBytes (String.concat acc)) + 2)
          end
        end;

  fun count_utf8_code_points s =
    size (String.explode s);

  fun yyGetCodePoints str =
    (String.explode str, size (String.explode str));

  fun yyCodePointsConcat arr =
    String.concat arr;

  (* time.c *)
  fun yyCurrentNanosecondTime () =
    let
      val time = Time.now ();
      val {sec, usec} = Time.toMilliseconds time;
    in
      sec * 1_000_000_000 + usec * 1_000
    end;

  (* child_processes.c *)
  fun yyRunProcessGetOutputSync (command, args) =
    let
      val result = Process.systemOut (command ^ " " ^ String.concatWith " " args);
    in
      (result = 0, Process.getStdOut (), Process.getStdErr ())
    end;

  fun yyRunProcessSync (command, args) =
    Process.system (command ^ " " ^ String.concatWith " " args) = 0;

  fun yyRunProcessSyncPipeOutput (command, args) =
    Process.system (command ^ " " ^ String.concatWith " " args);

  (* rand.c *)
  fun yyGetRandomInt upperBound =
    Random.rand (Random.range (0, upperBound));

  fun yyGetRandomDouble () =
    Random.randReal ();

end;

(* Define the main program *)
val _ = () (* Add your main program here *)
