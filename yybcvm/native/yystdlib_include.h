
#include "type_defs.h"

yyvalue yyRunningOnWindows();
yyvalue yyRunningOnMacOS();
yyvalue yyRunningOnLinux();

yyvalue yyNewRef(yyvalue value);
yyvalue yyReadRef(yyvalue addr);
yyvalue yyWriteRef(yyvalue new_value, yyvalue addr);
yyvalue yyNewRefArray(yyvalue value, yyvalue lengthAddr);
yyvalue yyReadRefArray(yyvalue addr, yyvalue indexAddr);
yyvalue yyWriteRefArray(yyvalue new_value, yyvalue indexAddr, yyvalue refAddr);
yyvalue yyNewRefArrayGeneric(yyvalue lengthAddr);
yyvalue yy_豫言不安全转换(yyvalue value);

yyvalue yyGetCommandLineProgramName();
yyvalue yyGetCommandLineArgs();

yyvalue 获取当前异常处理器();
yyvalue 设置当前异常处理器(yyvalue 处理器);
yyvalue yyTopExceptHandler(yyvalue errMsg);

yyvalue yyPrintln(yyvalue s);
yyvalue yyPrintlnStdErr(yyvalue s);
yyvalue yyPrintStr(yyvalue s);
yyvalue yyReadAllStdIn();
yyvalue yyReadLineFromStdin();
yyvalue yyPrintGeneric(yyvalue msg, yyvalue obj);

yyvalue yyIsSubstring(yyvalue s1, yyvalue s2);
yyvalue yyStringEq(yyvalue s1, yyvalue s2);
yyvalue yyStringByteLength(yyvalue s1);
yyvalue yyStringByteArrayGetLength(yyvalue s1);
yyvalue yy_豫言字符串获取字节数组(yyvalue s1);
yyvalue yy_豫言字符转整数(yyvalue s1, yyvalue idx_ptr);
yyvalue yy_豫言子字符串从字节序数开始(yyvalue sVal, yyvalue idxVal);
yyvalue yy_豫言字符串匹配(yyvalue searchVal, yyvalue startIdxVal, yyvalue matchVal);
yyvalue yy_豫言字符串获取字节序数当前字符(yyvalue strVal, yyvalue idxVal);
yyvalue yy_豫言字符串获取JSON字符串(yyvalue strVal, yyvalue startIdxVal);
size_t count_utf8_code_points(const char *s);
yyvalue yyGetCodePoints(yyvalue str_addr);
yyvalue yyCodePointsConcat(yyvalue str_list_addr);



yyvalue yyReadFileSync(yyvalue filenamearg);
yyvalue yyDeleteFileSync(yyvalue filenamearg);
yyvalue yyWriteFileSync(yyvalue file_name_addr, yyvalue content_addr);
yyvalue yyListDirectorySync(yyvalue dirname);
yyvalue yyIsPathDirectory(yyvalue path);
yyvalue yyIsPathRegularFile(yyvalue path);
yyvalue yyPathExists(yyvalue path);
yyvalue yyGetFileModifiedTime(yyvalue path);
yyvalue yyGetCurrentWorkingDirectory();

yyvalue yyGetCurrentLocalDateTimeStr();
yyvalue yyGetCurrentLocalDateTimeFmt(yyvalue fmt);

yyvalue yyProcessExit(yyvalue exitStatusAddr);

yyvalue yyIntEqTest(yyvalue i1, yyvalue i2);
yyvalue yyIntGtTest(yyvalue i1, yyvalue i2);
yyvalue yyIntAdd(yyvalue i1, yyvalue i2);
yyvalue yyIntSub(yyvalue i1, yyvalue i2);
yyvalue yyIntMult(yyvalue i1, yyvalue i2);
yyvalue yyIntDiv(yyvalue i1, yyvalue i2);
yyvalue yyIntToString(yyvalue i1);
yyvalue yyDoubleAdd(yyvalue i1, yyvalue i2);
yyvalue yyDoubleSub(yyvalue i1, yyvalue i2);
yyvalue yyDoubleMult(yyvalue i1, yyvalue i2);
yyvalue yyDoubleDiv(yyvalue i1, yyvalue i2);
yyvalue yyDoubleToString(yyvalue i1);
yyvalue yyDoubleToInt(yyvalue d) ;
yyvalue yyIntToDouble(yyvalue i) ;
yyvalue yyStringToInt(yyvalue i1);
yyvalue yyStringToDouble(yyvalue i1);




yyvalue yyRunProcessGetOutputSync(yyvalue program, yyvalue arguments);
yyvalue yyRunProcessSync(yyvalue program, yyvalue arguments);
yyvalue yyRunProcessSyncPipeOutput(yyvalue program, yyvalue arguments);

yyvalue yyGetRandomInt(yyvalue upperBoundPtr);
yyvalue yyGetRandomDouble();

yyvalue yyCurrentNanosecondTime();

