const fs = require('fs');
const os = require('os');
const path = require('path');
const process = require('process');
const { spawnSync } = require('child_process');

let yyGlobals = {};

class yyContinuationException {
    constructor(value, id) {
      this.value = value;
      this.id = id;
    }
  }

const 全局异常处理器 = (errorMsg, kont) => {
    console.error(
      "！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！\n" +
      "豫言运行环境(yy_runtime)：未捕捉的异常(Uncaught Exception)：\n" +
      "尝试打印值：（可能会出现 异常）：\n"
    );
    console.error(errorMsg);
    console.error("\n");
    process.exit(1);
    return -1;
  };

  
const to_yy_list = (arr) => {
    return [arr, arr.length]
}

let yy_tailcall = (arg) => {
  let f = arg
  while (f && typeof f === 'function') {
    f = f()
  }
}
  

let 当前异常处理器 = 全局异常处理器;
let yyExternalCalls = {

    // commandline.c
    yyGetCommandLineProgramName: () => {
        return process.argv[0];
    }, 
    yyGetCommandLineArgs: () => {
        let args = process.argv.slice(2);
        return [args, args.length];
    },

    // exception.c

    获取当前异常处理器 : () => {
        return 当前异常处理器;
    },
      
    设置当前异常处理器 : (处理器) => {
        当前异常处理器 = 处理器;
        return 0;
    },

    // file_system.c

    yyReadFileSync : (filename) => {
      
        try {
          const fileContent = fs.readFileSync(filename, 'utf-8');
          return fileContent;
        } catch (error) {
          console.error(`Error opening file: ${filename}`);
          process.exit(1)
        }
    },

    yyWriteFileSync: (filePath, content) => {
        const dirname = path.dirname(filePath);
      
        // Create intermediate directories if they don't exist
        if (!fs.existsSync(dirname)) {
          fs.mkdirSync(dirname, { recursive: true });
        }
      
        // Write content to the file
        try {
          fs.writeFileSync(filePath, content);
          // console.log(`Content successfully written to ${filePath}`);
        } catch (error) {
          console.error(`Error writing content to ${filePath}: ${error.message}`);
          process.exit(1)
        }
      }, 

      yyListDirectorySync: (dirPath) => {
        const entriesList = [];
      
        try {
          const entries = fs.readdirSync(dirPath);
          entriesList.push(...entries);
        } catch (error) {
          console.error(`Error listing entries in directory: ${dirPath}`, error);
          process.exit(1)
        }
      
        return [entriesList, entriesList.length];
    },
      
    yyIsPathDirectory : (path) => {
        try {
          const stats = fs.statSync(path);
          const isDirectory = stats.isDirectory();
          return isDirectory;
        } catch (error) {
          console.error(`Cannot stat directory: ${path}`, error);
          process.exit(1);
        }
      },

      yyIsPathRegularFile: (path) => {
        try {
          const stats = fs.statSync(path);
          const isRegularFile = stats.isFile();
          return isRegularFile;
        } catch (error) {
          console.error(`Cannot stat file: ${path}`, error);
          process.exit(1);
        }
      },

      yyPathExists : (path) => {
        try {
          fs.accessSync(path, fs.constants.F_OK);
          return true;
        } catch (error) {
          return false;
        }
      },

      yyGetFileModifiedTime : (path) => {
          try {
            const stats = fs.statSync(path);
            const modifiedTime = stats.mtimeMs;
            return Math.floor(modifiedTime/1000);
          } catch (error) {
            console.error(`Cannot stat file: ${path}`, error);
            process.exit(1);
          }
        },

        yyGetCurrentWorkingDirectory : () => {
              try {
                const currentWorkingDir = process.cwd();
                return currentWorkingDir;
              } catch (error) {
                console.error('Cannot get current working directory:', error);
                process.exit(1);
              }
            },

    // io.c 
    yyPrintln : (message) => {
            console.log(message);
        },
    yyPrintGeneric: (obj) => {
          console.log(obj)
        },

    yyPrintlnStdErr : (message) => {
            console.error(message);
    },
                
    yyPrintStr : (str) => {
        process.stdout.write(str);
      },

      //platform.c


      yyRunningOnWindows :  () => {
        return os.platform() === 'win32';
      },
      
      yyRunningOnMacOS : () => {
        return os.platform() === 'darwin';
      },
      
      yyRunningOnLinux : () => {
        return os.platform() === 'linux';
      },

      yyGetCurrentLocalDateTimeStr : () => {
        const now = new Date();
        return now.toString();
      },
      
      yyGetCurrentLocalDateTimeFmt : (fmt) => {
        const strftime = require('strftime');
        const now = new Date();
        return strftime(fmt, now);
      },

    // primop.c

    yyIntEqTest : (i1, i2) => {
        return i1 === i2;
      },
      
      yyIntAdd : (i1, i2) => {
        return i1 + i2;
      },
      
      yyIntSub : (i1, i2) => {
        return i1 - i2;
      },
      
      yyIntMult : (i1, i2) => {
        return i1 * i2;
      },
      
      yyIntDiv : (i1, i2) => {
        return Math.floor(i1 / i2);
      },
      
      yyIntToString : (i1) => {
        return i1.toString();
      },
      
      yyDoubleAdd : (i1, i2) => {
        return i1 + i2;
      },
      
      yyDoubleSub : (i1, i2) => {
        return i1 - i2;
      },
      
      yyDoubleMult : (i1, i2) => {
        return i1 * i2;
      },
      
      yyDoubleDiv : (i1, i2) => {
        return i1 / i2;
      },
      
      yyDoubleToString : (i1) => {
        return i1.toString();
      },
      
      yyDoubleToInt : (d) => {
        return Math.floor(d);
      },
      
      yyIntToDouble : (i) => {
        return i;
      },
      
      yyStringToInt : (i1) => {
        return parseInt(i1, 10);
      },
      
      yyStringToDouble : (i1) => {
        return parseFloat(i1);
      },
      
      //process_exist.c

      yyProcessExit : (st) => {
        process.exit(st);
      },
      
      // references.c
      yyNewRef : (value) => {
        const storage = [value];
        return storage;
      },
      
      yyReadRef : (addr) => {
        return addr[0];
      },
      
      yyWriteRef : (new_value, addr) => {
        addr[0] = new_value;
        return null;
      },
      
      yyNewRefArray : (value, length) => {
        const storage = new Array(length).fill(value);
        return storage;
      },
      
      yyReadRefArray : (addr, index) => {
        return addr[index];
      },
      
      yyWriteRefArray : (new_value, index, ref) => {
        ref[index] = new_value;
        return null;
      },
      
      yyNewRefArrayGeneric : (length) => {
        const storage = new Array(length).fill(null);
        return storage;
      },
      
      yy_豫言不安全转换 : (value) => {
        return value;
      },

    // strings.c

    yyIsSubstring : (s1, s2) => {
        if (s2.includes(s1)) {
          return true;
        } else {
          return false;
        }
      },
      
      yyStringEq : (s1, s2) => {
        return s1 === s2;
      },
      
      yyStringByteLength : (s1) => {
          return Buffer.from(s1, 'utf-8').length;
      },
      
      yy_豫言字符串获取字节数组 : (s1) => {
          return Buffer.from(s1, 'utf-8');
      },

    yyStringByteArrayGetLength: (b) => {
        return b.length;
      },
      
      yy_豫言字符转整数 : (s1, idx) => {
        let buffer = Buffer.from(s1, 'utf-8');
        if (idx < buffer.length) {
          return buffer.at(idx);
        } else if (idx == buffer.length) {
          return 0; // return the C null terminator //TODO: perhaps we should forbid this in stdlib
        } else {
          throw new Error("yy_豫言字符转整数 序数超限")
        }
      },
      
      yy_豫言子字符串从字节序数开始 : (b1, idx) => {
          return b1.subarray(idx);
      },
      
       yy_豫言字符串匹配 : (search, startIdx, match) => {
        const matchTarget = search.subarray(startIdx).toString('utf-8'); 
      
            if (matchTarget.startsWith(match)) {
                return true;
            } else {
                return false;
            }
      },
      
      yy_豫言字符串获取字节序数当前字符 : (b, startIdx) => {
        return b.subarray(startIdx).toString('utf-8')[0]; 
      },
      
      yy_豫言字符串获取JSON字符串 : (b, startIdx) => {
          const str = b.subarray(startIdx).toString('utf-8');
        const start = 0;
        if (str[start] !== '"') {
          throw new Error("JSON字符串必须以引号开始");
        }
        let end = start + 1;
        while (end < str.length) {
          if (str[end] === '\\') {
            end++;
            continue;
          }
          if (str[end] === '"') {
            break;
          }
          end++;
        }
        if (str[end] !== '"') {
          throw new Error("JSON字符串必须以引号结束");
        }
        const escapedStr = str.substring(1, end);
        const byteLength = Buffer.from(escapedStr, 'utf-8').length + 2; // 2 is for " end "
        return [escapedStr, byteLength];
      },
      
      count_utf8_code_points : (s) => {
          return s.length;
      },
      
      yyGetCodePoints : (str) => {
        const characters = new Array(str.length);
        for (let i = 0; i < str.length; i++) {
          characters[i] = str[i];
        }
        return [characters, characters.length];
      },
      
      yyCodePointsConcat :(arr) => {
          let resultString = arr[0].join('');
      
        return resultString;
      },

      
      // time.c
      yyCurrentNanosecondTime : () => {
        const [seconds, nanoseconds] = process.hrtime();
        const nanosecondsTotal = seconds * 1e9 + nanoseconds;
        return nanosecondsTotal;
      },
      
      //child_processes.c

      yyRunProcessGetOutputSync : (command, args) => {
        const result = spawnSync(command, args[0], { encoding: 'utf-8', stdio: 'pipe' });
        const { status, error, stdout, stderr } = result;
      
        if (error) {
          console.error(error.message);
            process.exit();
        }
      
      
        return [status === 0, stdout, stderr]; // Trim trailing newlines or whitespaces
      },
      yyRunProcessSync : (command, args) => {
        const result = spawnSync(command, args[0], { encoding: 'utf-8', stdio: 'pipe' });
        const { status, error, stdout, stderr } = result;
      
        if (error) {
          console.error(error.message);
            process.exit();
        }
      
      
        return status === 0; // Trim trailing newlines or whitespaces
      },
      yyRunProcessSyncPipeOutput : (command, args) => {
        const result = spawnSync(command, args[0], { encoding: 'utf-8', stdio: 'inherit' });
        const { status, error, stdout, stderr } = result;
      
        if (error) {
          console.error(error.message);
            process.exit();
        }
      
      
        return status; // Trim trailing newlines or whitespaces
      },
    

    // rand.c
    yyGetRandomInt: (upperBound) => {
        const randomInt = Math.floor(Math.random() * upperBound);
        return randomInt;
      },
    
    yyGetRandomDouble: () => {
    const r = Math.random();
    return r;
    },
}