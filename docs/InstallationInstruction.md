
# 安装指令

## Mac

1. 克隆本仓库
2. 安装依赖库
```
brew install bdw-gc libuv mlton matplotplusplus llvm
```
3. 安装
```
make
make install
```


## Linux (Ubuntu)

1. 克隆本仓库
2. 安装依赖库
```
sudo apt install libgc-dev libuv1-dev libbsd-dev llvm
```
3. 安装MLton
http://mlton.org

4. 安装其他依赖（现阶段安装无法运行，之后会改）
+ MatPlot++ 
https://alandefreitas.github.io/matplotplusplus/

5. 安装
```
make
make install
```

## Windows

请暂时用Cygwin或者Windows Subsystem for Linux运行。

