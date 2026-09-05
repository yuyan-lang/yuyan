import { Container } from "@cloudflare/containers";
import {
  下载已发布包,
  处理数据库,
  处理摘要,
  处理待发布对象,
  处理上传入口,
  处理固化,
  回应错误,
  是包下载路径,
  是包数据上传路径,
  上传待发布包,
} from "./桥接操作.js";

export { ContainerProxy } from "@cloudflare/containers";
export {
  处理数据库,
  处理摘要,
  处理待发布对象,
  处理上传入口,
  处理固化,
  是包下载路径,
  是包数据上传路径,
  上传待发布包,
} from "./桥接操作.js";

// 古曰：桥之任，惟通两岸，不断其狱。
// 今释：本文件只装配 Container、D1/R2 桥和入口路由，不包含包管理业务规则。

export class PackageRegistryContainer extends Container {
  defaultPort = 8080;
  requiredPorts = [8080];
  sleepAfter = "10m";
  enableInternet = false;
  // 古曰：小室不可先辟巨府，量其室而节其栈与堆。
  // 今释：lite 容器仅有 256 MiB；现行栈变量仍以百万个十六字节槽位计，故 2 表示 32 MiB。
  envVars = {
    YY_GC_INITIAL_STACK_SIZE_MB: "2",
    YY_GC_INITIAL_HEAP_SIZE_MB: "4",
    YY_GC_MAX_HEAP_SIZE_MB: "64",
  };

  onStart() {
    console.log("豫言包管理容器已启动");
  }

  onStop(信息) {
    console.error("豫言包管理容器已停止", 信息);
  }

  onError(错误) {
    console.error("豫言包管理容器错误", 错误);
    throw 错误;
  }
}

PackageRegistryContainer.outboundByHost = {
  "database.internal": 处理数据库,
  "digest.internal": 处理摘要,
  "packages.internal": 处理待发布对象,
  "upload.internal": 处理上传入口,
  "publish.internal": 处理固化,
};

export default {
  async fetch(请求, 环境) {
    try {
      const 文件名 = 是包下载路径(请求);
      if (文件名 !== null) return await 下载已发布包(请求, 环境, 文件名);
      const 上传摘要 = 是包数据上传路径(请求);
      if (上传摘要 !== null) return await 上传待发布包(请求, 环境, 上传摘要);

      const 标头们 = new Headers(请求.headers);
      try {
        const 编码路径 = new URL(请求.url).pathname;
        decodeURIComponent(编码路径);
        标头们.set("X-Yuyan-Path", 编码路径);
      } catch {
        return 回应错误(400, "网址路径的百分号编码无效");
      }
      const 容器 = 环境.PACKAGE_CONTAINER.getByName("豫言包管理");
      return await 容器.fetch(new Request(请求, { headers: 标头们 }));
    } catch (错误) {
      console.error("包管理云桥内部错误", 错误);
      return 回应错误(500, "包管理服务内部错误");
    }
  },
};
