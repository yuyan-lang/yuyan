-- 古曰：新户未验，官户明记。今释：只给既有官方账户授予标识，新注册用户默认未验证，客户端不能自授。
ALTER TABLE "用户" ADD COLUMN "已验证" INTEGER NOT NULL DEFAULT 0 CHECK ("已验证" IN (0,1));
UPDATE "用户" SET "已验证"=1 WHERE "编号"=1 AND "名称"='豫言';
