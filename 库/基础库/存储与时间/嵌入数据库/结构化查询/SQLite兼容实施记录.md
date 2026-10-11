# 结构化查询 SQLite 兼容实施记录

2026-10-10 起实施。目标：云仓 Worker 应用的验收改在纯豫言里运行时，关系数据库接口（`豫言操作系统/适配/关系数据库通用`）背后的嵌入数据库按 SQLite 的语义执行云仓发给 D1 的全部 SQL。本记录先列盘点结论，再写实现结构、与 SQLite 的已知差异和验证。

## 一、盘点来源

- 云仓 `应用/包管理服务/迁移/` 的 24 条迁移（0001–0024，共 634 行），其中 CREATE TRIGGER 48 处。
- 云仓豫言源码里经关系数据库接口发出的语句（取自云仓 main a3fa7f2）：
  - 第一阶段：`库/身份`（会话查询、主凭据查询、金额单位查询、三条“记最后使用时间”的 UPDATE）、`库/限流`（UPSERT … RETURNING）、`库/云计费`（费率查询、用量写入、旧事件查询、管理员与账户目录、审计写入、钱包、全平台钱包、分组细项、分日、事件明细、汇总、事件页）。
  - 第二阶段：其余 105 条，按文件为 `应用/账户服务/库/账户/账户语句。豫` 23、`命令行登录。豫` 2、`凭据管理。豫` 4、`库/AI市场逻辑/账本SQL。豫` 28、`应用/包管理服务/库/` 下的公开包 1、包名表 4、发布查询 5、发布身份 1、归档发布 5、构建任务 16、贡献协议 6、阅读处理 8（其中 5 条经“关系查询”窄接口）、验收应用 2。
- 云仓 JS 测试 `应用/包管理服务/测试/AI市场账本.test.js`、`人民币迁移.test.js` 的用例（已改写进对照用例）。

## 二、要支持的语法与语义

**数据定义**：CREATE TABLE [IF NOT EXISTS]（列类型名可带括号参数；列约束 PRIMARY KEY [ASC|DESC] [AUTOINCREMENT]、NOT NULL、UNIQUE、CHECK、DEFAULT（字面、带符号数、括号式、CURRENT_TIMESTAMP）、COLLATE、REFERENCES … [ON DELETE|UPDATE 动作]；表约束 PRIMARY KEY、UNIQUE、CHECK、FOREIGN KEY；可带 CONSTRAINT 名与 ON CONFLICT）；CREATE [UNIQUE] INDEX [IF NOT EXISTS] … [WHERE]（列可带 ASC、DESC、COLLATE）；CREATE TRIGGER [IF NOT EXISTS] [BEFORE|AFTER|INSTEAD OF] INSERT|DELETE|UPDATE [OF 列…] ON 表 [FOR EACH ROW] [WHEN] BEGIN … END；CREATE VIEW [IF NOT EXISTS] [(列…)] AS 选择；DROP TABLE|INDEX|TRIGGER|VIEW [IF EXISTS]；ALTER TABLE RENAME TO、ADD [COLUMN]。

**数据操作**：INSERT [OR ROLLBACK|ABORT|FAIL|IGNORE|REPLACE] / REPLACE INTO 表 [(列…)] VALUES 多行 | SELECT | DEFAULT VALUES；不写列名时按表的列序对位；UPSERT：ON CONFLICT [(列…)] DO NOTHING | DO UPDATE SET … [WHERE]（不限定的列名指冲突的旧行，excluded.列 指本要插入的行）；UPDATE [OR …] 表 [别名] SET … [WHERE]；DELETE FROM 表 [别名] [WHERE]；三者都可带 RETURNING（返回改后的行，DELETE 返回删前的行）。命中行在语句开始时的表上求定；INSERT … SELECT 先求出全部来源行再插入。

**查询**：SELECT [DISTINCT] 列、*、表.*、表达式 [AS 别名]；FROM 表、视图、WITH 公共表、子查询（可无别名，内含 ORDER BY 与 LIMIT），以逗号、[INNER|LEFT|CROSS] JOIN … ON|USING 连接，表别名可省 AS；WHERE；GROUP BY（可引结果列别名）、HAVING；聚合 count(*)、count、sum、total、avg、min、max、group_concat（可 DISTINCT），无 GROUP BY 的聚合在空表上也产出一行，组按键升序；UNION [ALL]、INTERSECT、EXCEPT；ORDER BY 多键（ASC、DESC、NULLS FIRST|LAST、输出列序号、结果列别名、不在输出里的列与表达式）；LIMIT、OFFSET（与「LIMIT 偏移, 数」）可用参数；VALUES 作选择。

**表达式**：三值逻辑的 AND、OR、NOT；= == <> != < <= > >=、IS、IS NOT、IS [NOT] DISTINCT FROM、IS NULL、ISNULL、NOTNULL、NOT NULL；[NOT] IN（列表、子查询）、[NOT] BETWEEN、[NOT] LIKE … [ESCAPE]（只对 ASCII 不分大小写）、[NOT] GLOB；EXISTS、标量子查询、行值比较 (a,b,c)=(SELECT …)；CASE（有无基式）；CAST；COLLATE（BINARY、NOCASE、RTRIM）；+ - * / %（整数溢出改实数、除以零得空）、||、& | << >> ~、一元负号；参数 ?、?NNN（:名 按出现次序编号，待办事项：同名参数共用一号）；RAISE(IGNORE|ABORT|FAIL|ROLLBACK, 消息)；NEW.列、OLD.列、excluded.列、rowid。

**函数**：typeof、length、lower、upper、substr/substring、instr、replace、trim/ltrim/rtrim、hex、coalesce、ifnull、nullif、iif、标量 max/min（任一参数为空得空）、abs、round、random、randomblob、unixepoch、date、time、datetime、julianday、strftime（%Y %m %d %H %M %S %f %s %j %w %J %%；时刻可为 now、日期时间文字、数值加 unixepoch；修饰 ±N days|hours|minutes|seconds、start of day|month|year）、CURRENT_TIMESTAMP/DATE/TIME、json_object、changes、last_insert_rowid、total_changes、likely、unlikely、sqlite_version。

**类型与比较**：五种存储类；列按声明类型名取亲和（INT→整数，CHAR/CLOB/TEXT→文字，BLOB 或无类型→无，REAL/FLOA/DOUB→实数，其余→数）；存入时施亲和（数形文字转数，整值实数转整数，文字列存数为文字）；比较前按两边亲和转换（一边数亲和而另一边不是则对另一边施数亲和，文字对无亲和则施文字亲和）；全序为空 < 数 < 文字 < 块，文字按字节；实数转文字取十五位有效数字，读回不等时取十七位。

**约束与报错**：NOT NULL constraint failed: 表.列；CHECK constraint failed: 名（取表达式原文，按 SQLite 去引号，以引号开头时只留第一个名字）；UNIQUE constraint failed: 表.列, 表.列（依次检查行号、唯一索引（后建者先）、表定义里的主键与 UNIQUE（后声明者先））；部分唯一索引只约束满足 WHERE 的行，空值不冲突；FOREIGN KEY constraint failed；datatype mismatch；RAISE 的消息原样带出。

**外键与事务**：外键恒开，语句结束时检查本句改动的表相关的外键；显式事务里开了 PRAGMA defer_foreign_keys 则留到 COMMIT 检查；ON DELETE CASCADE、SET NULL、SET DEFAULT；BEGIN [DEFERRED|IMMEDIATE|EXCLUSIVE]、COMMIT/END、ROLLBACK；每条语句原子（失败恢复语句前状态）；PRAGMA foreign_keys、defer_foreign_keys、foreign_key_check。

**接口语义**（D1 的 `prepare/first/all/run/batch`）：changes 只计本句直接改动的行（触发器所改不计；UPSERT 改写计一）；lastRowId 是会话的最后插入行号（触发器内的插入结束后复原）；带 RETURNING 的写语句经「取关系库首行」执行时整句执行并提交，只返回首行，无行得 null；批内后句看得见前句的写入，任一句失败整批回滚；错误消息里须含 SQLite 的原因文字（代码按 UNIQUE、constraint 与触发器消息分流）。

## 三、实现结构

`结构化查询/` 模块组：

- `值域`：查询值（空、整数、实数、文字、块）、亲和、全序比较、算术（溢出检测）、数形解析、实数文字。
- `词法`：按字节切词，带原文字节区间。
- `语法树`：表达式、选择、来源、结果列、约束、触发器、语句等代数类型。
- `解析`：递归下降；表达式用一个按优先级爬升的递归函数；子查询经析态里的回调调用选择解析器。
- `库态`：表信息（由表定义推出亲和、非空、默认、行号别名列、唯一约束、检查、外键）、行（按行号升序）、不可变的库状态、查询会话（当前状态、事务快照、延迟外键、待查与本句改动的表、最后行号、变更数、固定时钟、随机数状态）。
- `标量函数`：上述标量函数；时刻在本句内首次用到时才读取。
- `求值`：环境（作用域层、组、别名、公共表、本句时刻、活动触发器、选择执行器回调）、列解析、亲和与校对比较、LIKE/GLOB、聚合、CAST、子查询；选择执行：来源展开、连接、过滤、分组聚合、HAVING、结果列展开、排序键、DISTINCT、复合、WITH、LIMIT/OFFSET。
- `执行`：触发器、约束检查与 UPSERT、插入/更新/删除一行、外键动作与检查、各语句、DDL、事务、PRAGMA、语句级原子与外键检查、脚本执行、原子批、结果文字。

库状态是不可变数据，事务与语句回滚只需保存与恢复一个值。`关系查询/接口适配`、`快照`、`持久` 建在这一组之上；旧的 SQL 子集实现（`关系查询/文本`、`索引目录`、`排序值`）已删去。

## 四、与 SQLite 的已知差异（待办）

- 十七位实数文字个别值末位与 SQLite 不同（SQLite 的取位有近似误差，如 1.0/3 得 …332，本实现得 …331）。
- 不支持双引号字符串回退（找不到列时把 "名" 当文字）。
- PRAGMA foreign_keys 恒开；DEFERRABLE INITIALLY DEFERRED 按立即检查；DROP TABLE 不做隐式删除带来的外键动作；REPLACE 删除冲突行时不做外键动作；ON UPDATE 动作未做。
- ALTER TABLE RENAME COLUMN、DROP COLUMN 未做；RENAME TO 不改写触发器体与视图里的旧表名。
- 表达式索引不参与唯一检查；列声明的 COLLATE 不影响比较；RAISE(ROLLBACK) 只回滚本句。
- WITH RECURSIVE、窗口函数、printf、json 其余函数、NATURAL 与 RIGHT/FULL 连接未做；AUTOINCREMENT 按最大行号加一。
- 语法错误时整段文字都不执行（SQLite 逐句执行，错前各句已生效）。
- 时间修饰 localtime、utc 按 UTC 处理。
- 没有查询优化；没有 ORDER BY 时按行号与连接次序输出。

## 五、验证

- `测试/结构化查询/对照。测试。豫`：对照用例 12 例（云仓全部 24 条迁移后的种子数据与约束；AI 市场账本三例；人民币亿分与百万分迁移；身份库的会话、主凭据与金额单位；限流 UPSERT；云计费用量写入与报表；表达式与函数；建表约束与亲和；查询结构；触发器、外键与事务），期望由 `生成对照期望。脚本。豫` 调用 /usr/bin/sqlite3 3.54.0 生成，Node 上全部一致。
- `测试/结构化查询/逻辑子集。测试。豫`：sqllogictest select1 的三段 90 行，摘要与官方记录一致。
- `测试/关系查询/接口适配。测试。豫`、`持久。测试。豫` 与 `豫言操作系统/适配/关系数据库通用/测试/语句。测试。豫` 在 Node 上通过。
