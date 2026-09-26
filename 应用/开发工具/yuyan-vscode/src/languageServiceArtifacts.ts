import { sourceRelativePathToArtifactStem } from './buildArtifacts';

export interface SourceRangeInfo {
  文件: string;
  开始行: number;
  开始列: number;
  结束行: number;
  结束列: number;
}

export interface DefinitionInfo {
  种类: '定义';
  范围: SourceRangeInfo;
  目标: SourceRangeInfo;
}

export interface HoverInfo {
  种类: '悬停';
  范围: SourceRangeInfo;
  内容: string;
}

export type LanguageServiceInfo = DefinitionInfo | HoverInfo;

export interface LanguageServiceDocument {
  版本: 1;
  源文件: string;
  信息: LanguageServiceInfo[];
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isPosition(value: unknown): value is number {
  return Number.isInteger(value) && (value as number) >= 0;
}

function parseSourceRange(value: unknown): SourceRangeInfo | undefined {
  if (!isRecord(value)) {
    return undefined;
  }

  const range = value as Record<string, unknown>;
  if (
    typeof range.文件 !== 'string' ||
    !isPosition(range.开始行) ||
    !isPosition(range.开始列) ||
    !isPosition(range.结束行) ||
    !isPosition(range.结束列)
  ) {
    return undefined;
  }

  if (
    range.结束行 < range.开始行 ||
    (range.结束行 === range.开始行 && range.结束列 < range.开始列)
  ) {
    return undefined;
  }

  return {
    文件: range.文件,
    开始行: range.开始行,
    开始列: range.开始列,
    结束行: range.结束行,
    结束列: range.结束列
  };
}

function parseInfo(value: unknown): LanguageServiceInfo | undefined {
  if (!isRecord(value)) {
    return undefined;
  }

  const range = parseSourceRange(value.范围);
  if (!range) {
    return undefined;
  }

  if (value.种类 === '定义') {
    const target = parseSourceRange(value.目标);
    return target ? { 种类: '定义', 范围: range, 目标: target } : undefined;
  }

  if (value.种类 === '悬停' && typeof value.内容 === 'string') {
    return { 种类: '悬停', 范围: range, 内容: value.内容 };
  }

  return undefined;
}

export function parseLanguageServiceDocument(text: string): LanguageServiceDocument | undefined {
  let value: unknown;
  try {
    value = JSON.parse(text);
  } catch {
    return undefined;
  }

  if (
    !isRecord(value) ||
    value.版本 !== 1 ||
    typeof value.源文件 !== 'string' ||
    !Array.isArray(value.信息)
  ) {
    return undefined;
  }

  const info = value.信息.map(parseInfo);
  if (info.some(item => item === undefined)) {
    return undefined;
  }

  return {
    版本: 1,
    源文件: value.源文件,
    信息: info as LanguageServiceInfo[]
  };
}

export function languageServiceArtifactPath(relativeSourcePath: string): string | undefined {
  const stem = sourceRelativePathToArtifactStem(relativeSourcePath);
  return stem ? `${stem}.语言服务.json` : undefined;
}

export function positionIsInRange(
  line: number,
  column: number,
  range: SourceRangeInfo
): boolean {
  if (line < range.开始行 || line > range.结束行) {
    return false;
  }
  if (line === range.开始行 && column < range.开始列) {
    return false;
  }
  if (line === range.结束行 && column >= range.结束列) {
    return false;
  }
  return true;
}

export function selectNarrowestInfo<T extends LanguageServiceInfo>(
  info: T[],
  line: number,
  column: number
): T | undefined {
  return info
    .filter(item => positionIsInRange(line, column, item.范围))
    .sort((left, right) => {
      const lineDifference =
        (left.范围.结束行 - left.范围.开始行) -
        (right.范围.结束行 - right.范围.开始行);
      if (lineDifference !== 0) {
        return lineDifference;
      }
      return (
        (left.范围.结束列 - left.范围.开始列) -
        (right.范围.结束列 - right.范围.开始列)
      );
    })[0];
}
