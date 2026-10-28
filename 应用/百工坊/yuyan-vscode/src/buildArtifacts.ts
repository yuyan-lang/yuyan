export interface BuildArtifactEntry {
  stage: string;
  path: string;
  present: boolean;
}

export interface BuildArtifactReport {
  sourceFile: string;
  packageContext: string;
  artifacts: BuildArtifactEntry[];
}

const YUYAN_SOURCE_SUFFIXES = ['。豫', '.yuyan', '.yyon'];

function isRecord(value: unknown): value is { [key: string]: unknown } {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

export function isSupportedYuyanSource(filePath: string): boolean {
  const normalizedPath = filePath.replace(/\\/g, '/');
  return YUYAN_SOURCE_SUFFIXES.some(suffix => normalizedPath.endsWith(suffix));
}

// 解析 `豫构 产物 <源文件>` 印出的一行结绳文字。阶段、路径、在不在都由豫构给出，
// 扩展不再按缓存布局自己拼路径；认不出的项跳过，整份认不出时返回 undefined，
// 调用方据此提示重新构建，而不是自己猜位置。
export function parseBuildArtifactReport(text: string): BuildArtifactReport | undefined {
  let value: unknown;
  try {
    value = JSON.parse(text);
  } catch {
    return undefined;
  }
  if (!isRecord(value)) {
    return undefined;
  }

  const sourceFile = value['源文件'];
  const packageContext = value['包上下文'];
  const rawArtifacts = value['阶段产物'];
  if (
    typeof sourceFile !== 'string' ||
    typeof packageContext !== 'string' ||
    !Array.isArray(rawArtifacts)
  ) {
    return undefined;
  }

  const artifacts: BuildArtifactEntry[] = [];
  for (const rawArtifact of rawArtifacts) {
    if (!isRecord(rawArtifact)) {
      continue;
    }
    const stage = rawArtifact['阶段'];
    const artifactPath = rawArtifact['路径'];
    const present = rawArtifact['在'];
    if (
      typeof stage !== 'string' ||
      typeof artifactPath !== 'string' ||
      typeof present !== 'boolean' ||
      stage.length === 0 ||
      artifactPath.length === 0
    ) {
      continue;
    }
    artifacts.push({ stage, path: artifactPath, present });
  }

  return { sourceFile, packageContext, artifacts };
}
