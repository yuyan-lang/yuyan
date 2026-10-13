export interface BuildCacheInfo {
  name: string;
  mtime: number;
}

const YUYAN_SOURCE_SUFFIXES = ['。豫', '.yuyan', '.yyon'];

export function sourceRelativePathToArtifactStem(relativePath: string): string | undefined {
  const normalizedPath = relativePath.replace(/\\/g, '/');
  if (
    normalizedPath.length === 0 ||
    normalizedPath.startsWith('/') ||
    /^(?:\.\.)(?:\/|$)/.test(normalizedPath) ||
    /^[A-Za-z]:\//.test(normalizedPath)
  ) {
    return undefined;
  }

  const suffix = YUYAN_SOURCE_SUFFIXES.find(candidate => normalizedPath.endsWith(candidate));
  if (!suffix) {
    return undefined;
  }

  const stem = normalizedPath.slice(0, -suffix.length);
  return stem.length > 0 ? stem : undefined;
}

export function treeArtifactStage(
  artifactFileName: string,
  sourceBaseName: string
): string | undefined {
  // 新名用全角句号（名。阶段。树码），旧缓存目录里还有半角的（名.阶段.树码）。
  for (const dot of ['。', '.']) {
    const prefix = `${sourceBaseName}${dot}`;
    const suffix = `${dot}树码`;
    if (artifactFileName.startsWith(prefix) && artifactFileName.endsWith(suffix)) {
      const stage = artifactFileName.slice(prefix.length, -suffix.length);
      return stage.length > 0 ? stage : undefined;
    }
  }
  return undefined;
}

export function sortBuildCachesNewestFirst<T extends BuildCacheInfo>(caches: T[]): T[] {
  return [...caches].sort((left, right) => {
    const mtimeDifference = right.mtime - left.mtime;
    return mtimeDifference !== 0 ? mtimeDifference : right.name.localeCompare(left.name);
  });
}
