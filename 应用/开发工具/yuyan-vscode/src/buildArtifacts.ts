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

export function jsonArtifactStage(
  artifactFileName: string,
  sourceBaseName: string
): string | undefined {
  const prefix = `${sourceBaseName}.`;
  const suffix = '.json';
  if (!artifactFileName.startsWith(prefix) || !artifactFileName.endsWith(suffix)) {
    return undefined;
  }

  const stage = artifactFileName.slice(prefix.length, -suffix.length);
  return stage.length > 0 ? stage : undefined;
}

export function sortBuildCachesNewestFirst<T extends BuildCacheInfo>(caches: T[]): T[] {
  return [...caches].sort((left, right) => {
    const mtimeDifference = right.mtime - left.mtime;
    return mtimeDifference !== 0 ? mtimeDifference : right.name.localeCompare(left.name);
  });
}
