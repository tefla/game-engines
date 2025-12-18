// VCS Package - Version Control System for Oort Engine

export {
  type Snapshot,
  type FileSnapshot,
  createSnapshot,
  restoreSnapshot,
  cloneSnapshot,
  getChangedFiles,
  serializeSnapshot,
  deserializeSnapshot,
} from "./snapshot";

export {
  type ChangeType,
  type DiffEntry,
  type DiffResult,
  diffPrograms,
  diffSource,
  formatDiff,
} from "./diff";

export {
  type HistoryOptions,
  type HistoryEntry,
  History,
  createHistory,
} from "./history";

export {
  createVcsStdlib,
  createVcsNamespace,
} from "./vcs-stdlib";
