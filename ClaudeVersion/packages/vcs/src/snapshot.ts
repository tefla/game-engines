// Snapshot system for capturing VFS and world state

import type { VirtualFileSystem } from "@oort/engine";
import type { Permissions } from "@oort/engine";

// Represents a captured file state
export interface FileSnapshot {
  path: string;
  content: string;
  permissions: Permissions;
  isDirectory: boolean;
}

// A complete snapshot of the system state
export interface Snapshot {
  id: string;
  name?: string;
  timestamp: number;
  files: FileSnapshot[];
  metadata: Map<string, unknown>;
}

// Generate unique snapshot ID
function generateSnapshotId(): string {
  return `snap_${Date.now()}_${Math.random().toString(36).slice(2, 8)}`;
}

// Recursively capture all files from a VFS
function captureFiles(
  vfs: VirtualFileSystem,
  path: string = "/"
): FileSnapshot[] {
  const files: FileSnapshot[] = [];

  try {
    const items = vfs.ls(path);

    for (const item of items) {
      const fullPath = item.path;

      // Parse permissions string back to Permissions object
      const perms = parsePermissionString(item.permissions);

      if (item.isDirectory) {
        files.push({
          path: fullPath,
          content: "",
          permissions: perms,
          isDirectory: true,
        });

        // Recursively capture children
        try {
          const children = captureFiles(vfs, fullPath);
          files.push(...children);
        } catch {
          // Permission denied for children, skip
        }
      } else {
        try {
          const content = vfs.read(fullPath);
          files.push({
            path: fullPath,
            content,
            permissions: perms,
            isDirectory: false,
          });
        } catch {
          // Permission denied, capture metadata only
          files.push({
            path: fullPath,
            content: "",
            permissions: perms,
            isDirectory: false,
          });
        }
      }
    }
  } catch {
    // Permission denied for this directory
  }

  return files;
}

// Parse permission string like "rw-" back to Permissions
function parsePermissionString(permStr: string): Permissions {
  return {
    read: permStr[0] === "r",
    write: permStr[1] === "w",
    execute: permStr[2] === "x",
  };
}

// Create a snapshot from a VFS
export function createSnapshot(
  vfs: VirtualFileSystem,
  name?: string,
  metadata?: Map<string, unknown>
): Snapshot {
  const files = captureFiles(vfs);

  return {
    id: generateSnapshotId(),
    name,
    timestamp: Date.now(),
    files,
    metadata: metadata ?? new Map(),
  };
}

// Restore a VFS from a snapshot
// Note: This recreates the entire filesystem state
export function restoreSnapshot(
  vfs: VirtualFileSystem,
  snapshot: Snapshot
): void {
  // Sort files by path depth to ensure parents are created first
  const sortedFiles = [...snapshot.files].sort((a, b) => {
    const depthA = a.path.split("/").length;
    const depthB = b.path.split("/").length;
    return depthA - depthB;
  });

  // First pass: create all directories
  for (const file of sortedFiles) {
    if (file.isDirectory && file.path !== "/") {
      try {
        if (!vfs.exists(file.path)) {
          vfs.mkdir(file.path);
        }
      } catch {
        // Directory might already exist or permission denied
      }
    }
  }

  // Second pass: set directory permissions and create files
  for (const file of sortedFiles) {
    try {
      if (file.isDirectory) {
        vfs.chmod(file.path, file.permissions);
      } else {
        // Create or update file
        vfs.write(file.path, file.content);
        vfs.chmod(file.path, file.permissions);
      }
    } catch {
      // Permission denied or other error
    }
  }
}

// Clone a snapshot (deep copy)
export function cloneSnapshot(snapshot: Snapshot): Snapshot {
  return {
    id: generateSnapshotId(),
    name: snapshot.name,
    timestamp: Date.now(),
    files: snapshot.files.map((f) => ({
      ...f,
      permissions: { ...f.permissions },
    })),
    metadata: new Map(snapshot.metadata),
  };
}

// Compare two snapshots and return the changed file paths
export function getChangedFiles(
  before: Snapshot,
  after: Snapshot
): {
  added: string[];
  modified: string[];
  removed: string[];
} {
  const beforePaths = new Set(before.files.map((f) => f.path));
  const afterPaths = new Set(after.files.map((f) => f.path));

  const beforeMap = new Map(before.files.map((f) => [f.path, f]));
  const afterMap = new Map(after.files.map((f) => [f.path, f]));

  const added: string[] = [];
  const modified: string[] = [];
  const removed: string[] = [];

  // Find added and modified files
  for (const path of afterPaths) {
    if (!beforePaths.has(path)) {
      added.push(path);
    } else {
      const beforeFile = beforeMap.get(path)!;
      const afterFile = afterMap.get(path)!;

      if (
        beforeFile.content !== afterFile.content ||
        beforeFile.permissions.read !== afterFile.permissions.read ||
        beforeFile.permissions.write !== afterFile.permissions.write ||
        beforeFile.permissions.execute !== afterFile.permissions.execute
      ) {
        modified.push(path);
      }
    }
  }

  // Find removed files
  for (const path of beforePaths) {
    if (!afterPaths.has(path)) {
      removed.push(path);
    }
  }

  return { added, modified, removed };
}

// Serialize snapshot to JSON
export function serializeSnapshot(snapshot: Snapshot): string {
  return JSON.stringify({
    ...snapshot,
    metadata: Array.from(snapshot.metadata.entries()),
  });
}

// Deserialize snapshot from JSON
export function deserializeSnapshot(json: string): Snapshot {
  const data = JSON.parse(json);
  return {
    ...data,
    metadata: new Map(data.metadata),
  };
}
