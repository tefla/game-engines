// VFS Permission System

export interface Permissions {
  read: boolean;
  write: boolean;
  execute: boolean;
}

// Parse permission string like "rwx", "r--", "rw-"
export function parsePermissions(str: string): Permissions {
  if (str.length !== 3) {
    throw new Error(`Invalid permission string: ${str}`);
  }
  return {
    read: str[0] === "r",
    write: str[1] === "w",
    execute: str[2] === "x",
  };
}

// Convert permissions to string like "rwx", "r--"
export function permissionsToString(perms: Permissions): string {
  return (
    (perms.read ? "r" : "-") +
    (perms.write ? "w" : "-") +
    (perms.execute ? "x" : "-")
  );
}

// Default permissions
export const DEFAULT_PERMISSIONS: Permissions = {
  read: true,
  write: true,
  execute: true,
};

export const READ_ONLY: Permissions = {
  read: true,
  write: false,
  execute: true,
};

export const HIDDEN: Permissions = {
  read: false,
  write: false,
  execute: false,
};

export const READ_WRITE: Permissions = {
  read: true,
  write: true,
  execute: true,
};

// Permission errors
export class PermissionError extends Error {
  constructor(
    public path: string,
    public operation: "read" | "write" | "execute",
    message?: string
  ) {
    super(message || `Permission denied: cannot ${operation} '${path}'`);
    this.name = "PermissionError";
  }
}
