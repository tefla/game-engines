import { useState, useEffect, useCallback } from "react";
import { store } from "@/core/state-store";

/**
 * Hook to subscribe to a state path
 * Re-renders when the value changes
 */
export function useStore<T>(path: string): T {
  const [value, setValue] = useState<T>(() => store.get<T>(path));

  useEffect(() => {
    // Update immediately in case state changed between render and effect
    setValue(store.get<T>(path));

    // Subscribe to changes
    const unsubscribe = store.subscribe(path, (newValue) => {
      setValue(newValue);
    });

    return unsubscribe;
  }, [path]);

  return value;
}

/**
 * Hook to get a state setter
 */
export function useStoreSetter(path: string): (value: any) => void {
  return useCallback(
    (value: any) => {
      store.set(path, value);
    },
    [path]
  );
}

/**
 * Hook to get both value and setter (like useState)
 */
export function useStoreState<T>(path: string): [T, (value: T) => void] {
  const value = useStore<T>(path);
  const setValue = useStoreSetter(path);
  return [value, setValue];
}

/**
 * Hook to subscribe to multiple state paths
 */
export function useStoreMulti<T extends Record<string, any>>(
  paths: string[]
): T {
  const [values, setValues] = useState<T>(() => {
    const initial: any = {};
    paths.forEach((path) => {
      const key = path.split(".").pop()!;
      initial[key] = store.get(path);
    });
    return initial;
  });

  useEffect(() => {
    const unsubscribers = paths.map((path) => {
      return store.subscribe(path, (newValue) => {
        const key = path.split(".").pop()!;
        setValues((prev) => ({ ...prev, [key]: newValue }));
      });
    });

    return () => {
      unsubscribers.forEach((unsub) => unsub());
    };
  }, [paths.join(",")]);

  return values;
}

/**
 * Hook for file operations
 */
export function useFiles() {
  const currentFile = useStore<string | null>("files.current");
  const openFiles = useStore<string[]>("files.open");
  const modifiedFiles = useStore<Set<string>>("files.modified");

  return {
    currentFile,
    openFiles,
    modifiedFiles,
    isModified: (path: string) => modifiedFiles.has(path),
    openFile: (path: string) => store.openFile(path),
    closeFile: (path: string) => store.closeFile(path),
    markModified: (path: string, modified?: boolean) =>
      store.markModified(path, modified),
  };
}

/**
 * Hook for project state
 */
export function useProject() {
  const path = useStore<string | null>("project.path");
  const name = useStore<string>("project.name");
  const config = useStore<any>("project.config");

  return {
    path,
    name,
    config,
    isOpen: path !== null,
    setProject: (projectPath: string, projectConfig: any) =>
      store.setProject(projectPath, projectConfig),
    closeProject: () => store.closeProject(),
  };
}

/**
 * Hook for UI state
 */
export function useUI() {
  const theme = useStore<"dark" | "light">("ui.theme");
  const sidebarWidth = useStore<number>("ui.sidebarWidth");
  const bottomPanelHeight = useStore<number>("ui.bottomPanelHeight");
  const activePanel = useStore<string | null>("ui.activePanel");

  return {
    theme,
    sidebarWidth,
    bottomPanelHeight,
    activePanel,
    setTheme: (t: "dark" | "light") => store.set("ui.theme", t),
    setSidebarWidth: (w: number) => store.set("ui.sidebarWidth", w),
    setBottomPanelHeight: (h: number) => store.set("ui.bottomPanelHeight", h),
    setActivePanel: (id: string | null) => store.set("ui.activePanel", id),
  };
}
