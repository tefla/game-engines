import { test as base, _electron as electron, ElectronApplication, Page } from "@playwright/test";
import path from "path";

// Extend base test with Electron fixtures
export const test = base.extend<{
  electronApp: ElectronApplication;
  window: Page;
}>({
  electronApp: async ({}, use) => {
    // Build paths
    const appPath = path.join(__dirname, "..");
    const mainPath = path.join(appPath, "dist/main/main/index.js");

    // Launch Electron app
    const app = await electron.launch({
      args: [mainPath],
      env: {
        ...process.env,
        NODE_ENV: "test",
      },
    });

    await use(app);

    // Cleanup
    await app.close();
  },

  window: async ({ electronApp }, use) => {
    // Wait for first window
    const window = await electronApp.firstWindow();

    // Wait for window to be ready
    await window.waitForLoadState("domcontentloaded");

    await use(window);
  },
});

export { expect } from "@playwright/test";
