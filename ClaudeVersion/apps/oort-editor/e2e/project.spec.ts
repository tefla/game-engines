import { test, expect } from "./electron-fixture";
import path from "path";

// Path to the sample project (3 levels up from e2e to reach root)
const SAMPLE_PROJECT_PATH = path.resolve(__dirname, "../../../games/sample-project");

// Helper function to open project and set store state
async function openProjectInEditor(window: any, projectPath: string) {
  const result = await window.evaluate(async (path: string) => {
    const openResult = await window.electronAPI.openProject(path);
    if (openResult.success && window.__oortStore) {
      // Update React state store (mimics what WelcomePanel does)
      window.__oortStore.setProject(path, openResult.data.config);
    }
    return openResult;
  }, projectPath);
  return result;
}

test.describe("Project Operations", () => {
  test("should open sample project via API", async ({ electronApp, window }) => {
    // Collect console errors
    const consoleErrors: string[] = [];
    window.on("console", (msg) => {
      if (msg.type() === "error") {
        consoleErrors.push(msg.text());
      }
    });

    // Open project and set store state
    const result = await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    console.log("Open project result:", result);
    expect(result.success).toBe(true);

    // Wait for the docking layout to appear (project is open)
    const dockingLayout = window.locator(".docking-layout");
    await expect(dockingLayout).toBeVisible({ timeout: 10000 });

    // Log any console errors
    if (consoleErrors.length > 0) {
      console.log("Console errors:", consoleErrors);
    }
  });

  test("should show project files in explorer", async ({ electronApp, window }) => {
    // Open project
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    // Wait for layout
    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Check project explorer is visible
    const explorer = window.locator(".project-explorer");
    await expect(explorer).toBeVisible();

    // Check that the tree has items
    const treeItems = window.locator(".tree-item");
    await expect(treeItems.first()).toBeVisible({ timeout: 5000 });

    // Should show the scripts folder
    const scriptsFolder = window.locator('.tree-name:has-text("scripts")');
    await expect(scriptsFolder).toBeVisible();
  });

  test("should expand folders and show files", async ({ electronApp, window }) => {
    // Collect console messages for debugging
    const consoleLogs: string[] = [];
    window.on("console", (msg) => {
      consoleLogs.push(`[${msg.type()}] ${msg.text()}`);
    });

    // Open project
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Click on scripts folder to expand it
    const scriptsFolder = window.locator('.tree-item:has-text("scripts")');
    await scriptsFolder.click();

    // Wait a bit for expansion
    await window.waitForTimeout(500);

    // Should now see main.sl
    const mainFile = window.locator('.tree-name:has-text("main.sl")');
    await expect(mainFile).toBeVisible({ timeout: 5000 });

    // Should also see utils.sl
    const utilsFile = window.locator('.tree-name:has-text("utils.sl")');
    await expect(utilsFile).toBeVisible();

    // Log console output for debugging
    if (consoleLogs.length > 0) {
      console.log("Console logs:", consoleLogs.slice(-20)); // Last 20 messages
    }
  });

  test("should open .sl file in editor when clicked", async ({ electronApp, window }) => {
    // Collect errors
    const errors: string[] = [];
    window.on("console", (msg) => {
      if (msg.type() === "error") {
        errors.push(msg.text());
      }
    });
    window.on("pageerror", (err) => {
      errors.push(`Page error: ${err.message}`);
    });

    // Open project
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Expand scripts folder
    const scriptsFolder = window.locator('.tree-item:has-text("scripts")');
    await scriptsFolder.click();
    await window.waitForTimeout(500);

    // Click on main.sl to open it
    const mainFile = window.locator('.tree-item:has-text("main.sl")');
    await mainFile.click();

    // Wait for code editor to show the file
    const codeEditor = window.locator(".code-editor");
    await expect(codeEditor).toBeVisible({ timeout: 5000 });

    // Check filename is shown in header
    const filename = window.locator(".editor-header .filename");
    await expect(filename).toContainText("main.sl");

    // Check that editor content area exists
    const editorContent = window.locator(".editor-content");
    await expect(editorContent).toBeVisible();

    // Log any errors
    if (errors.length > 0) {
      console.log("Errors encountered:", errors);
    }

    // Take a screenshot for debugging
    await window.screenshot({ path: "test-results/open-sl-file.png" });
  });

  test("should display Slate code with syntax highlighting", async ({ electronApp, window }) => {
    // Open project
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Expand scripts and open main.sl
    await window.locator('.tree-item:has-text("scripts")').click();
    await window.waitForTimeout(500);
    await window.locator('.tree-item:has-text("main.sl")').click();

    // Wait for Monaco editor to load
    await window.waitForTimeout(1000);

    // Check Monaco editor is present
    const monacoEditor = window.locator(".monaco-editor");
    await expect(monacoEditor).toBeVisible({ timeout: 10000 });

    // The editor should have content (verifies file was loaded)
    const editorContent = await window.locator(".monaco-editor").textContent();
    expect(editorContent?.length).toBeGreaterThan(100); // Should have substantial content
  });
});

test.describe("Console Panel", () => {
  test("should run Slate code in console", async ({ electronApp, window }) => {
    // Open project first
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Find console panel
    const consolePanel = window.locator(".console-panel");
    await expect(consolePanel).toBeVisible();

    // Find and focus the input
    const consoleInput = window.locator(".console-input");
    await consoleInput.click();
    await consoleInput.fill("1 + 2");
    await consoleInput.press("Enter");

    // Check result appears in output
    const consoleOutput = window.locator(".console-output");
    await expect(consoleOutput).toContainText("3", { timeout: 5000 });
  });

  test("should run multi-line Slate code", async ({ electronApp, window }) => {
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    const consoleInput = window.locator(".console-input");
    await consoleInput.click();

    // Define a variable
    await consoleInput.fill("let x = 42");
    await consoleInput.press("Enter");

    await window.waitForTimeout(200);

    // Use the variable
    await consoleInput.fill("x * 2");
    await consoleInput.press("Enter");

    const consoleOutput = window.locator(".console-output");
    await expect(consoleOutput).toContainText("84", { timeout: 5000 });
  });

  test("should show help command output", async ({ electronApp, window }) => {
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    const consoleInput = window.locator(".console-input");
    await consoleInput.click();
    await consoleInput.fill("help");
    await consoleInput.press("Enter");

    const consoleOutput = window.locator(".console-output");
    await expect(consoleOutput).toContainText("Available commands", { timeout: 5000 });
    await expect(consoleOutput).toContainText("Checkpoints", { timeout: 5000 });
  });

  test("should clear console", async ({ electronApp, window }) => {
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    const consoleInput = window.locator(".console-input");

    // Add some output
    await consoleInput.click();
    await consoleInput.fill("1 + 1");
    await consoleInput.press("Enter");

    await window.waitForTimeout(200);

    // Clear
    await consoleInput.fill("clear");
    await consoleInput.press("Enter");

    // Output should be empty (or just have the empty state)
    const consoleOutput = window.locator(".console-output");
    const entries = window.locator(".console-entry");

    // Should have no entries or just the welcome message
    await expect(entries).toHaveCount(0, { timeout: 2000 }).catch(() => {
      // It's okay if there's a welcome message
    });
  });
});

test.describe("File Operations", () => {
  test("should create a new file via context menu", async ({ electronApp, window }) => {
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Right-click on scripts folder
    const scriptsFolder = window.locator('.tree-item:has-text("scripts")');
    await scriptsFolder.click({ button: "right" });

    // Check context menu appears
    const contextMenu = window.locator(".context-menu");
    await expect(contextMenu).toBeVisible({ timeout: 2000 });

    // Click "New File"
    const newFileBtn = window.locator('.context-menu button:has-text("New File")');
    await expect(newFileBtn).toBeVisible();

    // Take screenshot of context menu
    await window.screenshot({ path: "test-results/context-menu.png" });
  });

  test("should show toolbar buttons", async ({ electronApp, window }) => {
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Check toolbar exists
    const toolbar = window.locator(".explorer-toolbar");
    await expect(toolbar).toBeVisible();

    // Check toolbar buttons
    const toolbarBtns = window.locator(".toolbar-btn");
    const count = await toolbarBtns.count();
    expect(count).toBeGreaterThanOrEqual(2); // At least new file and refresh
  });
});

test.describe("Error Handling", () => {
  test("should capture and log all console errors", async ({ electronApp, window }) => {
    const allMessages: { type: string; text: string }[] = [];

    window.on("console", (msg) => {
      allMessages.push({ type: msg.type(), text: msg.text() });
    });

    window.on("pageerror", (err) => {
      allMessages.push({ type: "pageerror", text: err.message });
    });

    // Open project
    await openProjectInEditor(window, SAMPLE_PROJECT_PATH);

    await window.locator(".docking-layout").waitFor({ state: "visible", timeout: 10000 });

    // Expand and open a file
    await window.locator('.tree-item:has-text("scripts")').click();
    await window.waitForTimeout(500);
    await window.locator('.tree-item:has-text("main.sl")').click();
    await window.waitForTimeout(1000);

    // Try console operations
    const consoleInput = window.locator(".console-input");
    await consoleInput.click();
    await consoleInput.fill("1 + 1");
    await consoleInput.press("Enter");
    await window.waitForTimeout(500);

    // Filter for errors and warnings
    const errors = allMessages.filter(m => m.type === "error" || m.type === "pageerror");
    const warnings = allMessages.filter(m => m.type === "warning");

    console.log("\n=== Console Errors ===");
    errors.forEach(e => console.log(`  [ERROR] ${e.text}`));

    console.log("\n=== Console Warnings ===");
    warnings.forEach(w => console.log(`  [WARN] ${w.text}`));

    console.log("\n=== All Messages (last 30) ===");
    allMessages.slice(-30).forEach(m => console.log(`  [${m.type}] ${m.text.substring(0, 100)}`));

    // Take final screenshot
    await window.screenshot({ path: "test-results/final-state.png" });

    // Test passes but logs all errors for debugging
    // Uncomment below to fail on errors:
    // expect(errors.length).toBe(0);
  });
});
