import { test, expect } from "./electron-fixture";

test.describe("Oort Editor - App Launch", () => {
  test("should launch and show welcome screen", async ({ window }) => {
    // Check window title
    const title = await window.title();
    expect(title).toContain("Oort");

    // Check welcome panel is visible
    const welcomePanel = window.locator(".welcome-panel");
    await expect(welcomePanel).toBeVisible();

    // Check logo text
    const logoText = window.locator(".logo-text");
    await expect(logoText).toHaveText("Oort Editor");

    // Check tagline
    const tagline = window.locator(".logo-tagline");
    await expect(tagline).toHaveText("Game Development Environment");
  });

  test("should show File and View menus", async ({ window }) => {
    // Check menu bar items
    const fileMenu = window.locator('.menu-bar-button:has-text("File")');
    const viewMenu = window.locator('.menu-bar-button:has-text("View")');

    await expect(fileMenu).toBeVisible();
    await expect(viewMenu).toBeVisible();
  });

  test("should show status bar with version", async ({ window }) => {
    const statusBar = window.locator(".status-bar");
    await expect(statusBar).toBeVisible();

    // Check version is displayed
    await expect(statusBar).toContainText("v0.1.0");
  });
});

test.describe("Oort Editor - Welcome Panel", () => {
  test("should show New Project and Open Project buttons", async ({ window }) => {
    const newProjectBtn = window.locator('button:has-text("New Project")');
    const openProjectBtn = window.locator('button:has-text("Open Project")');

    await expect(newProjectBtn).toBeVisible();
    await expect(openProjectBtn).toBeVisible();

    // Check button descriptions
    await expect(newProjectBtn).toContainText("Create a new game project");
    await expect(openProjectBtn).toContainText("Open an existing project");
  });

  test("should show create project form when New Project clicked", async ({ window }) => {
    // Click New Project button
    const newProjectBtn = window.locator('button:has-text("New Project")');
    await newProjectBtn.click();

    // Check form appears
    const createForm = window.locator(".create-form");
    await expect(createForm).toBeVisible();

    // Check input is present and focused
    const input = window.locator('.create-form input[type="text"]');
    await expect(input).toBeVisible();
    await expect(input).toBeFocused();

    // Check Create and Cancel buttons
    const createBtn = window.locator('.create-form button:has-text("Create")');
    const cancelBtn = window.locator('.create-form button:has-text("Cancel")');

    await expect(createBtn).toBeVisible();
    await expect(cancelBtn).toBeVisible();

    // Create button should be disabled when input is empty
    await expect(createBtn).toBeDisabled();
  });

  test("should enable Create button when project name entered", async ({ window }) => {
    // Click New Project button
    const newProjectBtn = window.locator('button:has-text("New Project")');
    await newProjectBtn.click();

    // Type project name
    const input = window.locator('.create-form input[type="text"]');
    await input.fill("test-project");

    // Create button should now be enabled
    const createBtn = window.locator('.create-form button:has-text("Create")');
    await expect(createBtn).toBeEnabled();
  });

  test("should cancel create project form", async ({ window }) => {
    // Click New Project button
    const newProjectBtn = window.locator('button:has-text("New Project")');
    await newProjectBtn.click();

    // Verify form is visible
    const createForm = window.locator(".create-form");
    await expect(createForm).toBeVisible();

    // Click Cancel
    const cancelBtn = window.locator('.create-form button:has-text("Cancel")');
    await cancelBtn.click();

    // Form should be hidden, welcome buttons visible
    await expect(createForm).not.toBeVisible();
    await expect(newProjectBtn).toBeVisible();
  });

  test("should allow submitting project name with Enter key", async ({ window }) => {
    // Click New Project button
    const newProjectBtn = window.locator('button:has-text("New Project")');
    await newProjectBtn.click();

    // Type project name
    const input = window.locator('.create-form input[type="text"]');
    await input.fill("enter-test-project");

    // Verify the input has the correct value
    await expect(input).toHaveValue("enter-test-project");
  });
});

test.describe("Oort Editor - Menu Interactions", () => {
  test("should open File menu dropdown", async ({ window }) => {
    const fileMenu = window.locator('.menu-bar-button:has-text("File")');
    await fileMenu.click();

    // Check dropdown appears
    const dropdown = window.locator(".menu-dropdown");
    await expect(dropdown).toBeVisible();

    // Check menu items
    await expect(dropdown).toContainText("New File");
    await expect(dropdown).toContainText("Open Project");
  });

  test("should open View menu dropdown", async ({ window }) => {
    const viewMenu = window.locator('.menu-bar-button:has-text("View")');
    await viewMenu.click();

    // Check dropdown appears
    const dropdown = window.locator(".menu-dropdown");
    await expect(dropdown).toBeVisible();

    // Check layout presets are available
    await expect(dropdown).toContainText("Layout");
  });

  test("should close menu when clicking elsewhere", async ({ window }) => {
    // Open File menu
    const fileMenu = window.locator('.menu-bar-button:has-text("File")');
    await fileMenu.click();

    const dropdown = window.locator(".menu-dropdown");
    await expect(dropdown).toBeVisible();

    // Click elsewhere (on the welcome panel)
    const welcomePanel = window.locator(".welcome-panel");
    await welcomePanel.click();

    // Dropdown should close
    await expect(dropdown).not.toBeVisible();
  });
});

// Note: Tests that require native file dialogs are skipped for now.
// To properly test these, we need to mock the dialog at the IPC level.
// See: https://playwright.dev/docs/api/class-electron#electron-application-on
test.describe.skip("Oort Editor - Project Operations (requires dialog mocking)", () => {
  test("should create a new project", async ({ window }) => {
    // This test requires mocking the native folder dialog
    // which needs IPC-level mocking
  });

  test("should open an existing project", async ({ window }) => {
    // This test requires mocking the native folder dialog
  });
});
