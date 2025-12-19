/**
 * Helper for defining extensions with type safety
 */

import type { ExtensionContext, ExtensionModule } from "./types.js";

/**
 * Define an extension with full type inference
 *
 * @example
 * ```typescript
 * export default defineExtension({
 *   activate(ctx) {
 *     ctx.commands.register({
 *       id: "my-extension.hello",
 *       title: "Say Hello",
 *       handler: () => console.log("Hello!"),
 *     });
 *   },
 *   deactivate() {
 *     console.log("Extension deactivated");
 *   },
 * });
 * ```
 */
export function defineExtension(module: ExtensionModule): ExtensionModule {
  return module;
}

/**
 * Helper type for extension activate function
 */
export type ActivateFunction = (context: ExtensionContext) => void | Promise<void>;

/**
 * Helper type for extension deactivate function
 */
export type DeactivateFunction = () => void | Promise<void>;
