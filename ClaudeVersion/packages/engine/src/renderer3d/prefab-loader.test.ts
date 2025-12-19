import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  PrefabLoader,
  createEditorPrefabLoader,
  createRuntimePrefabLoader,
} from "./prefab-loader";
import { DEFAULT_TRANSFORM, type OortScene } from "./types";

// Helper to create mock prefab scenes
function createMockPrefab(name: string, entityId = "root"): OortScene {
  return {
    version: "1.0",
    name,
    settings: {
      background: "#000",
      ambientLight: { color: "#fff", intensity: 0.5 },
    },
    entities: [
      {
        id: entityId,
        name,
        enabled: true,
        transform: DEFAULT_TRANSFORM,
        components: [{ type: "mesh", source: "/default.glb" }],
        properties: { defaultProp: "value" },
      },
    ],
  };
}

describe("prefab-loader", () => {
  describe("PrefabLoader", () => {
    let loader: PrefabLoader;
    let mockReadFile: ReturnType<typeof vi.fn>;

    beforeEach(() => {
      mockReadFile = vi.fn();
      loader = new PrefabLoader({
        readFile: mockReadFile,
        basePath: "/project",
        maxCacheSize: 10,
        cacheTTL: 0,
      });
    });

    describe("loadPrefab", () => {
      it("should load and parse prefab file", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const result = await loader.loadPrefab("prefabs/test.oort-scene");

        expect(mockReadFile).toHaveBeenCalledWith(
          "/project/prefabs/test.oort-scene"
        );
        expect(result.name).toBe("TestPrefab");
        expect(result.entities.length).toBe(1);
      });

      it("should cache loaded prefabs", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        await loader.loadPrefab("test.oort-scene");
        await loader.loadPrefab("test.oort-scene");

        expect(mockReadFile).toHaveBeenCalledTimes(1);
      });

      it("should throw on invalid JSON", async () => {
        mockReadFile.mockResolvedValue("not valid json");

        await expect(loader.loadPrefab("test.oort-scene")).rejects.toThrow(
          "Invalid prefab JSON"
        );
      });

      it("should throw on invalid prefab format", async () => {
        mockReadFile.mockResolvedValue('{"foo": "bar"}');

        await expect(loader.loadPrefab("test.oort-scene")).rejects.toThrow(
          "Invalid prefab format"
        );
      });

      it("should handle absolute paths", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        await loader.loadPrefab("/absolute/path/test.oort-scene");

        expect(mockReadFile).toHaveBeenCalledWith(
          "/absolute/path/test.oort-scene"
        );
      });

      it("should deduplicate concurrent loads", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockImplementation(
          () =>
            new Promise((resolve) =>
              setTimeout(() => resolve(JSON.stringify(prefab)), 10)
            )
        );

        const [result1, result2] = await Promise.all([
          loader.loadPrefab("test.oort-scene"),
          loader.loadPrefab("test.oort-scene"),
        ]);

        expect(mockReadFile).toHaveBeenCalledTimes(1);
        expect(result1).toBe(result2);
      });
    });

    describe("isLoaded", () => {
      it("should return false for unloaded prefab", () => {
        expect(loader.isLoaded("test.oort-scene")).toBe(false);
      });

      it("should return true for loaded prefab", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        await loader.loadPrefab("test.oort-scene");

        expect(loader.isLoaded("test.oort-scene")).toBe(true);
      });
    });

    describe("preload", () => {
      it("should preload multiple prefabs", async () => {
        mockReadFile.mockImplementation((path: string) => {
          const name = path.split("/").pop()?.replace(".oort-scene", "") || "";
          return Promise.resolve(JSON.stringify(createMockPrefab(name)));
        });

        await loader.preload(["prefab1.oort-scene", "prefab2.oort-scene"]);

        expect(loader.isLoaded("prefab1.oort-scene")).toBe(true);
        expect(loader.isLoaded("prefab2.oort-scene")).toBe(true);
      });
    });

    describe("instantiate", () => {
      it("should create instance with new id", async () => {
        const prefab = createMockPrefab("TestPrefab", "prefab_root");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const instance = await loader.instantiate(
          "test.oort-scene",
          "instance_1"
        );

        expect(instance.id).toBe("instance_1");
        expect(instance.prefab).toBe("test.oort-scene");
      });

      it("should clone prefab data", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const instance = await loader.instantiate(
          "test.oort-scene",
          "instance_1"
        );

        // Modify instance should not affect prefab
        instance.name = "Modified";

        const instance2 = await loader.instantiate(
          "test.oort-scene",
          "instance_2"
        );
        expect(instance2.name).toBe("TestPrefab");
      });

      it("should apply position override", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const instance = await loader.instantiate(
          "test.oort-scene",
          "instance_1",
          { position: [10, 20, 30] }
        );

        expect(instance.transform.position).toEqual([10, 20, 30]);
      });

      it("should apply rotation override", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const instance = await loader.instantiate(
          "test.oort-scene",
          "instance_1",
          { rotation: [0, 90, 0] }
        );

        expect(instance.transform.rotation).toEqual([0, 90, 0]);
      });

      it("should apply scale override", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const instance = await loader.instantiate(
          "test.oort-scene",
          "instance_1",
          { scale: [2, 2, 2] }
        );

        expect(instance.transform.scale).toEqual([2, 2, 2]);
      });

      it("should apply property overrides", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        const instance = await loader.instantiate(
          "test.oort-scene",
          "instance_1",
          {
            overrides: {
              properties: { customProp: "custom value" },
            },
          }
        );

        expect(instance.properties?.customProp).toBe("custom value");
        expect(instance.properties?.defaultProp).toBe("value");
      });

      it("should throw if prefab has no root entity", async () => {
        const emptyPrefab: OortScene = {
          version: "1.0",
          name: "Empty",
          settings: {
            background: "#000",
            ambientLight: { color: "#fff", intensity: 0.5 },
          },
          entities: [],
        };
        mockReadFile.mockResolvedValue(JSON.stringify(emptyPrefab));

        await expect(
          loader.instantiate("test.oort-scene", "instance_1")
        ).rejects.toThrow("has no root entity");
      });
    });

    describe("cache management", () => {
      it("should evict oldest entry when cache is full", async () => {
        const smallLoader = new PrefabLoader({
          readFile: mockReadFile,
          basePath: "",
          maxCacheSize: 2,
        });

        mockReadFile.mockImplementation((path: string) =>
          Promise.resolve(JSON.stringify(createMockPrefab(path)))
        );

        await smallLoader.loadPrefab("prefab1.oort-scene");
        await smallLoader.loadPrefab("prefab2.oort-scene");
        await smallLoader.loadPrefab("prefab3.oort-scene");

        const stats = smallLoader.getCacheStats();
        expect(stats.size).toBe(2);
        expect(stats.paths).not.toContain("/prefab1.oort-scene");
      });

      it("should clear cache", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        await loader.loadPrefab("test.oort-scene");
        expect(loader.isLoaded("test.oort-scene")).toBe(true);

        loader.clearCache();
        expect(loader.isLoaded("test.oort-scene")).toBe(false);
      });

      it("should report cache stats", async () => {
        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        await loader.loadPrefab("test.oort-scene");

        const stats = loader.getCacheStats();
        expect(stats.size).toBe(1);
        expect(stats.maxSize).toBe(10);
        expect(stats.paths).toContain("/project/test.oort-scene");
      });
    });

    describe("cache TTL", () => {
      it("should expire cached entries after TTL", async () => {
        const loaderWithTTL = new PrefabLoader({
          readFile: mockReadFile,
          basePath: "",
          cacheTTL: 50, // 50ms TTL
        });

        const prefab = createMockPrefab("TestPrefab");
        mockReadFile.mockResolvedValue(JSON.stringify(prefab));

        await loaderWithTTL.loadPrefab("test.oort-scene");
        expect(loaderWithTTL.isLoaded("test.oort-scene")).toBe(true);

        // Wait for TTL to expire
        await new Promise((resolve) => setTimeout(resolve, 60));

        expect(loaderWithTTL.isLoaded("test.oort-scene")).toBe(false);
      });
    });
  });

  describe("factory functions", () => {
    it("createEditorPrefabLoader should create loader with editor defaults", () => {
      const mockReadFile = vi.fn();
      const loader = createEditorPrefabLoader(mockReadFile, "/project");

      const stats = loader.getCacheStats();
      expect(stats.maxSize).toBe(50);
    });

    it("createRuntimePrefabLoader should create loader with runtime defaults", () => {
      const mockReadFile = vi.fn();
      const loader = createRuntimePrefabLoader(mockReadFile, "/game");

      const stats = loader.getCacheStats();
      expect(stats.maxSize).toBe(100);
    });
  });
});
