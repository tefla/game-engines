import { describe, it, expect } from "vitest";
import {
  loadScene,
  serializeScene,
  createEmptyScene,
  createEntity,
} from "./scene-loader";
import { DEFAULT_TRANSFORM, type OortScene } from "./types";

describe("scene-loader", () => {
  describe("createEmptyScene", () => {
    it("should create scene with correct name", () => {
      const scene = createEmptyScene("Test Scene");
      expect(scene.name).toBe("Test Scene");
    });

    it("should have version 1.0", () => {
      const scene = createEmptyScene("Test");
      expect(scene.version).toBe("1.0");
    });

    it("should have default settings", () => {
      const scene = createEmptyScene("Test");
      expect(scene.settings.background).toBe("#1a1a2e");
      expect(scene.settings.ambientLight.color).toBe("#ffffff");
      expect(scene.settings.ambientLight.intensity).toBe(0.4);
    });

    it("should have empty entities array", () => {
      const scene = createEmptyScene("Test");
      expect(scene.entities).toEqual([]);
    });
  });

  describe("createEntity", () => {
    it("should create entity with id and name", () => {
      const entity = createEntity("entity1", "My Entity");
      expect(entity.id).toBe("entity1");
      expect(entity.name).toBe("My Entity");
    });

    it("should be enabled by default", () => {
      const entity = createEntity("entity1", "Test");
      expect(entity.enabled).toBe(true);
    });

    it("should have default transform", () => {
      const entity = createEntity("entity1", "Test");
      expect(entity.transform.position).toEqual([0, 0, 0]);
      expect(entity.transform.rotation).toEqual([0, 0, 0]);
      expect(entity.transform.scale).toEqual([1, 1, 1]);
    });

    it("should have empty components array", () => {
      const entity = createEntity("entity1", "Test");
      expect(entity.components).toEqual([]);
    });

    it("should accept options overrides", () => {
      const entity = createEntity("entity1", "Test", {
        enabled: false,
        transform: {
          position: [1, 2, 3],
          rotation: [0, 90, 0],
          scale: [2, 2, 2],
        },
        components: [{ type: "mesh", source: "/model.glb" }],
      });
      expect(entity.enabled).toBe(false);
      expect(entity.transform.position).toEqual([1, 2, 3]);
      expect(entity.components.length).toBe(1);
    });
  });

  describe("serializeScene", () => {
    it("should serialize scene to JSON string", () => {
      const scene = createEmptyScene("Test");
      const json = serializeScene(scene);
      expect(typeof json).toBe("string");
      const parsed = JSON.parse(json);
      expect(parsed.name).toBe("Test");
    });

    it("should pretty print by default", () => {
      const scene = createEmptyScene("Test");
      const json = serializeScene(scene);
      expect(json).toContain("\n");
    });

    it("should compact when pretty=false", () => {
      const scene = createEmptyScene("Test");
      const json = serializeScene(scene, false);
      expect(json).not.toContain("\n");
    });

    it("should round-trip correctly", () => {
      const scene = createEmptyScene("Test Scene");
      scene.entities.push(createEntity("e1", "Entity 1"));
      const json = serializeScene(scene);
      const parsed = JSON.parse(json);
      expect(parsed.name).toBe("Test Scene");
      expect(parsed.entities.length).toBe(1);
      expect(parsed.entities[0].name).toBe("Entity 1");
    });
  });

  describe("loadScene", () => {
    it("should load valid scene JSON", async () => {
      const sceneData: OortScene = {
        version: "1.0",
        name: "Test Scene",
        settings: {
          background: "#000000",
          ambientLight: { color: "#ffffff", intensity: 0.5 },
        },
        entities: [],
      };
      const json = JSON.stringify(sceneData);
      const result = await loadScene(json);

      expect(result.scene.name).toBe("Test Scene");
      expect(result.entities.size).toBe(0);
      expect(result.rootIds).toEqual([]);
    });

    it("should throw on invalid JSON", async () => {
      await expect(loadScene("not valid json")).rejects.toThrow(
        "Invalid scene JSON"
      );
    });

    it("should throw on invalid scene format", async () => {
      await expect(loadScene('{"foo": "bar"}')).rejects.toThrow(
        "Invalid scene format"
      );
    });

    it("should throw on wrong version", async () => {
      const data = {
        version: "2.0",
        name: "Test",
        settings: { background: "#000", ambientLight: { color: "#fff", intensity: 1 } },
        entities: [],
      };
      await expect(loadScene(JSON.stringify(data))).rejects.toThrow(
        "Invalid scene format"
      );
    });

    it("should build entity map for single entity", async () => {
      const sceneData: OortScene = {
        version: "1.0",
        name: "Test",
        settings: {
          background: "#000",
          ambientLight: { color: "#fff", intensity: 0.5 },
        },
        entities: [
          {
            id: "entity1",
            name: "Entity 1",
            enabled: true,
            transform: DEFAULT_TRANSFORM,
            components: [],
          },
        ],
      };

      const result = await loadScene(JSON.stringify(sceneData));
      expect(result.entities.size).toBe(1);
      expect(result.entities.has("entity1")).toBe(true);
      expect(result.rootIds).toEqual(["entity1"]);
    });

    it("should build entity map for multiple entities", async () => {
      const sceneData: OortScene = {
        version: "1.0",
        name: "Test",
        settings: {
          background: "#000",
          ambientLight: { color: "#fff", intensity: 0.5 },
        },
        entities: [
          {
            id: "entity1",
            name: "Entity 1",
            enabled: true,
            transform: DEFAULT_TRANSFORM,
            components: [],
          },
          {
            id: "entity2",
            name: "Entity 2",
            enabled: true,
            transform: DEFAULT_TRANSFORM,
            components: [],
          },
        ],
      };

      const result = await loadScene(JSON.stringify(sceneData));
      expect(result.entities.size).toBe(2);
      expect(result.rootIds).toHaveLength(2);
    });

    it("should handle inline children", async () => {
      const sceneData: OortScene = {
        version: "1.0",
        name: "Test",
        settings: {
          background: "#000",
          ambientLight: { color: "#fff", intensity: 0.5 },
        },
        entities: [
          {
            id: "parent",
            name: "Parent",
            enabled: true,
            transform: DEFAULT_TRANSFORM,
            components: [],
            children: [
              {
                id: "child",
                name: "Child",
                enabled: true,
                transform: DEFAULT_TRANSFORM,
                components: [],
              },
            ],
          },
        ],
      };

      const result = await loadScene(JSON.stringify(sceneData));
      expect(result.entities.size).toBe(2);
      expect(result.rootIds).toEqual(["parent"]);

      const parent = result.entities.get("parent");
      const child = result.entities.get("child");

      expect(parent?.childIds).toContain("child");
      expect(child?.parentId).toBe("parent");
    });

    it("should calculate world transforms", async () => {
      const sceneData: OortScene = {
        version: "1.0",
        name: "Test",
        settings: {
          background: "#000",
          ambientLight: { color: "#fff", intensity: 0.5 },
        },
        entities: [
          {
            id: "parent",
            name: "Parent",
            enabled: true,
            transform: {
              position: [10, 0, 0],
              rotation: [0, 0, 0],
              scale: [2, 2, 2],
            },
            components: [],
            children: [
              {
                id: "child",
                name: "Child",
                enabled: true,
                transform: {
                  position: [5, 0, 0],
                  rotation: [0, 0, 0],
                  scale: [1, 1, 1],
                },
                components: [],
              },
            ],
          },
        ],
      };

      const result = await loadScene(JSON.stringify(sceneData));
      const child = result.entities.get("child");

      // Child position (5,0,0) * parent scale (2) + parent position (10,0,0) = (20,0,0)
      expect(child?.worldTransform.position).toEqual([20, 0, 0]);
      // Child scale (1,1,1) * parent scale (2,2,2) = (2,2,2)
      expect(child?.worldTransform.scale).toEqual([2, 2, 2]);
    });

    it("should call progress callback", async () => {
      const sceneData: OortScene = {
        version: "1.0",
        name: "Test",
        settings: {
          background: "#000",
          ambientLight: { color: "#fff", intensity: 0.5 },
        },
        entities: [],
      };

      const progressCalls: { progress: number; message: string }[] = [];
      await loadScene(JSON.stringify(sceneData), {
        onProgress: (progress, message) => {
          progressCalls.push({ progress, message });
        },
      });

      expect(progressCalls.length).toBeGreaterThan(0);
      expect(progressCalls[0].progress).toBe(0);
      expect(progressCalls[progressCalls.length - 1].progress).toBe(100);
    });
  });
});
