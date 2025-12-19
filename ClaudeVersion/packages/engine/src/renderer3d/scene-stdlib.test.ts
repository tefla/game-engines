import { describe, it, expect, beforeEach, vi } from "vitest";
import {
  sceneStdlib,
  setEntityCallbacks,
  getEntity,
  getAllEntities,
  clearEntities,
} from "./scene-stdlib";
import { Num, Str, Record, Bool } from "@oort/core";

// Helper to create vec3 record
function vec3(x: number, y: number, z: number) {
  const fields = new Map();
  fields.set("x", Num(x));
  fields.set("y", Num(y));
  fields.set("z", Num(z));
  return Record(fields);
}

// Helper to call a stdlib function
function call(name: string, ...args: any[]) {
  const fn = sceneStdlib.get(name);
  if (!fn) throw new Error(`Function ${name} not found`);
  return fn.fn(args);
}

describe("scene-stdlib", () => {
  beforeEach(() => {
    clearEntities();
    setEntityCallbacks({});
  });

  describe("spawn3d", () => {
    it("should create entity with required id", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      const result = call("spawn3d", config);

      expect(result.type).toBe("record");
      expect(result.fields.get("id")?.value).toBe("entity1");
    });

    it("should use id as name if name not provided", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      const result = call("spawn3d", config);

      expect(result.fields.get("name")?.value).toBe("entity1");
    });

    it("should use provided name", () => {
      const config = Record(
        new Map([
          ["id", Str("entity1")],
          ["name", Str("My Entity")],
        ])
      );
      const result = call("spawn3d", config);

      expect(result.fields.get("name")?.value).toBe("My Entity");
    });

    it("should set default position to origin", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      const result = call("spawn3d", config);

      const pos = result.fields.get("position");
      expect(pos?.fields.get("x")?.value).toBe(0);
      expect(pos?.fields.get("y")?.value).toBe(0);
      expect(pos?.fields.get("z")?.value).toBe(0);
    });

    it("should accept custom position", () => {
      const config = Record(
        new Map([
          ["id", Str("entity1")],
          ["position", vec3(1, 2, 3)],
        ])
      );
      const result = call("spawn3d", config);

      const pos = result.fields.get("position");
      expect(pos?.fields.get("x")?.value).toBe(1);
      expect(pos?.fields.get("y")?.value).toBe(2);
      expect(pos?.fields.get("z")?.value).toBe(3);
    });

    it("should store mesh path", () => {
      const config = Record(
        new Map([
          ["id", Str("entity1")],
          ["mesh", Str("/models/robot.glb")],
        ])
      );
      const result = call("spawn3d", config);

      expect(result.fields.get("mesh")?.value).toBe("/models/robot.glb");
    });

    it("should throw if id is missing", () => {
      const config = Record(new Map([["name", Str("Test")]]));
      expect(() => call("spawn3d", config)).toThrow("requires an 'id' field");
    });

    it("should throw if entity already exists", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);

      expect(() => call("spawn3d", config)).toThrow("already exists");
    });

    it("should call onSpawned callback", () => {
      const onSpawned = vi.fn();
      setEntityCallbacks({ onSpawned });

      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);

      expect(onSpawned).toHaveBeenCalledTimes(1);
      expect(onSpawned).toHaveBeenCalledWith(
        expect.objectContaining({ id: "entity1" })
      );
    });
  });

  describe("destroy3d", () => {
    it("should remove entity by id string", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);

      const result = call("destroy3d", Str("entity1"));

      expect(result.value).toBe(true);
      expect(getEntity("entity1")).toBeUndefined();
    });

    it("should remove entity by entity record", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      const entity = call("spawn3d", config);

      const result = call("destroy3d", entity);

      expect(result.value).toBe(true);
      expect(getEntity("entity1")).toBeUndefined();
    });

    it("should return false if entity not found", () => {
      const result = call("destroy3d", Str("nonexistent"));
      expect(result.value).toBe(false);
    });

    it("should call onDestroyed callback", () => {
      const onDestroyed = vi.fn();
      setEntityCallbacks({ onDestroyed });

      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);
      call("destroy3d", Str("entity1"));

      expect(onDestroyed).toHaveBeenCalledWith("entity1");
    });
  });

  describe("getEntity3d", () => {
    it("should return entity by id", () => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);

      const result = call("getEntity3d", Str("entity1"));

      expect(result.type).toBe("record");
      expect(result.fields.get("id")?.value).toBe("entity1");
    });

    it("should return null for nonexistent entity", () => {
      const result = call("getEntity3d", Str("nonexistent"));
      expect(result.type).toBe("null");
    });
  });

  describe("setPosition / getPosition", () => {
    beforeEach(() => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);
    });

    it("should set position", () => {
      call("setPosition", Str("entity1"), vec3(10, 20, 30));

      const entity = getEntity("entity1");
      expect(entity?.position).toEqual([10, 20, 30]);
    });

    it("should get position", () => {
      call("setPosition", Str("entity1"), vec3(10, 20, 30));

      const result = call("getPosition", Str("entity1"));
      expect(result.fields.get("x")?.value).toBe(10);
      expect(result.fields.get("y")?.value).toBe(20);
      expect(result.fields.get("z")?.value).toBe(30);
    });

    it("should call onUpdated callback", () => {
      const onUpdated = vi.fn();
      setEntityCallbacks({ onUpdated });

      call("setPosition", Str("entity1"), vec3(1, 2, 3));

      expect(onUpdated).toHaveBeenCalled();
    });

    it("should throw for nonexistent entity", () => {
      expect(() => call("setPosition", Str("nope"), vec3(1, 2, 3))).toThrow(
        "not found"
      );
    });
  });

  describe("setRotation / getRotation", () => {
    beforeEach(() => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);
    });

    it("should set rotation", () => {
      call("setRotation", Str("entity1"), vec3(0, 90, 0));

      const entity = getEntity("entity1");
      expect(entity?.rotation).toEqual([0, 90, 0]);
    });

    it("should get rotation", () => {
      call("setRotation", Str("entity1"), vec3(45, 90, 180));

      const result = call("getRotation", Str("entity1"));
      expect(result.fields.get("x")?.value).toBe(45);
      expect(result.fields.get("y")?.value).toBe(90);
      expect(result.fields.get("z")?.value).toBe(180);
    });
  });

  describe("setScale / getScale", () => {
    beforeEach(() => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);
    });

    it("should set scale", () => {
      call("setScale", Str("entity1"), vec3(2, 2, 2));

      const entity = getEntity("entity1");
      expect(entity?.scale).toEqual([2, 2, 2]);
    });

    it("should get scale", () => {
      call("setScale", Str("entity1"), vec3(0.5, 1, 2));

      const result = call("getScale", Str("entity1"));
      expect(result.fields.get("x")?.value).toBe(0.5);
      expect(result.fields.get("y")?.value).toBe(1);
      expect(result.fields.get("z")?.value).toBe(2);
    });
  });

  describe("translate", () => {
    it("should add offset to position", () => {
      const config = Record(
        new Map([
          ["id", Str("entity1")],
          ["position", vec3(10, 20, 30)],
        ])
      );
      call("spawn3d", config);

      call("translate", Str("entity1"), vec3(5, 5, 5));

      const entity = getEntity("entity1");
      expect(entity?.position).toEqual([15, 25, 35]);
    });
  });

  describe("rotate", () => {
    it("should add angles to rotation", () => {
      const config = Record(
        new Map([
          ["id", Str("entity1")],
          ["rotation", vec3(0, 45, 0)],
        ])
      );
      call("spawn3d", config);

      call("rotate", Str("entity1"), vec3(0, 45, 0));

      const entity = getEntity("entity1");
      expect(entity?.rotation).toEqual([0, 90, 0]);
    });
  });

  describe("enable3d / disable3d / isEnabled3d", () => {
    beforeEach(() => {
      const config = Record(new Map([["id", Str("entity1")]]));
      call("spawn3d", config);
    });

    it("should disable entity", () => {
      call("disable3d", Str("entity1"));

      const result = call("isEnabled3d", Str("entity1"));
      expect(result.value).toBe(false);
    });

    it("should enable entity", () => {
      call("disable3d", Str("entity1"));
      call("enable3d", Str("entity1"));

      const result = call("isEnabled3d", Str("entity1"));
      expect(result.value).toBe(true);
    });
  });

  describe("findEntities3d", () => {
    it("should return empty list when no entities", () => {
      const result = call("findEntities3d");
      expect(result.type).toBe("list");
      expect(result.elements).toHaveLength(0);
    });

    it("should return all entities", () => {
      call("spawn3d", Record(new Map([["id", Str("entity1")]])));
      call("spawn3d", Record(new Map([["id", Str("entity2")]])));
      call("spawn3d", Record(new Map([["id", Str("entity3")]])));

      const result = call("findEntities3d");
      expect(result.elements).toHaveLength(3);
    });
  });

  describe("helper functions", () => {
    it("getAllEntities should return all entities", () => {
      call("spawn3d", Record(new Map([["id", Str("e1")]])));
      call("spawn3d", Record(new Map([["id", Str("e2")]])));

      const entities = getAllEntities();
      expect(entities).toHaveLength(2);
    });

    it("clearEntities should remove all entities", () => {
      call("spawn3d", Record(new Map([["id", Str("e1")]])));
      call("spawn3d", Record(new Map([["id", Str("e2")]])));

      clearEntities();

      expect(getAllEntities()).toHaveLength(0);
    });
  });

  describe("sceneStdlib map", () => {
    it("should contain all expected functions", () => {
      const expectedFunctions = [
        "spawn3d",
        "destroy3d",
        "getEntity3d",
        "findEntities3d",
        "setPosition",
        "getPosition",
        "setRotation",
        "getRotation",
        "setScale",
        "getScale",
        "translate",
        "rotate",
        "enable3d",
        "disable3d",
        "isEnabled3d",
      ];

      for (const name of expectedFunctions) {
        expect(sceneStdlib.has(name)).toBe(true);
      }
    });
  });
});
