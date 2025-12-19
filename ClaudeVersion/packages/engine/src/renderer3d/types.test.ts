import { describe, it, expect } from "vitest";
import {
  isValidTransform,
  isValidScene,
  mergeTransforms,
  DEFAULT_TRANSFORM,
  type Transform3D,
  type OortScene,
} from "./types";

describe("types", () => {
  describe("DEFAULT_TRANSFORM", () => {
    it("should have correct default values", () => {
      expect(DEFAULT_TRANSFORM.position).toEqual([0, 0, 0]);
      expect(DEFAULT_TRANSFORM.rotation).toEqual([0, 0, 0]);
      expect(DEFAULT_TRANSFORM.scale).toEqual([1, 1, 1]);
    });
  });

  describe("isValidTransform", () => {
    it("should return true for valid transform", () => {
      const transform: Transform3D = {
        position: [1, 2, 3],
        rotation: [0, 90, 0],
        scale: [1, 1, 1],
      };
      expect(isValidTransform(transform)).toBe(true);
    });

    it("should return false for null", () => {
      expect(isValidTransform(null)).toBe(false);
    });

    it("should return false for undefined", () => {
      expect(isValidTransform(undefined)).toBe(false);
    });

    it("should return false for non-object", () => {
      expect(isValidTransform("not an object")).toBe(false);
      expect(isValidTransform(123)).toBe(false);
    });

    it("should return false for missing position", () => {
      expect(
        isValidTransform({
          rotation: [0, 0, 0],
          scale: [1, 1, 1],
        })
      ).toBe(false);
    });

    it("should return false for missing rotation", () => {
      expect(
        isValidTransform({
          position: [0, 0, 0],
          scale: [1, 1, 1],
        })
      ).toBe(false);
    });

    it("should return false for missing scale", () => {
      expect(
        isValidTransform({
          position: [0, 0, 0],
          rotation: [0, 0, 0],
        })
      ).toBe(false);
    });

    it("should return false for wrong array length", () => {
      expect(
        isValidTransform({
          position: [0, 0], // too short
          rotation: [0, 0, 0],
          scale: [1, 1, 1],
        })
      ).toBe(false);

      expect(
        isValidTransform({
          position: [0, 0, 0, 0], // too long
          rotation: [0, 0, 0],
          scale: [1, 1, 1],
        })
      ).toBe(false);
    });

    it("should return false for non-array position", () => {
      expect(
        isValidTransform({
          position: "not an array",
          rotation: [0, 0, 0],
          scale: [1, 1, 1],
        })
      ).toBe(false);
    });
  });

  describe("isValidScene", () => {
    it("should return true for valid scene", () => {
      const scene: OortScene = {
        version: "1.0",
        name: "Test Scene",
        settings: {
          background: "#000000",
          ambientLight: { color: "#ffffff", intensity: 0.5 },
        },
        entities: [],
      };
      expect(isValidScene(scene)).toBe(true);
    });

    it("should return true for scene with entities", () => {
      const scene: OortScene = {
        version: "1.0",
        name: "Test Scene",
        settings: {
          background: "#000000",
          ambientLight: { color: "#ffffff", intensity: 0.5 },
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
      expect(isValidScene(scene)).toBe(true);
    });

    it("should return false for null", () => {
      expect(isValidScene(null)).toBe(false);
    });

    it("should return false for undefined", () => {
      expect(isValidScene(undefined)).toBe(false);
    });

    it("should return false for wrong version", () => {
      expect(
        isValidScene({
          version: "2.0",
          name: "Test",
          settings: {},
          entities: [],
        })
      ).toBe(false);
    });

    it("should return false for missing name", () => {
      expect(
        isValidScene({
          version: "1.0",
          settings: {},
          entities: [],
        })
      ).toBe(false);
    });

    it("should return false for non-string name", () => {
      expect(
        isValidScene({
          version: "1.0",
          name: 123,
          settings: {},
          entities: [],
        })
      ).toBe(false);
    });

    it("should return false for missing settings", () => {
      expect(
        isValidScene({
          version: "1.0",
          name: "Test",
          entities: [],
        })
      ).toBe(false);
    });

    it("should return false for non-array entities", () => {
      expect(
        isValidScene({
          version: "1.0",
          name: "Test",
          settings: {},
          entities: "not an array",
        })
      ).toBe(false);
    });
  });

  describe("mergeTransforms", () => {
    it("should return base transform when override is empty", () => {
      const base: Transform3D = {
        position: [1, 2, 3],
        rotation: [10, 20, 30],
        scale: [2, 2, 2],
      };
      const result = mergeTransforms(base, {});
      expect(result).toEqual(base);
    });

    it("should override position only", () => {
      const base: Transform3D = {
        position: [1, 2, 3],
        rotation: [10, 20, 30],
        scale: [2, 2, 2],
      };
      const result = mergeTransforms(base, { position: [5, 5, 5] });
      expect(result.position).toEqual([5, 5, 5]);
      expect(result.rotation).toEqual([10, 20, 30]);
      expect(result.scale).toEqual([2, 2, 2]);
    });

    it("should override rotation only", () => {
      const base: Transform3D = {
        position: [1, 2, 3],
        rotation: [10, 20, 30],
        scale: [2, 2, 2],
      };
      const result = mergeTransforms(base, { rotation: [45, 45, 45] });
      expect(result.position).toEqual([1, 2, 3]);
      expect(result.rotation).toEqual([45, 45, 45]);
      expect(result.scale).toEqual([2, 2, 2]);
    });

    it("should override scale only", () => {
      const base: Transform3D = {
        position: [1, 2, 3],
        rotation: [10, 20, 30],
        scale: [2, 2, 2],
      };
      const result = mergeTransforms(base, { scale: [0.5, 0.5, 0.5] });
      expect(result.position).toEqual([1, 2, 3]);
      expect(result.rotation).toEqual([10, 20, 30]);
      expect(result.scale).toEqual([0.5, 0.5, 0.5]);
    });

    it("should override multiple properties", () => {
      const base: Transform3D = {
        position: [1, 2, 3],
        rotation: [10, 20, 30],
        scale: [2, 2, 2],
      };
      const result = mergeTransforms(base, {
        position: [0, 0, 0],
        scale: [1, 1, 1],
      });
      expect(result.position).toEqual([0, 0, 0]);
      expect(result.rotation).toEqual([10, 20, 30]);
      expect(result.scale).toEqual([1, 1, 1]);
    });

    it("should not mutate base transform", () => {
      const base: Transform3D = {
        position: [1, 2, 3],
        rotation: [10, 20, 30],
        scale: [2, 2, 2],
      };
      const originalPosition = [...base.position];
      mergeTransforms(base, { position: [5, 5, 5] });
      expect(base.position).toEqual(originalPosition);
    });
  });
});
