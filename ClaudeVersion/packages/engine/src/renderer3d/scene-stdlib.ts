/**
 * Scene Standard Library - 3D entity manipulation for Slate
 *
 * Provides functions for creating and manipulating 3D entities from Slate scripts.
 * These functions bridge Slate values to the Three.js rendering system.
 */

import {
  type SlateValue,
  type SlateNativeFunction,
  Num,
  Str,
  Bool,
  Null,
  Record,
  isNumber,
  isString,
  isRecord,
  RuntimeError,
} from "@oort/core";

// ============================================================================
// Entity Registry
// ============================================================================

/**
 * Entities created from scripts, keyed by ID.
 * This is populated by the runtime and used by scene functions.
 */
export interface Entity3D {
  id: string;
  name: string;
  position: [number, number, number];
  rotation: [number, number, number];
  scale: [number, number, number];
  mesh?: string;
  enabled: boolean;
}

const entities = new Map<string, Entity3D>();

// Callbacks for when entities change (set by runtime to update ThreeService)
type EntityCallback = (entity: Entity3D) => void;
let onEntitySpawned: EntityCallback | null = null;
let onEntityUpdated: EntityCallback | null = null;
let onEntityDestroyed: ((id: string) => void) | null = null;

export function setEntityCallbacks(callbacks: {
  onSpawned?: EntityCallback;
  onUpdated?: EntityCallback;
  onDestroyed?: (id: string) => void;
}): void {
  onEntitySpawned = callbacks.onSpawned || null;
  onEntityUpdated = callbacks.onUpdated || null;
  onEntityDestroyed = callbacks.onDestroyed || null;
}

export function getEntity(id: string): Entity3D | undefined {
  return entities.get(id);
}

export function getAllEntities(): Entity3D[] {
  return Array.from(entities.values());
}

export function clearEntities(): void {
  entities.clear();
}

// ============================================================================
// Helpers
// ============================================================================

function native(
  name: string,
  arity: number | "variadic",
  fn: (args: SlateValue[]) => SlateValue
): SlateNativeFunction {
  return { type: "native", name, arity, fn };
}

function assertNumber(v: SlateValue, name: string): number {
  if (v.type !== "number") {
    throw new RuntimeError(`${name} expects a number, got ${v.type}`);
  }
  return v.value;
}

function assertString(v: SlateValue, name: string): string {
  if (v.type !== "string") {
    throw new RuntimeError(`${name} expects a string, got ${v.type}`);
  }
  return v.value;
}

function assertRecord(v: SlateValue, name: string): Map<string, SlateValue> {
  if (v.type !== "record") {
    throw new RuntimeError(`${name} expects a record, got ${v.type}`);
  }
  return v.fields;
}

function vec3ToArray(v: SlateValue, name: string): [number, number, number] {
  const fields = assertRecord(v, name);
  return [
    assertNumber(fields.get("x") || Num(0), name),
    assertNumber(fields.get("y") || Num(0), name),
    assertNumber(fields.get("z") || Num(0), name),
  ];
}

function arrayToVec3(arr: [number, number, number]): SlateValue {
  const fields = new Map<string, SlateValue>();
  fields.set("x", Num(arr[0]));
  fields.set("y", Num(arr[1]));
  fields.set("z", Num(arr[2]));
  return Record(fields);
}

function entityToRecord(entity: Entity3D): SlateValue {
  const fields = new Map<string, SlateValue>();
  fields.set("id", Str(entity.id));
  fields.set("name", Str(entity.name));
  fields.set("position", arrayToVec3(entity.position));
  fields.set("rotation", arrayToVec3(entity.rotation));
  fields.set("scale", arrayToVec3(entity.scale));
  fields.set("enabled", Bool(entity.enabled));
  if (entity.mesh) {
    fields.set("mesh", Str(entity.mesh));
  }
  return Record(fields);
}

// ============================================================================
// Entity Functions
// ============================================================================

/**
 * spawn3d(config) - Create a 3D entity
 *
 * Config fields:
 * - id: string (required)
 * - name: string (optional, defaults to id)
 * - mesh: string (optional, path to glTF file)
 * - position: vec3 (optional, defaults to 0,0,0)
 * - rotation: vec3 (optional, degrees, defaults to 0,0,0)
 * - scale: vec3 (optional, defaults to 1,1,1)
 */
export const spawn3d = native("spawn3d", 1, ([config]) => {
  const fields = assertRecord(config, "spawn3d");

  const idVal = fields.get("id");
  if (!idVal || idVal.type !== "string") {
    throw new RuntimeError("spawn3d requires an 'id' field");
  }
  const id = idVal.value;

  if (entities.has(id)) {
    throw new RuntimeError(`Entity with id '${id}' already exists`);
  }

  const entity: Entity3D = {
    id,
    name: fields.get("name")?.type === "string" ? fields.get("name")!.value : id,
    position: fields.get("position") ? vec3ToArray(fields.get("position")!, "spawn3d") : [0, 0, 0],
    rotation: fields.get("rotation") ? vec3ToArray(fields.get("rotation")!, "spawn3d") : [0, 0, 0],
    scale: fields.get("scale") ? vec3ToArray(fields.get("scale")!, "spawn3d") : [1, 1, 1],
    mesh: fields.get("mesh")?.type === "string" ? fields.get("mesh")!.value : undefined,
    enabled: true,
  };

  entities.set(id, entity);
  onEntitySpawned?.(entity);

  return entityToRecord(entity);
});

/**
 * destroy3d(entityOrId) - Remove a 3D entity
 */
export const destroy3d = native("destroy3d", 1, ([entityOrId]) => {
  let id: string;

  if (isString(entityOrId)) {
    id = entityOrId.value;
  } else if (isRecord(entityOrId)) {
    const idField = entityOrId.fields.get("id");
    if (!idField || idField.type !== "string") {
      throw new RuntimeError("destroy3d: entity record must have an 'id' field");
    }
    id = idField.value;
  } else {
    throw new RuntimeError("destroy3d expects an entity record or id string");
  }

  if (!entities.has(id)) {
    return Bool(false);
  }

  entities.delete(id);
  onEntityDestroyed?.(id);
  return Bool(true);
});

/**
 * getEntity3d(id) - Get an entity by ID
 */
export const getEntity3d = native("getEntity3d", 1, ([idVal]) => {
  const id = assertString(idVal, "getEntity3d");
  const entity = entities.get(id);

  if (!entity) {
    return Null();
  }

  return entityToRecord(entity);
});

// ============================================================================
// Transform Functions
// ============================================================================

/**
 * setPosition(entityOrId, vec3) - Set entity position
 */
export const setPosition = native("setPosition", 2, ([entityOrId, pos]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "setPosition").get("id")!, "setPosition");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  entity.position = vec3ToArray(pos, "setPosition");
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

/**
 * getPosition(entityOrId) - Get entity position
 */
export const getPosition = native("getPosition", 1, ([entityOrId]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "getPosition").get("id")!, "getPosition");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  return arrayToVec3(entity.position);
});

/**
 * setRotation(entityOrId, vec3) - Set entity rotation (degrees)
 */
export const setRotation = native("setRotation", 2, ([entityOrId, rot]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "setRotation").get("id")!, "setRotation");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  entity.rotation = vec3ToArray(rot, "setRotation");
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

/**
 * getRotation(entityOrId) - Get entity rotation (degrees)
 */
export const getRotation = native("getRotation", 1, ([entityOrId]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "getRotation").get("id")!, "getRotation");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  return arrayToVec3(entity.rotation);
});

/**
 * setScale(entityOrId, vec3) - Set entity scale
 */
export const setScale = native("setScale", 2, ([entityOrId, scale]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "setScale").get("id")!, "setScale");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  entity.scale = vec3ToArray(scale, "setScale");
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

/**
 * getScale(entityOrId) - Get entity scale
 */
export const getScale = native("getScale", 1, ([entityOrId]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "getScale").get("id")!, "getScale");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  return arrayToVec3(entity.scale);
});

/**
 * translate(entityOrId, vec3) - Move entity by offset
 */
export const translate = native("translate", 2, ([entityOrId, offset]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "translate").get("id")!, "translate");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  const off = vec3ToArray(offset, "translate");
  entity.position = [
    entity.position[0] + off[0],
    entity.position[1] + off[1],
    entity.position[2] + off[2],
  ];
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

/**
 * rotate(entityOrId, vec3) - Rotate entity by degrees
 */
export const rotate = native("rotate", 2, ([entityOrId, angles]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "rotate").get("id")!, "rotate");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  const rot = vec3ToArray(angles, "rotate");
  entity.rotation = [
    entity.rotation[0] + rot[0],
    entity.rotation[1] + rot[1],
    entity.rotation[2] + rot[2],
  ];
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

// ============================================================================
// Entity State Functions
// ============================================================================

/**
 * enable3d(entityOrId) - Enable entity
 */
export const enable3d = native("enable3d", 1, ([entityOrId]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "enable3d").get("id")!, "enable3d");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  entity.enabled = true;
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

/**
 * disable3d(entityOrId) - Disable entity
 */
export const disable3d = native("disable3d", 1, ([entityOrId]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "disable3d").get("id")!, "disable3d");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  entity.enabled = false;
  onEntityUpdated?.(entity);
  return entityToRecord(entity);
});

/**
 * isEnabled3d(entityOrId) - Check if entity is enabled
 */
export const isEnabled3d = native("isEnabled3d", 1, ([entityOrId]) => {
  const id = isString(entityOrId) ? entityOrId.value : assertString(assertRecord(entityOrId, "isEnabled3d").get("id")!, "isEnabled3d");
  const entity = entities.get(id);

  if (!entity) {
    throw new RuntimeError(`Entity '${id}' not found`);
  }

  return Bool(entity.enabled);
});

// ============================================================================
// Scene Query Functions
// ============================================================================

/**
 * findEntities3d() - Get all entities
 */
export const findEntities3d = native("findEntities3d", 0, () => {
  const list: SlateValue[] = [];
  for (const entity of entities.values()) {
    list.push(entityToRecord(entity));
  }
  return { type: "list" as const, elements: list };
});

// ============================================================================
// Stdlib Export
// ============================================================================

export const sceneStdlib: Map<string, SlateNativeFunction> = new Map([
  // Entity management
  ["spawn3d", spawn3d],
  ["destroy3d", destroy3d],
  ["getEntity3d", getEntity3d],
  ["findEntities3d", findEntities3d],

  // Transform
  ["setPosition", setPosition],
  ["getPosition", getPosition],
  ["setRotation", setRotation],
  ["getRotation", getRotation],
  ["setScale", setScale],
  ["getScale", getScale],
  ["translate", translate],
  ["rotate", rotate],

  // State
  ["enable3d", enable3d],
  ["disable3d", disable3d],
  ["isEnabled3d", isEnabled3d],
]);
