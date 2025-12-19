/**
 * Oort Scene Format Types
 *
 * Defines the structure for .oort-scene files, including:
 * - Scene settings (background, lighting, fog)
 * - Entity hierarchy with transforms
 * - Component system (mesh, light, camera, script)
 * - Prefab system with deep-merge overrides
 */

// ============================================================================
// Scene Root
// ============================================================================

export interface OortScene {
  /** Schema version */
  version: "1.0";

  /** Scene display name */
  name: string;

  /** Global scene settings */
  settings: SceneSettings;

  /** Root-level entities */
  entities: SceneEntity[];
}

// ============================================================================
// Scene Settings
// ============================================================================

export interface SceneSettings {
  /** Background color (hex string, e.g., "#1a1a2e") */
  background: string;

  /** Ambient light settings */
  ambientLight: {
    color: string;
    intensity: number;
  };

  /** Optional fog settings */
  fog?: FogSettings;

  /** Optional skybox */
  skybox?: SkyboxSettings;
}

export interface FogSettings {
  type: "linear" | "exponential";
  color: string;
  /** For linear fog */
  near?: number;
  far?: number;
  /** For exponential fog */
  density?: number;
}

export interface SkyboxSettings {
  /** Path to cubemap or equirectangular image */
  source: string;
  type: "cubemap" | "equirectangular";
}

// ============================================================================
// Transform
// ============================================================================

export interface Transform3D {
  /** Position in world space [x, y, z] */
  position: [number, number, number];

  /** Rotation in degrees (Euler angles) [x, y, z] */
  rotation: [number, number, number];

  /** Scale factors [x, y, z] */
  scale: [number, number, number];
}

/** Default transform values */
export const DEFAULT_TRANSFORM: Transform3D = {
  position: [0, 0, 0],
  rotation: [0, 0, 0],
  scale: [1, 1, 1],
};

// ============================================================================
// Entity
// ============================================================================

export interface SceneEntity {
  /** Unique identifier within the scene */
  id: string;

  /** Display name */
  name: string;

  /** Parent entity ID for hierarchy */
  parent?: string;

  /** Whether entity is active */
  enabled: boolean;

  /** Entity transform */
  transform: Transform3D;

  /** Attached components */
  components: Component[];

  /** Inline child entities (alternative to parent references) */
  children?: SceneEntity[];

  // ---- Prefab Support ----

  /** Path to prefab scene file (like Godot's PackedScene) */
  prefab?: string;

  /** Property overrides applied on top of prefab (deep-merged) */
  overrides?: EntityOverrides;

  /** Custom properties accessible from scripts */
  properties?: Record<string, unknown>;
}

// ============================================================================
// Prefab Override System
// ============================================================================

/**
 * Overrides applied to a prefab instance.
 * Uses deep-merge: only specified properties are overridden,
 * the rest come from the prefab.
 */
export interface EntityOverrides {
  /** Override transform properties */
  transform?: Partial<Transform3D>;

  /** Override specific components */
  components?: ComponentOverride[];

  /** Override custom properties */
  properties?: Record<string, unknown>;

  /** Override child entities (by ID) */
  children?: Record<string, Partial<EntityOverrides>>;
}

/**
 * Override for a specific component.
 * Matched by type (and optionally index if multiple of same type).
 */
export interface ComponentOverride {
  /** Component type to match */
  type: ComponentType;

  /** Index if multiple components of same type (0-based) */
  index?: number;

  /** Properties to override (merged into component) */
  [key: string]: unknown;
}

// ============================================================================
// Components
// ============================================================================

export type ComponentType =
  | "mesh"
  | "light"
  | "camera"
  | "script"
  | "collider"
  | "rigidbody"
  | "audio";

export type Component =
  | MeshComponent
  | LightComponent
  | CameraComponent
  | ScriptComponent
  | ColliderComponent
  | RigidbodyComponent
  | AudioComponent;

// ---- Mesh Component ----

export interface MeshComponent {
  type: "mesh";

  /** Path to .glb/.gltf file */
  source: string;

  /** Whether mesh casts shadows */
  castShadow?: boolean;

  /** Whether mesh receives shadows */
  receiveShadow?: boolean;

  /** Material overrides by slot */
  materials?: MaterialOverride[];
}

export interface MaterialOverride {
  /** Material slot (index or name) */
  slot: number | string;

  /** Override color */
  color?: string;

  /** Override texture path */
  texture?: string;

  /** PBR properties */
  metalness?: number;
  roughness?: number;
  emissive?: string;
  emissiveIntensity?: number;
}

// ---- Light Component ----

export interface LightComponent {
  type: "light";

  /** Light type */
  lightType: "point" | "spot" | "directional";

  /** Light color (hex string) */
  color: string;

  /** Light intensity */
  intensity: number;

  /** Whether light casts shadows */
  castShadow?: boolean;

  /** Shadow map size (power of 2) */
  shadowMapSize?: number;

  // Point/Spot light properties
  /** Maximum range (0 = infinite) */
  distance?: number;

  /** Light decay factor */
  decay?: number;

  // Spot light properties
  /** Spot angle in degrees */
  angle?: number;

  /** Penumbra (soft edge) 0-1 */
  penumbra?: number;
}

// ---- Camera Component ----

export interface CameraComponent {
  type: "camera";

  /** Camera projection type */
  cameraType: "perspective" | "orthographic";

  /** Field of view in degrees (perspective only) */
  fov?: number;

  /** Orthographic size (orthographic only) */
  orthoSize?: number;

  /** Near clipping plane */
  near: number;

  /** Far clipping plane */
  far: number;

  /** Whether this is the main camera */
  main?: boolean;
}

// ---- Script Component ----

export interface ScriptComponent {
  type: "script";

  /** Path to .sl script file */
  source: string;

  /** Properties passed to script */
  properties?: Record<string, unknown>;
}

// ---- Collider Component ----

export interface ColliderComponent {
  type: "collider";

  /** Collider shape */
  shape: "box" | "sphere" | "capsule" | "mesh";

  /** Size for box collider [x, y, z] */
  size?: [number, number, number];

  /** Radius for sphere/capsule */
  radius?: number;

  /** Height for capsule */
  height?: number;

  /** Whether this is a trigger (no physics response) */
  isTrigger?: boolean;

  /** Physics layer for collision filtering */
  layer?: string;
}

// ---- Rigidbody Component ----

export interface RigidbodyComponent {
  type: "rigidbody";

  /** Body type */
  bodyType: "dynamic" | "kinematic" | "static";

  /** Mass in kg */
  mass?: number;

  /** Linear drag */
  drag?: number;

  /** Angular drag */
  angularDrag?: number;

  /** Use gravity */
  useGravity?: boolean;

  /** Freeze position axes */
  freezePosition?: [boolean, boolean, boolean];

  /** Freeze rotation axes */
  freezeRotation?: [boolean, boolean, boolean];
}

// ---- Audio Component ----

export interface AudioComponent {
  type: "audio";

  /** Path to audio file */
  source: string;

  /** Volume 0-1 */
  volume?: number;

  /** Pitch multiplier */
  pitch?: number;

  /** Whether to loop */
  loop?: boolean;

  /** Play on start */
  playOnAwake?: boolean;

  /** Spatial audio settings */
  spatial?: boolean;
  minDistance?: number;
  maxDistance?: number;
}

// ============================================================================
// Utility Types
// ============================================================================

/** Entity lookup by ID */
export type EntityMap = Map<string, SceneEntity>;

/** Component lookup by entity ID */
export type ComponentMap<T extends Component = Component> = Map<string, T[]>;

/** Loaded prefab cache entry */
export interface PrefabCacheEntry {
  scene: OortScene;
  loadedAt: number;
}

// ============================================================================
// Validation Helpers
// ============================================================================

export function isValidTransform(t: unknown): t is Transform3D {
  if (!t || typeof t !== "object") return false;
  const obj = t as Record<string, unknown>;

  return (
    Array.isArray(obj.position) &&
    obj.position.length === 3 &&
    Array.isArray(obj.rotation) &&
    obj.rotation.length === 3 &&
    Array.isArray(obj.scale) &&
    obj.scale.length === 3
  );
}

export function isValidScene(s: unknown): s is OortScene {
  if (!s || typeof s !== "object") return false;
  const obj = s as Record<string, unknown>;

  return (
    obj.version === "1.0" &&
    typeof obj.name === "string" &&
    typeof obj.settings === "object" &&
    Array.isArray(obj.entities)
  );
}

export function mergeTransforms(
  base: Transform3D,
  override: Partial<Transform3D>
): Transform3D {
  return {
    position: override.position ?? base.position,
    rotation: override.rotation ?? base.rotation,
    scale: override.scale ?? base.scale,
  };
}
