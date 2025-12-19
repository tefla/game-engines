# 3D Engine Implementation Plan

## Overview
Add Three.js-based 3D rendering to the Oort game editor with minimal MVP scope: basic 3D rendering, glTF asset support, and custom scene format.

## User Choices
- **Rendering Library**: Three.js
- **Editor Scope**: Minimal MVP (basic 3D, add features later)
- **Asset Formats**: glTF/GLB + custom scene format

---

## Phase 1: Core Three.js Integration

### 1.1 Add Dependencies
**File**: `apps/oort-editor/package.json`
```json
"three": "^0.160.0",
"@types/three": "^0.160.0"
```

### 1.2 Create ThreeService
**New File**: `apps/oort-editor/src/renderer/core/three-service.ts`
- Singleton service managing Three.js instances
- Scene creation with default lighting/grid/axes
- WebGL renderer management
- Render loop control
- Asset caching (glTF models)
- Entity spawn/destroy methods

### 1.3 Create 3D Viewport Panel
**New Files**:
- `apps/oort-editor/src/renderer/panels/SceneViewport3D.tsx`
- `apps/oort-editor/src/renderer/panels/SceneViewport3D.css`

Features:
- Canvas with WebGL context
- OrbitControls (pan, zoom, rotate)
- Grid helper (20x20)
- Axes helper
- Default lighting (ambient + directional)
- Resize handling

### 1.4 Register Panel
**Modify**: `apps/oort-editor/src/renderer/panels/index.ts`
- Import and register `scene-viewport-3d` panel
- Add command to open 3D viewport

---

## Phase 2: Asset Loading

### 2.1 Binary File IPC Handler
**Modify**: `apps/oort-editor/src/main/ipc-handlers.ts`
```typescript
ipcMain.handle("file:read-binary", async (_event, filePath: string) => {
  const buffer = await fs.readFile(filePath);
  return { success: true, data: buffer.buffer };
});
```

**Modify**: `apps/oort-editor/src/main/preload.ts`
- Add `readBinaryFile` to exposed API

### 2.2 GLTFLoader Integration
**In ThreeService**:
- Import `GLTFLoader` from three/examples
- Implement `loadGLTF(path)` with caching
- Parse ArrayBuffer from IPC
- Return cloned Object3D

### 2.3 Update AssetBrowser Icons
**Modify**: `apps/oort-editor/src/renderer/panels/AssetBrowser.tsx`
- Add icons for `.gltf`, `.glb` extensions

---

## Phase 3: Scene Format

### 3.1 Define Scene Schema
**New File**: `packages/engine/src/renderer3d/types.ts`

```typescript
interface OortScene {
  version: "1.0";
  name: string;
  settings: SceneSettings;
  entities: SceneEntity[];
}

interface SceneSettings {
  background: string;
  ambientLight: { color: string; intensity: number };
  fog?: { type: "linear" | "exponential"; color: string; near?: number; far?: number };
}

interface SceneEntity {
  id: string;
  name: string;
  parent?: string;
  enabled: boolean;
  transform: Transform3D;
  components: Component[];

  // Prefab support (like Godot's PackedScene)
  prefab?: string;              // Path to .oort-scene file
  overrides?: EntityOverrides;  // Deep-merged property overrides
}

// Prefab override system - deep merge into prefab
interface EntityOverrides {
  transform?: Partial<Transform3D>;
  components?: ComponentOverride[];
  properties?: Record<string, unknown>;  // Custom script properties
}

interface ComponentOverride {
  type: string;                    // Match by component type
  index?: number;                  // Or by index if multiple of same type
  [key: string]: unknown;          // Properties to override
}

interface Transform3D {
  position: [number, number, number];
  rotation: [number, number, number];
  scale: [number, number, number];
}

type Component = MeshComponent | LightComponent | CameraComponent | ScriptComponent;

interface MeshComponent {
  type: "mesh";
  source: string;  // Path to .glb file
  castShadow?: boolean;
  receiveShadow?: boolean;
}

interface LightComponent {
  type: "light";
  lightType: "point" | "spot" | "directional";
  color: string;
  intensity: number;
  castShadow?: boolean;
}

interface CameraComponent {
  type: "camera";
  cameraType: "perspective" | "orthographic";
  fov?: number;
  near: number;
  far: number;
}

interface ScriptComponent {
  type: "script";
  source: string;  // Path to .sl file
  properties?: Record<string, unknown>;
}
```

### 3.2 Scene Loader
**New File**: `packages/engine/src/renderer3d/scene-loader.ts`
- Load `.oort-scene` JSON files
- Create Three.js objects from entities
- Handle component types (mesh, light, script)

### 3.3 Prefab System (PackedScene-like)
**New File**: `packages/engine/src/renderer3d/prefab-loader.ts`

**Features:**
- **Prefab References**: Entity can reference another `.oort-scene` via `prefab` field
- **Deep Merge Overrides**: Override specific properties while keeping prefab defaults
- **Caching**: Load prefab definition once, clone for each instance
- **Nested Prefabs**: Prefabs can contain other prefabs
- **Runtime Instantiation**: Create prefab instances dynamically via Slate

**Example Scene with Prefabs:**
```json
{
  "entities": [
    {
      "id": "enemy-1",
      "prefab": "/prefabs/enemy.oort-scene",
      "transform": { "position": [5, 0, 0] },
      "overrides": {
        "properties": { "health": 100, "speed": 2.5 }
      }
    },
    {
      "id": "enemy-2",
      "prefab": "/prefabs/enemy.oort-scene",
      "transform": { "position": [10, 0, 3] },
      "overrides": {
        "properties": { "health": 50 },
        "components": [
          { "type": "mesh", "source": "/assets/enemy-variant.glb" }
        ]
      }
    }
  ]
}
```

**Prefab Loading Algorithm:**
1. Check if entity has `prefab` field
2. Load prefab scene (cached after first load)
3. Clone prefab's root entity
4. Deep-merge `overrides` into cloned entity
5. Apply instance-specific `transform`
6. Recursively process nested prefabs

---

## Phase 4: Slate 3D API

### 4.1 Add Math Functions
**Modify**: `packages/slate/src/stdlib/stdlib.ts`

```typescript
// Quaternion
quat(x, y, z, w) -> Record {x, y, z, w}

// Euler angles
euler(x, y, z) -> Record {x, y, z}

// Cross product
cross(a, b) -> vec3

// Angle conversions
radians(degrees) -> number
degrees(radians) -> number

// Additional trig
atan2(y, x) -> number
tan(x) -> number
asin(x) -> number
acos(x) -> number
```

### 4.2 Add Scene Functions
**New File**: `packages/engine/src/renderer3d/scene-stdlib.ts`

```typescript
// Spawn a 3D entity
spawn3d({
  id: string,
  mesh?: string,
  position?: vec3,
  rotation?: vec3,
  scale?: vec3
}) -> Entity

// Transform manipulation
setPosition(entity, vec3) -> entity
setRotation(entity, vec3) -> entity
setScale(entity, vec3) -> entity
getPosition(entity) -> vec3
getRotation(entity) -> vec3
getScale(entity) -> vec3

// Scene loading
loadScene(path) -> Scene

// Prefab instantiation (runtime PackedScene)
instantiate(prefabPath, {
    position?: vec3,
    rotation?: vec3,
    scale?: vec3,
    overrides?: Record
}) -> Entity
```

### 4.3 Integrate with Runtime
**Modify**: `packages/engine/src/runtime/runtime.ts`
- Add 3D stdlib to globals
- Bridge entity signals to ThreeService
- Sync transforms on game tick

---

## New Files Summary

| File | Purpose |
|------|---------|
| `src/renderer/core/three-service.ts` | Three.js singleton manager |
| `src/renderer/panels/SceneViewport3D.tsx` | 3D viewport panel |
| `src/renderer/panels/SceneViewport3D.css` | 3D viewport styles |
| `packages/engine/src/renderer3d/types.ts` | Scene format types |
| `packages/engine/src/renderer3d/scene-loader.ts` | Scene file parser |
| `packages/engine/src/renderer3d/prefab-loader.ts` | Prefab loading & caching |
| `packages/engine/src/renderer3d/scene-stdlib.ts` | Slate 3D functions |
| `packages/engine/src/renderer3d/index.ts` | Public exports |

## Modified Files Summary

| File | Changes |
|------|---------|
| `apps/oort-editor/package.json` | Add three.js dependency |
| `src/main/ipc-handlers.ts` | Add binary file reading |
| `src/main/preload.ts` | Expose readBinaryFile |
| `src/shared/ipc-channels.ts` | Add FILE_READ_BINARY channel |
| `src/renderer/panels/index.ts` | Register 3D viewport |
| `src/renderer/panels/AssetBrowser.tsx` | Add glTF icons |
| `src/renderer/core/builtin-commands.ts` | Add 3D viewport command |
| `packages/slate/src/stdlib/stdlib.ts` | Add 3D math functions |
| `packages/engine/src/runtime/runtime.ts` | Add 3D stdlib |
| `packages/engine/src/index.ts` | Export renderer3d |

---

## Example Usage

### Slate Script - Player Controller
```slate
# player-controller.sl

let player = null

on game.start:
    player = spawn3d({
        id: "player",
        mesh: "/assets/models/robot.glb",
        position: vec3(0, 1, 0)
    })
    say "Player spawned!"

on game.tick with {delta: dt}:
    if player != null:
        let pos = getPosition(player)
        let newY = pos.y + sin(dt) * 0.01
        player = setPosition(player, vec3(pos.x, newY, pos.z))
```

### Slate Script - Prefab Spawner
```slate
# enemy-spawner.sl
# Demonstrates runtime prefab instantiation (like Godot's PackedScene)

let enemies = []
let spawn_timer = 0

on game.start:
    # Spawn initial enemies from prefab
    for i in range(5):
        let enemy = instantiate("/prefabs/enemy.oort-scene", {
            position: vec3(i * 3, 0, 10),
            overrides: { health: 100 + i * 20 }
        })
        enemies = append(enemies, enemy)
    say "Spawned 5 enemies from prefab"

on game.tick with {delta: dt}:
    spawn_timer = spawn_timer + dt

    # Spawn new enemy every 5 seconds
    if spawn_timer > 5:
        let x = random() * 20 - 10
        let enemy = instantiate("/prefabs/enemy.oort-scene", {
            position: vec3(x, 0, 15),
            overrides: { speed: 1.5 + random() }
        })
        enemies = append(enemies, enemy)
        spawn_timer = 0
```

### Scene File (.oort-scene)
```json
{
  "version": "1.0",
  "name": "Level 1",
  "settings": {
    "background": "#1a1a2e",
    "ambientLight": { "color": "#ffffff", "intensity": 0.4 }
  },
  "entities": [
    {
      "id": "player",
      "name": "Player",
      "enabled": true,
      "transform": {
        "position": [0, 1, 0],
        "rotation": [0, 0, 0],
        "scale": [1, 1, 1]
      },
      "components": [
        { "type": "mesh", "source": "/assets/models/robot.glb" },
        { "type": "script", "source": "/scripts/player-controller.sl" }
      ]
    },
    {
      "id": "sun",
      "name": "Sun Light",
      "enabled": true,
      "transform": {
        "position": [10, 20, 10],
        "rotation": [-45, 30, 0],
        "scale": [1, 1, 1]
      },
      "components": [
        {
          "type": "light",
          "lightType": "directional",
          "color": "#fff5e6",
          "intensity": 1.2,
          "castShadow": true
        }
      ]
    }
  ]
}
```

---

## Implementation Order

1. **Phase 1** - Core Three.js (get something rendering)
2. **Phase 2** - Asset loading (view glTF models)
3. **Phase 3** - Scene format (save/load scenes)
4. **Phase 4** - Slate API (script 3D entities)

This order prioritizes getting visual results quickly, then building out the authoring tools.

---

## Future Enhancements (Post-MVP)

- Entity selection with raycasting
- Transform gizmos (translate/rotate/scale)
- Camera presets (top/front/side views)
- Hierarchy panel for scene tree
- Material editor
- Physics integration
- Animation system
- Particle effects
- Post-processing pipeline
