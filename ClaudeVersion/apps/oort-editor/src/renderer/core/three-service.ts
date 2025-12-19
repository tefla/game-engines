/**
 * Three.js Service - Singleton managing 3D rendering
 *
 * Provides:
 * - Scene creation and management
 * - WebGL renderer lifecycle
 * - Asset loading and caching (glTF)
 * - Entity management bridging to Slate runtime
 */

import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";
import { TransformControls } from "three/examples/jsm/controls/TransformControls.js";
import { GLTFLoader } from "three/examples/jsm/loaders/GLTFLoader.js";
import { messageBus } from "./message-bus";

export interface Scene3DState {
  scene: THREE.Scene;
  camera: THREE.PerspectiveCamera;
  renderer: THREE.WebGLRenderer;
  controls: OrbitControls;
  transformControls: TransformControls | null;
  entities: Map<string, THREE.Object3D>;
  animationId: number | null;
  selectedEntity: string | null;
  raycaster: THREE.Raycaster;
  mouse: THREE.Vector2;
}

export type TransformMode = "translate" | "rotate" | "scale";
export type TransformSpace = "world" | "local";

export interface Entity3DOptions {
  id: string;
  mesh?: string; // Path to glTF file
  position?: [number, number, number];
  rotation?: [number, number, number];
  scale?: [number, number, number];
}

class ThreeService {
  private scenes: Map<string, Scene3DState> = new Map();
  private gltfLoader: GLTFLoader;
  private assetCache: Map<string, THREE.Object3D> = new Map();
  private loadingAssets: Map<string, Promise<THREE.Object3D>> = new Map();

  constructor() {
    this.gltfLoader = new GLTFLoader();
  }

  /**
   * Create a new 3D scene for a viewport
   */
  createScene(viewportId: string, canvas: HTMLCanvasElement): Scene3DState {
    // Dispose existing scene if any
    if (this.scenes.has(viewportId)) {
      this.disposeScene(viewportId);
    }

    // Create scene
    const scene = new THREE.Scene();
    scene.background = new THREE.Color(0x1a1a2e);

    // Create camera
    const camera = new THREE.PerspectiveCamera(
      60,
      canvas.clientWidth / canvas.clientHeight,
      0.1,
      1000
    );
    camera.position.set(5, 5, 5);
    camera.lookAt(0, 0, 0);

    // Create renderer
    const renderer = new THREE.WebGLRenderer({
      canvas,
      antialias: true,
      alpha: false,
    });
    renderer.setSize(canvas.clientWidth, canvas.clientHeight);
    renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFSoftShadowMap;

    // Create controls
    const controls = new OrbitControls(camera, canvas);
    controls.enableDamping = true;
    controls.dampingFactor = 0.05;
    controls.minDistance = 1;
    controls.maxDistance = 100;

    const state: Scene3DState = {
      scene,
      camera,
      renderer,
      controls,
      transformControls: null,
      entities: new Map(),
      animationId: null,
      selectedEntity: null,
      raycaster: new THREE.Raycaster(),
      mouse: new THREE.Vector2(),
    };

    // Setup default scene elements
    this.setupDefaultScene(state);

    this.scenes.set(viewportId, state);
    messageBus.emit("scene3d:created", { viewportId });

    return state;
  }

  /**
   * Setup default scene elements (grid, axes, lights)
   */
  private setupDefaultScene(state: Scene3DState): void {
    // Grid helper
    const grid = new THREE.GridHelper(20, 20, 0x4a4a6a, 0x2a2a4a);
    grid.name = "__grid";
    state.scene.add(grid);

    // Axes helper
    const axes = new THREE.AxesHelper(3);
    axes.name = "__axes";
    state.scene.add(axes);

    // Ambient light
    const ambient = new THREE.AmbientLight(0xffffff, 0.4);
    ambient.name = "__ambient";
    state.scene.add(ambient);

    // Directional light (sun)
    const directional = new THREE.DirectionalLight(0xffffff, 0.8);
    directional.name = "__directional";
    directional.position.set(5, 10, 5);
    directional.castShadow = true;
    directional.shadow.mapSize.width = 2048;
    directional.shadow.mapSize.height = 2048;
    directional.shadow.camera.near = 0.5;
    directional.shadow.camera.far = 50;
    state.scene.add(directional);
  }

  /**
   * Get a scene by viewport ID
   */
  getScene(viewportId: string): Scene3DState | undefined {
    return this.scenes.get(viewportId);
  }

  /**
   * Start render loop for a viewport
   */
  startRenderLoop(viewportId: string): void {
    const state = this.scenes.get(viewportId);
    if (!state || state.animationId !== null) return;

    const animate = () => {
      if (!this.scenes.has(viewportId)) return;

      state.animationId = requestAnimationFrame(animate);
      state.controls.update();
      state.renderer.render(state.scene, state.camera);
    };

    animate();
  }

  /**
   * Stop render loop for a viewport
   */
  stopRenderLoop(viewportId: string): void {
    const state = this.scenes.get(viewportId);
    if (!state || state.animationId === null) return;

    cancelAnimationFrame(state.animationId);
    state.animationId = null;
  }

  /**
   * Resize renderer to match canvas
   */
  resize(viewportId: string, width: number, height: number): void {
    const state = this.scenes.get(viewportId);
    if (!state) return;

    state.camera.aspect = width / height;
    state.camera.updateProjectionMatrix();
    state.renderer.setSize(width, height);
  }

  /**
   * Load a glTF/GLB model
   */
  async loadGLTF(path: string): Promise<THREE.Object3D> {
    // Return cached model (cloned)
    if (this.assetCache.has(path)) {
      return this.assetCache.get(path)!.clone();
    }

    // Return in-progress load
    if (this.loadingAssets.has(path)) {
      const cached = await this.loadingAssets.get(path)!;
      return cached.clone();
    }

    // Start new load
    const loadPromise = this.loadGLTFInternal(path);
    this.loadingAssets.set(path, loadPromise);

    try {
      const model = await loadPromise;
      this.assetCache.set(path, model);
      this.loadingAssets.delete(path);
      messageBus.emit("asset:loaded", { path, type: "gltf" });
      return model.clone();
    } catch (error) {
      this.loadingAssets.delete(path);
      throw error;
    }
  }

  /**
   * Internal glTF loading
   */
  private async loadGLTFInternal(path: string): Promise<THREE.Object3D> {
    // Read binary file via IPC
    const result = await window.electronAPI?.readBinaryFile?.(path);

    if (!result?.success || !result.data) {
      throw new Error(`Failed to load: ${path}`);
    }

    // Convert to ArrayBuffer if needed
    const arrayBuffer =
      result.data instanceof ArrayBuffer
        ? result.data
        : new Uint8Array(result.data).buffer;

    return new Promise((resolve, reject) => {
      this.gltfLoader.parse(
        arrayBuffer,
        "",
        (gltf) => {
          // Enable shadows on all meshes
          gltf.scene.traverse((child) => {
            if (child instanceof THREE.Mesh) {
              child.castShadow = true;
              child.receiveShadow = true;
            }
          });
          resolve(gltf.scene);
        },
        (error) => {
          reject(new Error(`GLTF parse error: ${error}`));
        }
      );
    });
  }

  /**
   * Spawn a 3D entity in a scene
   */
  async spawnEntity(
    viewportId: string,
    options: Entity3DOptions
  ): Promise<THREE.Object3D | null> {
    const state = this.scenes.get(viewportId);
    if (!state) return null;

    // Create entity container
    const entity = new THREE.Group();
    entity.name = options.id;
    entity.userData = { entityId: options.id };

    // Apply transform
    if (options.position) {
      entity.position.set(...options.position);
    }
    if (options.rotation) {
      entity.rotation.set(
        THREE.MathUtils.degToRad(options.rotation[0]),
        THREE.MathUtils.degToRad(options.rotation[1]),
        THREE.MathUtils.degToRad(options.rotation[2])
      );
    }
    if (options.scale) {
      entity.scale.set(...options.scale);
    }

    // Add to scene
    state.entities.set(options.id, entity);
    state.scene.add(entity);

    // Load mesh if specified
    if (options.mesh) {
      try {
        const mesh = await this.loadGLTF(options.mesh);
        entity.add(mesh);
        messageBus.emit("entity:mesh-loaded", {
          entityId: options.id,
          mesh: options.mesh,
        });
      } catch (error) {
        console.error(`Failed to load mesh for entity ${options.id}:`, error);
      }
    }

    messageBus.emit("entity:spawned", { entityId: options.id, viewportId });
    return entity;
  }

  /**
   * Get an entity by ID
   */
  getEntity(viewportId: string, entityId: string): THREE.Object3D | undefined {
    const state = this.scenes.get(viewportId);
    return state?.entities.get(entityId);
  }

  /**
   * Remove an entity from a scene
   */
  removeEntity(viewportId: string, entityId: string): boolean {
    const state = this.scenes.get(viewportId);
    if (!state) return false;

    const entity = state.entities.get(entityId);
    if (!entity) return false;

    state.scene.remove(entity);
    state.entities.delete(entityId);

    // Dispose geometry and materials
    entity.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        child.geometry.dispose();
        if (Array.isArray(child.material)) {
          child.material.forEach((m) => m.dispose());
        } else {
          child.material.dispose();
        }
      }
    });

    messageBus.emit("entity:removed", { entityId, viewportId });
    return true;
  }

  /**
   * Update entity transform
   */
  updateEntityTransform(
    viewportId: string,
    entityId: string,
    transform: {
      position?: [number, number, number];
      rotation?: [number, number, number];
      scale?: [number, number, number];
    }
  ): void {
    const entity = this.getEntity(viewportId, entityId);
    if (!entity) return;

    if (transform.position) {
      entity.position.set(...transform.position);
    }
    if (transform.rotation) {
      entity.rotation.set(
        THREE.MathUtils.degToRad(transform.rotation[0]),
        THREE.MathUtils.degToRad(transform.rotation[1]),
        THREE.MathUtils.degToRad(transform.rotation[2])
      );
    }
    if (transform.scale) {
      entity.scale.set(...transform.scale);
    }
  }

  /**
   * Add a primitive shape to a scene (for testing)
   */
  addPrimitive(
    viewportId: string,
    type: "box" | "sphere" | "cylinder" | "plane",
    options?: {
      position?: [number, number, number];
      color?: number;
      size?: number;
    }
  ): THREE.Mesh | null {
    const state = this.scenes.get(viewportId);
    if (!state) return null;

    const size = options?.size ?? 1;
    let geometry: THREE.BufferGeometry;

    switch (type) {
      case "box":
        geometry = new THREE.BoxGeometry(size, size, size);
        break;
      case "sphere":
        geometry = new THREE.SphereGeometry(size / 2, 32, 32);
        break;
      case "cylinder":
        geometry = new THREE.CylinderGeometry(size / 2, size / 2, size, 32);
        break;
      case "plane":
        geometry = new THREE.PlaneGeometry(size * 2, size * 2);
        break;
    }

    const material = new THREE.MeshStandardMaterial({
      color: options?.color ?? 0x4a9eff,
      roughness: 0.7,
      metalness: 0.3,
    });

    const mesh = new THREE.Mesh(geometry, material);
    mesh.castShadow = true;
    mesh.receiveShadow = true;

    if (options?.position) {
      mesh.position.set(...options.position);
    }

    state.scene.add(mesh);
    return mesh;
  }

  /**
   * Enable transform gizmo for a viewport
   */
  enableTransformControls(viewportId: string): void {
    const state = this.scenes.get(viewportId);
    if (!state || state.transformControls) return;

    const transformControls = new TransformControls(
      state.camera,
      state.renderer.domElement
    );

    // Disable orbit controls while using transform controls
    transformControls.addEventListener("dragging-changed", (event) => {
      state.controls.enabled = !event.value;
    });

    // Emit transform changes
    transformControls.addEventListener("objectChange", () => {
      if (state.selectedEntity) {
        const entity = state.entities.get(state.selectedEntity);
        if (entity) {
          messageBus.emit("entity:transform-changed", {
            entityId: state.selectedEntity,
            position: [entity.position.x, entity.position.y, entity.position.z],
            rotation: [
              entity.rotation.x * 180 / Math.PI,
              entity.rotation.y * 180 / Math.PI,
              entity.rotation.z * 180 / Math.PI,
            ],
            scale: [entity.scale.x, entity.scale.y, entity.scale.z],
          });
        }
      }
    });

    state.scene.add(transformControls as unknown as THREE.Object3D);
    state.transformControls = transformControls;
  }

  /**
   * Disable transform gizmo for a viewport
   */
  disableTransformControls(viewportId: string): void {
    const state = this.scenes.get(viewportId);
    if (!state || !state.transformControls) return;

    state.transformControls.detach();
    state.scene.remove(state.transformControls as unknown as THREE.Object3D);
    state.transformControls.dispose();
    state.transformControls = null;
  }

  /**
   * Set transform control mode
   */
  setTransformMode(viewportId: string, mode: TransformMode): void {
    const state = this.scenes.get(viewportId);
    if (!state?.transformControls) return;

    state.transformControls.setMode(mode);
    messageBus.emit("transform:mode-changed", { viewportId, mode });
  }

  /**
   * Get current transform mode
   */
  getTransformMode(viewportId: string): TransformMode | null {
    const state = this.scenes.get(viewportId);
    if (!state?.transformControls) return null;
    return state.transformControls.mode as TransformMode;
  }

  /**
   * Set transform space (world or local)
   */
  setTransformSpace(viewportId: string, space: TransformSpace): void {
    const state = this.scenes.get(viewportId);
    if (!state?.transformControls) return;

    state.transformControls.setSpace(space);
    messageBus.emit("transform:space-changed", { viewportId, space });
  }

  /**
   * Get current transform space
   */
  getTransformSpace(viewportId: string): TransformSpace | null {
    const state = this.scenes.get(viewportId);
    if (!state?.transformControls) return null;
    return state.transformControls.space as TransformSpace;
  }

  /**
   * Select an entity by ID
   */
  selectEntity(viewportId: string, entityId: string | null): void {
    const state = this.scenes.get(viewportId);
    if (!state) return;

    // Deselect previous
    if (state.selectedEntity) {
      const prev = state.entities.get(state.selectedEntity);
      if (prev) {
        this.setEntityHighlight(prev, false);
      }
    }

    state.selectedEntity = entityId;

    if (entityId) {
      const entity = state.entities.get(entityId);
      if (entity) {
        // Highlight selected entity
        this.setEntityHighlight(entity, true);

        // Attach transform controls
        if (state.transformControls) {
          state.transformControls.attach(entity);
        }

        messageBus.emit("entity:selected", { entityId, viewportId });
        messageBus.emit("selection:changed", {
          target: {
            type: "Entity",
            name: entity.name || entityId,
            properties: {
              position: `(${entity.position.x.toFixed(2)}, ${entity.position.y.toFixed(2)}, ${entity.position.z.toFixed(2)})`,
              rotation: `(${(entity.rotation.x * 180 / Math.PI).toFixed(1)}°, ${(entity.rotation.y * 180 / Math.PI).toFixed(1)}°, ${(entity.rotation.z * 180 / Math.PI).toFixed(1)}°)`,
              scale: `(${entity.scale.x.toFixed(2)}, ${entity.scale.y.toFixed(2)}, ${entity.scale.z.toFixed(2)})`,
              visible: entity.visible,
            },
          },
        });
      }
    } else {
      // Detach transform controls
      if (state.transformControls) {
        state.transformControls.detach();
      }
      messageBus.emit("entity:deselected", { viewportId });
      messageBus.emit("selection:changed", { target: null });
    }
  }

  /**
   * Get selected entity ID
   */
  getSelectedEntity(viewportId: string): string | null {
    const state = this.scenes.get(viewportId);
    return state?.selectedEntity || null;
  }

  /**
   * Set entity highlight (selection indicator)
   */
  private setEntityHighlight(entity: THREE.Object3D, highlighted: boolean): void {
    entity.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        if (highlighted) {
          // Store original material
          if (!child.userData.originalMaterial) {
            child.userData.originalMaterial = child.material;
          }
          // Create highlighted material
          const material = (child.material as THREE.MeshStandardMaterial).clone();
          material.emissive = new THREE.Color(0x4a9eff);
          material.emissiveIntensity = 0.3;
          child.material = material;
        } else {
          // Restore original material
          if (child.userData.originalMaterial) {
            child.material = child.userData.originalMaterial;
            delete child.userData.originalMaterial;
          }
        }
      }
    });
  }

  /**
   * Perform raycast selection from mouse coordinates
   */
  raycastSelect(
    viewportId: string,
    mouseX: number,
    mouseY: number,
    canvasWidth: number,
    canvasHeight: number
  ): string | null {
    const state = this.scenes.get(viewportId);
    if (!state) return null;

    // Convert to normalized device coordinates
    state.mouse.x = (mouseX / canvasWidth) * 2 - 1;
    state.mouse.y = -(mouseY / canvasHeight) * 2 + 1;

    // Update raycaster
    state.raycaster.setFromCamera(state.mouse, state.camera);

    // Get all entity objects
    const entityObjects: THREE.Object3D[] = [];
    for (const entity of state.entities.values()) {
      entityObjects.push(entity);
    }

    // Perform raycast
    const intersects = state.raycaster.intersectObjects(entityObjects, true);

    if (intersects.length > 0) {
      // Find the entity that owns the intersected object
      let object: THREE.Object3D | null = intersects[0].object;
      while (object) {
        if (object.userData?.entityId) {
          return object.userData.entityId;
        }
        object = object.parent;
      }
    }

    return null;
  }

  /**
   * Dispose a scene and all its resources
   */
  disposeScene(viewportId: string): void {
    const state = this.scenes.get(viewportId);
    if (!state) return;

    // Stop render loop
    this.stopRenderLoop(viewportId);

    // Dispose transform controls
    if (state.transformControls) {
      state.transformControls.detach();
      state.scene.remove(state.transformControls as unknown as THREE.Object3D);
      state.transformControls.dispose();
    }

    // Dispose all entities
    for (const entityId of state.entities.keys()) {
      this.removeEntity(viewportId, entityId);
    }

    // Dispose scene objects
    state.scene.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        child.geometry.dispose();
        if (Array.isArray(child.material)) {
          child.material.forEach((m) => m.dispose());
        } else {
          child.material.dispose();
        }
      }
    });

    // Dispose renderer
    state.renderer.dispose();
    state.controls.dispose();

    this.scenes.delete(viewportId);
    messageBus.emit("scene3d:disposed", { viewportId });
  }

  /**
   * Clear asset cache
   */
  clearCache(): void {
    this.assetCache.clear();
  }

  /**
   * Get all active viewport IDs
   */
  getActiveViewports(): string[] {
    return Array.from(this.scenes.keys());
  }
}

// Singleton instance
export const threeService = new ThreeService();
