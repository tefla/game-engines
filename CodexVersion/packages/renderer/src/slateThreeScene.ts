import * as THREE from "three";
import { OrbitControls } from "three/examples/jsm/controls/OrbitControls.js";

export type RecordLike = { readonly entries: Record<string, unknown> };

function isRecordLike(value: unknown): value is RecordLike {
  return (
    typeof value === "object" &&
    value !== null &&
    !Array.isArray(value) &&
    "entries" in (value as any) &&
    typeof (value as any).entries === "object" &&
    (value as any).entries !== null
  );
}

function getVec3(value: unknown): THREE.Vector3 | null {
  if (Array.isArray(value) && value.length === 3 && value.every((v) => typeof v === "number")) {
    return new THREE.Vector3(value[0] as number, value[1] as number, value[2] as number);
  }
  if (isRecordLike(value)) {
    const x = value.entries.x;
    const y = value.entries.y;
    const z = value.entries.z;
    if (typeof x === "number" && typeof y === "number" && typeof z === "number") return new THREE.Vector3(x, y, z);
  }
  return null;
}

function stableJson(value: unknown): unknown {
  if (Array.isArray(value)) return value.map(stableJson);
  if (isRecordLike(value)) return stableJson(value.entries);
  if (value && typeof value === "object") {
    const obj = value as Record<string, unknown>;
    const out = Object.create(null) as Record<string, unknown>;
    for (const key of Object.keys(obj).sort()) out[key] = stableJson(obj[key]);
    return out;
  }
  return value;
}

function stableKey(value: unknown): string {
  return JSON.stringify(stableJson(value));
}

function hexToColor(value: unknown, fallback = 0xffffff): THREE.Color {
  if (typeof value === "string" && /^#[0-9a-fA-F]{3,8}$/.test(value)) {
    return new THREE.Color(value);
  }
  return new THREE.Color(fallback);
}

function disposeObject(object: THREE.Object3D): void {
  object.traverse((child) => {
    const mesh = child as THREE.Mesh;
    const geom = (mesh as any).geometry as THREE.BufferGeometry | undefined;
    if (geom?.dispose) geom.dispose();

    const mat = (mesh as any).material as THREE.Material | THREE.Material[] | undefined;
    if (Array.isArray(mat)) mat.forEach((m) => m.dispose?.());
    else mat?.dispose?.();
  });
}

function buildVisual(entity: RecordLike): THREE.Object3D | null {
  const visual = entity.entries.visual;
  if (!isRecordLike(visual)) return null;
  const kind = visual.entries.kind;
  if (typeof kind !== "string") return null;

  const color = hexToColor(entity.entries.color, 0xcccccc);

  if (kind === "box") {
    const size = visual.entries.size;
    const v = getVec3(size);
    const geometry = new THREE.BoxGeometry(v?.x ?? 1, v?.y ?? 1, v?.z ?? 1);
    const material = new THREE.MeshStandardMaterial({ color });
    return new THREE.Mesh(geometry, material);
  }

  if (kind === "plane") {
    const args = visual.entries.args;
    const a = Array.isArray(args) ? args : [];
    const maybe = a.length === 1 && Array.isArray(a[0]) ? (a[0] as unknown[]) : a;
    const w = typeof maybe[0] === "number" ? (maybe[0] as number) : 10;
    const h = typeof maybe[1] === "number" ? (maybe[1] as number) : 10;
    const geometry = new THREE.PlaneGeometry(w, h);
    const material = new THREE.MeshStandardMaterial({ color, side: THREE.DoubleSide });
    const mesh = new THREE.Mesh(geometry, material);
    mesh.rotateX(-Math.PI / 2);
    return mesh;
  }

  if (kind === "line") {
    const args = visual.entries.args;
    const a = Array.isArray(args) ? args : [];
    const maybe = a.length === 1 && Array.isArray(a[0]) ? (a[0] as unknown[]) : a;
    const p0 = getVec3(maybe[0]);
    const p1 = getVec3(maybe[1]);
    if (!p0 || !p1) return null;
    const geometry = new THREE.BufferGeometry().setFromPoints([p0, p1]);
    const material = new THREE.LineBasicMaterial({ color });
    return new THREE.Line(geometry, material);
  }

  if (kind === "point") {
    const geometry = new THREE.SphereGeometry(0.1, 16, 16);
    const material = new THREE.MeshStandardMaterial({ color });
    return new THREE.Mesh(geometry, material);
  }

  return null;
}

function applyTransform(object: THREE.Object3D, entity: RecordLike): void {
  const pos = getVec3(entity.entries.position);
  if (pos) object.position.copy(pos);

  const rot = getVec3(entity.entries.rotation);
  if (rot) object.rotation.set(THREE.MathUtils.degToRad(rot.x), THREE.MathUtils.degToRad(rot.y), THREE.MathUtils.degToRad(rot.z));

  const scale = getVec3(entity.entries.scale);
  if (scale) object.scale.set(scale.x, scale.y, scale.z);

  const visible = entity.entries.visible;
  if (typeof visible === "boolean") object.visible = visible;

  const name = entity.entries.name;
  if (typeof name === "string") object.name = name;
}

export class SlateThreeScene {
  readonly scene: THREE.Scene;
  readonly camera: THREE.PerspectiveCamera;
  readonly renderer: THREE.WebGLRenderer;
  readonly controls: OrbitControls;

  private readonly objectByEntity = new Map<RecordLike, THREE.Object3D>();

  constructor(opts: { canvas: HTMLCanvasElement }) {
    this.renderer = new THREE.WebGLRenderer({ canvas: opts.canvas, antialias: true });
    this.renderer.setPixelRatio(window.devicePixelRatio || 1);

    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x111827);

    const w = opts.canvas.clientWidth || window.innerWidth;
    const h = opts.canvas.clientHeight || window.innerHeight;
    this.camera = new THREE.PerspectiveCamera(60, w / h, 0.1, 1000);
    this.camera.position.set(10, 8, 10);
    this.camera.lookAt(0, 0, 0);

    this.controls = new OrbitControls(this.camera, this.renderer.domElement);
    this.controls.enableDamping = true;

    const ambient = new THREE.AmbientLight(0xffffff, 0.5);
    this.scene.add(ambient);

    const sun = new THREE.DirectionalLight(0xffffff, 1.0);
    sun.position.set(10, 20, 10);
    this.scene.add(sun);

    const grid = new THREE.GridHelper(50, 50, 0x334155, 0x1f2937);
    this.scene.add(grid);
  }

  resizeToDisplay(): void {
    const canvas = this.renderer.domElement;
    const width = canvas.clientWidth || window.innerWidth;
    const height = canvas.clientHeight || window.innerHeight;
    const needsResize = canvas.width !== Math.floor(width * (window.devicePixelRatio || 1)) || canvas.height !== Math.floor(height * (window.devicePixelRatio || 1));
    if (!needsResize) return;
    this.renderer.setSize(width, height, false);
    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();
  }

  sync(entities: Iterable<RecordLike>): void {
    const incoming = new Set<RecordLike>();
    for (const e of entities) incoming.add(e);

    for (const [entity, object] of this.objectByEntity) {
      if (incoming.has(entity)) continue;
      this.scene.remove(object);
      disposeObject(object);
      this.objectByEntity.delete(entity);
    }

    for (const entity of incoming) {
      const visual = buildVisual(entity);
      const existing = this.objectByEntity.get(entity);

      if (!visual) {
        if (existing) {
          this.scene.remove(existing);
          disposeObject(existing);
          this.objectByEntity.delete(entity);
        }
        continue;
      }

      const nextVisualKey = stableKey(entity.entries.visual);
      const prevVisualKey = (existing as any)?.userData?.visualKey as string | undefined;

      if (!existing || prevVisualKey !== nextVisualKey) {
        if (existing) {
          this.scene.remove(existing);
          disposeObject(existing);
        }
        (visual as any).userData.visualKey = nextVisualKey;
        this.objectByEntity.set(entity, visual);
        this.scene.add(visual);
      }

      applyTransform(this.objectByEntity.get(entity)!, entity);

      const obj = this.objectByEntity.get(entity)!;
      if (obj instanceof THREE.Mesh) {
        const material = obj.material as THREE.MeshStandardMaterial;
        material.color.copy(hexToColor(entity.entries.color, material.color.getHex()));
      }
    }
  }

  render(): void {
    this.controls.update();
    this.renderer.render(this.scene, this.camera);
  }

  dispose(): void {
    for (const [, object] of this.objectByEntity) disposeObject(object);
    this.objectByEntity.clear();
    this.controls.dispose();
    this.renderer.dispose();
  }
}

