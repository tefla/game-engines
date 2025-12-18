import { join, normalize } from "node:path";

const rootDir = normalize(join(import.meta.dir, "."));
const port = Number(process.env.PORT ?? 5173);

function contentType(pathname: string): string {
  if (pathname.endsWith(".html")) return "text/html; charset=utf-8";
  if (pathname.endsWith(".css")) return "text/css; charset=utf-8";
  if (pathname.endsWith(".js")) return "text/javascript; charset=utf-8";
  if (pathname.endsWith(".map")) return "application/json; charset=utf-8";
  if (pathname.endsWith(".png")) return "image/png";
  if (pathname.endsWith(".jpg") || pathname.endsWith(".jpeg")) return "image/jpeg";
  return "application/octet-stream";
}

function safePath(pathname: string): string {
  const cleaned = pathname.replaceAll("..", "");
  return cleaned === "/" ? "/index.html" : cleaned;
}

const server = Bun.serve({
  port,
  async fetch(req) {
    const url = new URL(req.url);
    const path = safePath(url.pathname);
    const filePath = join(rootDir, path);
    const file = Bun.file(filePath);
    if (!(await file.exists())) {
      if (path.startsWith("/dist/")) return new Response("Not found", { status: 404 });
      return new Response(Bun.file(join(rootDir, "index.html")), {
        headers: { "content-type": "text/html; charset=utf-8" },
      });
    }
    return new Response(file, { headers: { "content-type": contentType(filePath) } });
  },
});

// eslint-disable-next-line no-console
console.log(`Puzzle demo web: http://localhost:${server.port}`);

