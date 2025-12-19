import { describe, it, expect, beforeEach, vi } from "vitest";
import { Runtime } from "./runtime";
import { Str, Num, Null, Bool, isList, isString, isNumber } from "@oort/core";
import { VirtualFileSystem } from "../vfs";

// Mock requestAnimationFrame and cancelAnimationFrame for Node.js environment
global.requestAnimationFrame = vi.fn((callback) => {
  return setTimeout(() => callback(performance.now()), 0) as unknown as number;
});
global.cancelAnimationFrame = vi.fn((id) => {
  clearTimeout(id);
});

// Helper to dedent multi-line strings for tests
function dedent(str: string): string {
  const lines = str.split("\n");
  if (lines[0] === "") lines.shift();
  if (lines.length === 0) return "";

  const indent = lines[0].match(/^\s*/)?.[0].length ?? 0;
  return lines.map((line) => line.slice(indent)).join("\n");
}

describe("Runtime", () => {
  let runtime: Runtime;
  let output: string[];

  beforeEach(() => {
    output = [];
    runtime = new Runtime({
      onOutput: (msg) => output.push(msg),
    });
  });

  describe("basic execution", () => {
    it("runs simple Slate code", () => {
      runtime.run('say("hello world")');
      expect(output).toEqual(["hello world"]);
    });

    it("has access to stdlib functions", () => {
      const result = runtime.run("abs(-5)");
      expect(isNumber(result)).toBe(true);
      expect((result as any).value).toBe(5);
    });
  });

  describe("VFS integration", () => {
    it("can read and write files", () => {
      runtime.run(dedent(`
        write("/test.txt", "hello from slate")
        let content = read("/test.txt")
        say(content)
      `));
      expect(output).toEqual(["hello from slate"]);
    });

    it("can list directories", () => {
      const result = runtime.run(dedent(`
        mkdir("/mydir")
        write("/mydir/file.txt", "content")
        ls("/mydir")
      `));
      expect(isList(result)).toBe(true);
      expect((result as any).elements.length).toBe(1);
    });

    it("respects permissions", () => {
      expect(() => {
        runtime.run('write("/engine/hack.txt", "evil code")');
      }).toThrow();
    });
  });

  describe("signal integration", () => {
    it("emits signals via signal function", () => {
      const received: any[] = [];
      runtime.on("test.signal", (data) => {
        received.push(data);
      });

      // Verify signal function exists (note: "emit" is a keyword, so we use "signal")
      const result = runtime.run('typeof(signal)');
      expect((result as any).value).toBe("native");

      runtime.run('signal("test.signal", "hello")');
      expect(received.length).toBe(1);
      expect((received[0] as any).value).toBe("hello");
    });

    it("hasListeners works correctly", () => {
      runtime.on("test.signal", () => {});

      const result1 = runtime.run('hasListeners("test.signal")');
      expect((result1 as any).value).toBe(true);

      const result2 = runtime.run('hasListeners("other.signal")');
      expect((result2 as any).value).toBe(false);
    });

    it("getHandlerCount returns correct count", () => {
      runtime.on("a", () => {});
      runtime.on("b", () => {});
      runtime.on("c", () => {});

      const result = runtime.run("getHandlerCount()");
      expect((result as any).value).toBe(3);
    });

    it("tracks signal history", () => {
      runtime.run(dedent(`
        signal("test.one", 1)
        signal("test.two", 2)
        signal("test.three", 3)
      `));

      const history = runtime.run("getSignalHistory()");
      expect(isList(history)).toBe(true);
      expect((history as any).elements.length).toBe(3);
    });

    it("can pause and resume signals", () => {
      let count = 0;
      runtime.on("test", () => {
        count++;
      });

      runtime.run(dedent(`
        pauseSignals()
        signal("test", 0)
        signal("test", 0)
      `));
      expect(count).toBe(0);

      runtime.run("resumeSignals()");
      expect(count).toBe(2);
    });
  });

  describe("on statement integration", () => {
    it("registers handlers via on statement", () => {
      runtime.run(dedent(`
        on @test.event:
          say("event received")
      `));

      runtime.emit("test.event");
      expect(output).toEqual(["event received"]);
    });

    it("passes data to handler", () => {
      runtime.run(dedent(`
        on @player.move:
          say(data)
      `));

      runtime.emit("player.move", Str("left"));
      expect(output).toEqual(["left"]);
    });
  });

  describe("emit statement integration", () => {
    it("emits signals via emit statement", () => {
      const received: any[] = [];
      runtime.on("custom.signal", (data) => {
        received.push(data);
      });

      runtime.run(dedent(`
        emit @custom.signal "test data"
      `));

      expect(received.length).toBe(1);
      expect((received[0] as any).value).toBe("test data");
    });
  });

  describe("tick", () => {
    it("emits game.tick signal", () => {
      let tickData: any = null;
      runtime.on("game.tick", (data) => {
        tickData = data;
      });

      runtime.tick(16.67);
      expect(tickData).not.toBeNull();
      expect(tickData.fields.get("delta").value).toBeCloseTo(16.67);
    });
  });

  describe("runFile", () => {
    it("runs a file from VFS", () => {
      const vfs = runtime.getVfs();
      vfs.write("/player/test.sl", 'say("from file")');

      runtime.runFile("/player/test.sl");
      expect(output).toEqual(["from file"]);
    });
  });

  describe("game loop", () => {
    it("starts and stops game loop", () => {
      const events: string[] = [];
      runtime.on("game.start", () => events.push("start"));
      runtime.on("game.stop", () => events.push("stop"));

      expect(runtime.getGameState()).toBe("stopped");
      expect(runtime.isRunning()).toBe(false);

      runtime.startGameLoop();
      expect(runtime.getGameState()).toBe("running");
      expect(runtime.isRunning()).toBe(true);
      expect(events).toContain("start");

      runtime.stopGameLoop();
      expect(runtime.getGameState()).toBe("stopped");
      expect(runtime.isRunning()).toBe(false);
      expect(events).toContain("stop");
    });

    it("pauses and resumes game loop", () => {
      const events: string[] = [];
      runtime.on("game.start", () => events.push("start"));
      runtime.on("game.pause", () => events.push("pause"));
      runtime.on("game.resume", () => events.push("resume"));
      runtime.on("game.stop", () => events.push("stop"));

      runtime.startGameLoop();
      expect(runtime.isRunning()).toBe(true);

      runtime.pauseGameLoop();
      expect(runtime.isPaused()).toBe(true);
      expect(runtime.getGameState()).toBe("paused");
      expect(events).toContain("pause");

      runtime.resumeGameLoop();
      expect(runtime.isRunning()).toBe(true);
      expect(events).toContain("resume");

      runtime.stopGameLoop();
      expect(events).toContain("stop");
    });

    it("does not emit start when resuming from pause", () => {
      const events: string[] = [];
      runtime.on("game.start", () => events.push("start"));
      runtime.on("game.resume", () => events.push("resume"));

      runtime.startGameLoop();
      runtime.pauseGameLoop();
      runtime.resumeGameLoop();

      expect(events.filter((e) => e === "start").length).toBe(1);
      expect(events).toContain("resume");

      runtime.stopGameLoop();
    });

    it("tracks game time", () => {
      runtime.startGameLoop();
      expect(runtime.getGameTime()).toBe(0);
      runtime.stopGameLoop();
    });
  });

  describe("JavaScript interop", () => {
    it("allows registering JS handlers", () => {
      const events: string[] = [];

      runtime.on("game.start", () => events.push("start"));
      runtime.on("game.stop", () => events.push("stop"));

      runtime.emit("game.start");
      runtime.emit("game.stop");

      expect(events).toEqual(["start", "stop"]);
    });

    it("allows unregistering handlers", () => {
      let count = 0;
      const id = runtime.on("test", () => {
        count++;
      });

      runtime.emit("test");
      expect(count).toBe(1);

      runtime.off(id);
      runtime.emit("test");
      expect(count).toBe(1);
    });
  });
});
