import { describe, it, expect, beforeEach } from "vitest";
import { SignalBus, GameSignals } from "./signal-bus";
import { Str, Num, Null, Record, Bool } from "@oort/core";

describe("SignalBus", () => {
  let bus: SignalBus;

  beforeEach(() => {
    bus = new SignalBus();
  });

  describe("on and emit", () => {
    it("registers and triggers a handler", () => {
      let received: any = null;
      bus.on(["test", "signal"], (data) => {
        received = data;
      });

      bus.emit(["test", "signal"], Str("hello"));
      expect(received).not.toBeNull();
      expect(received.value).toBe("hello");
    });

    it("passes signal path to handler", () => {
      let receivedSignal: string[] = [];
      bus.on(["test", "signal"], (data, signal) => {
        receivedSignal = signal;
      });

      bus.emit(["test", "signal"], Null());
      expect(receivedSignal).toEqual(["test", "signal"]);
    });

    it("handles multiple handlers for same signal", () => {
      const calls: string[] = [];
      bus.on(["test"], () => calls.push("first"));
      bus.on(["test"], () => calls.push("second"));

      bus.emit(["test"], Null());
      expect(calls).toEqual(["first", "second"]);
    });

    it("does not trigger handlers for different signals", () => {
      let called = false;
      bus.on(["other", "signal"], () => {
        called = true;
      });

      bus.emit(["test", "signal"], Null());
      expect(called).toBe(false);
    });
  });

  describe("wildcard matching", () => {
    it("matches single wildcard *", () => {
      let received: any = null;
      bus.on(["player", "*"], (data) => {
        received = data;
      });

      bus.emit(["player", "move"], Str("moved"));
      expect(received?.value).toBe("moved");

      received = null;
      bus.emit(["player", "jump"], Str("jumped"));
      expect(received?.value).toBe("jumped");
    });

    it("does not match extra path segments with *", () => {
      let called = false;
      bus.on(["player", "*"], () => {
        called = true;
      });

      bus.emit(["player", "move", "left"], Null());
      expect(called).toBe(false);
    });

    it("matches multiple segments with **", () => {
      const received: string[] = [];
      bus.on(["game", "**"], (data, signal) => {
        received.push(signal.join("."));
      });

      bus.emit(["game", "start"], Null());
      bus.emit(["game", "player", "spawn"], Null());
      bus.emit(["game", "entity", "enemy", "attack"], Null());

      expect(received).toContain("game.start");
      expect(received).toContain("game.player.spawn");
      expect(received).toContain("game.entity.enemy.attack");
    });
  });

  describe("filter", () => {
    it("filters by exact value match", () => {
      const calls: string[] = [];

      bus.on(["input"], (data) => calls.push("all: " + (data as any).value), {});
      bus.on(["input"], (data) => calls.push("w: " + (data as any).value), {
        filter: Str("w"),
      });
      bus.on(["input"], (data) => calls.push("a: " + (data as any).value), {
        filter: Str("a"),
      });

      bus.emit(["input"], Str("w"));
      expect(calls).toEqual(["all: w", "w: w"]);

      calls.length = 0;
      bus.emit(["input"], Str("a"));
      expect(calls).toEqual(["all: a", "a: a"]);
    });
  });

  describe("once", () => {
    it("triggers only once and removes itself", () => {
      let count = 0;
      bus.once(["test"], () => {
        count++;
      });

      bus.emit(["test"], Null());
      bus.emit(["test"], Null());
      bus.emit(["test"], Null());

      expect(count).toBe(1);
    });
  });

  describe("off", () => {
    it("removes a handler by id", () => {
      let count = 0;
      const id = bus.on(["test"], () => {
        count++;
      });

      bus.emit(["test"], Null());
      expect(count).toBe(1);

      bus.off(id);
      bus.emit(["test"], Null());
      expect(count).toBe(1);
    });

    it("returns true when handler existed", () => {
      const id = bus.on(["test"], () => {});
      expect(bus.off(id)).toBe(true);
    });

    it("returns false when handler did not exist", () => {
      expect(bus.off(999)).toBe(false);
    });
  });

  describe("offAll", () => {
    it("removes all handlers when no signal specified", () => {
      bus.on(["test1"], () => {});
      bus.on(["test2"], () => {});
      bus.on(["test3"], () => {});

      expect(bus.getHandlerCount()).toBe(3);
      bus.offAll();
      expect(bus.getHandlerCount()).toBe(0);
    });

    it("removes handlers for specific signal only", () => {
      bus.on(["test"], () => {});
      bus.on(["test"], () => {});
      bus.on(["other"], () => {});

      expect(bus.getHandlerCount()).toBe(3);
      bus.offAll(["test"]);
      expect(bus.getHandlerCount()).toBe(1);
    });
  });

  describe("pause and resume", () => {
    it("queues emissions while paused", () => {
      let count = 0;
      bus.on(["test"], () => {
        count++;
      });

      bus.pause();
      expect(bus.isPaused()).toBe(true);

      bus.emit(["test"], Null());
      bus.emit(["test"], Null());
      expect(count).toBe(0);

      bus.resume();
      expect(bus.isPaused()).toBe(false);
      expect(count).toBe(2);
    });
  });

  describe("history", () => {
    it("records emitted events", () => {
      bus.emit(["test", "a"], Str("first"));
      bus.emit(["test", "b"], Str("second"));

      const history = bus.getHistory();
      expect(history.length).toBe(2);
      expect(history[0].signal).toEqual(["test", "a"]);
      expect(history[1].signal).toEqual(["test", "b"]);
    });

    it("limits history by count", () => {
      bus.setHistoryLimit(2);

      bus.emit(["test", "1"], Null());
      bus.emit(["test", "2"], Null());
      bus.emit(["test", "3"], Null());

      const history = bus.getHistory();
      expect(history.length).toBe(2);
      expect(history[0].signal).toEqual(["test", "2"]);
      expect(history[1].signal).toEqual(["test", "3"]);
    });

    it("clears history", () => {
      bus.emit(["test"], Null());
      expect(bus.getHistory().length).toBe(1);

      bus.clearHistory();
      expect(bus.getHistory().length).toBe(0);
    });

    it("returns limited history", () => {
      bus.emit(["test", "1"], Null());
      bus.emit(["test", "2"], Null());
      bus.emit(["test", "3"], Null());

      const history = bus.getHistory(2);
      expect(history.length).toBe(2);
      expect(history[0].signal).toEqual(["test", "2"]);
    });
  });

  describe("hasHandlers", () => {
    it("returns true when handlers exist", () => {
      bus.on(["test"], () => {});
      expect(bus.hasHandlers(["test"])).toBe(true);
    });

    it("returns false when no handlers exist", () => {
      expect(bus.hasHandlers(["test"])).toBe(false);
    });

    it("matches wildcard handlers", () => {
      bus.on(["player", "*"], () => {});
      expect(bus.hasHandlers(["player", "move"])).toBe(true);
      expect(bus.hasHandlers(["player", "jump"])).toBe(true);
      expect(bus.hasHandlers(["enemy", "move"])).toBe(false);
    });
  });

  describe("GameSignals", () => {
    it("provides common signal paths", () => {
      expect(GameSignals.TICK).toEqual(["game", "tick"]);
      expect(GameSignals.PLAYER_SPAWN).toEqual(["player", "spawn"]);
      expect(GameSignals.ENTITY_COLLISION).toEqual(["entity", "collision"]);
    });
  });
});
