import React, { useState, useEffect } from "react";
import { useProject } from "@/hooks/useStore";
import { MenuBar } from "./MenuBar";
import { runtimeService } from "@/core/runtime-service";
import { inputService } from "@/core/input-service";
import { messageBus } from "@/core/message-bus";
import type { GameState } from "@oort/engine";
import "./TitleBar.css";

export function TitleBar() {
  const { name, isOpen } = useProject();
  const [gameState, setGameState] = useState<GameState>("stopped");
  const isMac = navigator.platform.toLowerCase().includes("mac");

  const handleMinimize = () => window.electronAPI?.minimize();
  const handleMaximize = () => window.electronAPI?.maximize();
  const handleClose = () => window.electronAPI?.close();

  // Listen for game state changes from the runtime
  useEffect(() => {
    const runtime = runtimeService.getRuntime();
    if (!runtime) return;

    const unsubStart = messageBus.on("game:state-changed", (data) => {
      setGameState(data.state);
    });

    // Listen for runtime signals
    const startHandler = runtime.on("game.start", () => setGameState("running"));
    const stopHandler = runtime.on("game.stop", () => setGameState("stopped"));
    const pauseHandler = runtime.on("game.pause", () => setGameState("paused"));
    const resumeHandler = runtime.on("game.resume", () => setGameState("running"));

    return () => {
      unsubStart();
      runtime.off(startHandler);
      runtime.off(stopHandler);
      runtime.off(pauseHandler);
      runtime.off(resumeHandler);
    };
  }, []);

  // Play button handler
  const handlePlay = () => {
    const runtime = runtimeService.getRuntime();
    if (!runtime) {
      messageBus.emit("console:error", { message: "No project open. Open a project first." });
      return;
    }

    if (gameState === "stopped" || gameState === "paused") {
      runtime.startGameLoop();
      setGameState("running");
      messageBus.emit("game:started", {});
    }
  };

  // Pause button handler
  const handlePause = () => {
    const runtime = runtimeService.getRuntime();
    if (!runtime) return;

    if (gameState === "running") {
      runtime.pauseGameLoop();
      setGameState("paused");
      messageBus.emit("game:paused", {});
    }
  };

  // Stop button handler
  const handleStop = () => {
    const runtime = runtimeService.getRuntime();
    if (!runtime) return;

    if (gameState !== "stopped") {
      runtime.stopGameLoop();
      setGameState("stopped");
      messageBus.emit("game:stopped", {});
    }
  };

  return (
    <div className={`title-bar ${isMac ? "mac" : "windows"}`}>
      {/* macOS: Leave space for traffic lights */}
      {isMac && <div className="traffic-light-space" />}

      {/* Menu bar */}
      <div className="menu-bar-container no-drag">
        <MenuBar />
      </div>

      {/* Playback Controls */}
      <div className="playback-controls no-drag">
        {gameState === "running" ? (
          <button
            className="playback-btn pause-btn"
            onClick={handlePause}
            title="Pause (Ctrl+P)"
          >
            <svg width="12" height="12" viewBox="0 0 12 12">
              <rect x="1" y="1" width="3" height="10" fill="currentColor" />
              <rect x="8" y="1" width="3" height="10" fill="currentColor" />
            </svg>
          </button>
        ) : (
          <button
            className="playback-btn play-btn"
            onClick={handlePlay}
            title="Play (Ctrl+Enter)"
          >
            <svg width="12" height="12" viewBox="0 0 12 12">
              <path d="M2 1 L10 6 L2 11 Z" fill="currentColor" />
            </svg>
          </button>
        )}
        <button
          className={`playback-btn stop-btn ${gameState === "stopped" ? "disabled" : ""}`}
          onClick={handleStop}
          disabled={gameState === "stopped"}
          title="Stop (Ctrl+.)"
        >
          <svg width="12" height="12" viewBox="0 0 12 12">
            <rect x="1" y="1" width="10" height="10" fill="currentColor" />
          </svg>
        </button>
        {gameState !== "stopped" && (
          <span className="game-state-indicator">
            {gameState === "running" ? "Playing" : "Paused"}
          </span>
        )}
      </div>

      {/* Title */}
      <div className="title-bar-title drag-region">
        <span className="title-text">{isOpen ? name : "Oort Editor"}</span>
      </div>

      {/* Windows: Window controls */}
      {!isMac && (
        <div className="window-controls no-drag">
          <button className="window-control" onClick={handleMinimize}>
            <svg width="10" height="1" viewBox="0 0 10 1">
              <rect width="10" height="1" fill="currentColor" />
            </svg>
          </button>
          <button className="window-control" onClick={handleMaximize}>
            <svg width="10" height="10" viewBox="0 0 10 10">
              <rect
                x="0.5"
                y="0.5"
                width="9"
                height="9"
                fill="none"
                stroke="currentColor"
              />
            </svg>
          </button>
          <button
            className="window-control window-close"
            onClick={handleClose}
          >
            <svg width="10" height="10" viewBox="0 0 10 10">
              <line x1="0" y1="0" x2="10" y2="10" stroke="currentColor" />
              <line x1="10" y1="0" x2="0" y2="10" stroke="currentColor" />
            </svg>
          </button>
        </div>
      )}
    </div>
  );
}
