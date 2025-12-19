import React, { useState, useEffect, useRef } from "react";
import { PanelProps } from "@/core/panel-registry";
import "./GameStatsPanel.css";

interface GameStats {
  fps: number;
  frameTime: number;
  entityCount: number;
  drawCalls: number;
  memoryUsage: number;
  updateTime: number;
  renderTime: number;
}

interface StatHistory {
  fps: number[];
  frameTime: number[];
}

const MAX_HISTORY = 60;

export function GameStatsPanel({ panelId, instanceId }: PanelProps) {
  const [stats, setStats] = useState<GameStats>({
    fps: 0,
    frameTime: 0,
    entityCount: 0,
    drawCalls: 0,
    memoryUsage: 0,
    updateTime: 0,
    renderTime: 0,
  });

  const [history, setHistory] = useState<StatHistory>({
    fps: [],
    frameTime: [],
  });

  const [isRunning, setIsRunning] = useState(false);
  const frameRef = useRef<number>(0);
  const lastTimeRef = useRef<number>(performance.now());
  const frameCountRef = useRef<number>(0);

  // Simulate stats updates when "running"
  useEffect(() => {
    if (!isRunning) return;

    let animationId: number;

    const updateFrame = () => {
      const now = performance.now();
      const delta = now - lastTimeRef.current;
      frameCountRef.current++;

      // Update FPS every second
      if (delta >= 1000) {
        const fps = Math.round((frameCountRef.current * 1000) / delta);
        const frameTime = delta / frameCountRef.current;

        const newStats: GameStats = {
          fps,
          frameTime: Math.round(frameTime * 100) / 100,
          entityCount: Math.floor(Math.random() * 50) + 100,
          drawCalls: Math.floor(Math.random() * 20) + 50,
          memoryUsage: Math.round((50 + Math.random() * 30) * 100) / 100,
          updateTime: Math.round(Math.random() * 5 * 100) / 100,
          renderTime: Math.round(Math.random() * 8 * 100) / 100,
        };

        setStats(newStats);
        setHistory((prev) => ({
          fps: [...prev.fps.slice(-MAX_HISTORY + 1), fps],
          frameTime: [...prev.frameTime.slice(-MAX_HISTORY + 1), frameTime],
        }));

        lastTimeRef.current = now;
        frameCountRef.current = 0;
      }

      animationId = requestAnimationFrame(updateFrame);
    };

    animationId = requestAnimationFrame(updateFrame);
    return () => cancelAnimationFrame(animationId);
  }, [isRunning]);

  const handleReset = () => {
    setStats({
      fps: 0,
      frameTime: 0,
      entityCount: 0,
      drawCalls: 0,
      memoryUsage: 0,
      updateTime: 0,
      renderTime: 0,
    });
    setHistory({ fps: [], frameTime: [] });
    frameCountRef.current = 0;
    lastTimeRef.current = performance.now();
  };

  const toggleRunning = () => {
    if (!isRunning) {
      lastTimeRef.current = performance.now();
      frameCountRef.current = 0;
    }
    setIsRunning(!isRunning);
  };

  const getFpsColor = (fps: number): string => {
    if (fps >= 55) return "var(--success)";
    if (fps >= 30) return "var(--warning)";
    return "var(--error)";
  };

  const renderMiniGraph = (data: number[], maxValue: number, color: string) => {
    if (data.length < 2) return null;

    const width = 120;
    const height = 30;
    const points = data
      .map((value, index) => {
        const x = (index / (MAX_HISTORY - 1)) * width;
        const y = height - (value / maxValue) * height;
        return `${x},${y}`;
      })
      .join(" ");

    return (
      <svg className="mini-graph" width={width} height={height}>
        <polyline
          fill="none"
          stroke={color}
          strokeWidth="1.5"
          points={points}
        />
      </svg>
    );
  };

  return (
    <div className="game-stats-panel">
      <div className="stats-header">
        <div className="stats-title">Performance Monitor</div>
        <div className="stats-controls">
          <button
            className={`stats-btn ${isRunning ? "running" : ""}`}
            onClick={toggleRunning}
            title={isRunning ? "Pause" : "Start"}
          >
            {isRunning ? "⏸" : "▶"}
          </button>
          <button
            className="stats-btn"
            onClick={handleReset}
            title="Reset Statistics"
          >
            ↺
          </button>
        </div>
      </div>

      <div className="stats-grid">
        <div className="stat-card primary">
          <div className="stat-label">FPS</div>
          <div className="stat-value" style={{ color: getFpsColor(stats.fps) }}>
            {stats.fps}
          </div>
          {renderMiniGraph(history.fps, 120, getFpsColor(stats.fps))}
        </div>

        <div className="stat-card">
          <div className="stat-label">Frame Time</div>
          <div className="stat-value">{stats.frameTime} ms</div>
          {renderMiniGraph(history.frameTime, 50, "var(--accent)")}
        </div>

        <div className="stat-card">
          <div className="stat-label">Entities</div>
          <div className="stat-value">{stats.entityCount}</div>
        </div>

        <div className="stat-card">
          <div className="stat-label">Draw Calls</div>
          <div className="stat-value">{stats.drawCalls}</div>
        </div>

        <div className="stat-card">
          <div className="stat-label">Memory</div>
          <div className="stat-value">{stats.memoryUsage} MB</div>
        </div>

        <div className="stat-card">
          <div className="stat-label">Update</div>
          <div className="stat-value">{stats.updateTime} ms</div>
        </div>

        <div className="stat-card">
          <div className="stat-label">Render</div>
          <div className="stat-value">{stats.renderTime} ms</div>
        </div>
      </div>

      <div className="stats-footer">
        <span className={`status-indicator ${isRunning ? "active" : ""}`} />
        <span className="status-text">
          {isRunning ? "Monitoring active" : "Monitoring paused"}
        </span>
      </div>
    </div>
  );
}
