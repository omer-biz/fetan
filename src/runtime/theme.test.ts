import { beforeEach, describe, expect, it, vi } from "vitest";
import { applyTheme, transitionTheme, type ThemeEnvironment } from "./theme";
import type { Logger } from "./types";

function environment(overrides: Partial<ThemeEnvironment> = {}): ThemeEnvironment {
  const logger: Logger = { warn: vi.fn(), error: vi.fn() };
  return {
    document,
    window,
    reducedMotion: false,
    requestFrame: (callback) => {
      callback(0);
      return 1;
    },
    logger,
    ...overrides,
  };
}

beforeEach(() => {
  delete (document as Document & { startViewTransition?: unknown })
    .startViewTransition;
  document.documentElement.className = "";
  document.body.innerHTML = '<button id="theme-toggle"></button>';
});

describe("theme runtime", () => {
  it("applies light and dark themes directly", () => {
    applyTheme(document, "dark");
    expect(document.documentElement.classList.contains("dark")).toBe(true);
    applyTheme(document, "light");
    expect(document.documentElement.classList.contains("dark")).toBe(false);
  });

  it("skips transitions when reduced motion is enabled", async () => {
    const start = vi.fn();
    Object.assign(document, { startViewTransition: start });

    await transitionTheme("dark", environment({ reducedMotion: true }));

    expect(document.documentElement.classList.contains("dark")).toBe(true);
    expect(start).not.toHaveBeenCalled();
  });

  it("animates a supported view transition from the toggle location", async () => {
    const button = document.getElementById("theme-toggle")!;
    button.getBoundingClientRect = () =>
      ({ left: 10, top: 20, width: 30, height: 40 }) as DOMRect;
    const animate = vi.fn();
    document.documentElement.animate = animate;
    const start = vi.fn((update: () => void) => {
      update();
      return { ready: Promise.resolve() };
    });
    Object.assign(document, { startViewTransition: start });

    await transitionTheme("dark", environment());

    expect(start).toHaveBeenCalledOnce();
    expect(animate).toHaveBeenCalledWith(
      expect.objectContaining({ clipPath: expect.any(Array) }),
      expect.objectContaining({ pseudoElement: "::view-transition-new(root)" }),
    );
  });

  it("falls back cleanly when transition readiness rejects", async () => {
    const logger: Logger = { warn: vi.fn(), error: vi.fn() };
    Object.assign(document, {
      startViewTransition: (update: () => void) => {
        update();
        return { ready: Promise.reject(new Error("unsupported")) };
      },
    });

    await transitionTheme("light", environment({ logger }));

    expect(logger.warn).toHaveBeenCalled();
    expect(document.documentElement.classList.contains("dark")).toBe(false);
  });
});
