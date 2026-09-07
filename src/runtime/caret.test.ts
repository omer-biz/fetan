import { beforeEach, describe, expect, it, vi } from "vitest";
import { setupCaretTracking } from "./caret";

beforeEach(() => {
  document.body.innerHTML = "";
});

describe("caret tracking", () => {
  it("does not start without an application root", () => {
    const tracker = setupCaretTracking({
      document,
      window,
      createObserver: (callback) => new MutationObserver(callback),
      requestFrame: requestAnimationFrame,
      cancelFrame: cancelAnimationFrame,
    });
    expect(tracker).toBeNull();
  });

  it("creates, positions, hides, and removes the animated highlight", () => {
    document.body.innerHTML = '<div id="app"><span id="active-letter">መ</span></div>';
    const active = document.getElementById("active-letter")!;
    active.getBoundingClientRect = () =>
      ({ left: 10, top: 20, width: 30, height: 40 }) as DOMRect;
    const frames: FrameRequestCallback[] = [];
    const disconnect = vi.fn();
    const tracker = setupCaretTracking({
      document,
      window,
      createObserver: () =>
        ({ observe: vi.fn(), disconnect }) as unknown as MutationObserver,
      requestFrame: (callback) => {
        frames.push(callback);
        return frames.length;
      },
      cancelFrame: vi.fn(),
    })!;

    const caret = document.querySelector<HTMLElement>('[data-testid="animated-caret"]')!;
    expect(caret.style.opacity).toBe("0");
    frames.shift()?.(0);
    expect(caret.style.transform).toContain("translate(10px, 20px)");
    expect(caret.style.width).toBe("30px");
    expect(caret.style.height).toBe("40px");
    expect(caret.style.opacity).toBe("1");
    expect(caret.style.zIndex).toBe("20");
    expect(caret.style.transition).toContain("transform 0.15s ease-out");

    active.remove();
    tracker.update();
    expect(caret.style.opacity).toBe("0");

    tracker.destroy();
    expect(disconnect).toHaveBeenCalledOnce();
    expect(caret.isConnected).toBe(false);
  });

  it("coalesces scheduled frames and cancels pending work on cleanup", () => {
    document.body.innerHTML = '<div id="app"></div>';
    const cancelFrame = vi.fn();
    const tracker = setupCaretTracking({
      document,
      window,
      createObserver: () =>
        ({ observe: vi.fn(), disconnect: vi.fn() }) as unknown as MutationObserver,
      requestFrame: () => 42,
      cancelFrame,
    })!;

    tracker.schedule();
    tracker.schedule();
    tracker.destroy();
    expect(cancelFrame).toHaveBeenCalledOnce();
    expect(cancelFrame).toHaveBeenCalledWith(42);
  });
});
