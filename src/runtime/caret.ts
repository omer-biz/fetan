export type CaretEnvironment = {
  document: Document;
  window: Window;
  createObserver: (callback: MutationCallback) => MutationObserver;
  requestFrame: (callback: FrameRequestCallback) => number;
  cancelFrame: (handle: number) => void;
};

export type CaretTracker = {
  update: () => void;
  schedule: () => void;
  destroy: () => void;
};

export function setupCaretTracking(
  environment: CaretEnvironment,
): CaretTracker | null {
  const { document, window, createObserver, requestFrame, cancelFrame } = environment;
  const appRoot = document.querySelector("main") ?? document.getElementById("app");
  if (!appRoot) {
    return null;
  }

  const caretElement = document.createElement("div");
  caretElement.dataset.testid = "animated-caret";
  caretElement.className =
    "absolute top-0 left-0 bg-slate-500/10 dark:bg-slate-400/20 rounded-sm pointer-events-none";
  caretElement.setAttribute("aria-hidden", "true");
  caretElement.style.zIndex = "20";
  caretElement.style.opacity = "0";
  caretElement.style.transition =
    "transform 0.15s ease-out, width 0.15s ease-out, height 0.15s ease-out, opacity 0.1s ease-out";
  caretElement.style.transformOrigin = "top left";

  const caretBorder = document.createElement("div");
  caretBorder.className =
    "absolute top-[10%] h-[80%] w-[3px] bg-slate-700 dark:bg-slate-300 animate-blink rounded-full shadow-[0_0_4px_rgba(51,65,85,0.75)]";
  caretBorder.style.left = "-2px";
  caretElement.appendChild(caretBorder);
  document.body.appendChild(caretElement);

  let pendingFrame: number | null = null;

  const update = () => {
    pendingFrame = null;
    const active = document.getElementById("active-letter");

    if (!active) {
      caretElement.style.opacity = "0";
      return;
    }

    const rect = active.getBoundingClientRect();
    caretElement.style.transform = `translate(${rect.left + window.scrollX}px, ${rect.top + window.scrollY}px)`;
    caretElement.style.width = `${rect.width}px`;
    caretElement.style.height = `${rect.height}px`;
    caretElement.style.opacity = "1";
  };

  const schedule = () => {
    if (pendingFrame === null) {
      pendingFrame = requestFrame(update);
    }
  };

  const observer = createObserver(schedule);
  observer.observe(appRoot, { childList: true, subtree: true, attributes: true });
  window.addEventListener("resize", schedule);
  window.addEventListener("scroll", schedule, { passive: true });
  schedule();

  return {
    update,
    schedule,
    destroy: () => {
      observer.disconnect();
      window.removeEventListener("resize", schedule);
      window.removeEventListener("scroll", schedule);
      if (pendingFrame !== null) {
        cancelFrame(pendingFrame);
        pendingFrame = null;
      }
      caretElement.remove();
    },
  };
}
