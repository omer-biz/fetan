import type { Logger, Theme } from "./types";

type ViewTransitionLike = {
  ready: Promise<void>;
};

type TransitionDocument = Document & {
  startViewTransition?: (update: () => void) => ViewTransitionLike;
};

export type ThemeEnvironment = {
  document: TransitionDocument;
  window: Window;
  reducedMotion: boolean;
  requestFrame: (callback: FrameRequestCallback) => number;
  logger: Logger;
};

export function applyTheme(document: Document, theme: Theme): void {
  document.documentElement.classList.toggle("dark", theme === "dark");
}

export async function transitionTheme(
  theme: Theme,
  environment: ThemeEnvironment,
): Promise<void> {
  const { document, window, reducedMotion, requestFrame, logger } = environment;
  const startViewTransition = document.startViewTransition?.bind(document);

  if (!startViewTransition || reducedMotion) {
    applyTheme(document, theme);
    return;
  }

  const button = document.getElementById("theme-toggle");
  const rect = button?.getBoundingClientRect();
  const x = rect ? rect.left + window.scrollX + rect.width / 2 : window.innerWidth / 2;
  const y = rect ? rect.top + window.scrollY + rect.height / 2 : window.innerHeight / 2;
  const documentWidth = Math.max(document.documentElement.scrollWidth, window.innerWidth);
  const documentHeight = Math.max(document.documentElement.scrollHeight, window.innerHeight);
  const endRadius = Math.hypot(
    Math.max(x, documentWidth - x),
    Math.max(y, documentHeight - y),
  );

  await new Promise<void>((resolve) => {
    requestFrame(() => {
      const transition = startViewTransition(() => applyTheme(document, theme));
      void transition.ready
        .then(() => {
          const clipPath = [
            `circle(0px at ${x}px ${y}px)`,
            `circle(${endRadius}px at ${x}px ${y}px)`,
          ];

          document.documentElement.animate(
            { clipPath: theme === "dark" ? clipPath : [...clipPath].reverse() },
            {
              duration: 500,
              easing: "ease-in-out",
              pseudoElement:
                theme === "dark"
                  ? "::view-transition-new(root)"
                  : "::view-transition-old(root)",
              fill: "forwards",
            },
          );
        })
        .catch((error: unknown) => {
          logger.warn("[Theme] View transition was unavailable.", error);
          applyTheme(document, theme);
        })
        .finally(resolve);
    });
  });
}
