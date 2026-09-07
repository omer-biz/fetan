import type { Options as ConfettiOptions } from "canvas-confetti";
import { fetchCommunitySessions, submitSession } from "./firestore";
import { persistValue } from "./storage";
import { transitionTheme } from "./theme";
import type { ElmApp, KeyValueStore, Logger, Theme } from "./types";

export type PortEnvironment = {
  store: KeyValueStore;
  fetcher: typeof fetch;
  document: Document;
  window: Window;
  logger: Logger;
  now: () => Date;
  reducedMotion: () => boolean;
  requestFrame: (callback: FrameRequestCallback) => number;
  celebrate: (options: ConfettiOptions) => void;
};

export function wirePorts(
  app: ElmApp,
  initialAnalyticsConsent: boolean,
  environment: PortEnvironment,
): void {
  const {
    store,
    fetcher,
    document,
    window,
    logger,
    now,
    reducedMotion,
    requestFrame,
    celebrate,
  } = environment;
  let analyticsConsent = initialAnalyticsConsent;

  app.ports.saveInfo.subscribe((state) => {
    void persistValue(store, "lessonInfo", state, logger);
  });

  app.ports.saveAnalyticsConsent.subscribe((consent) => {
    analyticsConsent = consent;
    void persistValue(store, "analyticsConsent", consent, logger);
  });

  app.ports.saveTheme.subscribe((requestedTheme) => {
    const theme: Theme = requestedTheme === "light" ? "light" : "dark";
    void persistValue(store, "theme", theme, logger);
    void transitionTheme(theme, {
      document,
      window,
      reducedMotion: reducedMotion(),
      requestFrame,
      logger,
    });
  });

  app.ports.trackEvent.subscribe((payload) => {
    if (!analyticsConsent) {
      return;
    }

    void submitSession(fetcher, payload, now).catch((error: unknown) => {
      logger.error("[Analytics] Failed to log session.", error);
    });
  });

  app.ports.fetchCommunityStats.subscribe(() => {
    void fetchCommunitySessions(fetcher, now)
      .then((sessions) => {
        app.ports.receiveCommunityStats.send({ ok: true, sessions });
      })
      .catch((error: unknown) => {
        logger.error("[Analytics] Failed to fetch community stats.", error);
        app.ports.receiveCommunityStats.send({
          ok: false,
          error: "Community statistics are temporarily unavailable.",
        });
      });
  });

  app.ports.triggerLevelUp.subscribe((lessonIndex) => {
    if (reducedMotion()) {
      return;
    }

    requestFrame(() => {
      const element = document.getElementById(`progression-letter-${lessonIndex}`);
      const rect = element?.getBoundingClientRect();
      const origin = rect
        ? {
            x: (rect.left + rect.width / 2) / window.innerWidth,
            y: (rect.top + rect.height / 2) / window.innerHeight,
          }
        : { x: 0.5, y: 0.2 };

      celebrate({
        particleCount: 60,
        spread: 55,
        origin,
        colors: ["#10b981", "#fbbf24", "#f43f5e"],
        zIndex: 9999,
      });
    });
  });
}
