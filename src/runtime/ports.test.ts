import { beforeEach, describe, expect, it, vi } from "vitest";
import { wirePorts, type PortEnvironment } from "./ports";
import type {
  AnalyticsPayload,
  ElmApp,
  IncomingPort,
  KeyValueStore,
  Logger,
} from "./types";

type Emitter<T> = (value: T) => void;

function incomingPort<T>(): {
  port: IncomingPort<T>;
  emit: Emitter<T>;
} {
  let subscriber: Emitter<T> | undefined;
  return {
    port: {
      subscribe(callback) {
        subscriber = callback;
      },
    },
    emit(value) {
      if (!subscriber) {
        throw new Error("Port was not subscribed.");
      }
      subscriber(value);
    },
  };
}

function appHarness() {
  const saveInfo = incomingPort<unknown>();
  const saveTheme = incomingPort<string>();
  const saveAnalyticsConsent = incomingPort<boolean>();
  const trackEvent = incomingPort<AnalyticsPayload>();
  const triggerLevelUp = incomingPort<string>();
  const fetchCommunityStats = incomingPort<void>();
  const receiveCommunityStats = vi.fn();
  const app: ElmApp = {
    ports: {
      saveInfo: saveInfo.port,
      saveTheme: saveTheme.port,
      saveAnalyticsConsent: saveAnalyticsConsent.port,
      trackEvent: trackEvent.port,
      triggerLevelUp: triggerLevelUp.port,
      fetchCommunityStats: fetchCommunityStats.port,
      receiveCommunityStats: { send: receiveCommunityStats },
    },
  };

  return {
    app,
    emit: {
      saveInfo: saveInfo.emit,
      saveTheme: saveTheme.emit,
      saveAnalyticsConsent: saveAnalyticsConsent.emit,
      trackEvent: trackEvent.emit,
      triggerLevelUp: triggerLevelUp.emit,
      fetchCommunityStats: fetchCommunityStats.emit,
    },
    receiveCommunityStats,
  };
}

function environment(overrides: Partial<PortEnvironment> = {}) {
  const set = vi.fn(async (_key: string, _value: unknown) => undefined);
  const store: KeyValueStore = {
    get: async <T>() => undefined as T | undefined,
    set,
  };
  const logger: Logger = { warn: vi.fn(), error: vi.fn() };
  const fetcher = vi.fn(async () => new Response(null, { status: 200 }));
  const celebrate = vi.fn();
  const result: PortEnvironment = {
    store,
    fetcher,
    document,
    window,
    logger,
    now: () => new Date("2026-09-07T12:00:00.000Z"),
    reducedMotion: () => true,
    requestFrame: (callback) => {
      callback(0);
      return 1;
    },
    celebrate,
    ...overrides,
  };
  return { result, set, logger, fetcher, celebrate };
}

const session: AnalyticsPayload = {
  wpm: 42,
  accuracy: 97,
  duration: 31.5,
  lessonIdx: 3,
  slowestLetter: "ሀ",
  layoutKind: "GeezIME",
};

beforeEach(() => {
  document.documentElement.className = "";
  document.body.innerHTML = "";
});

describe("Elm port wiring", () => {
  it("persists lesson state, consent, and normalized themes", async () => {
    const harness = appHarness();
    const env = environment();
    wirePorts(harness.app, false, env.result);

    harness.emit.saveInfo({ level: 4 });
    harness.emit.saveAnalyticsConsent(true);
    harness.emit.saveTheme("unexpected");

    await vi.waitFor(() => expect(env.set).toHaveBeenCalledTimes(3));
    expect(env.set).toHaveBeenCalledWith("lessonInfo", { level: 4 });
    expect(env.set).toHaveBeenCalledWith("analyticsConsent", true);
    expect(env.set).toHaveBeenCalledWith("theme", "dark");
    expect(document.documentElement.classList.contains("dark")).toBe(true);
  });

  it("enforces analytics consent at the network boundary", async () => {
    const harness = appHarness();
    const env = environment();
    wirePorts(harness.app, false, env.result);

    harness.emit.trackEvent(session);
    expect(env.fetcher).not.toHaveBeenCalled();

    harness.emit.saveAnalyticsConsent(true);
    harness.emit.trackEvent(session);

    await vi.waitFor(() => expect(env.fetcher).toHaveBeenCalledOnce());
    expect(env.fetcher.mock.calls[0]?.[1]).toMatchObject({ method: "POST" });
  });

  it("returns decoded community sessions to Elm", async () => {
    const fetcher = vi.fn(async () =>
      new Response(
        JSON.stringify({
          documents: [
            {
              fields: {
                wpm: { integerValue: "38" },
                accuracy: { integerValue: "96" },
                duration: { doubleValue: 20.5 },
                lessonIdx: { integerValue: "2" },
                slowestLetter: { stringValue: "ለ" },
                timestamp: { timestampValue: "2026-09-06T08:00:00.000Z" },
              },
            },
          ],
        }),
        { status: 200, headers: { "Content-Type": "application/json" } },
      ),
    );
    const harness = appHarness();
    const env = environment({ fetcher });
    wirePorts(harness.app, false, env.result);

    harness.emit.fetchCommunityStats(undefined);

    await vi.waitFor(() =>
      expect(harness.receiveCommunityStats).toHaveBeenCalledWith({
        ok: true,
        sessions: [
          {
            wpm: 38,
            accuracy: 96,
            duration: 20.5,
            lessonIdx: 2,
            slowestLetter: "ለ",
            timestamp: "2026-09-06T08:00:00.000Z",
          },
        ],
      }),
    );
  });

  it("reports community failures without leaking transport errors", async () => {
    const fetcher = vi.fn(async () => new Response(null, { status: 503 }));
    const harness = appHarness();
    const env = environment({ fetcher });
    wirePorts(harness.app, false, env.result);

    harness.emit.fetchCommunityStats(undefined);

    await vi.waitFor(() =>
      expect(harness.receiveCommunityStats).toHaveBeenCalledWith({
        ok: false,
        error: "Community statistics are temporarily unavailable.",
      }),
    );
    expect(env.logger.error).toHaveBeenCalled();
  });

  it("anchors celebration to the promoted letter and respects reduced motion", () => {
    const letter = document.createElement("div");
    letter.id = "progression-letter-7";
    letter.getBoundingClientRect = () =>
      ({ left: 100, top: 150, width: 40, height: 20 }) as DOMRect;
    document.body.append(letter);
    Object.defineProperty(window, "innerWidth", { configurable: true, value: 400 });
    Object.defineProperty(window, "innerHeight", { configurable: true, value: 400 });

    const harness = appHarness();
    const reduced = environment();
    wirePorts(harness.app, false, reduced.result);
    harness.emit.triggerLevelUp("7");
    expect(reduced.celebrate).not.toHaveBeenCalled();

    const animated = environment({ reducedMotion: () => false });
    const secondHarness = appHarness();
    wirePorts(secondHarness.app, false, animated.result);
    secondHarness.emit.triggerLevelUp("7");
    expect(animated.celebrate).toHaveBeenCalledWith(
      expect.objectContaining({ origin: { x: 0.3, y: 0.4 } }),
    );
  });
});
