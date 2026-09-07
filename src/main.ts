import "./style.css";
import { Elm } from "./Main.elm";

import confetti from "canvas-confetti";
import { get, set } from "idb-keyval";
import { setupCaretTracking } from "./runtime/caret";
import { wirePorts } from "./runtime/ports";
import { migrateLegacyStorage, readStoredValue } from "./runtime/storage";
import { applyTheme } from "./runtime/theme";
import type { ElmApp, KeyValueStore, Theme } from "./runtime/types";

const store: KeyValueStore = { get, set };
const reducedMotion = window.matchMedia("(prefers-reduced-motion: reduce)");

async function initApp(): Promise<void> {
  await migrateLegacyStorage(store, localStorage, console);

  const flagsInfo = await readStoredValue<unknown>(store, "lessonInfo", null, console);
  const storedTheme = await readStoredValue<unknown>(store, "theme", "dark", console);
  const storedConsent = await readStoredValue<unknown>(
    store,
    "analyticsConsent",
    false,
    console,
  );
  const theme: Theme = storedTheme === "light" ? "light" : "dark";
  const analyticsConsent = storedConsent === true;
  const initialDictation =
    import.meta.env.MODE === "test"
      ? new URLSearchParams(window.location.search).get("dictation") ?? "አ"
      : null;
  applyTheme(document, theme);

  const app = Elm.Main.init({
    flags: {
      lessonInfo: flagsInfo,
      theme,
      now: Date.now(),
      timeOrigin: performance.timeOrigin,
      analyticsConsent,
      initialDictation,
    },
  }) as ElmApp;

  wirePorts(app, analyticsConsent, {
    store,
    fetcher: fetch,
    document,
    window,
    logger: console,
    now: () => new Date(),
    reducedMotion: () => reducedMotion.matches,
    requestFrame: requestAnimationFrame,
    celebrate: confetti,
  });
  setupCaretTracking({
    document,
    window,
    createObserver: (callback) => new MutationObserver(callback),
    requestFrame: requestAnimationFrame,
    cancelFrame: cancelAnimationFrame,
  });
}

void initApp().catch((error: unknown) => {
  console.error("[App] Qelm could not start.", error);
  const appRoot = document.getElementById("app");
  if (appRoot) {
    appRoot.textContent = "Qelm could not start. Please reload the page.";
  }
});
