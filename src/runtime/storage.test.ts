import { beforeEach, describe, expect, it, vi } from "vitest";
import { migrateLegacyStorage, persistValue, readStoredValue } from "./storage";
import type { KeyValueStore, Logger } from "./types";

function setup() {
  const store: KeyValueStore = {
    get: vi.fn(),
    set: vi.fn().mockResolvedValue(undefined),
  };
  const logger: Logger = { warn: vi.fn(), error: vi.fn() };
  return { store, logger };
}

beforeEach(() => localStorage.clear());

describe("storage runtime", () => {
  it("migrates valid lesson and theme data before removing legacy values", async () => {
    const { store, logger } = setup();
    localStorage.setItem("lessonInfo", JSON.stringify({ lessonIdx: 7 }));
    localStorage.setItem("theme", "light");

    await migrateLegacyStorage(store, localStorage, logger);

    expect(store.set).toHaveBeenNthCalledWith(1, "lessonInfo", { lessonIdx: 7 });
    expect(store.set).toHaveBeenNthCalledWith(2, "theme", "light");
    expect(localStorage.getItem("lessonInfo")).toBeNull();
    expect(localStorage.getItem("theme")).toBeNull();
  });

  it("discards corrupt legacy JSON and reports it", async () => {
    const { store, logger } = setup();
    localStorage.setItem("lessonInfo", "{bad json");

    await migrateLegacyStorage(store, localStorage, logger);

    expect(store.set).not.toHaveBeenCalled();
    expect(localStorage.getItem("lessonInfo")).toBeNull();
    expect(logger.warn).toHaveBeenCalled();
  });

  it("retains legacy values when IndexedDB migration fails", async () => {
    const { store, logger } = setup();
    vi.mocked(store.set).mockRejectedValue(new Error("quota"));
    localStorage.setItem("lessonInfo", JSON.stringify({ lessonIdx: 4 }));
    localStorage.setItem("theme", "dark");

    await migrateLegacyStorage(store, localStorage, logger);

    expect(localStorage.getItem("lessonInfo")).not.toBeNull();
    expect(localStorage.getItem("theme")).toBe("dark");
    expect(logger.warn).toHaveBeenCalledTimes(2);
  });

  it("continues when localStorage itself is unavailable", async () => {
    const { store, logger } = setup();
    const unavailableStorage = {
      getItem: () => {
        throw new Error("blocked");
      },
    } as unknown as Storage;

    await migrateLegacyStorage(store, unavailableStorage, logger);

    expect(logger.warn).toHaveBeenCalledWith(
      expect.stringContaining("Legacy storage is unavailable"),
      expect.any(Error),
    );
  });

  it("reads stored values and falls back for missing or rejected reads", async () => {
    const { store, logger } = setup();
    vi.mocked(store.get)
      .mockResolvedValueOnce("light")
      .mockResolvedValueOnce(undefined)
      .mockRejectedValueOnce(new Error("blocked"));

    await expect(readStoredValue(store, "theme", "dark", logger)).resolves.toBe("light");
    await expect(readStoredValue(store, "theme", "dark", logger)).resolves.toBe("dark");
    await expect(readStoredValue(store, "theme", "dark", logger)).resolves.toBe("dark");
    expect(logger.warn).toHaveBeenCalledTimes(1);
  });

  it("persists values and contains write failures", async () => {
    const { store, logger } = setup();
    await persistValue(store, "theme", "light", logger);
    expect(store.set).toHaveBeenCalledWith("theme", "light");

    vi.mocked(store.set).mockRejectedValueOnce(new Error("quota"));
    await persistValue(store, "theme", "dark", logger);
    expect(logger.error).toHaveBeenCalledWith(
      expect.stringContaining("Could not save theme"),
      expect.any(Error),
    );
  });
});
