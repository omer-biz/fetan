import type { KeyValueStore, Logger } from "./types";

export async function migrateLegacyStorage(
  store: KeyValueStore,
  legacyStorage: Storage,
  logger: Logger,
): Promise<void> {
  try {
    const legacyLesson = legacyStorage.getItem("lessonInfo");
    if (legacyLesson) {
      try {
        const parsed: unknown = JSON.parse(legacyLesson);
        await store.set("lessonInfo", parsed);
        legacyStorage.removeItem("lessonInfo");
      } catch (error) {
        logger.warn("[Storage] Could not migrate legacy lesson data.", error);
        if (error instanceof SyntaxError) {
          legacyStorage.removeItem("lessonInfo");
        }
      }
    }

    const legacyTheme = legacyStorage.getItem("theme");
    if (legacyTheme) {
      try {
        await store.set("theme", legacyTheme);
        legacyStorage.removeItem("theme");
      } catch (error) {
        logger.warn("[Storage] Could not migrate the legacy theme.", error);
      }
    }
  } catch (error) {
    logger.warn(
      "[Storage] Legacy storage is unavailable; continuing with IndexedDB.",
      error,
    );
  }
}

export async function readStoredValue<T>(
  store: KeyValueStore,
  key: string,
  fallback: T,
  logger: Logger,
): Promise<T> {
  try {
    const value = await store.get<T>(key);
    return value ?? fallback;
  } catch (error) {
    logger.warn(`[Storage] Could not read ${key}; using the default.`, error);
    return fallback;
  }
}

export async function persistValue(
  store: KeyValueStore,
  key: string,
  value: unknown,
  logger: Logger,
): Promise<void> {
  try {
    await store.set(key, value);
  } catch (error) {
    logger.error(`[Storage] Could not save ${key}.`, error);
  }
}
