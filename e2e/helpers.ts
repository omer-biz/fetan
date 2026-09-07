import { expect, type Page } from "@playwright/test";

export type PersistedInfo = {
  onboardingStep?: number;
  layoutKind?: string;
  history?: Array<{ duration: number; layoutKind: string }>;
  analyticsConsent?: boolean;
};

export async function seedReturningUser(
  page: Page,
  info: Record<string, unknown> = {},
): Promise<void> {
  await page.addInitScript((seed) => {
    if (!window.sessionStorage.getItem("qelm-e2e-seeded")) {
      window.localStorage.setItem(
        "lessonInfo",
        JSON.stringify({
          onboardingStep: 8,
          layoutKind: "GeezIME",
          ...seed,
        }),
      );
      window.sessionStorage.setItem("qelm-e2e-seeded", "true");
    }
  }, info);
}

export async function readLessonInfo(page: Page): Promise<PersistedInfo | null> {
  return page.evaluate(async () => {
    const database = await new Promise<IDBDatabase>((resolve, reject) => {
      const request = indexedDB.open("keyval-store");
      request.onsuccess = () => resolve(request.result);
      request.onerror = () => reject(request.error);
    });

    try {
      return await new Promise<PersistedInfo | null>((resolve, reject) => {
        const transaction = database.transaction("keyval", "readonly");
        const request = transaction.objectStore("keyval").get("lessonInfo");
        request.onsuccess = () =>
          resolve((request.result as PersistedInfo | undefined) ?? null);
        request.onerror = () => reject(request.error);
      });
    } finally {
      database.close();
    }
  });
}

export async function completeSingleCharacter(page: Page, key: string): Promise<void> {
  const exercise = page.getByLabel("Typing exercise");
  await exercise.focus();
  await page.keyboard.down(key);
  await page.waitForTimeout(60);
  await page.keyboard.up(key);
  await expect
    .poll(async () => (await readLessonInfo(page))?.history?.length ?? 0)
    .toBe(1);
}

export async function mockCommunity(page: Page): Promise<void> {
  await page.route("**/firestore.googleapis.com/**", async (route) => {
    if (route.request().method() === "GET") {
      await route.fulfill({ status: 200, json: { documents: [] } });
    } else {
      await route.fulfill({ status: 200, json: {} });
    }
  });
}
