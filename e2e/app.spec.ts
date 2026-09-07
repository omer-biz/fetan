import { expect, test } from "@playwright/test";
import {
  completeSingleCharacter,
  mockCommunity,
  readLessonInfo,
  seedReturningUser,
} from "./helpers";

test("onboarding traps attention, recommends a layout, and can be dismissed", async ({
  page,
}) => {
  await page.goto("/");
  const dialog = page.getByRole("dialog", { name: "Welcome to Qelm" });
  await expect(dialog).toBeVisible();
  await expect(page.getByRole("button", { name: "GeezIME (Recommended)" })).toBeFocused();

  await page.keyboard.press("Escape");
  await expect(dialog).toBeHidden();
  await expect(page.getByLabel("Typing exercise")).toBeVisible();
});

test("choosing the recommended layout persists onboarding progress", async ({ page }) => {
  await page.goto("/");
  await page.getByRole("button", { name: "GeezIME (Recommended)" }).click();
  await expect(page.getByLabel("Keyboard layout")).toHaveValue("GeezIME");
  await expect.poll(async () => (await readLessonInfo(page))?.onboardingStep).toBe(1);
});

test("a completed session survives reload and appears in statistics", async ({ page }) => {
  await seedReturningUser(page);
  await page.goto("/");
  await completeSingleCharacter(page, "e");

  await page.reload();
  expect((await readLessonInfo(page))?.history).toHaveLength(1);
  await page.getByLabel("View performance statistics").click();
  await expect(page.getByRole("heading", { name: "Performance Stats" })).toBeVisible();
  await expect(page.getByText("All Time Statistics")).toBeVisible();
});

test("switching layouts does not record an abandoned dictation", async ({ page }) => {
  await seedReturningUser(page);
  await page.goto("/");
  await page.getByLabel("Typing exercise").focus();
  await page.keyboard.press("a");
  await page.getByLabel("Keyboard layout").selectOption("PowerGeez");

  await expect.poll(async () => (await readLessonInfo(page))?.layoutKind).toBe("PowerGeez");
  expect((await readLessonInfo(page))?.history ?? []).toHaveLength(0);
});

test("anonymous telemetry is off by default and sent only after opt-in", async ({
  page,
}) => {
  await seedReturningUser(page);
  let posts = 0;
  await page.route("**/firestore.googleapis.com/**", async (route) => {
    if (route.request().method() === "POST") {
      posts += 1;
    }
    await route.fulfill({
      status: 200,
      json: route.request().method() === "GET" ? { documents: [] } : {},
    });
  });
  await page.goto("/");
  await completeSingleCharacter(page, "e");
  expect(posts).toBe(0);

  await page.getByLabel("View community dashboard").click();
  await page.getByRole("checkbox", { name: "Share anonymous stats" }).check();
  await page.getByRole("button", { name: "Back" }).click();
  await page.reload();

  await page.getByLabel("Typing exercise").focus();
  await page.keyboard.down("e");
  await page.waitForTimeout(60);
  await page.keyboard.up("e");
  await expect.poll(() => posts).toBe(1);
});

test("hash routes load directly and return to typing", async ({ page }) => {
  await seedReturningUser(page);
  await mockCommunity(page);
  await page.goto("/#stats");
  await expect(page.getByRole("heading", { name: "Performance Stats" })).toBeVisible();

  await page.goto("/#community");
  await expect(page.getByRole("heading", { name: "Community Dashboard" })).toBeVisible();
  await page.getByRole("button", { name: "Back" }).click();
  await expect(page).toHaveURL(/\/$/);
});

test("Tab can move focus out of the typing surface", async ({ page }) => {
  await seedReturningUser(page);
  await page.goto("/");
  const exercise = page.getByLabel("Typing exercise");
  await exercise.focus();
  await page.keyboard.press("Tab");
  await expect(exercise).not.toBeFocused();
});

test("the highlighted caret is visible and animates to the next character", async ({
  page,
}) => {
  await seedReturningUser(page);
  await page.goto("/?dictation=አአ");
  const exercise = page.getByLabel("Typing exercise");
  await exercise.focus();
  await expect(exercise).toBeFocused();

  const active = page.locator("#active-letter");
  const caret = page.getByTestId("animated-caret");
  await expect(caret).toBeVisible();
  await expect
    .poll(async () => {
      const [currentCaret, currentActive] = await Promise.all([
        caret.boundingBox(),
        active.boundingBox(),
      ]);
      return Math.abs((currentCaret?.x ?? 0) - (currentActive?.x ?? 0));
    })
    .toBeLessThanOrEqual(1);
  const firstBox = await caret.boundingBox();
  const activeBox = await active.boundingBox();
  expect(firstBox).not.toBeNull();
  expect(activeBox).not.toBeNull();
  expect(Math.abs(firstBox!.x - activeBox!.x)).toBeLessThanOrEqual(1);
  const presentation = await caret.evaluate((element) => {
    const style = getComputedStyle(element);
    return {
      backgroundColor: style.backgroundColor,
      transitionProperty: style.transitionProperty,
    };
  });
  expect(presentation.backgroundColor).not.toBe("rgba(0, 0, 0, 0)");
  expect(presentation.transitionProperty).toContain("transform");

  await page.keyboard.press("e");
  await expect
    .poll(async () => (await caret.boundingBox())?.x ?? 0)
    .toBeGreaterThan(firstBox!.x);
});

test("PowerGeez Caps Lock input completes its mapped character", async ({ page }) => {
  await seedReturningUser(page, { layoutKind: "PowerGeez" });
  await page.goto("/?dictation=ሸ");
  await page.getByLabel("Typing exercise").focus();
  await page.keyboard.press("CapsLock");
  await completeSingleCharacter(page, "s");
  expect((await readLessonInfo(page))?.history?.[0]?.layoutKind).toBe("PowerGeez");
});
