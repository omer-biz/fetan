import { expect, test } from "@playwright/test";
import { seedReturningUser } from "./helpers";

test("the installed app reloads while offline", async ({ context, page }) => {
  await seedReturningUser(page);
  await page.goto("/");
  await page.evaluate(async () => {
    await navigator.serviceWorker.ready;
  });

  await context.setOffline(true);
  try {
    await page.reload({ waitUntil: "domcontentloaded" });
    await expect(page.getByText("Amharic Typing Practice")).toBeVisible();
    await expect(page.getByLabel("Typing exercise")).toBeVisible();
  } finally {
    await context.setOffline(false);
  }
});
