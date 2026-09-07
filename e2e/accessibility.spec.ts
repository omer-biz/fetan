import AxeBuilder from "@axe-core/playwright";
import { expect, test, type Page } from "@playwright/test";
import { mockCommunity, seedReturningUser } from "./helpers";

async function expectNoAxeViolations(page: Page): Promise<void> {
  const results = await new AxeBuilder({ page }).analyze();
  expect(results.violations).toEqual([]);
}

test("onboarding modal has no automated accessibility violations", async ({ page }) => {
  await page.goto("/");
  await expect(page.getByRole("dialog")).toBeVisible();
  await expectNoAxeViolations(page);
});

test("typing, statistics, and community surfaces pass Axe", async ({ page }) => {
  await seedReturningUser(page);
  await mockCommunity(page);
  await page.goto("/");
  await expect(page.getByLabel("Typing exercise")).toBeVisible();
  await expectNoAxeViolations(page);

  await page.goto("/#stats");
  await expect(page.getByRole("heading", { name: "Performance Stats" })).toBeVisible();
  await expectNoAxeViolations(page);

  await page.goto("/#community");
  await expect(page.getByRole("heading", { name: "Community Dashboard" })).toBeVisible();
  await expectNoAxeViolations(page);
});
