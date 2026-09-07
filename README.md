# Qelm

Qelm is an Amharic touch-typing tutor built with Elm, TypeScript, Vite, and Tailwind CSS. It supports GeezIME, SIL Power-G, and PowerGeez layouts, per-layout progression, targeted weakness practice, local statistics, and an installable offline experience.

## Development

Use Node.js 22 or later.

```sh
npm ci
npm run dev
```

Useful checks:

```sh
npm run test:elm
npm run test:ts
npm run test:coverage
npm run test:e2e
npm run build
npm run check
```

`npm test` runs the Elm and TypeScript unit suites. `npm run check` also enforces
the TypeScript coverage gate and creates a production build. The current gate is
80% for lines, statements, and functions and 70% for branches across the
TypeScript runtime modules.

The Playwright suite builds the application in a dedicated test mode, starts a
local preview server, and runs Chromium scenarios for onboarding, keyboard input,
persistence, navigation, analytics consent, abandoned sessions, accessibility,
and offline PWA reloads. Install its browser once on a new machine:

```sh
npx playwright install chromium
```

CI installs Chromium with its operating-system dependencies and runs the browser
suite only after the unit, coverage, and production-build gate succeeds.

## Architecture

- `src/Main.elm` initializes the Elm application and subscriptions.
- `src/Update.elm` owns state transitions and effects.
- `src/View.elm` renders the typing experience.
- `src/Layouts/` contains the three keyboard engines.
- `src/Dictation.elm` and `src/DictationLogic.elm` contain lesson generation and pure typing logic.
- `src/main.ts` bootstraps the application; testable browser integrations live in `src/runtime/`.
- `tests/` contains the Elm domain and update suites.
- `src/runtime/*.test.ts` covers browser integration units with Vitest and jsdom.
- `e2e/` contains the Playwright and Axe user-flow checks.

Progress is stored locally in IndexedDB. Users can opt in from the community dashboard to share completed-session speed, accuracy, duration, lesson, layout, and slowest-character fields with the `qelm-analytics` Firestore project. No name or account identifier is submitted, and sharing is disabled by default.
