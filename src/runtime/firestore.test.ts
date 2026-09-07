import { describe, expect, it, vi } from "vitest";
import {
  communityEndpoint,
  communitySessionFromDocument,
  fetchCommunitySessions,
  numberFromField,
  sessionDocument,
  sessionsEndpoint,
  submitSession,
} from "./firestore";
import type { AnalyticsPayload } from "./types";

const payload: AnalyticsPayload = {
  wpm: 42.4,
  accuracy: 97.6,
  duration: 30.5,
  lessonIdx: 4,
  slowestLetter: "መ",
  layoutKind: "GeezIME",
};

const fixedNow = () => new Date("2026-09-07T12:00:00.000Z");

describe("Firestore runtime", () => {
  it("builds stable endpoints and the Firestore document wire format", () => {
    expect(sessionsEndpoint("project")).toContain("projects/project/");
    expect(communityEndpoint("project")).toContain("orderBy=timestamp%20desc");
    expect(sessionDocument(payload, fixedNow().toISOString())).toEqual({
      fields: {
        wpm: { integerValue: "42" },
        accuracy: { integerValue: "98" },
        duration: { doubleValue: 30.5 },
        lessonIdx: { integerValue: "4" },
        slowestLetter: { stringValue: "መ" },
        layoutKind: { stringValue: "GeezIME" },
        timestamp: { timestampValue: "2026-09-07T12:00:00.000Z" },
      },
    });
  });

  it("submits sessions and rejects every non-success status", async () => {
    const fetcher = vi.fn<typeof fetch>();
    fetcher.mockResolvedValueOnce(new Response(null, { status: 200 }));
    await submitSession(fetcher, payload, fixedNow);
    expect(fetcher).toHaveBeenCalledWith(
      sessionsEndpoint(),
      expect.objectContaining({ method: "POST" }),
    );

    fetcher.mockResolvedValueOnce(new Response(null, { status: 403 }));
    await expect(submitSession(fetcher, payload, fixedNow)).rejects.toThrow(
      "Firestore returned 403",
    );
  });

  it("parses numeric Firestore variants and rejects invalid numbers", () => {
    expect(numberFromField({ integerValue: "41" })).toBe(41);
    expect(numberFromField({ doubleValue: 12.5 })).toBe(12.5);
    expect(numberFromField({ doubleValue: "bad" })).toBe(0);
    expect(numberFromField(undefined)).toBe(0);
  });

  it("normalizes documents including missing fields", () => {
    expect(
      communitySessionFromDocument(
        {
          fields: {
            wpm: { integerValue: "41.6" },
            accuracy: { integerValue: 98 },
            duration: { doubleValue: "10.5" },
            lessonIdx: { integerValue: "7" },
            slowestLetter: { stringValue: "ሀ" },
            timestamp: { timestampValue: "saved" },
          },
        },
        "fallback",
      ),
    ).toEqual({
      wpm: 42,
      accuracy: 98,
      duration: 10.5,
      lessonIdx: 7,
      slowestLetter: "ሀ",
      timestamp: "saved",
    });
    expect(communitySessionFromDocument({}, "fallback")).toEqual({
      wpm: 0,
      accuracy: 0,
      duration: 0,
      lessonIdx: 0,
      slowestLetter: "N/A",
      timestamp: "fallback",
    });
  });

  it("loads ordered community data and accepts an empty database", async () => {
    const fetcher = vi.fn<typeof fetch>();
    fetcher.mockResolvedValueOnce(
      new Response(
        JSON.stringify({ documents: [{ fields: { wpm: { integerValue: "35" } } }] }),
        { status: 200, headers: { "Content-Type": "application/json" } },
      ),
    );
    await expect(fetchCommunitySessions(fetcher, fixedNow)).resolves.toHaveLength(1);
    expect(fetcher).toHaveBeenCalledWith(communityEndpoint());

    fetcher.mockResolvedValueOnce(
      new Response(JSON.stringify({}), {
        status: 200,
        headers: { "Content-Type": "application/json" },
      }),
    );
    await expect(fetchCommunitySessions(fetcher, fixedNow)).resolves.toEqual([]);
  });

  it("surfaces failed community requests", async () => {
    const fetcher = vi.fn<typeof fetch>();
    fetcher.mockResolvedValue(new Response(null, { status: 500 }));
    await expect(fetchCommunitySessions(fetcher, fixedNow)).rejects.toThrow(
      "Firestore returned 500",
    );
  });
});
