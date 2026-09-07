import type {
  AnalyticsPayload,
  CommunitySession,
  FirestoreDocument,
  FirestoreValue,
} from "./types";

export const FIREBASE_PROJECT_ID = "qelm-analytics";

export function sessionsEndpoint(projectId = FIREBASE_PROJECT_ID): string {
  return `https://firestore.googleapis.com/v1/projects/${projectId}/databases/(default)/documents/sessions`;
}

export function communityEndpoint(projectId = FIREBASE_PROJECT_ID): string {
  return `${sessionsEndpoint(projectId)}?pageSize=300&orderBy=timestamp%20desc`;
}

export function sessionDocument(
  payload: AnalyticsPayload,
  timestamp: string,
): object {
  return {
    fields: {
      wpm: { integerValue: String(Math.round(payload.wpm)) },
      accuracy: { integerValue: String(Math.round(payload.accuracy)) },
      duration: { doubleValue: payload.duration },
      lessonIdx: { integerValue: String(payload.lessonIdx) },
      slowestLetter: { stringValue: payload.slowestLetter },
      layoutKind: { stringValue: payload.layoutKind },
      timestamp: { timestampValue: timestamp },
    },
  };
}

export async function submitSession(
  fetcher: typeof fetch,
  payload: AnalyticsPayload,
  now: () => Date = () => new Date(),
): Promise<void> {
  const response = await fetcher(sessionsEndpoint(), {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(sessionDocument(payload, now().toISOString())),
  });

  if (!response.ok) {
    throw new Error(`Firestore returned ${response.status}.`);
  }
}

export function numberFromField(value: FirestoreValue | undefined): number {
  const raw = value?.integerValue ?? value?.doubleValue;
  const parsed = typeof raw === "number" ? raw : Number.parseFloat(raw ?? "0");
  return Number.isFinite(parsed) ? parsed : 0;
}

export function communitySessionFromDocument(
  document: FirestoreDocument,
  fallbackTimestamp: string,
): CommunitySession {
  const fields = document.fields ?? {};
  return {
    wpm: Math.round(numberFromField(fields.wpm)),
    accuracy: Math.round(numberFromField(fields.accuracy)),
    duration: numberFromField(fields.duration),
    lessonIdx: Math.round(numberFromField(fields.lessonIdx)),
    slowestLetter: fields.slowestLetter?.stringValue ?? "N/A",
    timestamp: fields.timestamp?.timestampValue ?? fallbackTimestamp,
  };
}

export async function fetchCommunitySessions(
  fetcher: typeof fetch,
  now: () => Date = () => new Date(),
): Promise<CommunitySession[]> {
  const response = await fetcher(communityEndpoint());
  if (!response.ok) {
    throw new Error(`Firestore returned ${response.status}.`);
  }

  const body = (await response.json()) as { documents?: FirestoreDocument[] };
  const fallbackTimestamp = now().toISOString();
  return (body.documents ?? []).map((document) =>
    communitySessionFromDocument(document, fallbackTimestamp),
  );
}
