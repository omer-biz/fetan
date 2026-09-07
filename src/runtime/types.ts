export type Theme = "light" | "dark";

export type IncomingPort<T> = {
  subscribe: (callback: (value: T) => void) => void;
};

export type OutgoingPort<T> = {
  send: (value: T) => void;
};

export type AnalyticsPayload = {
  wpm: number;
  accuracy: number;
  duration: number;
  lessonIdx: number;
  slowestLetter: string;
  layoutKind: string;
};

export type ElmApp = {
  ports: {
    saveInfo: IncomingPort<unknown>;
    saveTheme: IncomingPort<string>;
    saveAnalyticsConsent: IncomingPort<boolean>;
    trackEvent: IncomingPort<AnalyticsPayload>;
    triggerLevelUp: IncomingPort<string>;
    fetchCommunityStats: IncomingPort<void>;
    receiveCommunityStats: OutgoingPort<unknown>;
  };
};

export type Logger = Pick<Console, "error" | "warn">;

export type KeyValueStore = {
  get: <T>(key: string) => Promise<T | undefined>;
  set: (key: string, value: unknown) => Promise<void>;
};

export type FirestoreValue = {
  integerValue?: string | number;
  doubleValue?: string | number;
  stringValue?: string;
  timestampValue?: string;
};

export type FirestoreDocument = {
  fields?: Record<string, FirestoreValue>;
};

export type CommunitySession = {
  wpm: number;
  accuracy: number;
  duration: number;
  lessonIdx: number;
  slowestLetter: string;
  timestamp: string;
};
