export class SlateError extends Error {
  readonly name = "SlateError";
}

export type Result<T> = { ok: true; value: T } | { ok: false; error: Error };

export function ok<T>(value: T): Result<T> {
  return { ok: true, value };
}

export function err(error: Error): Result<never> {
  return { ok: false, error };
}

