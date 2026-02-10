/**
 * A documented type for testing TypeScript JSDoc generation
 */
export type DocumentedType = number;

 
/**
 * A user record with name and age fields
 */
export type DocumentedRecord = { "name": string; "age": number };

 
/**
 * A type with *\/ in the documentation to test escaping
 */
export type DocumentedWithStarSlash = string;

 
/**
 * A user with documented fields
 */
export type UserWithFieldDocs = {
    /** The user's full name */ "name": string;
    /** Age in years */ "age": number;
    /** Optional email address */ "email"?: string
  };

 
/**
 * A result type with documented cases
 */
export type ResultWithCaseDocs =
  /** Success with a value */ number |
  /** Failure with an error message */ string;

