/** A user record with name and age fields
 */
public final class DocumentedRecord : Codable, Sendable {
  public let name: Swift.String
  public let age: Swift.Int
  public init(name:
  Swift.String,
  age:
  Swift.Int) { self.name = name
  self.age = age }
}


public typealias DocumentedType = Swift.Int

public typealias DocumentedWithStarSlash = Swift.String

/** A result type with documented cases
 */
public indirect enum ResultWithCaseDocs : Codable, Sendable {
  /** Success with a value
   */
  case ok(Swift.Int)
  /** Failure with an error message
   */
  case error(Swift.String)
}


/** A user with documented fields
 */
public final class UserWithFieldDocs : Codable, Sendable {
  /** The user's full name
   */
  public let name: Swift.String
  /** Age in years
   */
  public let age: Swift.Int
  /** Optional email address
   */
  public let email: Routine.Maybe<Swift.String>
  public init(name:
  Swift.String,
  age:
  Swift.Int,
  email:
  Routine.Maybe<Swift.String>) { self.name = name
  self.age = age
  self.email = email }
}
