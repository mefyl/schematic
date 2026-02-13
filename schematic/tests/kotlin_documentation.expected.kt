/** A user record with name and age fields */
@Serializable
data class DocumentedRecord(val name: String,
val age: Int)

/** A documented type for testing TypeScript JSDoc generation.

    With a long comment to ensures that we wrap things properly even in the
    presence of absurdly long lines in the original documentation comment */
typealias DocumentedType = Int

/** A type with *\/ in the documentation to test escaping */
typealias DocumentedWithStarSlash = String

/** A result type with documented cases */
@Serializable
@OptIn(ExperimentalSerializationApi::class)
@JsonClassDiscriminator("#type")
sealed interface ResultWithCaseDocs {
    /** Success with a value */ @Serializable @SerialName("Ok")
    data class ResultWithCaseDocsOk(val value: Int) : ResultWithCaseDocs 
    /** Failure with an error message */ @Serializable @SerialName("Error")
    data class ResultWithCaseDocsError(val value: String) : ResultWithCaseDocs
    
}

/** A user with documented fields */
@Serializable
data class UserWithFieldDocs(/** The user's full name */
val name: String,
/** Age in years */
val age: Int,
/** Optional email address */
val email: Option<String> = None)