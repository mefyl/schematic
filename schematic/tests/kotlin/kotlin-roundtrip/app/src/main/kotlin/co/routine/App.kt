package co.routine

import co.routine.*
import kotlin.reflect.full.createType
import kotlinx.serialization.InternalSerializationApi
import kotlinx.serialization.encodeToString
import kotlinx.serialization.json.Json
import kotlinx.serialization.serializer

@OptIn(InternalSerializationApi::class)
fun main(args: Array<String>) {
  val datatype = args.getOrNull(0)
  System.err.println("Input datatype is $datatype")
  val receivedJson = args.getOrNull(1)
  System.err.println("Received JSON is $receivedJson")

  // This is needed otherwise default values (in particular, null values) are not encoded by Kotlin.
  // See https://github.com/Kotlin/kotlinx.serialization/blob/master/docs/json.md#encoding-defaults
  val json = Json { encodeDefaults = true }

  if (datatype != null && receivedJson != null) {
    // ⚠ This does not work well **at all** for generic types
    val kClass = Class.forName(datatype).kotlin

    val decoded = json.decodeFromString(kClass.serializer(), receivedJson)

    val kType = kClass.createType()

    System.err.println("JSON was decoded into $decoded")

    val reEncoded = json.encodeToString(serializer(kType), decoded)

    System.err.println("Value was re-encoded into $reEncoded")

    println(reEncoded)
  } else {
    System.err.println("Both the datatype and the JSON are mandatory")
  }
}
