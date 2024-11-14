plugins {
  // Apply the application plugin to add support for building a CLI application in Java.
  application

  // Enable Kotlinx serialization
  // Cf https://github.com/Kotlin/kotlinx.serialization?tab=readme-ov-file#1-setting-up-the-serialization-plugin
  kotlin("jvm") version "2.0.20"
  kotlin("plugin.serialization") version "2.0.20"
}

repositories {
  // Use Maven Central for resolving dependencies.
  mavenCentral()
}

dependencies {
  // Use the Kotlin JUnit 5 integration.
  testImplementation("org.jetbrains.kotlin:kotlin-test-junit5")

  // Use the JUnit 5 integration.
  testImplementation(libs.junit.jupiter.engine)

  testRuntimeOnly("org.junit.platform:junit-platform-launcher")

  // This dependency is used by the application.
  implementation(libs.guava)

  implementation(libs.kotlinx.serialization.json)
  implementation(libs.kotlinx.serialization.datetime)

  implementation(libs.arrow.kt.core)

  implementation(kotlin("reflect"))
}

application {
  // Define the main class for the application.
  mainClass = "co.routine.AppKt"
}

tasks.named<Test>("test") {
  // Use JUnit Platform for unit tests.
  useJUnitPlatform()
}

tasks.named<JavaExec>("run") {
  // We can't use `gradle run --args` because this garbage application removes all spaces, single and double quotes.
  // Good luck passing a JSON without quotes :D
  // So instead, we use system properties to pass those...
  // Courtesy of https://stackoverflow.com/a/74702469
  args = listOf(System.getProperty("type"), System.getProperty("json"))
}

application {
  mainClass = "co.routine.AppKt"
}
