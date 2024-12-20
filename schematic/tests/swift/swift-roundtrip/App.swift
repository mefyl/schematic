import Foundation

@main
struct App {
  static func main() {

    if CommandLine.arguments.count != 3 {
        print("Usage: App datatype json, got \(CommandLine.arguments)")
    }

    let datatype = CommandLine.arguments[1]
    let receivedJson = CommandLine.arguments[2]

    let jsonDataRaw = "{ \"\(datatype)\": \(receivedJson) }"
    let jsonData = jsonDataRaw.data(using: .utf8)!
    let decoder = JSONDecoder()
    let encoder = JSONEncoder()

    do {
        let data = try decoder.decode(All.self, from:jsonData)
        let reencoded = try encoder.encode(data)
        print(String(data: reencoded, encoding: .utf8)!)
    } catch {
      print("Decoding error:", error)
    }
  }
}
