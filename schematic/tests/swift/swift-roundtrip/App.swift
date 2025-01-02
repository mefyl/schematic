import Foundation

@main
struct App {

  static func main() {
    if CommandLine.arguments.count != 3 {
      print("Usage: App datatype json, got \(CommandLine.arguments)")
    }

    let datatype = CommandLine.arguments[1]
    let receivedJson = CommandLine.arguments[2]

    let jsonDataRaw = "{ \"\(datatype)\": { \"_0\": \(receivedJson) } }"
    NSLog("Input type: " + datatype)
    NSLog("Input data: " + jsonDataRaw)
    let jsonData = jsonDataRaw.data(using: .utf8)!
    let decoder = JSONDecoder()
    let encoder = JSONEncoder()

    do {
      let data = try decoder.decode(All.self, from: jsonData)
      // let data = All.VersionSimple(VersionSimple.Int)
      let reencoded = try encoder.encode(data)
      let reencodedJson =
        try JSONSerialization.jsonObject(with: reencoded, options: []) as! [String: [String: Any]]
      let jsonWithoutExtras = try JSONSerialization.data(
        withJSONObject: reencodedJson[datatype]!["_0"]!,
        options: [.fragmentsAllowed])
      NSLog("Sending back: " + String(data: jsonWithoutExtras, encoding: .utf8)!)
      print(String(data: jsonWithoutExtras, encoding: .utf8)!)
    } catch {
      print("Decoding error:", error)
    }
  }
}
