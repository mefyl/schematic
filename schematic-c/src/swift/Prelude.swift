import Foundation

// XXX: should this be here? visibility?
func heapAlloc<T>(_ to: T) -> UnsafeMutablePointer<T> {
  let p = UnsafeMutablePointer<T>.allocate(capacity: 1)
  p.initialize(to: to)
  return p
}

//
// Protocol definitions
//
public protocol ToC {
  associatedtype C
  func toC() -> C
}

public protocol OfC {
  associatedtype C
  static func ofC(_ c: C) -> Self
}

protocol CConvertible: OfC, ToC {}

// Types
struct Unit: CConvertible {
  func toC() -> unit {
      return 0;
  }
  static func ofC(_ c: unit) -> Unit {
    assert(c == 0);
    return Self()
  }
}



//
// Extensions
//

// Types without allocation.
extension Bool: CConvertible {
  public typealias C = CBool
  public func toC() -> CBool {
    return CBool(self)
  }

  public static func ofC(_ c: CBool) -> Bool {
    return Bool(c)
  }
}


extension Int: CConvertible {
  public typealias C = CInt
  public static func ofC(_ c: CInt) -> Int {
    return Int(c)
  }

  public func toC() -> CInt {
    return CInt(self)
  }
}


extension Int32: CConvertible {
  public typealias C = Int32
  public static func ofC(_ c: Int32) -> Int32 {
    return c
  }

  public func toC() -> Int32 {
    return self
  }
}

extension Int64: CConvertible {
  public typealias C = Int64
  public static func ofC(_ c: Int64) -> Int64 {
    return c
  }

  public func toC() -> Int64 {
    return self
  }
}






extension Double: CConvertible {
  public typealias C = CDouble
  public static func ofC(_ c: CDouble) -> Double {
    return Double(c)
  }

  public func toC() -> CDouble {
    return CDouble(self)
  }
}


public struct SchematicTime: CConvertible {
  public typealias C = time_

  let sec: Int
  let nsec: Int64

  public func toC() -> C {
    let timespec = timespec(
      tv_sec: sec,
      tv_nsec: CLong(nsec)
    )
    return timespec_to_sch_time(timespec)
  }

  public static func ofC(_ c: C) -> SchematicTime {
    let ts = timespec_of_sch_time(c)
    return SchematicTime(
      sec: Int(ts.tv_sec),
      nsec: Int64(ts.tv_nsec)
    )
  }
}


extension URL: CConvertible {
  public typealias C = UnsafePointer<CChar>
  public func toC() -> C {
    return absoluteString.toC()
  }

  public static func ofC(_ c: C) -> URL {
    return URL(string: String.ofC(c))!
  }
}



extension String: CConvertible {
  public typealias C = UnsafePointer<CChar>
  public func toC() -> C {
    return UnsafePointer(strdup(self))
  }

  public static func ofC(_ c: C) -> String {
    let s = String(cString: c)
    free(UnsafeMutableRawPointer(mutating: c))
    return s
  }
}


// Won't work on tuples.
extension Array: OfC where Element: OfC {
  public typealias C = type_erased_array

  public static func ofC(_ c: type_erased_array) -> [Element] {
    assert(c.len >= 0)
    if c.len == 0 { return [] }

    return (0 ..< (c.len)).map {
      let p = c.data.assumingMemoryBound(to: Element.C.self)
        .advanced(by: $0)
      return Element.ofC(p.pointee)
    }
  }
}

extension Array: ToC where Element: ToC {
  public typealias C = type_erased_array

  public func toC() -> type_erased_array {
    let data = UnsafeMutablePointer<Element.C>.allocate(capacity: count)
    for (i, x) in enumerated() {
      data.advanced(by: i).initialize(to: x.toC())
    }

    return type_erased_array(
      len: count,
      data: data
    )
  }
}


// Freestanding generic functions let us work around some of the limitations of
// protocols.
//
// These functions are overloaded for tuples.
// The present blanket impletations are provided for convenience.

func schematicOfC<T: OfC>(_ c: T.C) -> T {
    T.ofC(c)
}

func schematicToC<T: ToC>(_ x: T) -> T.C{
    x.toC()
}
