open Ffi_test_utils.Types
open Schematic
open Swift_test_utils

module Primitive_record : Schema_test = struct
  let swift =
    {|
public struct PrimitiveRecord: CConvertible {
  public typealias C = primitive_record
  public var a: Int
  public var b: Int
  public init(a: Int, b: Int) {
    self.a = a
    self.b = b
  }
  public func toC() -> primitive_record {
    Schemas.primitive_record(
      a: a.toC(),
      b: b.toC()
    )
  }
  public static func ofC(_ c: primitive_record) -> PrimitiveRecord {
    PrimitiveRecord(
      a: Int.ofC(c.a),
      b: Int.ofC(c.b)
    )
  }
}
|}

  let schema = Schema.pack primitive_record_schema
end

module Wrapper : Schema_test = struct
  let swift =
    {|
public struct Wrapper: CConvertible {
  public typealias C = wrapper
  public var somevar: Int
  public var r: PrimitiveRecord
  public init(somevar: Int, r: PrimitiveRecord) {
    self.somevar = somevar
    self.r = r
  }
  public func toC() -> wrapper {
    Schemas.wrapper(
      somevar: somevar.toC(),
      r: r.toC()
    )
  }
  public static func ofC(_ c: wrapper) -> Wrapper {
    Wrapper(
      somevar: Int.ofC(c.somevar),
      r: PrimitiveRecord.ofC(c.r)
    )
  }
}
|}

  let schema = Schema.pack wrapper_schema
end

module Basic_variant : Schema_test = struct
  let swift =
    {|
public enum BasicVariant: CConvertible {
  public typealias C = basic_variant
  case a
  case b
  public func toC() -> basic_variant {
    switch self {
    case .a:
      return basic_variant_a
    case .b:
      return basic_variant_b
    }
  }
  public static func ofC(_ c: basic_variant) -> BasicVariant {
    switch c {
    case basic_variant_a:
      return BasicVariant.a
    case basic_variant_b:
      return BasicVariant.b
    default:
      fatalError("malformed C variant")
    }
  }
}
|}

  let schema = Schema.pack basic_variant_schema
end

module Payload_variant = struct
  let swift =
    {|
public enum PayloadVariant: CConvertible {
  public typealias C = payload_variant
  case nothing
  case nothing2
  case nothing3
  case someint(Int)
  case aPair(Int, String)
  public func toC() -> payload_variant {
    switch self {
    case .nothing:
      return payload_variant(
               tag: payload_variant_nothing_tag,
               payload: nil
      )
    case .nothing2:
      return payload_variant(
               tag: payload_variant_nothing2_tag,
               payload: nil
      )
    case .nothing3:
      return payload_variant(
               tag: payload_variant_nothing3_tag,
               payload: nil
      )
    case let .someint(x):
      return payload_variant(
               tag: payload_variant_someint_tag,
               payload: OpaquePointer(
                          heapAlloc(
                            x.toC()
                          ))
      )
    case let .aPair(t0, t1):
      return payload_variant(
               tag: payload_variant_a_pair_tag,
               payload: OpaquePointer(
                          heapAlloc(
                            tuple_int_string(
                              t0: t0.toC(),
                              t1: t1.toC()
                            )
                          )
               )
      )
    }
  }
  public static func ofC(_ c: payload_variant) -> PayloadVariant {
    switch c.tag {
    case payload_variant_nothing_tag:
      return PayloadVariant.nothing
    case payload_variant_nothing2_tag:
      return PayloadVariant.nothing2
    case payload_variant_nothing3_tag:
      return PayloadVariant.nothing3
    case payload_variant_someint_tag:
      let payload = UnsafePointer<CInt>(c.payload).pointee
      let res = PayloadVariant.someint(Int.ofC(payload))
      free(UnsafeMutableRawPointer(c.payload))
      return res
    case payload_variant_a_pair_tag:
      let payload = UnsafePointer<tuple_int_string>(c.payload).pointee
      let t0 = Int.ofC(payload.t0)
      let t1 = String.ofC(payload.t1)
      let res = PayloadVariant.aPair(t0, t1)
      free(UnsafeMutableRawPointer(c.payload))
      return res
    default:
      fatalError("malformed C variant")
    }
  }
}
|}

  let schema = Schema.pack payload_variant_schema
end
