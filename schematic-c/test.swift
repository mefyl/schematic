public enum Month {
  case January
  case February
  case March
  case April
  case May
  case June
  case July
  case August
  case September
  case October
  case November
  case December

  public func toC() -> month {
    switch self {
    case .January:
      return month_january
    case .February:
      return month_february
    case .March:
      return month_march
    case .April:
      return month_april
    case .May:
      return month_may
    case .June:
      return month_june
    case .July:
      return month_july
    case .August:
      return month_august
    case .September:
      return month_september
    case .October:
      return month_october
    case .November:
      return month_november
    case .December:
      return month_december
    }
  }
}
