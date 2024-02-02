import Foundation
// The Swift Programming Language
// https://docs.swift.org/swift-book

/// A macro that produces both a value and a string containing the
/// source code that generated the value. For example,
///
///     #stringify(x + y)
///
/// produces a tuple `(x + y, "x + y")`.
/// 
/// SUBSCRIBE ON MY YOUTUBE CHANNEL: OMERSANLI17

// MARK: FREESTANDING(EXPRESSION MACRO)
@freestanding(expression)
public macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroSamplePackages17Macros", type: "StringifyMacro")

@freestanding(expression)
public macro URL(_ stringLiteral: String) -> URL = #externalMacro(module: "MacroSamplePackages17Macros", type: "URLMacro")

// MARK: FREESTANDING DECLARATION MACRO
@freestanding(declaration)
macro warning(_ message: String) = #externalMacro(module: "MacroSamplePackages17Macros", type: "WarningMacro")

// MARK: ATTACHED PEER MACRO
@attached(peer, names: overloaded)
public macro AddAsync() = #externalMacro(module: "MacroSamplePackages17Macros", type: "AsyncPeerMacro")

//  MARK: ATTACHED ACCESOR MACRO
@attached(accessor)
public macro Access<T>(_ type: AccessContentType) = #externalMacro(module: "MacroSamplePackages17Macros", type: "Access")

// MARK: ATTACHED MEMBER MACRO
@attached(member, names: named(dictionary))
public macro DictionaryStorage() = #externalMacro(module: "MacroSamplePackages17Macros", type: "DictionaryStorageMacro")

// MARK: ATTACHED MEMBERAttribute MACRO
@attached(memberAttribute)
public macro Observable() =
  #externalMacro(module: "MacroSamplePackages17Macros", type: "ObservableMacro")

// MARK: ATTACHED CONFORMANCE MACRO
@attached(extension, conformances: Equatable, names: named(value))
public macro DeclarePropertyWithValue<T: Equatable>(_ value: T) =
  #externalMacro(module: "MacroSamplePackages17Macros", type: "AttachedExtensionMacroExample")

// MARK: AccessContentType ENUM FOR ACCESSOR

public enum AccessContentType {
    /// Retrieve value from or store value to [UserDefault](https://developer.apple.com/documentation/foundation/userdefaults).
    /// Default to [.standard](https://developer.apple.com/documentation/foundation/userdefaults/1416603-standard).
    case userDefaults(UserDefaults = .standard)
    /// Retrieve value from or store value to [NSCache](https://developer.apple.com/documentation/foundation/nscache).
    /// Have to provide an instance of **<NSString, AnyObject>**.
    case nsCache(NSCache<NSString, AnyObject>)
    /// Retrieve value from or store value to [NSMAPTable](https://developer.apple.com/documentation/foundation/nsmaptable)
    /// Have to provide an instance of **NSMapTable<NSString, AnyObject>**.
    case nsMapTable(NSMapTable<NSString, AnyObject>)
    /// Access system keychain
    case keychain
}

// MARK: SPECIAL THANKS TO

// Antoine v.d. Swiftlee
// Ömer Ulusal
// Alex Hoppen
// Tim Wang
// 강수진, sujinnaljin, Presentation at KDWC23
// Kishikawa Katsumi
// Nicola Lancellotti
// Matt Rothenberg
// B. Royal Gordon
// Anıl Taşkıran
// Jullian Mercier
// Pedro Rojas
// Maxim Krouk
