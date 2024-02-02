import SwiftSyntaxMacros
import SwiftSyntaxMacrosTestSupport
import XCTest

// Macro implementations build for the host, so the corresponding module is not available when cross-compiling. Cross-compiled tests may still make use of the macro itself in end-to-end tests.

#if canImport(MacroSamplePackages17Macros)
import MacroSamplePackages17Macros

let testMacros: [String: Macro.Type] = [
    "stringify": StringifyMacro.self, // Freestanding Expression
    "AddAsync": AsyncPeerMacro.self, // Attached Peer
    "URL": URLMacro.self, // Freestanding Expression
    "Access": Access.self, // ATTACHED ACCESSOR
    "DictionaryStorage": DictionaryStorageMacro.self,
    "Observable":  ObservableMacro.self,
    "DeclarePropertyWithValue": AttachedExtensionMacroExample.self
]

#endif

final class MacroSamplePackages17Tests: XCTestCase {
    
    // MARK: FREESTANDING EXPRESSION STRINGIFY MACRO TEST
    
    func testMacro() throws {
        assertMacroExpansion(
            """
            #stringify(a + b)
            """,
            expandedSource: """
            (a + b, "a + b")
            """,
            macros: testMacros
        )
        throw XCTSkip("macros are only supported when running tests for the host platform")
    }
    
    func testMacroWithStringLiteral() throws {
        assertMacroExpansion(
            #"""
            #stringify("Hello, \(name)")
            """#,
            expandedSource: #"""
            ("Hello, \(name)", #""Hello, \(name)""#)
            """#,
            macros: testMacros
        )
        throw XCTSkip("macros are only supported when running tests for the host platform")
    }
    
    // MARK: FREESTANDING EXPRESSION URL MACRO TEST
    
    func testURLMacro() throws {
        assertMacroExpansion(
        """
        #URL("https://pokeapi.co/api")
        """,
        expandedSource: """
        URL(string: "https://pokeapi.co/api")!
        """,
        macros: testMacros
        )
    }
    
    // MARK: ATTACHED PEER ASYNC MACRO TEST
    // Jullian Mercier?
    
    func testAsyncMacro() {
        assertMacroExpansion("""
        @AddAsync
        func fetchData(completion: (String) -> Void) {
            completion("Hello")
        }
        """, expandedSource: """
        func fetchData(completion: (String) -> Void) {
            completion("Hello")
        }
        
        func fetchData() async -> String {
            await withCheckedContinuation { continuation in
                fetchData { value in
                     continuation.resume(with: .success(value))
                }
            }
         }
        """, macros: testMacros)
    }
    
    // MARK: ATTACHED ACCESSOR MACRO TEST
    
    func testAccessUserDefaultsMacro() {
        assertMacroExpansion(
            """
            @Access<Bool?>(.userDefaults)
            var isPaidUser1 = false
            @Access<Bool?>(.userDefaults)
            var isPaidUser2: Bool = false
            @Access<Bool?>(.userDefaults)
            var isPaidUser3: Bool?
            """,
            expandedSource:
            """
            
            var isPaidUser1 = false {
                get {
                    (UserDefaults.standard.object(forKey: "AccessKey_isPaidUser1") as? Bool) ?? false
                }
                set {
                    UserDefaults.standard.set(newValue, forKey: "AccessKey_isPaidUser1")
                }
            }
            var isPaidUser2: Bool = false {
                get {
                    (UserDefaults.standard.object(forKey: "AccessKey_isPaidUser2") as? Bool) ?? false
                }
                set {
                    UserDefaults.standard.set(newValue, forKey: "AccessKey_isPaidUser2")
                }
            }
            var isPaidUser3: Bool? {
                get {
                    (UserDefaults.standard.object(forKey: "AccessKey_isPaidUser3") as? Bool)
                }
                set {
                    UserDefaults.standard.set(newValue, forKey: "AccessKey_isPaidUser3")
                }
            }
            """,
            macros: testMacros
        )
    }
    
    // MARK: ATTACHED MEMBER MACRO TEST
    
    func testDictionaryStorageMemberInitilizationPurposes() {
        assertMacroExpansion(
            """
            @DictionaryStorage
            struct Person {
            }
            """,
            expandedSource:
            """
            
            struct Person {
            
                init(dictionary: [String: Any]) {
                    self.dictionary = dictionary
                }
            
                var dictionary: [String: Any]
            }
            """,
            macros: testMacros
        )
    }
    
    // MARK: ATTACHED MEMBER_ATTRIBUTE MACRO TEST
    
    func testMemberAttribute() {
        assertMacroExpansion(
            """
            @Observable
            struct OldStorage {
              var x: Int
              var y: Int
            }
            """,
            expandedSource:
            """
            struct OldStorage {
              @ObservationTracked
              var x: Int
              @ObservationTracked
              var y: Int
            }
            """,
            macros: testMacros
        )
    }
    
    // MARK: ATTACHED EXTENSION  MACRO TEST
    func testExtensionMacro() {
        assertMacroExpansion(
                """
                @DeclarePropertyWithValue(value: 10)
                struct Struct {}
                """,
                expandedSource: """
                struct Struct {}
                
                extension Struct: Equatable {
                  static let value = 10
                }
                """,
                macros: testMacros
        )
    }
}
