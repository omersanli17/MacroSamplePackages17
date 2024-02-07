import SwiftCompilerPlugin
import Foundation
import SwiftSyntax
import SwiftSyntaxBuilder
import SwiftSyntaxMacros
import SwiftDiagnostics
import SwiftOperators
@_spi(ExperimentalLanguageFeature) import SwiftSyntaxMacros


/// Implementation of the `stringify` macro, which takes an expression
/// of any type and produces a tuple containing the value of that expression
/// and the source code that produced the value. For example
///
///     #stringify(x + y)
///
///  will expand to
///
///     (x + y, "x + y")

// MARK: FREESTANDING STRINGIFY EXPRESSION MACRO

public struct StringifyMacro: ExpressionMacro {
    public static func expansion(
        of node: some FreestandingMacroExpansionSyntax,
        in context: some MacroExpansionContext
    ) -> ExprSyntax {
        guard let argument = node.argumentList.first?.expression else {
            fatalError("compiler bug: the macro does not have any arguments")
        }

        return "(\(argument), \(literal: argument.description))"
    }
}

// MARK: FREESTANDING URL EXPRESSION MACRO

public struct URLMacro: ExpressionMacro {
    public static func expansion(
        of node: some FreestandingMacroExpansionSyntax,
        in context: some MacroExpansionContext
    ) throws -> ExprSyntax {
        guard
            /// 1. Grab the first (and only) Macro argument.
            let argument = node.argumentList.first?.expression,
            /// 2. Ensure the argument contains of a single String literal segment.
            let segments = argument.as(StringLiteralExprSyntax.self)?.segments,
            segments.count == 1,
            /// 3. Grab the actual String literal segment.
            case .stringSegment(let literalSegment)? = segments.first
        else {
            throw URLMacroError.requiresStaticStringLiteral
        }
        /// 4. Validate whether the String literal matches a valid URL structure.
        guard let _ = URL(string: literalSegment.content.text) else {
            throw URLMacroError.malformedURL(urlString: "\(argument)")
        }
        
        return "URL(string: \(argument))!"
    }
}

enum URLMacroError: Error, CustomStringConvertible {
    case requiresStaticStringLiteral
    case malformedURL(urlString: String)
    
    var description: String {
        switch self {
        case .requiresStaticStringLiteral:
            return "#URL requires a static string literal"
        case .malformedURL(let urlString):
            return "The input URL is malformed: \(urlString)"
        }
    }
}

// MARK: FREESTANDING DECLARATION MACRO

public struct WarningMacro: DeclarationMacro {
    public static func expansion(
        of node: some FreestandingMacroExpansionSyntax,
        in context: some MacroExpansionContext
    ) throws -> [DeclSyntax] {
        guard let messageExpr = node.argumentList.first?.expression.as(StringLiteralExprSyntax.self),
              messageExpr.segments.count == 1,
              let firstSegment = messageExpr.segments.first,
              case let .stringSegment(message) = firstSegment else {
            throw CustomError.message("warning macro requires a non-interpolated string literal")
        }
        
        context.diagnose(Diagnostic(node: Syntax(node), message: SimpleDiagnosticMessage(
            message: message.description,
            diagnosticID: .init(domain: "test", id: "error"),
            severity: .warning)))
        return []
    }
}

enum CustomError: Error, CustomStringConvertible {
    case message(String)
    
    var description: String {
        switch self {
        case .message(let text):
            return text
        }
    }
}

struct SimpleDiagnosticMessage: DiagnosticMessage {
    let message: String
    let diagnosticID: MessageID
    let severity: DiagnosticSeverity
}

extension SimpleDiagnosticMessage: FixItMessage {
    var fixItID: MessageID { diagnosticID }
}

// MARK: ATTACHED PEER MACRO

public enum AsyncDeclError: CustomStringConvertible, Error {
    case onlyApplicableToFunction
    case onlyApplicableToFunctionWithASingleFunctionArgument
    
    public var description: String {
        switch self {
        case .onlyApplicableToFunction:
            "@AddAsync can only be applied to a function."
        case .onlyApplicableToFunctionWithASingleFunctionArgument:
            "@AddAsync can only be applied to a function with the following signature: func someMethodName(someCompletionName: (someType) -> Void)"
        }
    }
}

public struct AsyncPeerMacro: PeerMacro {
    public static func expansion(of node: AttributeSyntax,
                                 providingPeersOf declaration: some DeclSyntaxProtocol,
                                 in context: some MacroExpansionContext) throws -> [DeclSyntax] {
        guard let function = declaration.as(FunctionDeclSyntax.self) else {
            throw AsyncDeclError.onlyApplicableToFunction
        }
        
        guard function.signature.input.parameterList.count == 1,
              let functionCompletionParameter = function.signature.input.parameterList.first?.type.as(FunctionTypeSyntax.self),
              let functionCompletionParameterType = functionCompletionParameter.arguments.first?.type.as(SimpleTypeIdentifierSyntax.self) else {
            throw AsyncDeclError.onlyApplicableToFunctionWithASingleFunctionArgument
        }
        
        return [DeclSyntax(stringLiteral: """
                func \(function.identifier.text)() async -> \(functionCompletionParameterType.name) {
                    await withCheckedContinuation { continuation in
                        \(function.identifier.text) { value in
                             continuation.resume(with: .success(value))
                        }
                    }
                 }
                """)]
    }
}

public struct AddPublisher: PeerMacro {
    public static func expansion<Context: MacroExpansionContext,
                                 Declaration: DeclSyntaxProtocol>(of node: AttributeSyntax,
                                                                  providingPeersOf declaration: Declaration,
                                                                  in context: Context) throws -> [DeclSyntax] {
        guard let variableDecl = declaration.as(VariableDeclSyntax.self),
              variableDecl.modifiers.map({ $0.name.text }).contains("private") else {
                  throw MacroDiagnostics.errorMacroUsage(message: "Please make the subject private and use the automated generated publisher variable outsite of this type")
              }
        guard let binding = variableDecl.bindings.first,
              let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier,
              let genericArgumentClause = binding.genericArgumentClause,
              ["PassthroughSubject", "CurrentValueSubject"].contains(binding.typeName) else {
            throw MacroDiagnostics.errorMacroUsage(message: "Can only be applied to a subject(PassthroughSubject/CurrentValueSubject) variable declaration")
        }

        let publisher: DeclSyntax =
        """
        var \(raw: identifier.text)Publisher: AnyPublisher<\(genericArgumentClause.arguments)> {
            \(raw: identifier.text).eraseToAnyPublisher()
        }
        """
        return [publisher]
    }
}

extension PatternBindingListSyntax.Element {
    var genericArgumentClause: GenericArgumentClauseSyntax? {
        initializer?.value.as(FunctionCallExprSyntax.self)?
            .calledExpression.as(GenericSpecializationExprSyntax.self)?
            .genericArgumentClause
        ?? typeAnnotation?.type.as(IdentifierTypeSyntax.self)?.genericArgumentClause
    }

    var typeName: String? {
        initializer?.value.as(FunctionCallExprSyntax.self)?.calledExpression.as(GenericSpecializationExprSyntax.self)?.expression.as(DeclReferenceExprSyntax.self)?.baseName.text
        ?? typeAnnotation?.type.as(IdentifierTypeSyntax.self)?.name.text
    }
}

// MARK: ATTACHED ACCESSOR MACRO

public struct Access: AccessorMacro {
    public static func expansion<Context: MacroExpansionContext,
                                 Declaration: DeclSyntaxProtocol>(of node: AttributeSyntax,
                                                                  providingAccessorsOf declaration: Declaration,
                                                                  in context: Context) throws -> [AccessorDeclSyntax] {
        guard let firstArg = node.arguments?.as(LabeledExprListSyntax.self)?.first,
              let type = firstArg.type else {
            throw MacroDiagnostics.errorMacroUsage(message: "Must specify a content type")
        }
        if type == "userDefaults",
           let dataType = node.attributeName.as(IdentifierTypeSyntax.self)?.type {
            return processUserDefaults(for: declaration,
                                       userDefaults: firstArg.userDefaults,
                                       type: "\(dataType)")
        } else if ["nsCache", "nsMapTable"].contains(type),
                  let object = firstArg.object,
                  let dataType = node.attributeName.as(IdentifierTypeSyntax.self)?.type {
            let isOptionalType = node.attributeName.as(IdentifierTypeSyntax.self)?.genericArgumentClause?.arguments
                .first?.as(GenericArgumentSyntax.self)?.argument.is(OptionalTypeSyntax.self) ?? false
            return processNSCacheAndNSMapTable(for: declaration,
                                               object: object,
                                               type: "\(dataType)",
                                               isOptionalType: isOptionalType)
        } else if type == "keychain" {
            return processKeychain(for: declaration)
        }
        
        return []
    }
    
    private static func processKeychain(for declaration: DeclSyntaxProtocol) -> [AccessorDeclSyntax] {
        guard let binding = declaration.as(VariableDeclSyntax.self)?.bindings.first,
              let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text,
              binding.accessorBlock == nil else { return [] }
        let getAccessor: AccessorDeclSyntax =
          """
          get {
              try? SwiftKeychain.search(key: "AccessKey_\(raw: identifier)")
          }
          """
        
        let setAccessor: AccessorDeclSyntax =
          """
          set {
              if let value = newValue {
                  SwiftKeychain.delete(key: "AccessKey_\(raw: identifier)")
                  try? SwiftKeychain.add(value: value, for: "AccessKey_\(raw: identifier)")
              } else {
                  SwiftKeychain.delete(key: "AccessKey_\(raw: identifier)")
              }
          }
          """
        return [getAccessor, setAccessor]
    }
    
    private static func processUserDefaults(for declaration: DeclSyntaxProtocol,
                                            userDefaults: ExprSyntax,
                                            type: String) -> [AccessorDeclSyntax] {
        guard let binding = declaration.as(VariableDeclSyntax.self)?.bindings.first,
              let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text,
              binding.accessorBlock == nil else { return [] }
        var defaultValue = ""
        if let value = binding.initializer?.value {
            defaultValue = " ?? \(value)"
        }
        let getAccessor: AccessorDeclSyntax =
          """
          get {
              (\(userDefaults).object(forKey: "AccessKey_\(raw: identifier)") as? \(raw: type))\(raw: defaultValue)
          }
          """
        
        let setAccessor: AccessorDeclSyntax =
          """
          set {
              \(userDefaults).set(newValue, forKey: "AccessKey_\(raw: identifier)")
          }
          """
        return [getAccessor, setAccessor]
    }
    
    private static func processNSCacheAndNSMapTable(for declaration: DeclSyntaxProtocol,
                                                    object: ExprSyntax,
                                                    type: String,
                                                    isOptionalType: Bool) -> [AccessorDeclSyntax] {
        guard let binding = declaration.as(VariableDeclSyntax.self)?.bindings.first,
              let identifier = binding.pattern.as(IdentifierPatternSyntax.self)?.identifier.text,
              binding.accessorBlock == nil else { return [] }
        var defaultValue = ""
        if let value = binding.initializer?.value {
            defaultValue = " ?? \(value)"
        }
        let getAccessor: AccessorDeclSyntax =
          """
          get {
              (\(object).object(forKey: "AccessKey_\(raw: identifier)") as? \(raw: type))\(raw: defaultValue)
          }
          """
        let setAccessor: AccessorDeclSyntax
        if isOptionalType {
            setAccessor =
          """
          set {
              if let value = newValue {
                  \(object).setObject(value, forKey: "AccessKey_\(raw: identifier)")
              } else {
                  \(object).removeObject(forKey: "AccessKey_\(raw: identifier)")
              }
          }
          """
        } else {
            setAccessor =
          """
          set {
              \(object).setObject(newValue, forKey: "AccessKey_\(raw: identifier)")
          }
          """
        }
        return [getAccessor, setAccessor]
    }
}

private extension LabeledExprSyntax {
    var type: String? {
        expression.as(MemberAccessExprSyntax.self)?.declName.baseName.text
        ?? expression.as(FunctionCallExprSyntax.self)?.calledExpression.as(MemberAccessExprSyntax.self)?.declName.baseName.text
    }
}

private extension LabeledExprSyntax {
    var userDefaults: ExprSyntax {
        if expression.is(MemberAccessExprSyntax.self) {
            return "UserDefaults.standard"
        }
        if let memeberAceess = expression.as(FunctionCallExprSyntax.self)?.arguments.first?
            .as(LabeledExprSyntax.self)?.expression.as(MemberAccessExprSyntax.self) {
            return "UserDefaults.\(raw: memeberAceess.declName.baseName.text)"
        } else {
            return expression.as(FunctionCallExprSyntax.self)?.arguments.first?.expression ?? "UserDefaults.standard"
        }
    }
    
    var object: ExprSyntax? {
        expression.as(FunctionCallExprSyntax.self)?.arguments.first?.as(LabeledExprSyntax.self)?.expression
    }
}

private extension IdentifierTypeSyntax {
    var type: SyntaxProtocol? {
        genericArgumentClause?.arguments.first?.as(GenericArgumentSyntax.self)?.argument.as(OptionalTypeSyntax.self)?.wrappedType
        ?? genericArgumentClause?.arguments.first?.as(GenericArgumentSyntax.self)
    }
}

enum MacroDiagnostics {
    struct Message: DiagnosticMessage, Error {
        let message: String
        let diagnosticID: MessageID
        let severity: DiagnosticSeverity
    }
    
    enum ErrorMacroUsage: Error, CustomStringConvertible {
        case message(String)
        
        var description: String {
            switch self {
            case .message(let text): return text
            }
        }
    }
    
    static func diagnostic(node: Syntax,
                           position: AbsolutePosition? = nil,
                           message: Message,
                           highlights: [Syntax]? = nil,
                           notes: [Note] = [],
                           fixIts: [FixIt] = []) -> Diagnostic {
        Diagnostic(node: node, message: message)
    }
    
    static func errorMacroUsage(message: String) -> ErrorMacroUsage {
        .message(message)
    }
}

extension MacroDiagnostics.Message: FixItMessage {
    var fixItID: MessageID { diagnosticID }
}

// MARK: ATTACHED MEMBER MACROS

public struct DictionaryStorageMacro {}

extension DictionaryStorageMacro: MemberMacro {
  public static func expansion(
    of node: AttributeSyntax,
    providingMembersOf declaration: some DeclGroupSyntax,
    in context: some MacroExpansionContext
  ) throws -> [DeclSyntax] {
    return [
        "init(dictionary: [String: Any]) { self.dictionary = dictionary }",
        "var dictionary: [String: Any]",
    ]
  }
}

// MARK: ATTACHED MEMBERATRIBUTE MACROS

extension ObservableMacro: MemberAttributeMacro {
  public static func expansion<
    Declaration: DeclGroupSyntax,
    MemberDeclaration: DeclSyntaxProtocol,
    Context: MacroExpansionContext
  >(
    of node: AttributeSyntax,
    attachedTo declaration: Declaration,
    providingAttributesFor member: MemberDeclaration,
    in context: Context
  ) throws -> [AttributeSyntax] {
    guard let property = member.as(VariableDeclSyntax.self), property.isValidForObservation,
          property.identifier != nil else {
      return []
    }

    // dont apply to ignored properties or properties that are already flagged as tracked
    if property.hasMacroApplication(ObservableMacro.ignoredMacroName) ||
       property.hasMacroApplication(ObservableMacro.trackedMacroName) {
      return []
    }
    
    
    return [
      AttributeSyntax(attributeName: IdentifierTypeSyntax(name: .identifier(ObservableMacro.trackedMacroName)))
    ]
  }
}

public struct ObservableMacro {
  static let moduleName = "Observation"

  static let conformanceName = "Observable"
  static var qualifiedConformanceName: String {
    return "\(moduleName).\(conformanceName)"
  }

  static var observableConformanceType: TypeSyntax {
    "\(raw: qualifiedConformanceName)"
  }

  static let registrarTypeName = "ObservationRegistrar"
  static var qualifiedRegistrarTypeName: String {
    return "\(moduleName).\(registrarTypeName)"
  }
  
  static let trackedMacroName = "ObservationTracked"
  static let ignoredMacroName = "ObservationIgnored"

  static let registrarVariableName = "_$observationRegistrar"
  
  static func registrarVariable(_ observableType: TokenSyntax) -> DeclSyntax {
    return
      """
      @\(raw: ignoredMacroName) private let \(raw: registrarVariableName) = \(raw: qualifiedRegistrarTypeName)()
      """
  }
  
  static func accessFunction(_ observableType: TokenSyntax) -> DeclSyntax {
    return
      """
      internal nonisolated func access<Member>(
      keyPath: KeyPath<\(observableType), Member>
      ) {
      \(raw: registrarVariableName).access(self, keyPath: keyPath)
      }
      """
  }
  
  static func withMutationFunction(_ observableType: TokenSyntax) -> DeclSyntax {
    return
      """
      internal nonisolated func withMutation<Member, MutationResult>(
      keyPath: KeyPath<\(observableType), Member>,
      _ mutation: () throws -> MutationResult
      ) rethrows -> MutationResult {
      try \(raw: registrarVariableName).withMutation(of: self, keyPath: keyPath, mutation)
      }
      """
  }

  static var ignoredAttribute: AttributeSyntax {
    AttributeSyntax(
      leadingTrivia: .space,
      atSign: .atSignToken(),
      attributeName: IdentifierTypeSyntax(name: .identifier(ignoredMacroName)),
      trailingTrivia: .space
    )
  }
}



extension DeclModifierListSyntax {
  func privatePrefixed(_ prefix: String) -> DeclModifierListSyntax {
    let modifier: DeclModifierSyntax = DeclModifierSyntax(name: "private", trailingTrivia: .space)
    return [modifier] + filter {
      switch $0.name.tokenKind {
      case .keyword(let keyword):
        switch keyword {
        case .fileprivate: fallthrough
        case .private: fallthrough
        case .internal: fallthrough
        case .public:
          return false
        default:
          return true
        }
      default:
        return true
      }
    }
  }
  
  init(keyword: Keyword) {
    self.init([DeclModifierSyntax(name: .keyword(keyword))])
  }
}

extension TokenSyntax {
  func privatePrefixed(_ prefix: String) -> TokenSyntax {
    switch tokenKind {
    case .identifier(let identifier):
      return TokenSyntax(.identifier(prefix + identifier), leadingTrivia: leadingTrivia, trailingTrivia: trailingTrivia, presence: presence)
    default:
      return self
    }
  }
}

extension PatternBindingListSyntax {
  func privatePrefixed(_ prefix: String) -> PatternBindingListSyntax {
    var bindings = self.map { $0 }
    for index in 0..<bindings.count {
      let binding = bindings[index]
      if let identifier = binding.pattern.as(IdentifierPatternSyntax.self) {
        bindings[index] = PatternBindingSyntax(
          leadingTrivia: binding.leadingTrivia,
          pattern: IdentifierPatternSyntax(
            leadingTrivia: identifier.leadingTrivia,
            identifier: identifier.identifier.privatePrefixed(prefix),
            trailingTrivia: identifier.trailingTrivia
          ),
          typeAnnotation: binding.typeAnnotation,
          initializer: binding.initializer,
          accessorBlock: binding.accessorBlock,
          trailingComma: binding.trailingComma,
          trailingTrivia: binding.trailingTrivia)
        
      }
    }
    
    return PatternBindingListSyntax(bindings)
  }
}

extension VariableDeclSyntax {
  func privatePrefixed(_ prefix: String, addingAttribute attribute: AttributeSyntax) -> VariableDeclSyntax {
    let newAttributes = attributes + [.attribute(attribute)]
    return VariableDeclSyntax(
      leadingTrivia: leadingTrivia,
      attributes: newAttributes,
      modifiers: modifiers.privatePrefixed(prefix),
      bindingSpecifier: TokenSyntax(bindingSpecifier.tokenKind, leadingTrivia: .space, trailingTrivia: .space, presence: .present),
      bindings: bindings.privatePrefixed(prefix),
      trailingTrivia: trailingTrivia
    )
  }
  
  var isValidForObservation: Bool {
    !isComputed && isInstance && !isImmutable && identifier != nil
  }
}

extension VariableDeclSyntax {
  var identifierPattern: IdentifierPatternSyntax? {
    bindings.first?.pattern.as(IdentifierPatternSyntax.self)
  }
  
  var isInstance: Bool {
    for modifier in modifiers {
      for token in modifier.tokens(viewMode: .all) {
        if token.tokenKind == .keyword(.static) || token.tokenKind == .keyword(.class) {
          return false
        }
      }
    }
    return true
  }
  
  var identifier: TokenSyntax? {
    identifierPattern?.identifier
  }
  
  var type: TypeSyntax? {
    bindings.first?.typeAnnotation?.type
  }

  func accessorsMatching(_ predicate: (TokenKind) -> Bool) -> [AccessorDeclSyntax] {
    let patternBindings = bindings.compactMap { binding in
      binding.as(PatternBindingSyntax.self)
    }
    let accessors: [AccessorDeclListSyntax.Element] = patternBindings.compactMap { patternBinding in
      switch patternBinding.accessorBlock?.accessors {
      case .accessors(let accessors):
        return accessors
      default:
        return nil
      }
    }.flatMap { $0 }
    return accessors.compactMap { accessor in
      guard let decl = accessor.as(AccessorDeclSyntax.self) else {
        return nil
      }
      if predicate(decl.accessorSpecifier.tokenKind) {
        return decl
      } else {
        return nil
      }
    }
  }
  
  var willSetAccessors: [AccessorDeclSyntax] {
    accessorsMatching { $0 == .keyword(.willSet) }
  }
  var didSetAccessors: [AccessorDeclSyntax] {
    accessorsMatching { $0 == .keyword(.didSet) }
  }
  
  var isComputed: Bool {
    if accessorsMatching({ $0 == .keyword(.get) }).count > 0 {
      return true
    } else {
      return bindings.contains { binding in
        if case .getter = binding.accessorBlock?.accessors {
          return true
        } else {
          return false
        }
      }
    }
  }
  
  
  var isImmutable: Bool {
    return bindingSpecifier.tokenKind == .keyword(.let)
  }
  
  func isEquivalent(to other: VariableDeclSyntax) -> Bool {
    if isInstance != other.isInstance {
      return false
    }
    return identifier?.text == other.identifier?.text
  }
  
  var initializer: InitializerClauseSyntax? {
    bindings.first?.initializer
  }
  
  func hasMacroApplication(_ name: String) -> Bool {
    for attribute in attributes {
      switch attribute {
      case .attribute(let attr):
        if attr.attributeName.tokens(viewMode: .all).map({ $0.tokenKind }) == [.identifier(name)] {
          return true
        }
      default:
        break
      }
    }
    return false
  }
}

extension TypeSyntax {
  var identifier: String? {
    for token in tokens(viewMode: .all) {
      switch token.tokenKind {
      case .identifier(let identifier):
        return identifier
      default:
        break
      }
    }
    return nil
  }
  
  func genericSubstitution(_ parameters: GenericParameterListSyntax?) -> String? {
    var genericParameters = [String : TypeSyntax?]()
    if let parameters {
      for parameter in parameters {
        genericParameters[parameter.name.text] = parameter.inheritedType
      }
    }
    var iterator = self.asProtocol(TypeSyntaxProtocol.self).tokens(viewMode: .sourceAccurate).makeIterator()
    guard let base = iterator.next() else {
      return nil
    }
    
    if let genericBase = genericParameters[base.text] {
      if let text = genericBase?.identifier {
        return "some " + text
      } else {
        return nil
      }
    }
    var substituted = base.text
    
    while let token = iterator.next() {
      switch token.tokenKind {
      case .leftAngle:
        substituted += "<"
      case .rightAngle:
        substituted += ">"
      case .comma:
        substituted += ","
      case .identifier(let identifier):
        let type: TypeSyntax = "\(raw: identifier)"
        guard let substituedType = type.genericSubstitution(parameters) else {
          return nil
        }
        substituted += substituedType
        break
      default:
        // ignore?
        break
      }
    }
    
    return substituted
  }
}

extension FunctionDeclSyntax {
  var isInstance: Bool {
    for modifier in modifiers {
      for token in modifier.tokens(viewMode: .all) {
        if token.tokenKind == .keyword(.static) || token.tokenKind == .keyword(.class) {
          return false
        }
      }
    }
    return true
  }
  
  struct SignatureStandin: Equatable {
    var isInstance: Bool
    var identifier: String
    var parameters: [String]
    var returnType: String
  }
  
  var signatureStandin: SignatureStandin {
    var parameters = [String]()
    for parameter in signature.parameterClause.parameters {
      parameters.append(parameter.firstName.text + ":" + (parameter.type.genericSubstitution(genericParameterClause?.parameters) ?? "" ))
    }
    let returnType = signature.returnClause?.type.genericSubstitution(genericParameterClause?.parameters) ?? "Void"
    return SignatureStandin(isInstance: isInstance, identifier: name.text, parameters: parameters, returnType: returnType)
  }
  
  func isEquivalent(to other: FunctionDeclSyntax) -> Bool {
    return signatureStandin == other.signatureStandin
  }
}

extension DeclGroupSyntax {
  var memberFunctionStandins: [FunctionDeclSyntax.SignatureStandin] {
    var standins = [FunctionDeclSyntax.SignatureStandin]()
    for member in memberBlock.members {
      if let function = member.as(MemberBlockItemSyntax.self)?.decl.as(FunctionDeclSyntax.self) {
        standins.append(function.signatureStandin)
      }
    }
    return standins
  }
  
  func hasMemberFunction(equvalentTo other: FunctionDeclSyntax) -> Bool {
    for member in memberBlock.members {
      if let function = member.as(MemberBlockItemSyntax.self)?.decl.as(FunctionDeclSyntax.self) {
        if function.isEquivalent(to: other) {
          return true
        }
      }
    }
    return false
  }
  
  func hasMemberProperty(equivalentTo other: VariableDeclSyntax) -> Bool {
    for member in memberBlock.members {
      if let variable = member.as(MemberBlockItemSyntax.self)?.decl.as(VariableDeclSyntax.self) {
        if variable.isEquivalent(to: other) {
          return true
        }
      }
    }
    return false
  }
  
  var definedVariables: [VariableDeclSyntax] {
    memberBlock.members.compactMap { member in
      if let variableDecl = member.as(MemberBlockItemSyntax.self)?.decl.as(VariableDeclSyntax.self) {
        return variableDecl
      }
      return nil
    }
  }
  
  func addIfNeeded(_ decl: DeclSyntax?, to declarations: inout [DeclSyntax]) {
    guard let decl else { return }
    if let fn = decl.as(FunctionDeclSyntax.self) {
      if !hasMemberFunction(equvalentTo: fn) {
        declarations.append(decl)
      }
    } else if let property = decl.as(VariableDeclSyntax.self) {
      if !hasMemberProperty(equivalentTo: property) {
        declarations.append(decl)
      }
    }
  }
  
  var isClass: Bool {
    return self.is(ClassDeclSyntax.self)
  }
  
  var isActor: Bool {
    return self.is(ActorDeclSyntax.self)
  }
  
  var isEnum: Bool {
    return self.is(EnumDeclSyntax.self)
  }
  
  var isStruct: Bool {
    return self.is(StructDeclSyntax.self)
  }
}

// MARK: ATTACHED EXTENSION MACRO

public struct AttachedExtensionMacroExample: ExtensionMacro {
  public static func expansion(
    of node: SwiftSyntax.AttributeSyntax,
    attachedTo declaration: some SwiftSyntax.DeclGroupSyntax,
    providingExtensionsOf type: some SwiftSyntax.TypeSyntaxProtocol,
    conformingTo protocols: [SwiftSyntax.TypeSyntax],
    in context: some SwiftSyntaxMacros.MacroExpansionContext
  ) throws -> [SwiftSyntax.ExtensionDeclSyntax] {
    guard case let .argumentList(arguments) = node.arguments,
          let expression = arguments.first?.expression else {
      fatalError("compiler bug: the macro does not have any arguments")
    }
    let decl: DeclSyntax = """
      extension \(type.trimmed): Equatable {
        static let value = \(expression)
      }
      """
    guard let extensionDecl = decl.as(ExtensionDeclSyntax.self) else {
      return []
    }
    return [extensionDecl]
  }
}



// MARK: COMPILER PLUGIN

@main
struct MacroSamplePackages17Plugin: CompilerPlugin {
    let providingMacros: [Macro.Type] = [
        StringifyMacro.self,
        URLMacro.self,
        AsyncPeerMacro.self,
        AddPublisher.self,
        DictionaryStorageMacro.self
    ]
}
