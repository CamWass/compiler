/*! *****************************************************************************
Copyright (c) Microsoft Corporation. All rights reserved.
Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at http://www.apache.org/licenses/LICENSE-2.0

THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
MERCHANTABLITY OR NON-INFRINGEMENT.

See the Apache Version 2.0 License for specific language governing permissions
and limitations under the License.
***************************************************************************** */

"use strict";
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (_) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        if (typeof b !== "function" && b !== null)
            throw new TypeError("Class extends value " + String(b) + " is not a constructor or null");
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();


var ts;
(function (ts) {
    var SignatureFlags;
    (function (SignatureFlags) {
        SignatureFlags[SignatureFlags["None"] = 0] = "None";
        SignatureFlags[SignatureFlags["Yield"] = 1] = "Yield";
        SignatureFlags[SignatureFlags["Await"] = 2] = "Await";
        SignatureFlags[SignatureFlags["Type"] = 4] = "Type";
        SignatureFlags[SignatureFlags["IgnoreMissingOpenBrace"] = 16] = "IgnoreMissingOpenBrace";
        SignatureFlags[SignatureFlags["JSDoc"] = 32] = "JSDoc";
    })(SignatureFlags || (SignatureFlags = {}));
    var SpeculationKind;
    (function (SpeculationKind) {
        SpeculationKind[SpeculationKind["TryParse"] = 0] = "TryParse";
        SpeculationKind[SpeculationKind["Lookahead"] = 1] = "Lookahead";
        SpeculationKind[SpeculationKind["Reparse"] = 2] = "Reparse";
    })(SpeculationKind || (SpeculationKind = {}));
    var NodeConstructor;
    var TokenConstructor;
    var IdentifierConstructor;
    var PrivateIdentifierConstructor;
    var SourceFileConstructor;
    /**
     * NOTE: You should not use this, it is only exported to support `createNode` in `~/src/deprecatedCompat/deprecations.ts`.
     */
    /* @internal */
    ts.parseBaseNodeFactory = {
        createBaseSourceFileNode: function (kind) { return new (SourceFileConstructor || (SourceFileConstructor = ts.objectAllocator.getSourceFileConstructor()))(kind, -1, -1); },
        createBaseIdentifierNode: function (kind) { return new (IdentifierConstructor || (IdentifierConstructor = ts.objectAllocator.getIdentifierConstructor()))(kind, -1, -1); },
        createBasePrivateIdentifierNode: function (kind) { return new (PrivateIdentifierConstructor || (PrivateIdentifierConstructor = ts.objectAllocator.getPrivateIdentifierConstructor()))(kind, -1, -1); },
        createBaseTokenNode: function (kind) { return new (TokenConstructor || (TokenConstructor = ts.objectAllocator.getTokenConstructor()))(kind, -1, -1); },
        createBaseNode: function (kind) { return new (NodeConstructor || (NodeConstructor = ts.objectAllocator.getNodeConstructor()))(kind, -1, -1); },
    };
    /* @internal */
    ts.parseNodeFactory = ts.createNodeFactory(1 /* NoParenthesizerRules */, ts.parseBaseNodeFactory);
    function visitNode(cbNode, node) {
        return node && cbNode(node);
    }
    function visitNodes(cbNode, cbNodes, nodes) {
        if (nodes) {
            if (cbNodes) {
                return cbNodes(nodes);
            }
            for (var _i = 0, nodes_1 = nodes; _i < nodes_1.length; _i++) {
                var node = nodes_1[_i];
                var result = cbNode(node);
                if (result) {
                    return result;
                }
            }
        }
    }
    /*@internal*/
    function isJSDocLikeText(text, start) {
        return text.charCodeAt(start + 1) === 42 /* asterisk */ &&
            text.charCodeAt(start + 2) === 42 /* asterisk */ &&
            text.charCodeAt(start + 3) !== 47 /* slash */;
    }
    ts.isJSDocLikeText = isJSDocLikeText;
    /**
     * Invokes a callback for each child of the given node. The 'cbNode' callback is invoked for all child nodes
     * stored in properties. If a 'cbNodes' callback is specified, it is invoked for embedded arrays; otherwise,
     * embedded arrays are flattened and the 'cbNode' callback is invoked for each element. If a callback returns
     * a truthy value, iteration stops and that value is returned. Otherwise, undefined is returned.
     *
     * @param node a given node to visit its children
     * @param cbNode a callback to be invoked for all child nodes
     * @param cbNodes a callback to be invoked for embedded array
     *
     * @remarks `forEachChild` must visit the children of a node in the order
     * that they appear in the source code. The language service depends on this property to locate nodes by position.
     */
    function forEachChild(node, cbNode, cbNodes) {
        if (!node || node.kind <= 159 /* LastToken */) {
            return;
        }
        switch (node.kind) {
            case 160 /* QualifiedName */:
                return visitNode(cbNode, node.left) ||
                    visitNode(cbNode, node.right);
            case 162 /* TypeParameter */:
                return visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.constraint) ||
                    visitNode(cbNode, node.default) ||
                    visitNode(cbNode, node.expression);
            case 295 /* ShorthandPropertyAssignment */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.exclamationToken) ||
                    visitNode(cbNode, node.equalsToken) ||
                    visitNode(cbNode, node.objectAssignmentInitializer);
            case 296 /* SpreadAssignment */:
                return visitNode(cbNode, node.expression);
            case 163 /* Parameter */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.dotDotDotToken) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.type) ||
                    visitNode(cbNode, node.initializer);
            case 166 /* PropertyDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.exclamationToken) ||
                    visitNode(cbNode, node.type) ||
                    visitNode(cbNode, node.initializer);
            case 165 /* PropertySignature */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.type) ||
                    visitNode(cbNode, node.initializer);
            case 294 /* PropertyAssignment */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.initializer);
            case 253 /* VariableDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.exclamationToken) ||
                    visitNode(cbNode, node.type) ||
                    visitNode(cbNode, node.initializer);
            case 202 /* BindingElement */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.dotDotDotToken) ||
                    visitNode(cbNode, node.propertyName) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.initializer);
            case 178 /* FunctionType */:
            case 179 /* ConstructorType */:
            case 173 /* CallSignature */:
            case 174 /* ConstructSignature */:
            case 175 /* IndexSignature */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNodes(cbNode, cbNodes, node.typeParameters) ||
                    visitNodes(cbNode, cbNodes, node.parameters) ||
                    visitNode(cbNode, node.type);
            case 168 /* MethodDeclaration */:
            case 167 /* MethodSignature */:
            case 170 /* Constructor */:
            case 171 /* GetAccessor */:
            case 172 /* SetAccessor */:
            case 212 /* FunctionExpression */:
            case 255 /* FunctionDeclaration */:
            case 213 /* ArrowFunction */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.asteriskToken) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.exclamationToken) ||
                    visitNodes(cbNode, cbNodes, node.typeParameters) ||
                    visitNodes(cbNode, cbNodes, node.parameters) ||
                    visitNode(cbNode, node.type) ||
                    visitNode(cbNode, node.equalsGreaterThanToken) ||
                    visitNode(cbNode, node.body);
            case 169 /* ClassStaticBlockDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.body);
            case 177 /* TypeReference */:
                return visitNode(cbNode, node.typeName) ||
                    visitNodes(cbNode, cbNodes, node.typeArguments);
            case 176 /* TypePredicate */:
                return visitNode(cbNode, node.assertsModifier) ||
                    visitNode(cbNode, node.parameterName) ||
                    visitNode(cbNode, node.type);
            case 180 /* TypeQuery */:
                return visitNode(cbNode, node.exprName);
            case 181 /* TypeLiteral */:
                return visitNodes(cbNode, cbNodes, node.members);
            case 182 /* ArrayType */:
                return visitNode(cbNode, node.elementType);
            case 183 /* TupleType */:
                return visitNodes(cbNode, cbNodes, node.elements);
            case 186 /* UnionType */:
            case 187 /* IntersectionType */:
                return visitNodes(cbNode, cbNodes, node.types);
            case 188 /* ConditionalType */:
                return visitNode(cbNode, node.checkType) ||
                    visitNode(cbNode, node.extendsType) ||
                    visitNode(cbNode, node.trueType) ||
                    visitNode(cbNode, node.falseType);
            case 189 /* InferType */:
                return visitNode(cbNode, node.typeParameter);
            case 199 /* ImportType */:
                return visitNode(cbNode, node.argument) ||
                    visitNode(cbNode, node.qualifier) ||
                    visitNodes(cbNode, cbNodes, node.typeArguments);
            case 190 /* ParenthesizedType */:
            case 192 /* TypeOperator */:
                return visitNode(cbNode, node.type);
            case 193 /* IndexedAccessType */:
                return visitNode(cbNode, node.objectType) ||
                    visitNode(cbNode, node.indexType);
            case 194 /* MappedType */:
                return visitNode(cbNode, node.readonlyToken) ||
                    visitNode(cbNode, node.typeParameter) ||
                    visitNode(cbNode, node.nameType) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.type) ||
                    visitNodes(cbNode, cbNodes, node.members);
            case 195 /* LiteralType */:
                return visitNode(cbNode, node.literal);
            case 196 /* NamedTupleMember */:
                return visitNode(cbNode, node.dotDotDotToken) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.type);
            case 200 /* ObjectBindingPattern */:
            case 201 /* ArrayBindingPattern */:
                return visitNodes(cbNode, cbNodes, node.elements);
            case 203 /* ArrayLiteralExpression */:
                return visitNodes(cbNode, cbNodes, node.elements);
            case 204 /* ObjectLiteralExpression */:
                return visitNodes(cbNode, cbNodes, node.properties);
            case 205 /* PropertyAccessExpression */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.questionDotToken) ||
                    visitNode(cbNode, node.name);
            case 206 /* ElementAccessExpression */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.questionDotToken) ||
                    visitNode(cbNode, node.argumentExpression);
            case 207 /* CallExpression */:
            case 208 /* NewExpression */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.questionDotToken) ||
                    visitNodes(cbNode, cbNodes, node.typeArguments) ||
                    visitNodes(cbNode, cbNodes, node.arguments);
            case 209 /* TaggedTemplateExpression */:
                return visitNode(cbNode, node.tag) ||
                    visitNode(cbNode, node.questionDotToken) ||
                    visitNodes(cbNode, cbNodes, node.typeArguments) ||
                    visitNode(cbNode, node.template);
            case 210 /* TypeAssertionExpression */:
                return visitNode(cbNode, node.type) ||
                    visitNode(cbNode, node.expression);
            case 211 /* ParenthesizedExpression */:
                return visitNode(cbNode, node.expression);
            case 214 /* DeleteExpression */:
                return visitNode(cbNode, node.expression);
            case 215 /* TypeOfExpression */:
                return visitNode(cbNode, node.expression);
            case 216 /* VoidExpression */:
                return visitNode(cbNode, node.expression);
            case 218 /* PrefixUnaryExpression */:
                return visitNode(cbNode, node.operand);
            case 223 /* YieldExpression */:
                return visitNode(cbNode, node.asteriskToken) ||
                    visitNode(cbNode, node.expression);
            case 217 /* AwaitExpression */:
                return visitNode(cbNode, node.expression);
            case 219 /* PostfixUnaryExpression */:
                return visitNode(cbNode, node.operand);
            case 220 /* BinaryExpression */:
                return visitNode(cbNode, node.left) ||
                    visitNode(cbNode, node.operatorToken) ||
                    visitNode(cbNode, node.right);
            case 228 /* AsExpression */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.type);
            case 229 /* NonNullExpression */:
                return visitNode(cbNode, node.expression);
            case 230 /* MetaProperty */:
                return visitNode(cbNode, node.name);
            case 221 /* ConditionalExpression */:
                return visitNode(cbNode, node.condition) ||
                    visitNode(cbNode, node.questionToken) ||
                    visitNode(cbNode, node.whenTrue) ||
                    visitNode(cbNode, node.colonToken) ||
                    visitNode(cbNode, node.whenFalse);
            case 224 /* SpreadElement */:
                return visitNode(cbNode, node.expression);
            case 234 /* Block */:
            case 261 /* ModuleBlock */:
                return visitNodes(cbNode, cbNodes, node.statements);
            case 303 /* SourceFile */:
                return visitNodes(cbNode, cbNodes, node.statements) ||
                    visitNode(cbNode, node.endOfFileToken);
            case 236 /* VariableStatement */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.declarationList);
            case 254 /* VariableDeclarationList */:
                return visitNodes(cbNode, cbNodes, node.declarations);
            case 237 /* ExpressionStatement */:
                return visitNode(cbNode, node.expression);
            case 238 /* IfStatement */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.thenStatement) ||
                    visitNode(cbNode, node.elseStatement);
            case 239 /* DoStatement */:
                return visitNode(cbNode, node.statement) ||
                    visitNode(cbNode, node.expression);
            case 240 /* WhileStatement */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.statement);
            case 241 /* ForStatement */:
                return visitNode(cbNode, node.initializer) ||
                    visitNode(cbNode, node.condition) ||
                    visitNode(cbNode, node.incrementor) ||
                    visitNode(cbNode, node.statement);
            case 242 /* ForInStatement */:
                return visitNode(cbNode, node.initializer) ||
                    visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.statement);
            case 243 /* ForOfStatement */:
                return visitNode(cbNode, node.awaitModifier) ||
                    visitNode(cbNode, node.initializer) ||
                    visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.statement);
            case 244 /* ContinueStatement */:
            case 245 /* BreakStatement */:
                return visitNode(cbNode, node.label);
            case 246 /* ReturnStatement */:
                return visitNode(cbNode, node.expression);
            case 247 /* WithStatement */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.statement);
            case 248 /* SwitchStatement */:
                return visitNode(cbNode, node.expression) ||
                    visitNode(cbNode, node.caseBlock);
            case 262 /* CaseBlock */:
                return visitNodes(cbNode, cbNodes, node.clauses);
            case 288 /* CaseClause */:
                return visitNode(cbNode, node.expression) ||
                    visitNodes(cbNode, cbNodes, node.statements);
            case 289 /* DefaultClause */:
                return visitNodes(cbNode, cbNodes, node.statements);
            case 249 /* LabeledStatement */:
                return visitNode(cbNode, node.label) ||
                    visitNode(cbNode, node.statement);
            case 250 /* ThrowStatement */:
                return visitNode(cbNode, node.expression);
            case 251 /* TryStatement */:
                return visitNode(cbNode, node.tryBlock) ||
                    visitNode(cbNode, node.catchClause) ||
                    visitNode(cbNode, node.finallyBlock);
            case 291 /* CatchClause */:
                return visitNode(cbNode, node.variableDeclaration) ||
                    visitNode(cbNode, node.block);
            case 164 /* Decorator */:
                return visitNode(cbNode, node.expression);
            case 256 /* ClassDeclaration */:
            case 225 /* ClassExpression */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNodes(cbNode, cbNodes, node.typeParameters) ||
                    visitNodes(cbNode, cbNodes, node.heritageClauses) ||
                    visitNodes(cbNode, cbNodes, node.members);
            case 257 /* InterfaceDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNodes(cbNode, cbNodes, node.typeParameters) ||
                    visitNodes(cbNode, cbNodes, node.heritageClauses) ||
                    visitNodes(cbNode, cbNodes, node.members);
            case 258 /* TypeAliasDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNodes(cbNode, cbNodes, node.typeParameters) ||
                    visitNode(cbNode, node.type);
            case 259 /* EnumDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNodes(cbNode, cbNodes, node.members);
            case 297 /* EnumMember */:
                return visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.initializer);
            case 260 /* ModuleDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.body);
            case 264 /* ImportEqualsDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.moduleReference);
            case 265 /* ImportDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.importClause) ||
                    visitNode(cbNode, node.moduleSpecifier) ||
                    visitNode(cbNode, node.assertClause);
            case 266 /* ImportClause */:
                return visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.namedBindings);
            case 292 /* AssertClause */:
                return visitNodes(cbNode, cbNodes, node.elements);
            case 293 /* AssertEntry */:
                return visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.value);
            case 263 /* NamespaceExportDeclaration */:
                return visitNode(cbNode, node.name);
            case 267 /* NamespaceImport */:
                return visitNode(cbNode, node.name);
            case 273 /* NamespaceExport */:
                return visitNode(cbNode, node.name);
            case 268 /* NamedImports */:
            case 272 /* NamedExports */:
                return visitNodes(cbNode, cbNodes, node.elements);
            case 271 /* ExportDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.exportClause) ||
                    visitNode(cbNode, node.moduleSpecifier) ||
                    visitNode(cbNode, node.assertClause);
            case 269 /* ImportSpecifier */:
            case 274 /* ExportSpecifier */:
                return visitNode(cbNode, node.propertyName) ||
                    visitNode(cbNode, node.name);
            case 270 /* ExportAssignment */:
                return visitNodes(cbNode, cbNodes, node.decorators) ||
                    visitNodes(cbNode, cbNodes, node.modifiers) ||
                    visitNode(cbNode, node.expression);
            case 222 /* TemplateExpression */:
                return visitNode(cbNode, node.head) || visitNodes(cbNode, cbNodes, node.templateSpans);
            case 232 /* TemplateSpan */:
                return visitNode(cbNode, node.expression) || visitNode(cbNode, node.literal);
            case 197 /* TemplateLiteralType */:
                return visitNode(cbNode, node.head) || visitNodes(cbNode, cbNodes, node.templateSpans);
            case 198 /* TemplateLiteralTypeSpan */:
                return visitNode(cbNode, node.type) || visitNode(cbNode, node.literal);
            case 161 /* ComputedPropertyName */:
                return visitNode(cbNode, node.expression);
            case 290 /* HeritageClause */:
                return visitNodes(cbNode, cbNodes, node.types);
            case 227 /* ExpressionWithTypeArguments */:
                return visitNode(cbNode, node.expression) ||
                    visitNodes(cbNode, cbNodes, node.typeArguments);
            case 276 /* ExternalModuleReference */:
                return visitNode(cbNode, node.expression);
            case 275 /* MissingDeclaration */:
                return visitNodes(cbNode, cbNodes, node.decorators);
            case 349 /* CommaListExpression */:
                return visitNodes(cbNode, cbNodes, node.elements);
            case 277 /* JsxElement */:
                return visitNode(cbNode, node.openingElement) ||
                    visitNodes(cbNode, cbNodes, node.children) ||
                    visitNode(cbNode, node.closingElement);
            case 281 /* JsxFragment */:
                return visitNode(cbNode, node.openingFragment) ||
                    visitNodes(cbNode, cbNodes, node.children) ||
                    visitNode(cbNode, node.closingFragment);
            case 278 /* JsxSelfClosingElement */:
            case 279 /* JsxOpeningElement */:
                return visitNode(cbNode, node.tagName) ||
                    visitNodes(cbNode, cbNodes, node.typeArguments) ||
                    visitNode(cbNode, node.attributes);
            case 285 /* JsxAttributes */:
                return visitNodes(cbNode, cbNodes, node.properties);
            case 284 /* JsxAttribute */:
                return visitNode(cbNode, node.name) ||
                    visitNode(cbNode, node.initializer);
            case 286 /* JsxSpreadAttribute */:
                return visitNode(cbNode, node.expression);
            case 287 /* JsxExpression */:
                return visitNode(cbNode, node.dotDotDotToken) ||
                    visitNode(cbNode, node.expression);
            case 280 /* JsxClosingElement */:
                return visitNode(cbNode, node.tagName);
            case 184 /* OptionalType */:
            case 185 /* RestType */:
            case 307 /* JSDocTypeExpression */:
            case 313 /* JSDocNonNullableType */:
            case 312 /* JSDocNullableType */:
            case 314 /* JSDocOptionalType */:
            case 316 /* JSDocVariadicType */:
                return visitNode(cbNode, node.type);
            case 315 /* JSDocFunctionType */:
                return visitNodes(cbNode, cbNodes, node.parameters) ||
                    visitNode(cbNode, node.type);
            case 318 /* JSDocComment */:
                return (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment))
                    || visitNodes(cbNode, cbNodes, node.tags);
            case 344 /* JSDocSeeTag */:
                return visitNode(cbNode, node.tagName) ||
                    visitNode(cbNode, node.name) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 308 /* JSDocNameReference */:
                return visitNode(cbNode, node.name);
            case 309 /* JSDocMemberName */:
                return visitNode(cbNode, node.left) ||
                    visitNode(cbNode, node.right);
            case 338 /* JSDocParameterTag */:
            case 345 /* JSDocPropertyTag */:
                return visitNode(cbNode, node.tagName) ||
                    (node.isNameFirst
                        ? visitNode(cbNode, node.name) ||
                            visitNode(cbNode, node.typeExpression) ||
                            (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment))
                        : visitNode(cbNode, node.typeExpression) ||
                            visitNode(cbNode, node.name) ||
                            (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment)));
            case 328 /* JSDocAuthorTag */:
                return visitNode(cbNode, node.tagName) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 327 /* JSDocImplementsTag */:
                return visitNode(cbNode, node.tagName) ||
                    visitNode(cbNode, node.class) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 326 /* JSDocAugmentsTag */:
                return visitNode(cbNode, node.tagName) ||
                    visitNode(cbNode, node.class) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 342 /* JSDocTemplateTag */:
                return visitNode(cbNode, node.tagName) ||
                    visitNode(cbNode, node.constraint) ||
                    visitNodes(cbNode, cbNodes, node.typeParameters) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 343 /* JSDocTypedefTag */:
                return visitNode(cbNode, node.tagName) ||
                    (node.typeExpression &&
                        node.typeExpression.kind === 307 /* JSDocTypeExpression */
                        ? visitNode(cbNode, node.typeExpression) ||
                            visitNode(cbNode, node.fullName) ||
                            (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment))
                        : visitNode(cbNode, node.fullName) ||
                            visitNode(cbNode, node.typeExpression) ||
                            (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment)));
            case 336 /* JSDocCallbackTag */:
                return visitNode(cbNode, node.tagName) ||
                    visitNode(cbNode, node.fullName) ||
                    visitNode(cbNode, node.typeExpression) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 339 /* JSDocReturnTag */:
            case 341 /* JSDocTypeTag */:
            case 340 /* JSDocThisTag */:
            case 337 /* JSDocEnumTag */:
                return visitNode(cbNode, node.tagName) ||
                    visitNode(cbNode, node.typeExpression) ||
                    (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 321 /* JSDocSignature */:
                return ts.forEach(node.typeParameters, cbNode) ||
                    ts.forEach(node.parameters, cbNode) ||
                    visitNode(cbNode, node.type);
            case 322 /* JSDocLink */:
            case 323 /* JSDocLinkCode */:
            case 324 /* JSDocLinkPlain */:
                return visitNode(cbNode, node.name);
            case 320 /* JSDocTypeLiteral */:
                return ts.forEach(node.jsDocPropertyTags, cbNode);
            case 325 /* JSDocTag */:
            case 330 /* JSDocClassTag */:
            case 331 /* JSDocPublicTag */:
            case 332 /* JSDocPrivateTag */:
            case 333 /* JSDocProtectedTag */:
            case 334 /* JSDocReadonlyTag */:
            case 329 /* JSDocDeprecatedTag */:
                return visitNode(cbNode, node.tagName)
                    || (typeof node.comment === "string" ? undefined : visitNodes(cbNode, cbNodes, node.comment));
            case 348 /* PartiallyEmittedExpression */:
                return visitNode(cbNode, node.expression);
        }
    }
    ts.forEachChild = forEachChild;
    /** @internal */
    /**
     * Invokes a callback for each child of the given node. The 'cbNode' callback is invoked for all child nodes
     * stored in properties. If a 'cbNodes' callback is specified, it is invoked for embedded arrays; additionally,
     * unlike `forEachChild`, embedded arrays are flattened and the 'cbNode' callback is invoked for each element.
     *  If a callback returns a truthy value, iteration stops and that value is returned. Otherwise, undefined is returned.
     *
     * @param node a given node to visit its children
     * @param cbNode a callback to be invoked for all child nodes
     * @param cbNodes a callback to be invoked for embedded array
     *
     * @remarks Unlike `forEachChild`, `forEachChildRecursively` handles recursively invoking the traversal on each child node found,
     * and while doing so, handles traversing the structure without relying on the callstack to encode the tree structure.
     */
    function forEachChildRecursively(rootNode, cbNode, cbNodes) {
        var queue = gatherPossibleChildren(rootNode);
        var parents = []; // tracks parent references for elements in queue
        while (parents.length < queue.length) {
            parents.push(rootNode);
        }
        while (queue.length !== 0) {
            var current = queue.pop();
            var parent = parents.pop();
            if (ts.isArray(current)) {
                if (cbNodes) {
                    var res = cbNodes(current, parent);
                    if (res) {
                        if (res === "skip")
                            continue;
                        return res;
                    }
                }
                for (var i = current.length - 1; i >= 0; --i) {
                    queue.push(current[i]);
                    parents.push(parent);
                }
            }
            else {
                var res = cbNode(current, parent);
                if (res) {
                    if (res === "skip")
                        continue;
                    return res;
                }
                if (current.kind >= 160 /* FirstNode */) {
                    // add children in reverse order to the queue, so popping gives the first child
                    for (var _i = 0, _a = gatherPossibleChildren(current); _i < _a.length; _i++) {
                        var child = _a[_i];
                        queue.push(child);
                        parents.push(current);
                    }
                }
            }
        }
    }
    ts.forEachChildRecursively = forEachChildRecursively;
    function gatherPossibleChildren(node) {
        var children = [];
        forEachChild(node, addWorkItem, addWorkItem); // By using a stack above and `unshift` here, we emulate a depth-first preorder traversal
        return children;
        function addWorkItem(n) {
            children.unshift(n);
        }
    }
    function createSourceFile(fileName, sourceText, languageVersion, setParentNodes, scriptKind) {
        if (setParentNodes === void 0) { setParentNodes = false; }
        ts.tracing === null || ts.tracing === void 0 ? void 0 : ts.tracing.push("parse" /* Parse */, "createSourceFile", { path: fileName }, /*separateBeginAndEnd*/ true);
        ts.performance.mark("beforeParse");
        var result;
        ts.perfLogger.logStartParseSourceFile(fileName);
        if (languageVersion === 100 /* JSON */) {
            result = Parser.parseSourceFile(fileName, sourceText, languageVersion, /*syntaxCursor*/ undefined, setParentNodes, 6 /* JSON */);
        }
        else {
            result = Parser.parseSourceFile(fileName, sourceText, languageVersion, /*syntaxCursor*/ undefined, setParentNodes, scriptKind);
        }
        ts.perfLogger.logStopParseSourceFile();
        ts.performance.mark("afterParse");
        ts.performance.measure("Parse", "beforeParse", "afterParse");
        ts.tracing === null || ts.tracing === void 0 ? void 0 : ts.tracing.pop();
        return result;
    }
    ts.createSourceFile = createSourceFile;
    function parseIsolatedEntityName(text, languageVersion) {
        return Parser.parseIsolatedEntityName(text, languageVersion);
    }
    ts.parseIsolatedEntityName = parseIsolatedEntityName;
    /**
     * Parse json text into SyntaxTree and return node and parse errors if any
     * @param fileName
     * @param sourceText
     */
    function parseJsonText(fileName, sourceText) {
        return Parser.parseJsonText(fileName, sourceText);
    }
    ts.parseJsonText = parseJsonText;
    // See also `isExternalOrCommonJsModule` in utilities.ts
    function isExternalModule(file) {
        return file.externalModuleIndicator !== undefined;
    }
    ts.isExternalModule = isExternalModule;
    // Produces a new SourceFile for the 'newText' provided. The 'textChangeRange' parameter
    // indicates what changed between the 'text' that this SourceFile has and the 'newText'.
    // The SourceFile will be created with the compiler attempting to reuse as many nodes from
    // this file as possible.
    //
    // Note: this function mutates nodes from this SourceFile. That means any existing nodes
    // from this SourceFile that are being held onto may change as a result (including
    // becoming detached from any SourceFile).  It is recommended that this SourceFile not
    // be used once 'update' is called on it.
    function updateSourceFile(sourceFile, newText, textChangeRange, aggressiveChecks) {
        if (aggressiveChecks === void 0) { aggressiveChecks = false; }
        var newSourceFile = IncrementalParser.updateSourceFile(sourceFile, newText, textChangeRange, aggressiveChecks);
        // Because new source file node is created, it may not have the flag PossiblyContainDynamicImport. This is the case if there is no new edit to add dynamic import.
        // We will manually port the flag to the new source file.
        newSourceFile.flags |= (sourceFile.flags & 3145728 /* PermanentlySetIncrementalFlags */);
        return newSourceFile;
    }
    ts.updateSourceFile = updateSourceFile;
    /* @internal */
    function parseIsolatedJSDocComment(content, start, length) {
        var result = Parser.JSDocParser.parseIsolatedJSDocComment(content, start, length);
        if (result && result.jsDoc) {
            // because the jsDocComment was parsed out of the source file, it might
            // not be covered by the fixupParentReferences.
            Parser.fixupParentReferences(result.jsDoc);
        }
        return result;
    }
    ts.parseIsolatedJSDocComment = parseIsolatedJSDocComment;
    /* @internal */
    // Exposed only for testing.
    function parseJSDocTypeExpressionForTests(content, start, length) {
        return Parser.JSDocParser.parseJSDocTypeExpressionForTests(content, start, length);
    }
    ts.parseJSDocTypeExpressionForTests = parseJSDocTypeExpressionForTests;
    // Implement the parser as a singleton module.  We do this for perf reasons because creating
    // parser instances can actually be expensive enough to impact us on projects with many source
    // files.
    var Parser;
    (function (Parser) {
        // Share a single scanner across all calls to parse a source file.  This helps speed things
        // up by avoiding the cost of creating/compiling scanners over and over again.
        var scanner = ts.createScanner(99 /* Latest */, /*skipTrivia*/ true);
        var disallowInAndDecoratorContext = 4096 /* DisallowInContext */ | 16384 /* DecoratorContext */;
        // capture constructors in 'initializeState' to avoid null checks
        // tslint:disable variable-name
        var NodeConstructor;
        var TokenConstructor;
        var IdentifierConstructor;
        var PrivateIdentifierConstructor;
        var SourceFileConstructor;
        // tslint:enable variable-name
        function countNode(node) {
            nodeCount++;
            return node;
        }
        // Rather than using `createBaseNodeFactory` here, we establish a `BaseNodeFactory` that closes over the
        // constructors above, which are reset each time `initializeState` is called.
        var baseNodeFactory = {
            createBaseSourceFileNode: function (kind) { return countNode(new SourceFileConstructor(kind, /*pos*/ 0, /*end*/ 0)); },
            createBaseIdentifierNode: function (kind) { return countNode(new IdentifierConstructor(kind, /*pos*/ 0, /*end*/ 0)); },
            createBasePrivateIdentifierNode: function (kind) { return countNode(new PrivateIdentifierConstructor(kind, /*pos*/ 0, /*end*/ 0)); },
            createBaseTokenNode: function (kind) { return countNode(new TokenConstructor(kind, /*pos*/ 0, /*end*/ 0)); },
            createBaseNode: function (kind) { return countNode(new NodeConstructor(kind, /*pos*/ 0, /*end*/ 0)); }
        };
        var factory = ts.createNodeFactory(1 /* NoParenthesizerRules */ | 2 /* NoNodeConverters */ | 8 /* NoOriginalNode */, baseNodeFactory);
        var fileName;
        var sourceFlags;
        var sourceText;
        var languageVersion;
        var scriptKind;
        var languageVariant;
        var parseDiagnostics;
        var jsDocDiagnostics;
        var syntaxCursor;
        var currentToken;
        var nodeCount;
        var identifiers;
        var privateIdentifiers;
        var identifierCount;
        var parsingContext;
        var notParenthesizedArrow;
        // Flags that dictate what parsing context we're in.  For example:
        // Whether or not we are in strict parsing mode.  All that changes in strict parsing mode is
        // that some tokens that would be considered identifiers may be considered keywords.
        //
        // When adding more parser context flags, consider which is the more common case that the
        // flag will be in.  This should be the 'false' state for that flag.  The reason for this is
        // that we don't store data in our nodes unless the value is in the *non-default* state.  So,
        // for example, more often than code 'allows-in' (or doesn't 'disallow-in').  We opt for
        // 'disallow-in' set to 'false'.  Otherwise, if we had 'allowsIn' set to 'true', then almost
        // all nodes would need extra state on them to store this info.
        //
        // Note: 'allowIn' and 'allowYield' track 1:1 with the [in] and [yield] concepts in the ES6
        // grammar specification.
        //
        // An important thing about these context concepts.  By default they are effectively inherited
        // while parsing through every grammar production.  i.e. if you don't change them, then when
        // you parse a sub-production, it will have the same context values as the parent production.
        // This is great most of the time.  After all, consider all the 'expression' grammar productions
        // and how nearly all of them pass along the 'in' and 'yield' context values:
        //
        // EqualityExpression[In, Yield] :
        //      RelationalExpression[?In, ?Yield]
        //      EqualityExpression[?In, ?Yield] == RelationalExpression[?In, ?Yield]
        //      EqualityExpression[?In, ?Yield] != RelationalExpression[?In, ?Yield]
        //      EqualityExpression[?In, ?Yield] === RelationalExpression[?In, ?Yield]
        //      EqualityExpression[?In, ?Yield] !== RelationalExpression[?In, ?Yield]
        //
        // Where you have to be careful is then understanding what the points are in the grammar
        // where the values are *not* passed along.  For example:
        //
        // SingleNameBinding[Yield,GeneratorParameter]
        //      [+GeneratorParameter]BindingIdentifier[Yield] Initializer[In]opt
        //      [~GeneratorParameter]BindingIdentifier[?Yield]Initializer[In, ?Yield]opt
        //
        // Here this is saying that if the GeneratorParameter context flag is set, that we should
        // explicitly set the 'yield' context flag to false before calling into the BindingIdentifier
        // and we should explicitly unset the 'yield' context flag before calling into the Initializer.
        // production.  Conversely, if the GeneratorParameter context flag is not set, then we
        // should leave the 'yield' context flag alone.
        //
        // Getting this all correct is tricky and requires careful reading of the grammar to
        // understand when these values should be changed versus when they should be inherited.
        //
        // Note: it should not be necessary to save/restore these flags during speculative/lookahead
        // parsing.  These context flags are naturally stored and restored through normal recursive
        // descent parsing and unwinding.
        var contextFlags;
        // Indicates whether we are currently parsing top-level statements.
        var topLevel = true;
        // Whether or not we've had a parse error since creating the last AST node.  If we have
        // encountered an error, it will be stored on the next AST node we create.  Parse errors
        // can be broken down into three categories:
        //
        // 1) An error that occurred during scanning.  For example, an unterminated literal, or a
        //    character that was completely not understood.
        //
        // 2) A token was expected, but was not present.  This type of error is commonly produced
        //    by the 'parseExpected' function.
        //
        // 3) A token was present that no parsing function was able to consume.  This type of error
        //    only occurs in the 'abortParsingListOrMoveToNextToken' function when the parser
        //    decides to skip the token.
        //
        // In all of these cases, we want to mark the next node as having had an error before it.
        // With this mark, we can know in incremental settings if this node can be reused, or if
        // we have to reparse it.  If we don't keep this information around, we may just reuse the
        // node.  in that event we would then not produce the same errors as we did before, causing
        // significant confusion problems.
        //
        // Note: it is necessary that this value be saved/restored during speculative/lookahead
        // parsing.  During lookahead parsing, we will often create a node.  That node will have
        // this value attached, and then this value will be set back to 'false'.  If we decide to
        // rewind, we must get back to the same value we had prior to the lookahead.
        //
        // Note: any errors at the end of the file that do not precede a regular node, should get
        // attached to the EOF token.
        var parseErrorBeforeNextFinishedNode = false;
        function parseSourceFile(fileName, sourceText, languageVersion, syntaxCursor, setParentNodes, scriptKind) {
            var _a;
            if (setParentNodes === void 0) { setParentNodes = false; }
            scriptKind = ts.ensureScriptKind(fileName, scriptKind);
            if (scriptKind === 6 /* JSON */) {
                var result_3 = parseJsonText(fileName, sourceText, languageVersion, syntaxCursor, setParentNodes);
                ts.convertToObjectWorker(result_3, (_a = result_3.statements[0]) === null || _a === void 0 ? void 0 : _a.expression, result_3.parseDiagnostics, /*returnValue*/ false, /*knownRootOptions*/ undefined, /*jsonConversionNotifier*/ undefined);
                result_3.referencedFiles = ts.emptyArray;
                result_3.typeReferenceDirectives = ts.emptyArray;
                result_3.libReferenceDirectives = ts.emptyArray;
                result_3.amdDependencies = ts.emptyArray;
                result_3.hasNoDefaultLib = false;
                result_3.pragmas = ts.emptyMap;
                return result_3;
            }
            initializeState(fileName, sourceText, languageVersion, syntaxCursor, scriptKind);
            var result = parseSourceFileWorker(languageVersion, setParentNodes, scriptKind);
            clearState();
            return result;
        }
        Parser.parseSourceFile = parseSourceFile;
        function parseIsolatedEntityName(content, languageVersion) {
            // Choice of `isDeclarationFile` should be arbitrary
            initializeState("", content, languageVersion, /*syntaxCursor*/ undefined, 1 /* JS */);
            // Prime the scanner.
            nextToken();
            var entityName = parseEntityName(/*allowReservedWords*/ true);
            var isInvalid = token() === 1 /* EndOfFileToken */ && !parseDiagnostics.length;
            clearState();
            return isInvalid ? entityName : undefined;
        }
        Parser.parseIsolatedEntityName = parseIsolatedEntityName;
        function parseJsonText(fileName, sourceText, languageVersion, syntaxCursor, setParentNodes) {
            if (languageVersion === void 0) { languageVersion = 2 /* ES2015 */; }
            if (setParentNodes === void 0) { setParentNodes = false; }
            initializeState(fileName, sourceText, languageVersion, syntaxCursor, 6 /* JSON */);
            sourceFlags = contextFlags;
            // Prime the scanner.
            nextToken();
            var pos = getNodePos();
            var statements, endOfFileToken;
            if (token() === 1 /* EndOfFileToken */) {
                statements = createNodeArray([], pos, pos);
                endOfFileToken = parseTokenNode();
            }
            else {
                // Loop and synthesize an ArrayLiteralExpression if there are more than
                // one top-level expressions to ensure all input text is consumed.
                var expressions = void 0;
                while (token() !== 1 /* EndOfFileToken */) {
                    var expression_1 = void 0;
                    switch (token()) {
                        case 22 /* OpenBracketToken */:
                            expression_1 = parseArrayLiteralExpression();
                            break;
                        case 110 /* TrueKeyword */:
                        case 95 /* FalseKeyword */:
                        case 104 /* NullKeyword */:
                            expression_1 = parseTokenNode();
                            break;
                        case 40 /* MinusToken */:
                            if (lookAhead(function () { return nextToken() === 8 /* NumericLiteral */ && nextToken() !== 58 /* ColonToken */; })) {
                                expression_1 = parsePrefixUnaryExpression();
                            }
                            else {
                                expression_1 = parseObjectLiteralExpression();
                            }
                            break;
                        case 8 /* NumericLiteral */:
                        case 10 /* StringLiteral */:
                            if (lookAhead(function () { return nextToken() !== 58 /* ColonToken */; })) {
                                expression_1 = parseLiteralNode();
                                break;
                            }
                        // falls through
                        default:
                            expression_1 = parseObjectLiteralExpression();
                            break;
                    }
                    // Error recovery: collect multiple top-level expressions
                    if (expressions && ts.isArray(expressions)) {
                        expressions.push(expression_1);
                    }
                    else if (expressions) {
                        expressions = [expressions, expression_1];
                    }
                    else {
                        expressions = expression_1;
                        if (token() !== 1 /* EndOfFileToken */) {
                            parseErrorAtCurrentToken(ts.Diagnostics.Unexpected_token);
                        }
                    }
                }
                var expression = ts.isArray(expressions) ? finishNode(factory.createArrayLiteralExpression(expressions), pos) : ts.Debug.checkDefined(expressions);
                var statement = factory.createExpressionStatement(expression);
                finishNode(statement, pos);
                statements = createNodeArray([statement], pos);
                endOfFileToken = parseExpectedToken(1 /* EndOfFileToken */, ts.Diagnostics.Unexpected_token);
            }
            // Set source file so that errors will be reported with this file name
            var sourceFile = createSourceFile(fileName, 2 /* ES2015 */, 6 /* JSON */, /*isDeclaration*/ false, statements, endOfFileToken, sourceFlags);
            if (setParentNodes) {
                fixupParentReferences(sourceFile);
            }
            sourceFile.nodeCount = nodeCount;
            sourceFile.identifierCount = identifierCount;
            sourceFile.identifiers = identifiers;
            sourceFile.parseDiagnostics = ts.attachFileToDiagnostics(parseDiagnostics, sourceFile);
            if (jsDocDiagnostics) {
                sourceFile.jsDocDiagnostics = ts.attachFileToDiagnostics(jsDocDiagnostics, sourceFile);
            }
            var result = sourceFile;
            clearState();
            return result;
        }
        Parser.parseJsonText = parseJsonText;
        function initializeState(_fileName, _sourceText, _languageVersion, _syntaxCursor, _scriptKind) {
            NodeConstructor = ts.objectAllocator.getNodeConstructor();
            TokenConstructor = ts.objectAllocator.getTokenConstructor();
            IdentifierConstructor = ts.objectAllocator.getIdentifierConstructor();
            PrivateIdentifierConstructor = ts.objectAllocator.getPrivateIdentifierConstructor();
            SourceFileConstructor = ts.objectAllocator.getSourceFileConstructor();
            fileName = ts.normalizePath(_fileName);
            sourceText = _sourceText;
            languageVersion = _languageVersion;
            syntaxCursor = _syntaxCursor;
            scriptKind = _scriptKind;
            languageVariant = ts.getLanguageVariant(_scriptKind);
            parseDiagnostics = [];
            parsingContext = 0;
            identifiers = new ts.Map();
            privateIdentifiers = new ts.Map();
            identifierCount = 0;
            nodeCount = 0;
            sourceFlags = 0;
            topLevel = true;
            switch (scriptKind) {
                case 1 /* JS */:
                case 2 /* JSX */:
                    contextFlags = 131072 /* JavaScriptFile */;
                    break;
                case 6 /* JSON */:
                    contextFlags = 131072 /* JavaScriptFile */ | 33554432 /* JsonFile */;
                    break;
                default:
                    contextFlags = 0 /* None */;
                    break;
            }
            parseErrorBeforeNextFinishedNode = false;
            // Initialize and prime the scanner before parsing the source elements.
            scanner.setText(sourceText);
            scanner.setOnError(scanError);
            scanner.setScriptTarget(languageVersion);
            scanner.setLanguageVariant(languageVariant);
        }
        function clearState() {
            // Clear out the text the scanner is pointing at, so it doesn't keep anything alive unnecessarily.
            scanner.clearCommentDirectives();
            scanner.setText("");
            scanner.setOnError(undefined);
            // Clear any data.  We don't want to accidentally hold onto it for too long.
            sourceText = undefined;
            languageVersion = undefined;
            syntaxCursor = undefined;
            scriptKind = undefined;
            languageVariant = undefined;
            sourceFlags = 0;
            parseDiagnostics = undefined;
            jsDocDiagnostics = undefined;
            parsingContext = 0;
            identifiers = undefined;
            notParenthesizedArrow = undefined;
            topLevel = true;
        }
        function parseSourceFileWorker(languageVersion, setParentNodes, scriptKind) {
            var isDeclarationFile = isDeclarationFileName(fileName);
            if (isDeclarationFile) {
                contextFlags |= 8388608 /* Ambient */;
            }
            sourceFlags = contextFlags;
            // Prime the scanner.
            nextToken();
            var statements = parseList(0 /* SourceElements */, parseStatement);
            ts.Debug.assert(token() === 1 /* EndOfFileToken */);
            var endOfFileToken = addJSDocComment(parseTokenNode());
            var sourceFile = createSourceFile(fileName, languageVersion, scriptKind, isDeclarationFile, statements, endOfFileToken, sourceFlags);
            // A member of ReadonlyArray<T> isn't assignable to a member of T[] (and prevents a direct cast) - but this is where we set up those members so they can be readonly in the future
            processCommentPragmas(sourceFile, sourceText);
            processPragmasIntoFields(sourceFile, reportPragmaDiagnostic);
            sourceFile.commentDirectives = scanner.getCommentDirectives();
            sourceFile.nodeCount = nodeCount;
            sourceFile.identifierCount = identifierCount;
            sourceFile.identifiers = identifiers;
            sourceFile.parseDiagnostics = ts.attachFileToDiagnostics(parseDiagnostics, sourceFile);
            if (jsDocDiagnostics) {
                sourceFile.jsDocDiagnostics = ts.attachFileToDiagnostics(jsDocDiagnostics, sourceFile);
            }
            if (setParentNodes) {
                fixupParentReferences(sourceFile);
            }
            return sourceFile;
            function reportPragmaDiagnostic(pos, end, diagnostic) {
                parseDiagnostics.push(ts.createDetachedDiagnostic(fileName, pos, end, diagnostic));
            }
        }
        function withJSDoc(node, hasJSDoc) {
            return hasJSDoc ? addJSDocComment(node) : node;
        }
        var hasDeprecatedTag = false;
        function addJSDocComment(node) {
            ts.Debug.assert(!node.jsDoc); // Should only be called once per node
            var jsDoc = ts.mapDefined(ts.getJSDocCommentRanges(node, sourceText), function (comment) { return JSDocParser.parseJSDocComment(node, comment.pos, comment.end - comment.pos); });
            if (jsDoc.length)
                node.jsDoc = jsDoc;
            if (hasDeprecatedTag) {
                hasDeprecatedTag = false;
                node.flags |= 134217728 /* Deprecated */;
            }
            return node;
        }
        function reparseTopLevelAwait(sourceFile) {
            var savedSyntaxCursor = syntaxCursor;
            var baseSyntaxCursor = IncrementalParser.createSyntaxCursor(sourceFile);
            syntaxCursor = { currentNode: currentNode };
            var statements = [];
            var savedParseDiagnostics = parseDiagnostics;
            parseDiagnostics = [];
            var pos = 0;
            var start = findNextStatementWithAwait(sourceFile.statements, 0);
            var _loop_3 = function () {
                // append all statements between pos and start
                var prevStatement = sourceFile.statements[pos];
                var nextStatement = sourceFile.statements[start];
                ts.addRange(statements, sourceFile.statements, pos, start);
                pos = findNextStatementWithoutAwait(sourceFile.statements, start);
                // append all diagnostics associated with the copied range
                var diagnosticStart = ts.findIndex(savedParseDiagnostics, function (diagnostic) { return diagnostic.start >= prevStatement.pos; });
                var diagnosticEnd = diagnosticStart >= 0 ? ts.findIndex(savedParseDiagnostics, function (diagnostic) { return diagnostic.start >= nextStatement.pos; }, diagnosticStart) : -1;
                if (diagnosticStart >= 0) {
                    ts.addRange(parseDiagnostics, savedParseDiagnostics, diagnosticStart, diagnosticEnd >= 0 ? diagnosticEnd : undefined);
                }
                // reparse all statements between start and pos. We skip existing diagnostics for the same range and allow the parser to generate new ones.
                speculationHelper(function () {
                    var savedContextFlags = contextFlags;
                    contextFlags |= 32768 /* AwaitContext */;
                    scanner.setTextPos(nextStatement.pos);
                    nextToken();
                    while (token() !== 1 /* EndOfFileToken */) {
                        var startPos = scanner.getStartPos();
                        var statement = parseListElement(0 /* SourceElements */, parseStatement);
                        statements.push(statement);
                        if (startPos === scanner.getStartPos()) {
                            nextToken();
                        }
                        if (pos >= 0) {
                            var nonAwaitStatement = sourceFile.statements[pos];
                            if (statement.end === nonAwaitStatement.pos) {
                                // done reparsing this section
                                break;
                            }
                            if (statement.end > nonAwaitStatement.pos) {
                                // we ate into the next statement, so we must reparse it.
                                pos = findNextStatementWithoutAwait(sourceFile.statements, pos + 1);
                            }
                        }
                    }
                    contextFlags = savedContextFlags;
                }, 2 /* Reparse */);
                // find the next statement containing an `await`
                start = pos >= 0 ? findNextStatementWithAwait(sourceFile.statements, pos) : -1;
            };
            while (start !== -1) {
                _loop_3();
            }
            // append all statements between pos and the end of the list
            if (pos >= 0) {
                var prevStatement_1 = sourceFile.statements[pos];
                ts.addRange(statements, sourceFile.statements, pos);
                // append all diagnostics associated with the copied range
                var diagnosticStart = ts.findIndex(savedParseDiagnostics, function (diagnostic) { return diagnostic.start >= prevStatement_1.pos; });
                if (diagnosticStart >= 0) {
                    ts.addRange(parseDiagnostics, savedParseDiagnostics, diagnosticStart);
                }
            }
            syntaxCursor = savedSyntaxCursor;
            return factory.updateSourceFile(sourceFile, ts.setTextRange(factory.createNodeArray(statements), sourceFile.statements));
            function containsPossibleTopLevelAwait(node) {
                return !(node.flags & 32768 /* AwaitContext */)
                    && !!(node.transformFlags & 16777216 /* ContainsPossibleTopLevelAwait */);
            }
            function findNextStatementWithAwait(statements, start) {
                for (var i = start; i < statements.length; i++) {
                    if (containsPossibleTopLevelAwait(statements[i])) {
                        return i;
                    }
                }
                return -1;
            }
            function findNextStatementWithoutAwait(statements, start) {
                for (var i = start; i < statements.length; i++) {
                    if (!containsPossibleTopLevelAwait(statements[i])) {
                        return i;
                    }
                }
                return -1;
            }
            function currentNode(position) {
                var node = baseSyntaxCursor.currentNode(position);
                if (topLevel && node && containsPossibleTopLevelAwait(node)) {
                    node.intersectsChange = true;
                }
                return node;
            }
        }
        function fixupParentReferences(rootNode) {
            // normally parent references are set during binding. However, for clients that only need
            // a syntax tree, and no semantic features, then the binding process is an unnecessary
            // overhead.  This functions allows us to set all the parents, without all the expense of
            // binding.
            ts.setParentRecursive(rootNode, /*incremental*/ true);
        }
        Parser.fixupParentReferences = fixupParentReferences;
        function createSourceFile(fileName, languageVersion, scriptKind, isDeclarationFile, statements, endOfFileToken, flags) {
            // code from createNode is inlined here so createNode won't have to deal with special case of creating source files
            // this is quite rare comparing to other nodes and createNode should be as fast as possible
            var sourceFile = factory.createSourceFile(statements, endOfFileToken, flags);
            ts.setTextRangePosWidth(sourceFile, 0, sourceText.length);
            setExternalModuleIndicator(sourceFile);
            // If we parsed this as an external module, it may contain top-level await
            if (!isDeclarationFile && isExternalModule(sourceFile) && sourceFile.transformFlags & 16777216 /* ContainsPossibleTopLevelAwait */) {
                sourceFile = reparseTopLevelAwait(sourceFile);
            }
            sourceFile.text = sourceText;
            sourceFile.bindDiagnostics = [];
            sourceFile.bindSuggestionDiagnostics = undefined;
            sourceFile.languageVersion = languageVersion;
            sourceFile.fileName = fileName;
            sourceFile.languageVariant = ts.getLanguageVariant(scriptKind);
            sourceFile.isDeclarationFile = isDeclarationFile;
            sourceFile.scriptKind = scriptKind;
            return sourceFile;
        }
        function setContextFlag(val, flag) {
            if (val) {
                contextFlags |= flag;
            }
            else {
                contextFlags &= ~flag;
            }
        }
        function setDisallowInContext(val) {
            setContextFlag(val, 4096 /* DisallowInContext */);
        }
        function setYieldContext(val) {
            setContextFlag(val, 8192 /* YieldContext */);
        }
        function setDecoratorContext(val) {
            setContextFlag(val, 16384 /* DecoratorContext */);
        }
        function setAwaitContext(val) {
            setContextFlag(val, 32768 /* AwaitContext */);
        }
        function doOutsideOfContext(context, func) {
            // contextFlagsToClear will contain only the context flags that are
            // currently set that we need to temporarily clear
            // We don't just blindly reset to the previous flags to ensure
            // that we do not mutate cached flags for the incremental
            // parser (ThisNodeHasError, ThisNodeOrAnySubNodesHasError, and
            // HasAggregatedChildData).
            var contextFlagsToClear = context & contextFlags;
            if (contextFlagsToClear) {
                // clear the requested context flags
                setContextFlag(/*val*/ false, contextFlagsToClear);
                var result = func();
                // restore the context flags we just cleared
                setContextFlag(/*val*/ true, contextFlagsToClear);
                return result;
            }
            // no need to do anything special as we are not in any of the requested contexts
            return func();
        }
        function doInsideOfContext(context, func) {
            // contextFlagsToSet will contain only the context flags that
            // are not currently set that we need to temporarily enable.
            // We don't just blindly reset to the previous flags to ensure
            // that we do not mutate cached flags for the incremental
            // parser (ThisNodeHasError, ThisNodeOrAnySubNodesHasError, and
            // HasAggregatedChildData).
            var contextFlagsToSet = context & ~contextFlags;
            if (contextFlagsToSet) {
                // set the requested context flags
                setContextFlag(/*val*/ true, contextFlagsToSet);
                var result = func();
                // reset the context flags we just set
                setContextFlag(/*val*/ false, contextFlagsToSet);
                return result;
            }
            // no need to do anything special as we are already in all of the requested contexts
            return func();
        }
        function allowInAnd(func) {
            return doOutsideOfContext(4096 /* DisallowInContext */, func);
        }
        function disallowInAnd(func) {
            return doInsideOfContext(4096 /* DisallowInContext */, func);
        }
        function doInYieldContext(func) {
            return doInsideOfContext(8192 /* YieldContext */, func);
        }
        function doInDecoratorContext(func) {
            return doInsideOfContext(16384 /* DecoratorContext */, func);
        }
        function doInAwaitContext(func) {
            return doInsideOfContext(32768 /* AwaitContext */, func);
        }
        function doOutsideOfAwaitContext(func) {
            return doOutsideOfContext(32768 /* AwaitContext */, func);
        }
        function doInYieldAndAwaitContext(func) {
            return doInsideOfContext(8192 /* YieldContext */ | 32768 /* AwaitContext */, func);
        }
        function doOutsideOfYieldAndAwaitContext(func) {
            return doOutsideOfContext(8192 /* YieldContext */ | 32768 /* AwaitContext */, func);
        }
        function inContext(flags) {
            return (contextFlags & flags) !== 0;
        }
        function inYieldContext() {
            return inContext(8192 /* YieldContext */);
        }
        function inDisallowInContext() {
            return inContext(4096 /* DisallowInContext */);
        }
        function inDecoratorContext() {
            return inContext(16384 /* DecoratorContext */);
        }
        function inAwaitContext() {
            return inContext(32768 /* AwaitContext */);
        }
        function parseErrorAtCurrentToken(message, arg0) {
            parseErrorAt(scanner.getTokenPos(), scanner.getTextPos(), message, arg0);
        }
        function parseErrorAtPosition(start, length, message, arg0) {
            // Don't report another error if it would just be at the same position as the last error.
            var lastError = ts.lastOrUndefined(parseDiagnostics);
            if (!lastError || start !== lastError.start) {
                parseDiagnostics.push(ts.createDetachedDiagnostic(fileName, start, length, message, arg0));
            }
            // Mark that we've encountered an error.  We'll set an appropriate bit on the next
            // node we finish so that it can't be reused incrementally.
            parseErrorBeforeNextFinishedNode = true;
        }
        function parseErrorAt(start, end, message, arg0) {
            parseErrorAtPosition(start, end - start, message, arg0);
        }
        function parseErrorAtRange(range, message, arg0) {
            parseErrorAt(range.pos, range.end, message, arg0);
        }
        function scanError(message, length) {
            parseErrorAtPosition(scanner.getTextPos(), length, message);
        }
        function getNodePos() {
            return scanner.getStartPos();
        }
        function hasPrecedingJSDocComment() {
            return scanner.hasPrecedingJSDocComment();
        }
        // Use this function to access the current token instead of reading the currentToken
        // variable. Since function results aren't narrowed in control flow analysis, this ensures
        // that the type checker doesn't make wrong assumptions about the type of the current
        // token (e.g. a call to nextToken() changes the current token but the checker doesn't
        // reason about this side effect).  Mainstream VMs inline simple functions like this, so
        // there is no performance penalty.
        function token() {
            return currentToken;
        }
        function nextTokenWithoutCheck() {
            return currentToken = scanner.scan();
        }
        function nextTokenAnd(func) {
            nextToken();
            return func();
        }
        function nextToken() {
            // if the keyword had an escape
            if (ts.isKeyword(currentToken) && (scanner.hasUnicodeEscape() || scanner.hasExtendedUnicodeEscape())) {
                // issue a parse error for the escape
                parseErrorAt(scanner.getTokenPos(), scanner.getTextPos(), ts.Diagnostics.Keywords_cannot_contain_escape_characters);
            }
            return nextTokenWithoutCheck();
        }
        function nextTokenJSDoc() {
            return currentToken = scanner.scanJsDocToken();
        }
        function reScanGreaterToken() {
            return currentToken = scanner.reScanGreaterToken();
        }
        function reScanSlashToken() {
            return currentToken = scanner.reScanSlashToken();
        }
        function reScanTemplateToken(isTaggedTemplate) {
            return currentToken = scanner.reScanTemplateToken(isTaggedTemplate);
        }
        function reScanTemplateHeadOrNoSubstitutionTemplate() {
            return currentToken = scanner.reScanTemplateHeadOrNoSubstitutionTemplate();
        }
        function reScanLessThanToken() {
            return currentToken = scanner.reScanLessThanToken();
        }
        function reScanHashToken() {
            return currentToken = scanner.reScanHashToken();
        }
        function scanJsxIdentifier() {
            return currentToken = scanner.scanJsxIdentifier();
        }
        function scanJsxText() {
            return currentToken = scanner.scanJsxToken();
        }
        function scanJsxAttributeValue() {
            return currentToken = scanner.scanJsxAttributeValue();
        }
        function speculationHelper(callback, speculationKind) {
            // Keep track of the state we'll need to rollback to if lookahead fails (or if the
            // caller asked us to always reset our state).
            var saveToken = currentToken;
            var saveParseDiagnosticsLength = parseDiagnostics.length;
            var saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode;
            // Note: it is not actually necessary to save/restore the context flags here.  That's
            // because the saving/restoring of these flags happens naturally through the recursive
            // descent nature of our parser.  However, we still store this here just so we can
            // assert that invariant holds.
            var saveContextFlags = contextFlags;
            // If we're only looking ahead, then tell the scanner to only lookahead as well.
            // Otherwise, if we're actually speculatively parsing, then tell the scanner to do the
            // same.
            var result = speculationKind !== 0 /* TryParse */
                ? scanner.lookAhead(callback)
                : scanner.tryScan(callback);
            ts.Debug.assert(saveContextFlags === contextFlags);
            // If our callback returned something 'falsy' or we're just looking ahead,
            // then unconditionally restore us to where we were.
            if (!result || speculationKind !== 0 /* TryParse */) {
                currentToken = saveToken;
                if (speculationKind !== 2 /* Reparse */) {
                    parseDiagnostics.length = saveParseDiagnosticsLength;
                }
                parseErrorBeforeNextFinishedNode = saveParseErrorBeforeNextFinishedNode;
            }
            return result;
        }
        /** Invokes the provided callback then unconditionally restores the parser to the state it
         * was in immediately prior to invoking the callback.  The result of invoking the callback
         * is returned from this function.
         */
        function lookAhead(callback) {
            return speculationHelper(callback, 1 /* Lookahead */);
        }
        /** Invokes the provided callback.  If the callback returns something falsy, then it restores
         * the parser to the state it was in immediately prior to invoking the callback.  If the
         * callback returns something truthy, then the parser state is not rolled back.  The result
         * of invoking the callback is returned from this function.
         */
        function tryParse(callback) {
            return speculationHelper(callback, 0 /* TryParse */);
        }
        function isBindingIdentifier() {
            if (token() === 79 /* Identifier */) {
                return true;
            }
            // `let await`/`let yield` in [Yield] or [Await] are allowed here and disallowed in the binder.
            return token() > 116 /* LastReservedWord */;
        }
        // Ignore strict mode flag because we will report an error in type checker instead.
        function isIdentifier() {
            if (token() === 79 /* Identifier */) {
                return true;
            }
            // If we have a 'yield' keyword, and we're in the [yield] context, then 'yield' is
            // considered a keyword and is not an identifier.
            if (token() === 125 /* YieldKeyword */ && inYieldContext()) {
                return false;
            }
            // If we have a 'await' keyword, and we're in the [Await] context, then 'await' is
            // considered a keyword and is not an identifier.
            if (token() === 132 /* AwaitKeyword */ && inAwaitContext()) {
                return false;
            }
            return token() > 116 /* LastReservedWord */;
        }
        function parseExpected(kind, diagnosticMessage, shouldAdvance) {
            if (shouldAdvance === void 0) { shouldAdvance = true; }
            if (token() === kind) {
                if (shouldAdvance) {
                    nextToken();
                }
                return true;
            }
            // Report specific message if provided with one.  Otherwise, report generic fallback message.
            if (diagnosticMessage) {
                parseErrorAtCurrentToken(diagnosticMessage);
            }
            else {
                parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(kind));
            }
            return false;
        }
        var viableKeywordSuggestions = Object.keys(ts.textToKeywordObj).filter(function (keyword) { return keyword.length > 2; });
        /**
         * Provides a better error message than the generic "';' expected" if possible for
         * known common variants of a missing semicolon, such as from a mispelled names.
         *
         * @param node Node preceding the expected semicolon location.
         */
        function parseErrorForMissingSemicolonAfter(node) {
            var _a;
            // Tagged template literals are sometimes used in places where only simple strings are allowed, i.e.:
            //   module `M1` {
            //   ^^^^^^^^^^^ This block is parsed as a template literal like module`M1`.
            if (ts.isTaggedTemplateExpression(node)) {
                parseErrorAt(ts.skipTrivia(sourceText, node.template.pos), node.template.end, ts.Diagnostics.Module_declaration_names_may_only_use_or_quoted_strings);
                return;
            }
            // Otherwise, if this isn't a well-known keyword-like identifier, give the generic fallback message.
            var expressionText = ts.isIdentifier(node) ? ts.idText(node) : undefined;
            if (!expressionText || !ts.isIdentifierText(expressionText, languageVersion)) {
                parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(26 /* SemicolonToken */));
                return;
            }
            var pos = ts.skipTrivia(sourceText, node.pos);
            // Some known keywords are likely signs of syntax being used improperly.
            switch (expressionText) {
                case "const":
                case "let":
                case "var":
                    parseErrorAt(pos, node.end, ts.Diagnostics.Variable_declaration_not_allowed_at_this_location);
                    return;
                case "declare":
                    // If a declared node failed to parse, it would have emitted a diagnostic already.
                    return;
                case "interface":
                    parseErrorForInvalidName(ts.Diagnostics.Interface_name_cannot_be_0, ts.Diagnostics.Interface_must_be_given_a_name, 18 /* OpenBraceToken */);
                    return;
                case "is":
                    parseErrorAt(pos, scanner.getTextPos(), ts.Diagnostics.A_type_predicate_is_only_allowed_in_return_type_position_for_functions_and_methods);
                    return;
                case "module":
                case "namespace":
                    parseErrorForInvalidName(ts.Diagnostics.Namespace_name_cannot_be_0, ts.Diagnostics.Namespace_must_be_given_a_name, 18 /* OpenBraceToken */);
                    return;
                case "type":
                    parseErrorForInvalidName(ts.Diagnostics.Type_alias_name_cannot_be_0, ts.Diagnostics.Type_alias_must_be_given_a_name, 63 /* EqualsToken */);
                    return;
            }
            // The user alternatively might have misspelled or forgotten to add a space after a common keyword.
            var suggestion = (_a = ts.getSpellingSuggestion(expressionText, viableKeywordSuggestions, function (n) { return n; })) !== null && _a !== void 0 ? _a : getSpaceSuggestion(expressionText);
            if (suggestion) {
                parseErrorAt(pos, node.end, ts.Diagnostics.Unknown_keyword_or_identifier_Did_you_mean_0, suggestion);
                return;
            }
            // Unknown tokens are handled with their own errors in the scanner
            if (token() === 0 /* Unknown */) {
                return;
            }
            // Otherwise, we know this some kind of unknown word, not just a missing expected semicolon.
            parseErrorAt(pos, node.end, ts.Diagnostics.Unexpected_keyword_or_identifier);
        }
        /**
         * Reports a diagnostic error for the current token being an invalid name.
         *
         * @param blankDiagnostic Diagnostic to report for the case of the name being blank (matched tokenIfBlankName).
         * @param nameDiagnostic Diagnostic to report for all other cases.
         * @param tokenIfBlankName Current token if the name was invalid for being blank (not provided / skipped).
         */
        function parseErrorForInvalidName(nameDiagnostic, blankDiagnostic, tokenIfBlankName) {
            if (token() === tokenIfBlankName) {
                parseErrorAtCurrentToken(blankDiagnostic);
            }
            else {
                parseErrorAtCurrentToken(nameDiagnostic, scanner.getTokenValue());
            }
        }
        function getSpaceSuggestion(expressionText) {
            for (var _i = 0, viableKeywordSuggestions_1 = viableKeywordSuggestions; _i < viableKeywordSuggestions_1.length; _i++) {
                var keyword = viableKeywordSuggestions_1[_i];
                if (expressionText.length > keyword.length + 2 && ts.startsWith(expressionText, keyword)) {
                    return "".concat(keyword, " ").concat(expressionText.slice(keyword.length));
                }
            }
            return undefined;
        }
        function parseSemicolonAfterPropertyName(name, type, initializer) {
            if (token() === 59 /* AtToken */ && !scanner.hasPrecedingLineBreak()) {
                parseErrorAtCurrentToken(ts.Diagnostics.Decorators_must_precede_the_name_and_all_keywords_of_property_declarations);
                return;
            }
            if (token() === 20 /* OpenParenToken */) {
                parseErrorAtCurrentToken(ts.Diagnostics.Cannot_start_a_function_call_in_a_type_annotation);
                nextToken();
                return;
            }
            if (type && !canParseSemicolon()) {
                if (initializer) {
                    parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(26 /* SemicolonToken */));
                }
                else {
                    parseErrorAtCurrentToken(ts.Diagnostics.Expected_for_property_initializer);
                }
                return;
            }
            if (tryParseSemicolon()) {
                return;
            }
            if (initializer) {
                parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(26 /* SemicolonToken */));
                return;
            }
            parseErrorForMissingSemicolonAfter(name);
        }
        function parseExpectedJSDoc(kind) {
            if (token() === kind) {
                nextTokenJSDoc();
                return true;
            }
            parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(kind));
            return false;
        }
        function parseOptional(t) {
            if (token() === t) {
                nextToken();
                return true;
            }
            return false;
        }
        function parseOptionalToken(t) {
            if (token() === t) {
                return parseTokenNode();
            }
            return undefined;
        }
        function parseOptionalTokenJSDoc(t) {
            if (token() === t) {
                return parseTokenNodeJSDoc();
            }
            return undefined;
        }
        function parseExpectedToken(t, diagnosticMessage, arg0) {
            return parseOptionalToken(t) ||
                createMissingNode(t, /*reportAtCurrentPosition*/ false, diagnosticMessage || ts.Diagnostics._0_expected, arg0 || ts.tokenToString(t));
        }
        function parseExpectedTokenJSDoc(t) {
            return parseOptionalTokenJSDoc(t) ||
                createMissingNode(t, /*reportAtCurrentPosition*/ false, ts.Diagnostics._0_expected, ts.tokenToString(t));
        }
        function parseTokenNode() {
            var pos = getNodePos();
            var kind = token();
            nextToken();
            return finishNode(factory.createToken(kind), pos);
        }
        function parseTokenNodeJSDoc() {
            var pos = getNodePos();
            var kind = token();
            nextTokenJSDoc();
            return finishNode(factory.createToken(kind), pos);
        }
        function canParseSemicolon() {
            // If there's a real semicolon, then we can always parse it out.
            if (token() === 26 /* SemicolonToken */) {
                return true;
            }
            // We can parse out an optional semicolon in ASI cases in the following cases.
            return token() === 19 /* CloseBraceToken */ || token() === 1 /* EndOfFileToken */ || scanner.hasPrecedingLineBreak();
        }
        function tryParseSemicolon() {
            if (!canParseSemicolon()) {
                return false;
            }
            if (token() === 26 /* SemicolonToken */) {
                // consume the semicolon if it was explicitly provided.
                nextToken();
            }
            return true;
        }
        function parseSemicolon() {
            return tryParseSemicolon() || parseExpected(26 /* SemicolonToken */);
        }
        function createNodeArray(elements, pos, end, hasTrailingComma) {
            var array = factory.createNodeArray(elements, hasTrailingComma);
            ts.setTextRangePosEnd(array, pos, end !== null && end !== void 0 ? end : scanner.getStartPos());
            return array;
        }
        function finishNode(node, pos, end) {
            ts.setTextRangePosEnd(node, pos, end !== null && end !== void 0 ? end : scanner.getStartPos());
            if (contextFlags) {
                node.flags |= contextFlags;
            }
            // Keep track on the node if we encountered an error while parsing it.  If we did, then
            // we cannot reuse the node incrementally.  Once we've marked this node, clear out the
            // flag so that we don't mark any subsequent nodes.
            if (parseErrorBeforeNextFinishedNode) {
                parseErrorBeforeNextFinishedNode = false;
                node.flags |= 65536 /* ThisNodeHasError */;
            }
            return node;
        }
        function createMissingNode(kind, reportAtCurrentPosition, diagnosticMessage, arg0) {
            if (reportAtCurrentPosition) {
                parseErrorAtPosition(scanner.getStartPos(), 0, diagnosticMessage, arg0);
            }
            else if (diagnosticMessage) {
                parseErrorAtCurrentToken(diagnosticMessage, arg0);
            }
            var pos = getNodePos();
            var result = kind === 79 /* Identifier */ ? factory.createIdentifier("", /*typeArguments*/ undefined, /*originalKeywordKind*/ undefined) :
                ts.isTemplateLiteralKind(kind) ? factory.createTemplateLiteralLikeNode(kind, "", "", /*templateFlags*/ undefined) :
                    kind === 8 /* NumericLiteral */ ? factory.createNumericLiteral("", /*numericLiteralFlags*/ undefined) :
                        kind === 10 /* StringLiteral */ ? factory.createStringLiteral("", /*isSingleQuote*/ undefined) :
                            kind === 275 /* MissingDeclaration */ ? factory.createMissingDeclaration() :
                                factory.createToken(kind);
            return finishNode(result, pos);
        }
        function internIdentifier(text) {
            var identifier = identifiers.get(text);
            if (identifier === undefined) {
                identifiers.set(text, identifier = text);
            }
            return identifier;
        }
        // An identifier that starts with two underscores has an extra underscore character prepended to it to avoid issues
        // with magic property names like '__proto__'. The 'identifiers' object is used to share a single string instance for
        // each identifier in order to reduce memory consumption.
        function createIdentifier(isIdentifier, diagnosticMessage, privateIdentifierDiagnosticMessage) {
            if (isIdentifier) {
                identifierCount++;
                var pos = getNodePos();
                // Store original token kind if it is not just an Identifier so we can report appropriate error later in type checker
                var originalKeywordKind = token();
                var text = internIdentifier(scanner.getTokenValue());
                nextTokenWithoutCheck();
                return finishNode(factory.createIdentifier(text, /*typeArguments*/ undefined, originalKeywordKind), pos);
            }
            if (token() === 80 /* PrivateIdentifier */) {
                parseErrorAtCurrentToken(privateIdentifierDiagnosticMessage || ts.Diagnostics.Private_identifiers_are_not_allowed_outside_class_bodies);
                return createIdentifier(/*isIdentifier*/ true);
            }
            if (token() === 0 /* Unknown */ && scanner.tryScan(function () { return scanner.reScanInvalidIdentifier() === 79 /* Identifier */; })) {
                // Scanner has already recorded an 'Invalid character' error, so no need to add another from the parser.
                return createIdentifier(/*isIdentifier*/ true);
            }
            identifierCount++;
            // Only for end of file because the error gets reported incorrectly on embedded script tags.
            var reportAtCurrentPosition = token() === 1 /* EndOfFileToken */;
            var isReservedWord = scanner.isReservedWord();
            var msgArg = scanner.getTokenText();
            var defaultMessage = isReservedWord ?
                ts.Diagnostics.Identifier_expected_0_is_a_reserved_word_that_cannot_be_used_here :
                ts.Diagnostics.Identifier_expected;
            return createMissingNode(79 /* Identifier */, reportAtCurrentPosition, diagnosticMessage || defaultMessage, msgArg);
        }
        function parseBindingIdentifier(privateIdentifierDiagnosticMessage) {
            return createIdentifier(isBindingIdentifier(), /*diagnosticMessage*/ undefined, privateIdentifierDiagnosticMessage);
        }
        function parseIdentifier(diagnosticMessage, privateIdentifierDiagnosticMessage) {
            return createIdentifier(isIdentifier(), diagnosticMessage, privateIdentifierDiagnosticMessage);
        }
        function parseIdentifierName(diagnosticMessage) {
            return createIdentifier(ts.tokenIsIdentifierOrKeyword(token()), diagnosticMessage);
        }
        function isLiteralPropertyName() {
            return ts.tokenIsIdentifierOrKeyword(token()) ||
                token() === 10 /* StringLiteral */ ||
                token() === 8 /* NumericLiteral */;
        }
        function isAssertionKey() {
            return ts.tokenIsIdentifierOrKeyword(token()) ||
                token() === 10 /* StringLiteral */;
        }
        function parsePropertyNameWorker(allowComputedPropertyNames) {
            if (token() === 10 /* StringLiteral */ || token() === 8 /* NumericLiteral */) {
                var node = parseLiteralNode();
                node.text = internIdentifier(node.text);
                return node;
            }
            if (allowComputedPropertyNames && token() === 22 /* OpenBracketToken */) {
                return parseComputedPropertyName();
            }
            if (token() === 80 /* PrivateIdentifier */) {
                return parsePrivateIdentifier();
            }
            return parseIdentifierName();
        }
        function parsePropertyName() {
            return parsePropertyNameWorker(/*allowComputedPropertyNames*/ true);
        }
        function parseComputedPropertyName() {
            // PropertyName [Yield]:
            //      LiteralPropertyName
            //      ComputedPropertyName[?Yield]
            var pos = getNodePos();
            parseExpected(22 /* OpenBracketToken */);
            // We parse any expression (including a comma expression). But the grammar
            // says that only an assignment expression is allowed, so the grammar checker
            // will error if it sees a comma expression.
            var expression = allowInAnd(parseExpression);
            parseExpected(23 /* CloseBracketToken */);
            return finishNode(factory.createComputedPropertyName(expression), pos);
        }
        function internPrivateIdentifier(text) {
            var privateIdentifier = privateIdentifiers.get(text);
            if (privateIdentifier === undefined) {
                privateIdentifiers.set(text, privateIdentifier = text);
            }
            return privateIdentifier;
        }
        function parsePrivateIdentifier() {
            var pos = getNodePos();
            var node = factory.createPrivateIdentifier(internPrivateIdentifier(scanner.getTokenText()));
            nextToken();
            return finishNode(node, pos);
        }
        function parseContextualModifier(t) {
            return token() === t && tryParse(nextTokenCanFollowModifier);
        }
        function nextTokenIsOnSameLineAndCanFollowModifier() {
            nextToken();
            if (scanner.hasPrecedingLineBreak()) {
                return false;
            }
            return canFollowModifier();
        }
        function nextTokenCanFollowModifier() {
            switch (token()) {
                case 85 /* ConstKeyword */:
                    // 'const' is only a modifier if followed by 'enum'.
                    return nextToken() === 92 /* EnumKeyword */;
                case 93 /* ExportKeyword */:
                    nextToken();
                    if (token() === 88 /* DefaultKeyword */) {
                        return lookAhead(nextTokenCanFollowDefaultKeyword);
                    }
                    if (token() === 151 /* TypeKeyword */) {
                        return lookAhead(nextTokenCanFollowExportModifier);
                    }
                    return canFollowExportModifier();
                case 88 /* DefaultKeyword */:
                    return nextTokenCanFollowDefaultKeyword();
                case 124 /* StaticKeyword */:
                case 136 /* GetKeyword */:
                case 148 /* SetKeyword */:
                    nextToken();
                    return canFollowModifier();
                default:
                    return nextTokenIsOnSameLineAndCanFollowModifier();
            }
        }
        function canFollowExportModifier() {
            return token() !== 41 /* AsteriskToken */
                && token() !== 127 /* AsKeyword */
                && token() !== 18 /* OpenBraceToken */
                && canFollowModifier();
        }
        function nextTokenCanFollowExportModifier() {
            nextToken();
            return canFollowExportModifier();
        }
        function parseAnyContextualModifier() {
            return ts.isModifierKind(token()) && tryParse(nextTokenCanFollowModifier);
        }
        function canFollowModifier() {
            return token() === 22 /* OpenBracketToken */
                || token() === 18 /* OpenBraceToken */
                || token() === 41 /* AsteriskToken */
                || token() === 25 /* DotDotDotToken */
                || isLiteralPropertyName();
        }
        function nextTokenCanFollowDefaultKeyword() {
            nextToken();
            return token() === 84 /* ClassKeyword */ || token() === 98 /* FunctionKeyword */ ||
                token() === 118 /* InterfaceKeyword */ ||
                (token() === 126 /* AbstractKeyword */ && lookAhead(nextTokenIsClassKeywordOnSameLine)) ||
                (token() === 131 /* AsyncKeyword */ && lookAhead(nextTokenIsFunctionKeywordOnSameLine));
        }
        // True if positioned at the start of a list element
        function isListElement(parsingContext, inErrorRecovery) {
            var node = currentNode(parsingContext);
            if (node) {
                return true;
            }
            switch (parsingContext) {
                case 0 /* SourceElements */:
                case 1 /* BlockStatements */:
                case 3 /* SwitchClauseStatements */:
                    // If we're in error recovery, then we don't want to treat ';' as an empty statement.
                    // The problem is that ';' can show up in far too many contexts, and if we see one
                    // and assume it's a statement, then we may bail out inappropriately from whatever
                    // we're parsing.  For example, if we have a semicolon in the middle of a class, then
                    // we really don't want to assume the class is over and we're on a statement in the
                    // outer module.  We just want to consume and move on.
                    return !(token() === 26 /* SemicolonToken */ && inErrorRecovery) && isStartOfStatement();
                case 2 /* SwitchClauses */:
                    return token() === 82 /* CaseKeyword */ || token() === 88 /* DefaultKeyword */;
                case 4 /* TypeMembers */:
                    return lookAhead(isTypeMemberStart);
                case 5 /* ClassMembers */:
                    // We allow semicolons as class elements (as specified by ES6) as long as we're
                    // not in error recovery.  If we're in error recovery, we don't want an errant
                    // semicolon to be treated as a class member (since they're almost always used
                    // for statements.
                    return lookAhead(isClassMemberStart) || (token() === 26 /* SemicolonToken */ && !inErrorRecovery);
                case 6 /* EnumMembers */:
                    // Include open bracket computed properties. This technically also lets in indexers,
                    // which would be a candidate for improved error reporting.
                    return token() === 22 /* OpenBracketToken */ || isLiteralPropertyName();
                case 12 /* ObjectLiteralMembers */:
                    switch (token()) {
                        case 22 /* OpenBracketToken */:
                        case 41 /* AsteriskToken */:
                        case 25 /* DotDotDotToken */:
                        case 24 /* DotToken */: // Not an object literal member, but don't want to close the object (see `tests/cases/fourslash/completionsDotInObjectLiteral.ts`)
                            return true;
                        default:
                            return isLiteralPropertyName();
                    }
                case 18 /* RestProperties */:
                    return isLiteralPropertyName();
                case 9 /* ObjectBindingElements */:
                    return token() === 22 /* OpenBracketToken */ || token() === 25 /* DotDotDotToken */ || isLiteralPropertyName();
                case 24 /* AssertEntries */:
                    return isAssertionKey();
                case 7 /* HeritageClauseElement */:
                    // If we see `{ ... }` then only consume it as an expression if it is followed by `,` or `{`
                    // That way we won't consume the body of a class in its heritage clause.
                    if (token() === 18 /* OpenBraceToken */) {
                        return lookAhead(isValidHeritageClauseObjectLiteral);
                    }
                    if (!inErrorRecovery) {
                        return isStartOfLeftHandSideExpression() && !isHeritageClauseExtendsOrImplementsKeyword();
                    }
                    else {
                        // If we're in error recovery we tighten up what we're willing to match.
                        // That way we don't treat something like "this" as a valid heritage clause
                        // element during recovery.
                        return isIdentifier() && !isHeritageClauseExtendsOrImplementsKeyword();
                    }
                case 8 /* VariableDeclarations */:
                    return isBindingIdentifierOrPrivateIdentifierOrPattern();
                case 10 /* ArrayBindingElements */:
                    return token() === 27 /* CommaToken */ || token() === 25 /* DotDotDotToken */ || isBindingIdentifierOrPrivateIdentifierOrPattern();
                case 19 /* TypeParameters */:
                    return isIdentifier();
                case 15 /* ArrayLiteralMembers */:
                    switch (token()) {
                        case 27 /* CommaToken */:
                        case 24 /* DotToken */: // Not an array literal member, but don't want to close the array (see `tests/cases/fourslash/completionsDotInArrayLiteralInObjectLiteral.ts`)
                            return true;
                    }
                // falls through
                case 11 /* ArgumentExpressions */:
                    return token() === 25 /* DotDotDotToken */ || isStartOfExpression();
                case 16 /* Parameters */:
                    return isStartOfParameter(/*isJSDocParameter*/ false);
                case 17 /* JSDocParameters */:
                    return isStartOfParameter(/*isJSDocParameter*/ true);
                case 20 /* TypeArguments */:
                case 21 /* TupleElementTypes */:
                    return token() === 27 /* CommaToken */ || isStartOfType();
                case 22 /* HeritageClauses */:
                    return isHeritageClause();
                case 23 /* ImportOrExportSpecifiers */:
                    return ts.tokenIsIdentifierOrKeyword(token());
                case 13 /* JsxAttributes */:
                    return ts.tokenIsIdentifierOrKeyword(token()) || token() === 18 /* OpenBraceToken */;
                case 14 /* JsxChildren */:
                    return true;
            }
            return ts.Debug.fail("Non-exhaustive case in 'isListElement'.");
        }
        function isValidHeritageClauseObjectLiteral() {
            ts.Debug.assert(token() === 18 /* OpenBraceToken */);
            if (nextToken() === 19 /* CloseBraceToken */) {
                // if we see "extends {}" then only treat the {} as what we're extending (and not
                // the class body) if we have:
                //
                //      extends {} {
                //      extends {},
                //      extends {} extends
                //      extends {} implements
                var next = nextToken();
                return next === 27 /* CommaToken */ || next === 18 /* OpenBraceToken */ || next === 94 /* ExtendsKeyword */ || next === 117 /* ImplementsKeyword */;
            }
            return true;
        }
        function nextTokenIsIdentifier() {
            nextToken();
            return isIdentifier();
        }
        function nextTokenIsIdentifierOrKeyword() {
            nextToken();
            return ts.tokenIsIdentifierOrKeyword(token());
        }
        function nextTokenIsIdentifierOrKeywordOrGreaterThan() {
            nextToken();
            return ts.tokenIsIdentifierOrKeywordOrGreaterThan(token());
        }
        function isHeritageClauseExtendsOrImplementsKeyword() {
            if (token() === 117 /* ImplementsKeyword */ ||
                token() === 94 /* ExtendsKeyword */) {
                return lookAhead(nextTokenIsStartOfExpression);
            }
            return false;
        }
        function nextTokenIsStartOfExpression() {
            nextToken();
            return isStartOfExpression();
        }
        function nextTokenIsStartOfType() {
            nextToken();
            return isStartOfType();
        }
        // True if positioned at a list terminator
        function isListTerminator(kind) {
            if (token() === 1 /* EndOfFileToken */) {
                // Being at the end of the file ends all lists.
                return true;
            }
            switch (kind) {
                case 1 /* BlockStatements */:
                case 2 /* SwitchClauses */:
                case 4 /* TypeMembers */:
                case 5 /* ClassMembers */:
                case 6 /* EnumMembers */:
                case 12 /* ObjectLiteralMembers */:
                case 9 /* ObjectBindingElements */:
                case 23 /* ImportOrExportSpecifiers */:
                case 24 /* AssertEntries */:
                    return token() === 19 /* CloseBraceToken */;
                case 3 /* SwitchClauseStatements */:
                    return token() === 19 /* CloseBraceToken */ || token() === 82 /* CaseKeyword */ || token() === 88 /* DefaultKeyword */;
                case 7 /* HeritageClauseElement */:
                    return token() === 18 /* OpenBraceToken */ || token() === 94 /* ExtendsKeyword */ || token() === 117 /* ImplementsKeyword */;
                case 8 /* VariableDeclarations */:
                    return isVariableDeclaratorListTerminator();
                case 19 /* TypeParameters */:
                    // Tokens other than '>' are here for better error recovery
                    return token() === 31 /* GreaterThanToken */ || token() === 20 /* OpenParenToken */ || token() === 18 /* OpenBraceToken */ || token() === 94 /* ExtendsKeyword */ || token() === 117 /* ImplementsKeyword */;
                case 11 /* ArgumentExpressions */:
                    // Tokens other than ')' are here for better error recovery
                    return token() === 21 /* CloseParenToken */ || token() === 26 /* SemicolonToken */;
                case 15 /* ArrayLiteralMembers */:
                case 21 /* TupleElementTypes */:
                case 10 /* ArrayBindingElements */:
                    return token() === 23 /* CloseBracketToken */;
                case 17 /* JSDocParameters */:
                case 16 /* Parameters */:
                case 18 /* RestProperties */:
                    // Tokens other than ')' and ']' (the latter for index signatures) are here for better error recovery
                    return token() === 21 /* CloseParenToken */ || token() === 23 /* CloseBracketToken */ /*|| token === SyntaxKind.OpenBraceToken*/;
                case 20 /* TypeArguments */:
                    // All other tokens should cause the type-argument to terminate except comma token
                    return token() !== 27 /* CommaToken */;
                case 22 /* HeritageClauses */:
                    return token() === 18 /* OpenBraceToken */ || token() === 19 /* CloseBraceToken */;
                case 13 /* JsxAttributes */:
                    return token() === 31 /* GreaterThanToken */ || token() === 43 /* SlashToken */;
                case 14 /* JsxChildren */:
                    return token() === 29 /* LessThanToken */ && lookAhead(nextTokenIsSlash);
                default:
                    return false;
            }
        }
        function isVariableDeclaratorListTerminator() {
            // If we can consume a semicolon (either explicitly, or with ASI), then consider us done
            // with parsing the list of variable declarators.
            if (canParseSemicolon()) {
                return true;
            }
            // in the case where we're parsing the variable declarator of a 'for-in' statement, we
            // are done if we see an 'in' keyword in front of us. Same with for-of
            if (isInOrOfKeyword(token())) {
                return true;
            }
            // ERROR RECOVERY TWEAK:
            // For better error recovery, if we see an '=>' then we just stop immediately.  We've got an
            // arrow function here and it's going to be very unlikely that we'll resynchronize and get
            // another variable declaration.
            if (token() === 38 /* EqualsGreaterThanToken */) {
                return true;
            }
            // Keep trying to parse out variable declarators.
            return false;
        }
        // True if positioned at element or terminator of the current list or any enclosing list
        function isInSomeParsingContext() {
            for (var kind = 0; kind < 25 /* Count */; kind++) {
                if (parsingContext & (1 << kind)) {
                    if (isListElement(kind, /*inErrorRecovery*/ true) || isListTerminator(kind)) {
                        return true;
                    }
                }
            }
            return false;
        }
        // Parses a list of elements
        function parseList(kind, parseElement) {
            var saveParsingContext = parsingContext;
            parsingContext |= 1 << kind;
            var list = [];
            var listPos = getNodePos();
            while (!isListTerminator(kind)) {
                if (isListElement(kind, /*inErrorRecovery*/ false)) {
                    list.push(parseListElement(kind, parseElement));
                    continue;
                }
                if (abortParsingListOrMoveToNextToken(kind)) {
                    break;
                }
            }
            parsingContext = saveParsingContext;
            return createNodeArray(list, listPos);
        }
        function parseListElement(parsingContext, parseElement) {
            var node = currentNode(parsingContext);
            if (node) {
                return consumeNode(node);
            }
            return parseElement();
        }
        function currentNode(parsingContext) {
            // If we don't have a cursor or the parsing context isn't reusable, there's nothing to reuse.
            //
            // If there is an outstanding parse error that we've encountered, but not attached to
            // some node, then we cannot get a node from the old source tree.  This is because we
            // want to mark the next node we encounter as being unusable.
            //
            // Note: This may be too conservative.  Perhaps we could reuse the node and set the bit
            // on it (or its leftmost child) as having the error.  For now though, being conservative
            // is nice and likely won't ever affect perf.
            if (!syntaxCursor || !isReusableParsingContext(parsingContext) || parseErrorBeforeNextFinishedNode) {
                return undefined;
            }
            var node = syntaxCursor.currentNode(scanner.getStartPos());
            // Can't reuse a missing node.
            // Can't reuse a node that intersected the change range.
            // Can't reuse a node that contains a parse error.  This is necessary so that we
            // produce the same set of errors again.
            if (ts.nodeIsMissing(node) || node.intersectsChange || ts.containsParseError(node)) {
                return undefined;
            }
            // We can only reuse a node if it was parsed under the same strict mode that we're
            // currently in.  i.e. if we originally parsed a node in non-strict mode, but then
            // the user added 'using strict' at the top of the file, then we can't use that node
            // again as the presence of strict mode may cause us to parse the tokens in the file
            // differently.
            //
            // Note: we *can* reuse tokens when the strict mode changes.  That's because tokens
            // are unaffected by strict mode.  It's just the parser will decide what to do with it
            // differently depending on what mode it is in.
            //
            // This also applies to all our other context flags as well.
            var nodeContextFlags = node.flags & 25358336 /* ContextFlags */;
            if (nodeContextFlags !== contextFlags) {
                return undefined;
            }
            // Ok, we have a node that looks like it could be reused.  Now verify that it is valid
            // in the current list parsing context that we're currently at.
            if (!canReuseNode(node, parsingContext)) {
                return undefined;
            }
            if (node.jsDocCache) {
                // jsDocCache may include tags from parent nodes, which might have been modified.
                node.jsDocCache = undefined;
            }
            return node;
        }
        function consumeNode(node) {
            // Move the scanner so it is after the node we just consumed.
            scanner.setTextPos(node.end);
            nextToken();
            return node;
        }
        function isReusableParsingContext(parsingContext) {
            switch (parsingContext) {
                case 5 /* ClassMembers */:
                case 2 /* SwitchClauses */:
                case 0 /* SourceElements */:
                case 1 /* BlockStatements */:
                case 3 /* SwitchClauseStatements */:
                case 6 /* EnumMembers */:
                case 4 /* TypeMembers */:
                case 8 /* VariableDeclarations */:
                case 17 /* JSDocParameters */:
                case 16 /* Parameters */:
                    return true;
            }
            return false;
        }
        function canReuseNode(node, parsingContext) {
            switch (parsingContext) {
                case 5 /* ClassMembers */:
                    return isReusableClassMember(node);
                case 2 /* SwitchClauses */:
                    return isReusableSwitchClause(node);
                case 0 /* SourceElements */:
                case 1 /* BlockStatements */:
                case 3 /* SwitchClauseStatements */:
                    return isReusableStatement(node);
                case 6 /* EnumMembers */:
                    return isReusableEnumMember(node);
                case 4 /* TypeMembers */:
                    return isReusableTypeMember(node);
                case 8 /* VariableDeclarations */:
                    return isReusableVariableDeclaration(node);
                case 17 /* JSDocParameters */:
                case 16 /* Parameters */:
                    return isReusableParameter(node);
                // Any other lists we do not care about reusing nodes in.  But feel free to add if
                // you can do so safely.  Danger areas involve nodes that may involve speculative
                // parsing.  If speculative parsing is involved with the node, then the range the
                // parser reached while looking ahead might be in the edited range (see the example
                // in canReuseVariableDeclaratorNode for a good case of this).
                // case ParsingContext.HeritageClauses:
                // This would probably be safe to reuse.  There is no speculative parsing with
                // heritage clauses.
                // case ParsingContext.TypeParameters:
                // This would probably be safe to reuse.  There is no speculative parsing with
                // type parameters.  Note that that's because type *parameters* only occur in
                // unambiguous *type* contexts.  While type *arguments* occur in very ambiguous
                // *expression* contexts.
                // case ParsingContext.TupleElementTypes:
                // This would probably be safe to reuse.  There is no speculative parsing with
                // tuple types.
                // Technically, type argument list types are probably safe to reuse.  While
                // speculative parsing is involved with them (since type argument lists are only
                // produced from speculative parsing a < as a type argument list), we only have
                // the types because speculative parsing succeeded.  Thus, the lookahead never
                // went past the end of the list and rewound.
                // case ParsingContext.TypeArguments:
                // Note: these are almost certainly not safe to ever reuse.  Expressions commonly
                // need a large amount of lookahead, and we should not reuse them as they may
                // have actually intersected the edit.
                // case ParsingContext.ArgumentExpressions:
                // This is not safe to reuse for the same reason as the 'AssignmentExpression'
                // cases.  i.e. a property assignment may end with an expression, and thus might
                // have lookahead far beyond it's old node.
                // case ParsingContext.ObjectLiteralMembers:
                // This is probably not safe to reuse.  There can be speculative parsing with
                // type names in a heritage clause.  There can be generic names in the type
                // name list, and there can be left hand side expressions (which can have type
                // arguments.)
                // case ParsingContext.HeritageClauseElement:
                // Perhaps safe to reuse, but it's unlikely we'd see more than a dozen attributes
                // on any given element. Same for children.
                // case ParsingContext.JsxAttributes:
                // case ParsingContext.JsxChildren:
            }
            return false;
        }
        function isReusableClassMember(node) {
            if (node) {
                switch (node.kind) {
                    case 170 /* Constructor */:
                    case 175 /* IndexSignature */:
                    case 171 /* GetAccessor */:
                    case 172 /* SetAccessor */:
                    case 166 /* PropertyDeclaration */:
                    case 233 /* SemicolonClassElement */:
                        return true;
                    case 168 /* MethodDeclaration */:
                        // Method declarations are not necessarily reusable.  An object-literal
                        // may have a method calls "constructor(...)" and we must reparse that
                        // into an actual .ConstructorDeclaration.
                        var methodDeclaration = node;
                        var nameIsConstructor = methodDeclaration.name.kind === 79 /* Identifier */ &&
                            methodDeclaration.name.originalKeywordKind === 134 /* ConstructorKeyword */;
                        return !nameIsConstructor;
                }
            }
            return false;
        }
        function isReusableSwitchClause(node) {
            if (node) {
                switch (node.kind) {
                    case 288 /* CaseClause */:
                    case 289 /* DefaultClause */:
                        return true;
                }
            }
            return false;
        }
        function isReusableStatement(node) {
            if (node) {
                switch (node.kind) {
                    case 255 /* FunctionDeclaration */:
                    case 236 /* VariableStatement */:
                    case 234 /* Block */:
                    case 238 /* IfStatement */:
                    case 237 /* ExpressionStatement */:
                    case 250 /* ThrowStatement */:
                    case 246 /* ReturnStatement */:
                    case 248 /* SwitchStatement */:
                    case 245 /* BreakStatement */:
                    case 244 /* ContinueStatement */:
                    case 242 /* ForInStatement */:
                    case 243 /* ForOfStatement */:
                    case 241 /* ForStatement */:
                    case 240 /* WhileStatement */:
                    case 247 /* WithStatement */:
                    case 235 /* EmptyStatement */:
                    case 251 /* TryStatement */:
                    case 249 /* LabeledStatement */:
                    case 239 /* DoStatement */:
                    case 252 /* DebuggerStatement */:
                    case 265 /* ImportDeclaration */:
                    case 264 /* ImportEqualsDeclaration */:
                    case 271 /* ExportDeclaration */:
                    case 270 /* ExportAssignment */:
                    case 260 /* ModuleDeclaration */:
                    case 256 /* ClassDeclaration */:
                    case 257 /* InterfaceDeclaration */:
                    case 259 /* EnumDeclaration */:
                    case 258 /* TypeAliasDeclaration */:
                        return true;
                }
            }
            return false;
        }
        function isReusableEnumMember(node) {
            return node.kind === 297 /* EnumMember */;
        }
        function isReusableTypeMember(node) {
            if (node) {
                switch (node.kind) {
                    case 174 /* ConstructSignature */:
                    case 167 /* MethodSignature */:
                    case 175 /* IndexSignature */:
                    case 165 /* PropertySignature */:
                    case 173 /* CallSignature */:
                        return true;
                }
            }
            return false;
        }
        function isReusableVariableDeclaration(node) {
            if (node.kind !== 253 /* VariableDeclaration */) {
                return false;
            }
            // Very subtle incremental parsing bug.  Consider the following code:
            //
            //      let v = new List < A, B
            //
            // This is actually legal code.  It's a list of variable declarators "v = new List<A"
            // on one side and "B" on the other. If you then change that to:
            //
            //      let v = new List < A, B >()
            //
            // then we have a problem.  "v = new List<A" doesn't intersect the change range, so we
            // start reparsing at "B" and we completely fail to handle this properly.
            //
            // In order to prevent this, we do not allow a variable declarator to be reused if it
            // has an initializer.
            var variableDeclarator = node;
            return variableDeclarator.initializer === undefined;
        }
        function isReusableParameter(node) {
            if (node.kind !== 163 /* Parameter */) {
                return false;
            }
            // See the comment in isReusableVariableDeclaration for why we do this.
            var parameter = node;
            return parameter.initializer === undefined;
        }
        // Returns true if we should abort parsing.
        function abortParsingListOrMoveToNextToken(kind) {
            parsingContextErrors(kind);
            if (isInSomeParsingContext()) {
                return true;
            }
            nextToken();
            return false;
        }
        function parsingContextErrors(context) {
            switch (context) {
                case 0 /* SourceElements */:
                    return token() === 88 /* DefaultKeyword */
                        ? parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(93 /* ExportKeyword */))
                        : parseErrorAtCurrentToken(ts.Diagnostics.Declaration_or_statement_expected);
                case 1 /* BlockStatements */: return parseErrorAtCurrentToken(ts.Diagnostics.Declaration_or_statement_expected);
                case 2 /* SwitchClauses */: return parseErrorAtCurrentToken(ts.Diagnostics.case_or_default_expected);
                case 3 /* SwitchClauseStatements */: return parseErrorAtCurrentToken(ts.Diagnostics.Statement_expected);
                case 18 /* RestProperties */: // fallthrough
                case 4 /* TypeMembers */: return parseErrorAtCurrentToken(ts.Diagnostics.Property_or_signature_expected);
                case 5 /* ClassMembers */: return parseErrorAtCurrentToken(ts.Diagnostics.Unexpected_token_A_constructor_method_accessor_or_property_was_expected);
                case 6 /* EnumMembers */: return parseErrorAtCurrentToken(ts.Diagnostics.Enum_member_expected);
                case 7 /* HeritageClauseElement */: return parseErrorAtCurrentToken(ts.Diagnostics.Expression_expected);
                case 8 /* VariableDeclarations */:
                    return ts.isKeyword(token())
                        ? parseErrorAtCurrentToken(ts.Diagnostics._0_is_not_allowed_as_a_variable_declaration_name, ts.tokenToString(token()))
                        : parseErrorAtCurrentToken(ts.Diagnostics.Variable_declaration_expected);
                case 9 /* ObjectBindingElements */: return parseErrorAtCurrentToken(ts.Diagnostics.Property_destructuring_pattern_expected);
                case 10 /* ArrayBindingElements */: return parseErrorAtCurrentToken(ts.Diagnostics.Array_element_destructuring_pattern_expected);
                case 11 /* ArgumentExpressions */: return parseErrorAtCurrentToken(ts.Diagnostics.Argument_expression_expected);
                case 12 /* ObjectLiteralMembers */: return parseErrorAtCurrentToken(ts.Diagnostics.Property_assignment_expected);
                case 15 /* ArrayLiteralMembers */: return parseErrorAtCurrentToken(ts.Diagnostics.Expression_or_comma_expected);
                case 17 /* JSDocParameters */: return parseErrorAtCurrentToken(ts.Diagnostics.Parameter_declaration_expected);
                case 16 /* Parameters */:
                    return ts.isKeyword(token())
                        ? parseErrorAtCurrentToken(ts.Diagnostics._0_is_not_allowed_as_a_parameter_name, ts.tokenToString(token()))
                        : parseErrorAtCurrentToken(ts.Diagnostics.Parameter_declaration_expected);
                case 19 /* TypeParameters */: return parseErrorAtCurrentToken(ts.Diagnostics.Type_parameter_declaration_expected);
                case 20 /* TypeArguments */: return parseErrorAtCurrentToken(ts.Diagnostics.Type_argument_expected);
                case 21 /* TupleElementTypes */: return parseErrorAtCurrentToken(ts.Diagnostics.Type_expected);
                case 22 /* HeritageClauses */: return parseErrorAtCurrentToken(ts.Diagnostics.Unexpected_token_expected);
                case 23 /* ImportOrExportSpecifiers */: return parseErrorAtCurrentToken(ts.Diagnostics.Identifier_expected);
                case 13 /* JsxAttributes */: return parseErrorAtCurrentToken(ts.Diagnostics.Identifier_expected);
                case 14 /* JsxChildren */: return parseErrorAtCurrentToken(ts.Diagnostics.Identifier_expected);
                default: return [undefined]; // TODO: GH#18217 `default: Debug.assertNever(context);`
            }
        }
        // Parses a comma-delimited list of elements
        function parseDelimitedList(kind, parseElement, considerSemicolonAsDelimiter) {
            var saveParsingContext = parsingContext;
            parsingContext |= 1 << kind;
            var list = [];
            var listPos = getNodePos();
            var commaStart = -1; // Meaning the previous token was not a comma
            while (true) {
                if (isListElement(kind, /*inErrorRecovery*/ false)) {
                    var startPos = scanner.getStartPos();
                    list.push(parseListElement(kind, parseElement));
                    commaStart = scanner.getTokenPos();
                    if (parseOptional(27 /* CommaToken */)) {
                        // No need to check for a zero length node since we know we parsed a comma
                        continue;
                    }
                    commaStart = -1; // Back to the state where the last token was not a comma
                    if (isListTerminator(kind)) {
                        break;
                    }
                    // We didn't get a comma, and the list wasn't terminated, explicitly parse
                    // out a comma so we give a good error message.
                    parseExpected(27 /* CommaToken */, getExpectedCommaDiagnostic(kind));
                    // If the token was a semicolon, and the caller allows that, then skip it and
                    // continue.  This ensures we get back on track and don't result in tons of
                    // parse errors.  For example, this can happen when people do things like use
                    // a semicolon to delimit object literal members.   Note: we'll have already
                    // reported an error when we called parseExpected above.
                    if (considerSemicolonAsDelimiter && token() === 26 /* SemicolonToken */ && !scanner.hasPrecedingLineBreak()) {
                        nextToken();
                    }
                    if (startPos === scanner.getStartPos()) {
                        // What we're parsing isn't actually remotely recognizable as a element and we've consumed no tokens whatsoever
                        // Consume a token to advance the parser in some way and avoid an infinite loop
                        // This can happen when we're speculatively parsing parenthesized expressions which we think may be arrow functions,
                        // or when a modifier keyword which is disallowed as a parameter name (ie, `static` in strict mode) is supplied
                        nextToken();
                    }
                    continue;
                }
                if (isListTerminator(kind)) {
                    break;
                }
                if (abortParsingListOrMoveToNextToken(kind)) {
                    break;
                }
            }
            parsingContext = saveParsingContext;
            // Recording the trailing comma is deliberately done after the previous
            // loop, and not just if we see a list terminator. This is because the list
            // may have ended incorrectly, but it is still important to know if there
            // was a trailing comma.
            // Check if the last token was a comma.
            // Always preserve a trailing comma by marking it on the NodeArray
            return createNodeArray(list, listPos, /*end*/ undefined, commaStart >= 0);
        }
        function getExpectedCommaDiagnostic(kind) {
            return kind === 6 /* EnumMembers */ ? ts.Diagnostics.An_enum_member_name_must_be_followed_by_a_or : undefined;
        }
        function createMissingList() {
            var list = createNodeArray([], getNodePos());
            list.isMissingList = true;
            return list;
        }
        function isMissingList(arr) {
            return !!arr.isMissingList;
        }
        function parseBracketedList(kind, parseElement, open, close) {
            if (parseExpected(open)) {
                var result = parseDelimitedList(kind, parseElement);
                parseExpected(close);
                return result;
            }
            return createMissingList();
        }
        function parseEntityName(allowReservedWords, diagnosticMessage) {
            var pos = getNodePos();
            var entity = allowReservedWords ? parseIdentifierName(diagnosticMessage) : parseIdentifier(diagnosticMessage);
            var dotPos = getNodePos();
            while (parseOptional(24 /* DotToken */)) {
                if (token() === 29 /* LessThanToken */) {
                    // the entity is part of a JSDoc-style generic, so record the trailing dot for later error reporting
                    entity.jsdocDotPos = dotPos;
                    break;
                }
                dotPos = getNodePos();
                entity = finishNode(factory.createQualifiedName(entity, parseRightSideOfDot(allowReservedWords, /* allowPrivateIdentifiers */ false)), pos);
            }
            return entity;
        }
        function createQualifiedName(entity, name) {
            return finishNode(factory.createQualifiedName(entity, name), entity.pos);
        }
        function parseRightSideOfDot(allowIdentifierNames, allowPrivateIdentifiers) {
            // Technically a keyword is valid here as all identifiers and keywords are identifier names.
            // However, often we'll encounter this in error situations when the identifier or keyword
            // is actually starting another valid construct.
            //
            // So, we check for the following specific case:
            //
            //      name.
            //      identifierOrKeyword identifierNameOrKeyword
            //
            // Note: the newlines are important here.  For example, if that above code
            // were rewritten into:
            //
            //      name.identifierOrKeyword
            //      identifierNameOrKeyword
            //
            // Then we would consider it valid.  That's because ASI would take effect and
            // the code would be implicitly: "name.identifierOrKeyword; identifierNameOrKeyword".
            // In the first case though, ASI will not take effect because there is not a
            // line terminator after the identifier or keyword.
            if (scanner.hasPrecedingLineBreak() && ts.tokenIsIdentifierOrKeyword(token())) {
                var matchesPattern = lookAhead(nextTokenIsIdentifierOrKeywordOnSameLine);
                if (matchesPattern) {
                    // Report that we need an identifier.  However, report it right after the dot,
                    // and not on the next token.  This is because the next token might actually
                    // be an identifier and the error would be quite confusing.
                    return createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ true, ts.Diagnostics.Identifier_expected);
                }
            }
            if (token() === 80 /* PrivateIdentifier */) {
                var node = parsePrivateIdentifier();
                return allowPrivateIdentifiers ? node : createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ true, ts.Diagnostics.Identifier_expected);
            }
            return allowIdentifierNames ? parseIdentifierName() : parseIdentifier();
        }
        function parseTemplateSpans(isTaggedTemplate) {
            var pos = getNodePos();
            var list = [];
            var node;
            do {
                node = parseTemplateSpan(isTaggedTemplate);
                list.push(node);
            } while (node.literal.kind === 16 /* TemplateMiddle */);
            return createNodeArray(list, pos);
        }
        function parseTemplateExpression(isTaggedTemplate) {
            var pos = getNodePos();
            return finishNode(factory.createTemplateExpression(parseTemplateHead(isTaggedTemplate), parseTemplateSpans(isTaggedTemplate)), pos);
        }
        function parseTemplateType() {
            var pos = getNodePos();
            return finishNode(factory.createTemplateLiteralType(parseTemplateHead(/*isTaggedTemplate*/ false), parseTemplateTypeSpans()), pos);
        }
        function parseTemplateTypeSpans() {
            var pos = getNodePos();
            var list = [];
            var node;
            do {
                node = parseTemplateTypeSpan();
                list.push(node);
            } while (node.literal.kind === 16 /* TemplateMiddle */);
            return createNodeArray(list, pos);
        }
        function parseTemplateTypeSpan() {
            var pos = getNodePos();
            return finishNode(factory.createTemplateLiteralTypeSpan(parseType(), parseLiteralOfTemplateSpan(/*isTaggedTemplate*/ false)), pos);
        }
        function parseLiteralOfTemplateSpan(isTaggedTemplate) {
            if (token() === 19 /* CloseBraceToken */) {
                reScanTemplateToken(isTaggedTemplate);
                return parseTemplateMiddleOrTemplateTail();
            }
            else {
                // TODO(rbuckton): Do we need to call `parseExpectedToken` or can we just call `createMissingNode` directly?
                return parseExpectedToken(17 /* TemplateTail */, ts.Diagnostics._0_expected, ts.tokenToString(19 /* CloseBraceToken */));
            }
        }
        function parseTemplateSpan(isTaggedTemplate) {
            var pos = getNodePos();
            return finishNode(factory.createTemplateSpan(allowInAnd(parseExpression), parseLiteralOfTemplateSpan(isTaggedTemplate)), pos);
        }
        function parseLiteralNode() {
            return parseLiteralLikeNode(token());
        }
        function parseTemplateHead(isTaggedTemplate) {
            if (isTaggedTemplate) {
                reScanTemplateHeadOrNoSubstitutionTemplate();
            }
            var fragment = parseLiteralLikeNode(token());
            ts.Debug.assert(fragment.kind === 15 /* TemplateHead */, "Template head has wrong token kind");
            return fragment;
        }
        function parseTemplateMiddleOrTemplateTail() {
            var fragment = parseLiteralLikeNode(token());
            ts.Debug.assert(fragment.kind === 16 /* TemplateMiddle */ || fragment.kind === 17 /* TemplateTail */, "Template fragment has wrong token kind");
            return fragment;
        }
        function getTemplateLiteralRawText(kind) {
            var isLast = kind === 14 /* NoSubstitutionTemplateLiteral */ || kind === 17 /* TemplateTail */;
            var tokenText = scanner.getTokenText();
            return tokenText.substring(1, tokenText.length - (scanner.isUnterminated() ? 0 : isLast ? 1 : 2));
        }
        function parseLiteralLikeNode(kind) {
            var pos = getNodePos();
            var node = ts.isTemplateLiteralKind(kind) ? factory.createTemplateLiteralLikeNode(kind, scanner.getTokenValue(), getTemplateLiteralRawText(kind), scanner.getTokenFlags() & 2048 /* TemplateLiteralLikeFlags */) :
                // Octal literals are not allowed in strict mode or ES5
                // Note that theoretically the following condition would hold true literals like 009,
                // which is not octal. But because of how the scanner separates the tokens, we would
                // never get a token like this. Instead, we would get 00 and 9 as two separate tokens.
                // We also do not need to check for negatives because any prefix operator would be part of a
                // parent unary expression.
                kind === 8 /* NumericLiteral */ ? factory.createNumericLiteral(scanner.getTokenValue(), scanner.getNumericLiteralFlags()) :
                    kind === 10 /* StringLiteral */ ? factory.createStringLiteral(scanner.getTokenValue(), /*isSingleQuote*/ undefined, scanner.hasExtendedUnicodeEscape()) :
                        ts.isLiteralKind(kind) ? factory.createLiteralLikeNode(kind, scanner.getTokenValue()) :
                            ts.Debug.fail();
            if (scanner.hasExtendedUnicodeEscape()) {
                node.hasExtendedUnicodeEscape = true;
            }
            if (scanner.isUnterminated()) {
                node.isUnterminated = true;
            }
            nextToken();
            return finishNode(node, pos);
        }
        // TYPES
        function parseEntityNameOfTypeReference() {
            return parseEntityName(/*allowReservedWords*/ true, ts.Diagnostics.Type_expected);
        }
        function parseTypeArgumentsOfTypeReference() {
            if (!scanner.hasPrecedingLineBreak() && reScanLessThanToken() === 29 /* LessThanToken */) {
                return parseBracketedList(20 /* TypeArguments */, parseType, 29 /* LessThanToken */, 31 /* GreaterThanToken */);
            }
        }
        function parseTypeReference() {
            var pos = getNodePos();
            return finishNode(factory.createTypeReferenceNode(parseEntityNameOfTypeReference(), parseTypeArgumentsOfTypeReference()), pos);
        }
        // If true, we should abort parsing an error function.
        function typeHasArrowFunctionBlockingParseError(node) {
            switch (node.kind) {
                case 177 /* TypeReference */:
                    return ts.nodeIsMissing(node.typeName);
                case 178 /* FunctionType */:
                case 179 /* ConstructorType */: {
                    var _a = node, parameters = _a.parameters, type = _a.type;
                    return isMissingList(parameters) || typeHasArrowFunctionBlockingParseError(type);
                }
                case 190 /* ParenthesizedType */:
                    return typeHasArrowFunctionBlockingParseError(node.type);
                default:
                    return false;
            }
        }
        function parseThisTypePredicate(lhs) {
            nextToken();
            return finishNode(factory.createTypePredicateNode(/*assertsModifier*/ undefined, lhs, parseType()), lhs.pos);
        }
        function parseThisTypeNode() {
            var pos = getNodePos();
            nextToken();
            return finishNode(factory.createThisTypeNode(), pos);
        }
        function parseJSDocAllType() {
            var pos = getNodePos();
            nextToken();
            return finishNode(factory.createJSDocAllType(), pos);
        }
        function parseJSDocNonNullableType() {
            var pos = getNodePos();
            nextToken();
            return finishNode(factory.createJSDocNonNullableType(parseNonArrayType()), pos);
        }
        function parseJSDocUnknownOrNullableType() {
            var pos = getNodePos();
            // skip the ?
            nextToken();
            // Need to lookahead to decide if this is a nullable or unknown type.
            // Here are cases where we'll pick the unknown type:
            //
            //      Foo(?,
            //      { a: ? }
            //      Foo(?)
            //      Foo<?>
            //      Foo(?=
            //      (?|
            if (token() === 27 /* CommaToken */ ||
                token() === 19 /* CloseBraceToken */ ||
                token() === 21 /* CloseParenToken */ ||
                token() === 31 /* GreaterThanToken */ ||
                token() === 63 /* EqualsToken */ ||
                token() === 51 /* BarToken */) {
                return finishNode(factory.createJSDocUnknownType(), pos);
            }
            else {
                return finishNode(factory.createJSDocNullableType(parseType()), pos);
            }
        }
        function parseJSDocFunctionType() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            if (lookAhead(nextTokenIsOpenParen)) {
                nextToken();
                var parameters = parseParameters(4 /* Type */ | 32 /* JSDoc */);
                var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
                return withJSDoc(finishNode(factory.createJSDocFunctionType(parameters, type), pos), hasJSDoc);
            }
            return finishNode(factory.createTypeReferenceNode(parseIdentifierName(), /*typeArguments*/ undefined), pos);
        }
        function parseJSDocParameter() {
            var pos = getNodePos();
            var name;
            if (token() === 108 /* ThisKeyword */ || token() === 103 /* NewKeyword */) {
                name = parseIdentifierName();
                parseExpected(58 /* ColonToken */);
            }
            return finishNode(factory.createParameterDeclaration(
            /*decorators*/ undefined, 
            /*modifiers*/ undefined, 
            /*dotDotDotToken*/ undefined, 
            // TODO(rbuckton): JSDoc parameters don't have names (except `this`/`new`), should we manufacture an empty identifier?
            name, 
            /*questionToken*/ undefined, parseJSDocType(), 
            /*initializer*/ undefined), pos);
        }
        function parseJSDocType() {
            scanner.setInJSDocType(true);
            var pos = getNodePos();
            if (parseOptional(141 /* ModuleKeyword */)) {
                // TODO(rbuckton): We never set the type for a JSDocNamepathType. What should we put here?
                var moduleTag = factory.createJSDocNamepathType(/*type*/ undefined);
                terminate: while (true) {
                    switch (token()) {
                        case 19 /* CloseBraceToken */:
                        case 1 /* EndOfFileToken */:
                        case 27 /* CommaToken */:
                        case 5 /* WhitespaceTrivia */:
                            break terminate;
                        default:
                            nextTokenJSDoc();
                    }
                }
                scanner.setInJSDocType(false);
                return finishNode(moduleTag, pos);
            }
            var hasDotDotDot = parseOptional(25 /* DotDotDotToken */);
            var type = parseTypeOrTypePredicate();
            scanner.setInJSDocType(false);
            if (hasDotDotDot) {
                type = finishNode(factory.createJSDocVariadicType(type), pos);
            }
            if (token() === 63 /* EqualsToken */) {
                nextToken();
                return finishNode(factory.createJSDocOptionalType(type), pos);
            }
            return type;
        }
        function parseTypeQuery() {
            var pos = getNodePos();
            parseExpected(112 /* TypeOfKeyword */);
            return finishNode(factory.createTypeQueryNode(parseEntityName(/*allowReservedWords*/ true)), pos);
        }
        function parseTypeParameter() {
            var pos = getNodePos();
            var name = parseIdentifier();
            var constraint;
            var expression;
            if (parseOptional(94 /* ExtendsKeyword */)) {
                // It's not uncommon for people to write improper constraints to a generic.  If the
                // user writes a constraint that is an expression and not an actual type, then parse
                // it out as an expression (so we can recover well), but report that a type is needed
                // instead.
                if (isStartOfType() || !isStartOfExpression()) {
                    constraint = parseType();
                }
                else {
                    // It was not a type, and it looked like an expression.  Parse out an expression
                    // here so we recover well.  Note: it is important that we call parseUnaryExpression
                    // and not parseExpression here.  If the user has:
                    //
                    //      <T extends "">
                    //
                    // We do *not* want to consume the `>` as we're consuming the expression for "".
                    expression = parseUnaryExpressionOrHigher();
                }
            }
            var defaultType = parseOptional(63 /* EqualsToken */) ? parseType() : undefined;
            var node = factory.createTypeParameterDeclaration(name, constraint, defaultType);
            node.expression = expression;
            return finishNode(node, pos);
        }
        function parseTypeParameters() {
            if (token() === 29 /* LessThanToken */) {
                return parseBracketedList(19 /* TypeParameters */, parseTypeParameter, 29 /* LessThanToken */, 31 /* GreaterThanToken */);
            }
        }
        function isStartOfParameter(isJSDocParameter) {
            return token() === 25 /* DotDotDotToken */ ||
                isBindingIdentifierOrPrivateIdentifierOrPattern() ||
                ts.isModifierKind(token()) ||
                token() === 59 /* AtToken */ ||
                isStartOfType(/*inStartOfParameter*/ !isJSDocParameter);
        }
        function parseNameOfParameter(modifiers) {
            // FormalParameter [Yield,Await]:
            //      BindingElement[?Yield,?Await]
            var name = parseIdentifierOrPattern(ts.Diagnostics.Private_identifiers_cannot_be_used_as_parameters);
            if (ts.getFullWidth(name) === 0 && !ts.some(modifiers) && ts.isModifierKind(token())) {
                // in cases like
                // 'use strict'
                // function foo(static)
                // isParameter('static') === true, because of isModifier('static')
                // however 'static' is not a legal identifier in a strict mode.
                // so result of this function will be ParameterDeclaration (flags = 0, name = missing, type = undefined, initializer = undefined)
                // and current token will not change => parsing of the enclosing parameter list will last till the end of time (or OOM)
                // to avoid this we'll advance cursor to the next token.
                nextToken();
            }
            return name;
        }
        function parseParameterInOuterAwaitContext() {
            return parseParameterWorker(/*inOuterAwaitContext*/ true);
        }
        function parseParameter() {
            return parseParameterWorker(/*inOuterAwaitContext*/ false);
        }
        function parseParameterWorker(inOuterAwaitContext) {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            // FormalParameter [Yield,Await]:
            //      BindingElement[?Yield,?Await]
            // Decorators are parsed in the outer [Await] context, the rest of the parameter is parsed in the function's [Await] context.
            var decorators = inOuterAwaitContext ? doInAwaitContext(parseDecorators) : parseDecorators();
            if (token() === 108 /* ThisKeyword */) {
                var node_1 = factory.createParameterDeclaration(decorators, 
                /*modifiers*/ undefined, 
                /*dotDotDotToken*/ undefined, createIdentifier(/*isIdentifier*/ true), 
                /*questionToken*/ undefined, parseTypeAnnotation(), 
                /*initializer*/ undefined);
                if (decorators) {
                    parseErrorAtRange(decorators[0], ts.Diagnostics.Decorators_may_not_be_applied_to_this_parameters);
                }
                return withJSDoc(finishNode(node_1, pos), hasJSDoc);
            }
            var savedTopLevel = topLevel;
            topLevel = false;
            var modifiers = parseModifiers();
            var node = withJSDoc(finishNode(factory.createParameterDeclaration(decorators, modifiers, parseOptionalToken(25 /* DotDotDotToken */), parseNameOfParameter(modifiers), parseOptionalToken(57 /* QuestionToken */), parseTypeAnnotation(), parseInitializer()), pos), hasJSDoc);
            topLevel = savedTopLevel;
            return node;
        }
        function parseReturnType(returnToken, isType) {
            if (shouldParseReturnType(returnToken, isType)) {
                return parseTypeOrTypePredicate();
            }
        }
        function shouldParseReturnType(returnToken, isType) {
            if (returnToken === 38 /* EqualsGreaterThanToken */) {
                parseExpected(returnToken);
                return true;
            }
            else if (parseOptional(58 /* ColonToken */)) {
                return true;
            }
            else if (isType && token() === 38 /* EqualsGreaterThanToken */) {
                // This is easy to get backward, especially in type contexts, so parse the type anyway
                parseErrorAtCurrentToken(ts.Diagnostics._0_expected, ts.tokenToString(58 /* ColonToken */));
                nextToken();
                return true;
            }
            return false;
        }
        function parseParametersWorker(flags) {
            // FormalParameters [Yield,Await]: (modified)
            //      [empty]
            //      FormalParameterList[?Yield,Await]
            //
            // FormalParameter[Yield,Await]: (modified)
            //      BindingElement[?Yield,Await]
            //
            // BindingElement [Yield,Await]: (modified)
            //      SingleNameBinding[?Yield,?Await]
            //      BindingPattern[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
            //
            // SingleNameBinding [Yield,Await]:
            //      BindingIdentifier[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
            var savedYieldContext = inYieldContext();
            var savedAwaitContext = inAwaitContext();
            setYieldContext(!!(flags & 1 /* Yield */));
            setAwaitContext(!!(flags & 2 /* Await */));
            var parameters = flags & 32 /* JSDoc */ ?
                parseDelimitedList(17 /* JSDocParameters */, parseJSDocParameter) :
                parseDelimitedList(16 /* Parameters */, savedAwaitContext ? parseParameterInOuterAwaitContext : parseParameter);
            setYieldContext(savedYieldContext);
            setAwaitContext(savedAwaitContext);
            return parameters;
        }
        function parseParameters(flags) {
            // FormalParameters [Yield,Await]: (modified)
            //      [empty]
            //      FormalParameterList[?Yield,Await]
            //
            // FormalParameter[Yield,Await]: (modified)
            //      BindingElement[?Yield,Await]
            //
            // BindingElement [Yield,Await]: (modified)
            //      SingleNameBinding[?Yield,?Await]
            //      BindingPattern[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
            //
            // SingleNameBinding [Yield,Await]:
            //      BindingIdentifier[?Yield,?Await]Initializer [In, ?Yield,?Await] opt
            if (!parseExpected(20 /* OpenParenToken */)) {
                return createMissingList();
            }
            var parameters = parseParametersWorker(flags);
            parseExpected(21 /* CloseParenToken */);
            return parameters;
        }
        function parseTypeMemberSemicolon() {
            // We allow type members to be separated by commas or (possibly ASI) semicolons.
            // First check if it was a comma.  If so, we're done with the member.
            if (parseOptional(27 /* CommaToken */)) {
                return;
            }
            // Didn't have a comma.  We must have a (possible ASI) semicolon.
            parseSemicolon();
        }
        function parseSignatureMember(kind) {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            if (kind === 174 /* ConstructSignature */) {
                parseExpected(103 /* NewKeyword */);
            }
            var typeParameters = parseTypeParameters();
            var parameters = parseParameters(4 /* Type */);
            var type = parseReturnType(58 /* ColonToken */, /*isType*/ true);
            parseTypeMemberSemicolon();
            var node = kind === 173 /* CallSignature */
                ? factory.createCallSignature(typeParameters, parameters, type)
                : factory.createConstructSignature(typeParameters, parameters, type);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function isIndexSignature() {
            return token() === 22 /* OpenBracketToken */ && lookAhead(isUnambiguouslyIndexSignature);
        }
        function isUnambiguouslyIndexSignature() {
            // The only allowed sequence is:
            //
            //   [id:
            //
            // However, for error recovery, we also check the following cases:
            //
            //   [...
            //   [id,
            //   [id?,
            //   [id?:
            //   [id?]
            //   [public id
            //   [private id
            //   [protected id
            //   []
            //
            nextToken();
            if (token() === 25 /* DotDotDotToken */ || token() === 23 /* CloseBracketToken */) {
                return true;
            }
            if (ts.isModifierKind(token())) {
                nextToken();
                if (isIdentifier()) {
                    return true;
                }
            }
            else if (!isIdentifier()) {
                return false;
            }
            else {
                // Skip the identifier
                nextToken();
            }
            // A colon signifies a well formed indexer
            // A comma should be a badly formed indexer because comma expressions are not allowed
            // in computed properties.
            if (token() === 58 /* ColonToken */ || token() === 27 /* CommaToken */) {
                return true;
            }
            // Question mark could be an indexer with an optional property,
            // or it could be a conditional expression in a computed property.
            if (token() !== 57 /* QuestionToken */) {
                return false;
            }
            // If any of the following tokens are after the question mark, it cannot
            // be a conditional expression, so treat it as an indexer.
            nextToken();
            return token() === 58 /* ColonToken */ || token() === 27 /* CommaToken */ || token() === 23 /* CloseBracketToken */;
        }
        function parseIndexSignatureDeclaration(pos, hasJSDoc, decorators, modifiers) {
            var parameters = parseBracketedList(16 /* Parameters */, parseParameter, 22 /* OpenBracketToken */, 23 /* CloseBracketToken */);
            var type = parseTypeAnnotation();
            parseTypeMemberSemicolon();
            var node = factory.createIndexSignature(decorators, modifiers, parameters, type);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parsePropertyOrMethodSignature(pos, hasJSDoc, modifiers) {
            var name = parsePropertyName();
            var questionToken = parseOptionalToken(57 /* QuestionToken */);
            var node;
            if (token() === 20 /* OpenParenToken */ || token() === 29 /* LessThanToken */) {
                // Method signatures don't exist in expression contexts.  So they have neither
                // [Yield] nor [Await]
                var typeParameters = parseTypeParameters();
                var parameters = parseParameters(4 /* Type */);
                var type = parseReturnType(58 /* ColonToken */, /*isType*/ true);
                node = factory.createMethodSignature(modifiers, name, questionToken, typeParameters, parameters, type);
            }
            else {
                var type = parseTypeAnnotation();
                node = factory.createPropertySignature(modifiers, name, questionToken, type);
                // Although type literal properties cannot not have initializers, we attempt
                // to parse an initializer so we can report in the checker that an interface
                // property or type literal property cannot have an initializer.
                if (token() === 63 /* EqualsToken */)
                    node.initializer = parseInitializer();
            }
            parseTypeMemberSemicolon();
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function isTypeMemberStart() {
            // Return true if we have the start of a signature member
            if (token() === 20 /* OpenParenToken */ ||
                token() === 29 /* LessThanToken */ ||
                token() === 136 /* GetKeyword */ ||
                token() === 148 /* SetKeyword */) {
                return true;
            }
            var idToken = false;
            // Eat up all modifiers, but hold on to the last one in case it is actually an identifier
            while (ts.isModifierKind(token())) {
                idToken = true;
                nextToken();
            }
            // Index signatures and computed property names are type members
            if (token() === 22 /* OpenBracketToken */) {
                return true;
            }
            // Try to get the first property-like token following all modifiers
            if (isLiteralPropertyName()) {
                idToken = true;
                nextToken();
            }
            // If we were able to get any potential identifier, check that it is
            // the start of a member declaration
            if (idToken) {
                return token() === 20 /* OpenParenToken */ ||
                    token() === 29 /* LessThanToken */ ||
                    token() === 57 /* QuestionToken */ ||
                    token() === 58 /* ColonToken */ ||
                    token() === 27 /* CommaToken */ ||
                    canParseSemicolon();
            }
            return false;
        }
        function parseTypeMember() {
            if (token() === 20 /* OpenParenToken */ || token() === 29 /* LessThanToken */) {
                return parseSignatureMember(173 /* CallSignature */);
            }
            if (token() === 103 /* NewKeyword */ && lookAhead(nextTokenIsOpenParenOrLessThan)) {
                return parseSignatureMember(174 /* ConstructSignature */);
            }
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var modifiers = parseModifiers();
            if (parseContextualModifier(136 /* GetKeyword */)) {
                return parseAccessorDeclaration(pos, hasJSDoc, /*decorators*/ undefined, modifiers, 171 /* GetAccessor */);
            }
            if (parseContextualModifier(148 /* SetKeyword */)) {
                return parseAccessorDeclaration(pos, hasJSDoc, /*decorators*/ undefined, modifiers, 172 /* SetAccessor */);
            }
            if (isIndexSignature()) {
                return parseIndexSignatureDeclaration(pos, hasJSDoc, /*decorators*/ undefined, modifiers);
            }
            return parsePropertyOrMethodSignature(pos, hasJSDoc, modifiers);
        }
        function nextTokenIsOpenParenOrLessThan() {
            nextToken();
            return token() === 20 /* OpenParenToken */ || token() === 29 /* LessThanToken */;
        }
        function nextTokenIsDot() {
            return nextToken() === 24 /* DotToken */;
        }
        function nextTokenIsOpenParenOrLessThanOrDot() {
            switch (nextToken()) {
                case 20 /* OpenParenToken */:
                case 29 /* LessThanToken */:
                case 24 /* DotToken */:
                    return true;
            }
            return false;
        }
        function parseTypeLiteral() {
            var pos = getNodePos();
            return finishNode(factory.createTypeLiteralNode(parseObjectTypeMembers()), pos);
        }
        function parseObjectTypeMembers() {
            var members;
            if (parseExpected(18 /* OpenBraceToken */)) {
                members = parseList(4 /* TypeMembers */, parseTypeMember);
                parseExpected(19 /* CloseBraceToken */);
            }
            else {
                members = createMissingList();
            }
            return members;
        }
        function isStartOfMappedType() {
            nextToken();
            if (token() === 39 /* PlusToken */ || token() === 40 /* MinusToken */) {
                return nextToken() === 144 /* ReadonlyKeyword */;
            }
            if (token() === 144 /* ReadonlyKeyword */) {
                nextToken();
            }
            return token() === 22 /* OpenBracketToken */ && nextTokenIsIdentifier() && nextToken() === 101 /* InKeyword */;
        }
        function parseMappedTypeParameter() {
            var pos = getNodePos();
            var name = parseIdentifierName();
            parseExpected(101 /* InKeyword */);
            var type = parseType();
            return finishNode(factory.createTypeParameterDeclaration(name, type, /*defaultType*/ undefined), pos);
        }
        function parseMappedType() {
            var pos = getNodePos();
            parseExpected(18 /* OpenBraceToken */);
            var readonlyToken;
            if (token() === 144 /* ReadonlyKeyword */ || token() === 39 /* PlusToken */ || token() === 40 /* MinusToken */) {
                readonlyToken = parseTokenNode();
                if (readonlyToken.kind !== 144 /* ReadonlyKeyword */) {
                    parseExpected(144 /* ReadonlyKeyword */);
                }
            }
            parseExpected(22 /* OpenBracketToken */);
            var typeParameter = parseMappedTypeParameter();
            var nameType = parseOptional(127 /* AsKeyword */) ? parseType() : undefined;
            parseExpected(23 /* CloseBracketToken */);
            var questionToken;
            if (token() === 57 /* QuestionToken */ || token() === 39 /* PlusToken */ || token() === 40 /* MinusToken */) {
                questionToken = parseTokenNode();
                if (questionToken.kind !== 57 /* QuestionToken */) {
                    parseExpected(57 /* QuestionToken */);
                }
            }
            var type = parseTypeAnnotation();
            parseSemicolon();
            var members = parseList(4 /* TypeMembers */, parseTypeMember);
            parseExpected(19 /* CloseBraceToken */);
            return finishNode(factory.createMappedTypeNode(readonlyToken, typeParameter, nameType, questionToken, type, members), pos);
        }
        function parseTupleElementType() {
            var pos = getNodePos();
            if (parseOptional(25 /* DotDotDotToken */)) {
                return finishNode(factory.createRestTypeNode(parseType()), pos);
            }
            var type = parseType();
            if (ts.isJSDocNullableType(type) && type.pos === type.type.pos) {
                var node = factory.createOptionalTypeNode(type.type);
                ts.setTextRange(node, type);
                node.flags = type.flags;
                return node;
            }
            return type;
        }
        function isNextTokenColonOrQuestionColon() {
            return nextToken() === 58 /* ColonToken */ || (token() === 57 /* QuestionToken */ && nextToken() === 58 /* ColonToken */);
        }
        function isTupleElementName() {
            if (token() === 25 /* DotDotDotToken */) {
                return ts.tokenIsIdentifierOrKeyword(nextToken()) && isNextTokenColonOrQuestionColon();
            }
            return ts.tokenIsIdentifierOrKeyword(token()) && isNextTokenColonOrQuestionColon();
        }
        function parseTupleElementNameOrTupleElementType() {
            if (lookAhead(isTupleElementName)) {
                var pos = getNodePos();
                var hasJSDoc = hasPrecedingJSDocComment();
                var dotDotDotToken = parseOptionalToken(25 /* DotDotDotToken */);
                var name = parseIdentifierName();
                var questionToken = parseOptionalToken(57 /* QuestionToken */);
                parseExpected(58 /* ColonToken */);
                var type = parseTupleElementType();
                var node = factory.createNamedTupleMember(dotDotDotToken, name, questionToken, type);
                return withJSDoc(finishNode(node, pos), hasJSDoc);
            }
            return parseTupleElementType();
        }
        function parseTupleType() {
            var pos = getNodePos();
            return finishNode(factory.createTupleTypeNode(parseBracketedList(21 /* TupleElementTypes */, parseTupleElementNameOrTupleElementType, 22 /* OpenBracketToken */, 23 /* CloseBracketToken */)), pos);
        }
        function parseParenthesizedType() {
            var pos = getNodePos();
            parseExpected(20 /* OpenParenToken */);
            var type = parseType();
            parseExpected(21 /* CloseParenToken */);
            return finishNode(factory.createParenthesizedType(type), pos);
        }
        function parseModifiersForConstructorType() {
            var modifiers;
            if (token() === 126 /* AbstractKeyword */) {
                var pos = getNodePos();
                nextToken();
                var modifier = finishNode(factory.createToken(126 /* AbstractKeyword */), pos);
                modifiers = createNodeArray([modifier], pos);
            }
            return modifiers;
        }
        function parseFunctionOrConstructorType() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var modifiers = parseModifiersForConstructorType();
            var isConstructorType = parseOptional(103 /* NewKeyword */);
            var typeParameters = parseTypeParameters();
            var parameters = parseParameters(4 /* Type */);
            var type = parseReturnType(38 /* EqualsGreaterThanToken */, /*isType*/ false);
            var node = isConstructorType
                ? factory.createConstructorTypeNode(modifiers, typeParameters, parameters, type)
                : factory.createFunctionTypeNode(typeParameters, parameters, type);
            if (!isConstructorType)
                node.modifiers = modifiers;
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseKeywordAndNoDot() {
            var node = parseTokenNode();
            return token() === 24 /* DotToken */ ? undefined : node;
        }
        function parseLiteralTypeNode(negative) {
            var pos = getNodePos();
            if (negative) {
                nextToken();
            }
            var expression = token() === 110 /* TrueKeyword */ || token() === 95 /* FalseKeyword */ || token() === 104 /* NullKeyword */ ?
                parseTokenNode() :
                parseLiteralLikeNode(token());
            if (negative) {
                expression = finishNode(factory.createPrefixUnaryExpression(40 /* MinusToken */, expression), pos);
            }
            return finishNode(factory.createLiteralTypeNode(expression), pos);
        }
        function isStartOfTypeOfImportType() {
            nextToken();
            return token() === 100 /* ImportKeyword */;
        }
        function parseImportType() {
            sourceFlags |= 1048576 /* PossiblyContainsDynamicImport */;
            var pos = getNodePos();
            var isTypeOf = parseOptional(112 /* TypeOfKeyword */);
            parseExpected(100 /* ImportKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var type = parseType();
            parseExpected(21 /* CloseParenToken */);
            var qualifier = parseOptional(24 /* DotToken */) ? parseEntityNameOfTypeReference() : undefined;
            var typeArguments = parseTypeArgumentsOfTypeReference();
            return finishNode(factory.createImportTypeNode(type, qualifier, typeArguments, isTypeOf), pos);
        }
        function nextTokenIsNumericOrBigIntLiteral() {
            nextToken();
            return token() === 8 /* NumericLiteral */ || token() === 9 /* BigIntLiteral */;
        }
        function parseNonArrayType() {
            switch (token()) {
                case 130 /* AnyKeyword */:
                case 154 /* UnknownKeyword */:
                case 149 /* StringKeyword */:
                case 146 /* NumberKeyword */:
                case 157 /* BigIntKeyword */:
                case 150 /* SymbolKeyword */:
                case 133 /* BooleanKeyword */:
                case 152 /* UndefinedKeyword */:
                case 143 /* NeverKeyword */:
                case 147 /* ObjectKeyword */:
                    // If these are followed by a dot, then parse these out as a dotted type reference instead.
                    return tryParse(parseKeywordAndNoDot) || parseTypeReference();
                case 66 /* AsteriskEqualsToken */:
                    // If there is '*=', treat it as * followed by postfix =
                    scanner.reScanAsteriskEqualsToken();
                // falls through
                case 41 /* AsteriskToken */:
                    return parseJSDocAllType();
                case 60 /* QuestionQuestionToken */:
                    // If there is '??', treat it as prefix-'?' in JSDoc type.
                    scanner.reScanQuestionToken();
                // falls through
                case 57 /* QuestionToken */:
                    return parseJSDocUnknownOrNullableType();
                case 98 /* FunctionKeyword */:
                    return parseJSDocFunctionType();
                case 53 /* ExclamationToken */:
                    return parseJSDocNonNullableType();
                case 14 /* NoSubstitutionTemplateLiteral */:
                case 10 /* StringLiteral */:
                case 8 /* NumericLiteral */:
                case 9 /* BigIntLiteral */:
                case 110 /* TrueKeyword */:
                case 95 /* FalseKeyword */:
                case 104 /* NullKeyword */:
                    return parseLiteralTypeNode();
                case 40 /* MinusToken */:
                    return lookAhead(nextTokenIsNumericOrBigIntLiteral) ? parseLiteralTypeNode(/*negative*/ true) : parseTypeReference();
                case 114 /* VoidKeyword */:
                    return parseTokenNode();
                case 108 /* ThisKeyword */: {
                    var thisKeyword = parseThisTypeNode();
                    if (token() === 139 /* IsKeyword */ && !scanner.hasPrecedingLineBreak()) {
                        return parseThisTypePredicate(thisKeyword);
                    }
                    else {
                        return thisKeyword;
                    }
                }
                case 112 /* TypeOfKeyword */:
                    return lookAhead(isStartOfTypeOfImportType) ? parseImportType() : parseTypeQuery();
                case 18 /* OpenBraceToken */:
                    return lookAhead(isStartOfMappedType) ? parseMappedType() : parseTypeLiteral();
                case 22 /* OpenBracketToken */:
                    return parseTupleType();
                case 20 /* OpenParenToken */:
                    return parseParenthesizedType();
                case 100 /* ImportKeyword */:
                    return parseImportType();
                case 128 /* AssertsKeyword */:
                    return lookAhead(nextTokenIsIdentifierOrKeywordOnSameLine) ? parseAssertsTypePredicate() : parseTypeReference();
                case 15 /* TemplateHead */:
                    return parseTemplateType();
                default:
                    return parseTypeReference();
            }
        }
        function isStartOfType(inStartOfParameter) {
            switch (token()) {
                case 130 /* AnyKeyword */:
                case 154 /* UnknownKeyword */:
                case 149 /* StringKeyword */:
                case 146 /* NumberKeyword */:
                case 157 /* BigIntKeyword */:
                case 133 /* BooleanKeyword */:
                case 144 /* ReadonlyKeyword */:
                case 150 /* SymbolKeyword */:
                case 153 /* UniqueKeyword */:
                case 114 /* VoidKeyword */:
                case 152 /* UndefinedKeyword */:
                case 104 /* NullKeyword */:
                case 108 /* ThisKeyword */:
                case 112 /* TypeOfKeyword */:
                case 143 /* NeverKeyword */:
                case 18 /* OpenBraceToken */:
                case 22 /* OpenBracketToken */:
                case 29 /* LessThanToken */:
                case 51 /* BarToken */:
                case 50 /* AmpersandToken */:
                case 103 /* NewKeyword */:
                case 10 /* StringLiteral */:
                case 8 /* NumericLiteral */:
                case 9 /* BigIntLiteral */:
                case 110 /* TrueKeyword */:
                case 95 /* FalseKeyword */:
                case 147 /* ObjectKeyword */:
                case 41 /* AsteriskToken */:
                case 57 /* QuestionToken */:
                case 53 /* ExclamationToken */:
                case 25 /* DotDotDotToken */:
                case 137 /* InferKeyword */:
                case 100 /* ImportKeyword */:
                case 128 /* AssertsKeyword */:
                case 14 /* NoSubstitutionTemplateLiteral */:
                case 15 /* TemplateHead */:
                    return true;
                case 98 /* FunctionKeyword */:
                    return !inStartOfParameter;
                case 40 /* MinusToken */:
                    return !inStartOfParameter && lookAhead(nextTokenIsNumericOrBigIntLiteral);
                case 20 /* OpenParenToken */:
                    // Only consider '(' the start of a type if followed by ')', '...', an identifier, a modifier,
                    // or something that starts a type. We don't want to consider things like '(1)' a type.
                    return !inStartOfParameter && lookAhead(isStartOfParenthesizedOrFunctionType);
                default:
                    return isIdentifier();
            }
        }
        function isStartOfParenthesizedOrFunctionType() {
            nextToken();
            return token() === 21 /* CloseParenToken */ || isStartOfParameter(/*isJSDocParameter*/ false) || isStartOfType();
        }
        function parsePostfixTypeOrHigher() {
            var pos = getNodePos();
            var type = parseNonArrayType();
            while (!scanner.hasPrecedingLineBreak()) {
                switch (token()) {
                    case 53 /* ExclamationToken */:
                        nextToken();
                        type = finishNode(factory.createJSDocNonNullableType(type), pos);
                        break;
                    case 57 /* QuestionToken */:
                        // If next token is start of a type we have a conditional type
                        if (lookAhead(nextTokenIsStartOfType)) {
                            return type;
                        }
                        nextToken();
                        type = finishNode(factory.createJSDocNullableType(type), pos);
                        break;
                    case 22 /* OpenBracketToken */:
                        parseExpected(22 /* OpenBracketToken */);
                        if (isStartOfType()) {
                            var indexType = parseType();
                            parseExpected(23 /* CloseBracketToken */);
                            type = finishNode(factory.createIndexedAccessTypeNode(type, indexType), pos);
                        }
                        else {
                            parseExpected(23 /* CloseBracketToken */);
                            type = finishNode(factory.createArrayTypeNode(type), pos);
                        }
                        break;
                    default:
                        return type;
                }
            }
            return type;
        }
        function parseTypeOperator(operator) {
            var pos = getNodePos();
            parseExpected(operator);
            return finishNode(factory.createTypeOperatorNode(operator, parseTypeOperatorOrHigher()), pos);
        }
        function parseTypeParameterOfInferType() {
            var pos = getNodePos();
            return finishNode(factory.createTypeParameterDeclaration(parseIdentifier(), 
            /*constraint*/ undefined, 
            /*defaultType*/ undefined), pos);
        }
        function parseInferType() {
            var pos = getNodePos();
            parseExpected(137 /* InferKeyword */);
            return finishNode(factory.createInferTypeNode(parseTypeParameterOfInferType()), pos);
        }
        function parseTypeOperatorOrHigher() {
            var operator = token();
            switch (operator) {
                case 140 /* KeyOfKeyword */:
                case 153 /* UniqueKeyword */:
                case 144 /* ReadonlyKeyword */:
                    return parseTypeOperator(operator);
                case 137 /* InferKeyword */:
                    return parseInferType();
            }
            return parsePostfixTypeOrHigher();
        }
        function parseFunctionOrConstructorTypeToError(isInUnionType) {
            // the function type and constructor type shorthand notation
            // are not allowed directly in unions and intersections, but we'll
            // try to parse them gracefully and issue a helpful message.
            if (isStartOfFunctionTypeOrConstructorType()) {
                var type = parseFunctionOrConstructorType();
                var diagnostic = void 0;
                if (ts.isFunctionTypeNode(type)) {
                    diagnostic = isInUnionType
                        ? ts.Diagnostics.Function_type_notation_must_be_parenthesized_when_used_in_a_union_type
                        : ts.Diagnostics.Function_type_notation_must_be_parenthesized_when_used_in_an_intersection_type;
                }
                else {
                    diagnostic = isInUnionType
                        ? ts.Diagnostics.Constructor_type_notation_must_be_parenthesized_when_used_in_a_union_type
                        : ts.Diagnostics.Constructor_type_notation_must_be_parenthesized_when_used_in_an_intersection_type;
                }
                parseErrorAtRange(type, diagnostic);
                return type;
            }
            return undefined;
        }
        function parseUnionOrIntersectionType(operator, parseConstituentType, createTypeNode) {
            var pos = getNodePos();
            var isUnionType = operator === 51 /* BarToken */;
            var hasLeadingOperator = parseOptional(operator);
            var type = hasLeadingOperator && parseFunctionOrConstructorTypeToError(isUnionType)
                || parseConstituentType();
            if (token() === operator || hasLeadingOperator) {
                var types = [type];
                while (parseOptional(operator)) {
                    types.push(parseFunctionOrConstructorTypeToError(isUnionType) || parseConstituentType());
                }
                type = finishNode(createTypeNode(createNodeArray(types, pos)), pos);
            }
            return type;
        }
        function parseIntersectionTypeOrHigher() {
            return parseUnionOrIntersectionType(50 /* AmpersandToken */, parseTypeOperatorOrHigher, factory.createIntersectionTypeNode);
        }
        function parseUnionTypeOrHigher() {
            return parseUnionOrIntersectionType(51 /* BarToken */, parseIntersectionTypeOrHigher, factory.createUnionTypeNode);
        }
        function nextTokenIsNewKeyword() {
            nextToken();
            return token() === 103 /* NewKeyword */;
        }
        function isStartOfFunctionTypeOrConstructorType() {
            if (token() === 29 /* LessThanToken */) {
                return true;
            }
            if (token() === 20 /* OpenParenToken */ && lookAhead(isUnambiguouslyStartOfFunctionType)) {
                return true;
            }
            return token() === 103 /* NewKeyword */ ||
                token() === 126 /* AbstractKeyword */ && lookAhead(nextTokenIsNewKeyword);
        }
        function skipParameterStart() {
            if (ts.isModifierKind(token())) {
                // Skip modifiers
                parseModifiers();
            }
            if (isIdentifier() || token() === 108 /* ThisKeyword */) {
                nextToken();
                return true;
            }
            if (token() === 22 /* OpenBracketToken */ || token() === 18 /* OpenBraceToken */) {
                // Return true if we can parse an array or object binding pattern with no errors
                var previousErrorCount = parseDiagnostics.length;
                parseIdentifierOrPattern();
                return previousErrorCount === parseDiagnostics.length;
            }
            return false;
        }
        function isUnambiguouslyStartOfFunctionType() {
            nextToken();
            if (token() === 21 /* CloseParenToken */ || token() === 25 /* DotDotDotToken */) {
                // ( )
                // ( ...
                return true;
            }
            if (skipParameterStart()) {
                // We successfully skipped modifiers (if any) and an identifier or binding pattern,
                // now see if we have something that indicates a parameter declaration
                if (token() === 58 /* ColonToken */ || token() === 27 /* CommaToken */ ||
                    token() === 57 /* QuestionToken */ || token() === 63 /* EqualsToken */) {
                    // ( xxx :
                    // ( xxx ,
                    // ( xxx ?
                    // ( xxx =
                    return true;
                }
                if (token() === 21 /* CloseParenToken */) {
                    nextToken();
                    if (token() === 38 /* EqualsGreaterThanToken */) {
                        // ( xxx ) =>
                        return true;
                    }
                }
            }
            return false;
        }
        function parseTypeOrTypePredicate() {
            var pos = getNodePos();
            var typePredicateVariable = isIdentifier() && tryParse(parseTypePredicatePrefix);
            var type = parseType();
            if (typePredicateVariable) {
                return finishNode(factory.createTypePredicateNode(/*assertsModifier*/ undefined, typePredicateVariable, type), pos);
            }
            else {
                return type;
            }
        }
        function parseTypePredicatePrefix() {
            var id = parseIdentifier();
            if (token() === 139 /* IsKeyword */ && !scanner.hasPrecedingLineBreak()) {
                nextToken();
                return id;
            }
        }
        function parseAssertsTypePredicate() {
            var pos = getNodePos();
            var assertsModifier = parseExpectedToken(128 /* AssertsKeyword */);
            var parameterName = token() === 108 /* ThisKeyword */ ? parseThisTypeNode() : parseIdentifier();
            var type = parseOptional(139 /* IsKeyword */) ? parseType() : undefined;
            return finishNode(factory.createTypePredicateNode(assertsModifier, parameterName, type), pos);
        }
        function parseType() {
            // The rules about 'yield' only apply to actual code/expression contexts.  They don't
            // apply to 'type' contexts.  So we disable these parameters here before moving on.
            return doOutsideOfContext(40960 /* TypeExcludesFlags */, parseTypeWorker);
        }
        function parseTypeWorker(noConditionalTypes) {
            if (isStartOfFunctionTypeOrConstructorType()) {
                return parseFunctionOrConstructorType();
            }
            var pos = getNodePos();
            var type = parseUnionTypeOrHigher();
            if (!noConditionalTypes && !scanner.hasPrecedingLineBreak() && parseOptional(94 /* ExtendsKeyword */)) {
                // The type following 'extends' is not permitted to be another conditional type
                var extendsType = parseTypeWorker(/*noConditionalTypes*/ true);
                parseExpected(57 /* QuestionToken */);
                var trueType = parseTypeWorker();
                parseExpected(58 /* ColonToken */);
                var falseType = parseTypeWorker();
                return finishNode(factory.createConditionalTypeNode(type, extendsType, trueType, falseType), pos);
            }
            return type;
        }
        function parseTypeAnnotation() {
            return parseOptional(58 /* ColonToken */) ? parseType() : undefined;
        }
        // EXPRESSIONS
        function isStartOfLeftHandSideExpression() {
            switch (token()) {
                case 108 /* ThisKeyword */:
                case 106 /* SuperKeyword */:
                case 104 /* NullKeyword */:
                case 110 /* TrueKeyword */:
                case 95 /* FalseKeyword */:
                case 8 /* NumericLiteral */:
                case 9 /* BigIntLiteral */:
                case 10 /* StringLiteral */:
                case 14 /* NoSubstitutionTemplateLiteral */:
                case 15 /* TemplateHead */:
                case 20 /* OpenParenToken */:
                case 22 /* OpenBracketToken */:
                case 18 /* OpenBraceToken */:
                case 98 /* FunctionKeyword */:
                case 84 /* ClassKeyword */:
                case 103 /* NewKeyword */:
                case 43 /* SlashToken */:
                case 68 /* SlashEqualsToken */:
                case 79 /* Identifier */:
                    return true;
                case 100 /* ImportKeyword */:
                    return lookAhead(nextTokenIsOpenParenOrLessThanOrDot);
                default:
                    return isIdentifier();
            }
        }
        function isStartOfExpression() {
            if (isStartOfLeftHandSideExpression()) {
                return true;
            }
            switch (token()) {
                case 39 /* PlusToken */:
                case 40 /* MinusToken */:
                case 54 /* TildeToken */:
                case 53 /* ExclamationToken */:
                case 89 /* DeleteKeyword */:
                case 112 /* TypeOfKeyword */:
                case 114 /* VoidKeyword */:
                case 45 /* PlusPlusToken */:
                case 46 /* MinusMinusToken */:
                case 29 /* LessThanToken */:
                case 132 /* AwaitKeyword */:
                case 125 /* YieldKeyword */:
                case 80 /* PrivateIdentifier */:
                    // Yield/await always starts an expression.  Either it is an identifier (in which case
                    // it is definitely an expression).  Or it's a keyword (either because we're in
                    // a generator or async function, or in strict mode (or both)) and it started a yield or await expression.
                    return true;
                default:
                    // Error tolerance.  If we see the start of some binary operator, we consider
                    // that the start of an expression.  That way we'll parse out a missing identifier,
                    // give a good message about an identifier being missing, and then consume the
                    // rest of the binary expression.
                    if (isBinaryOperator()) {
                        return true;
                    }
                    return isIdentifier();
            }
        }
        function isStartOfExpressionStatement() {
            // As per the grammar, none of '{' or 'function' or 'class' can start an expression statement.
            return token() !== 18 /* OpenBraceToken */ &&
                token() !== 98 /* FunctionKeyword */ &&
                token() !== 84 /* ClassKeyword */ &&
                token() !== 59 /* AtToken */ &&
                isStartOfExpression();
        }
        function parseExpression() {
            // Expression[in]:
            //      AssignmentExpression[in]
            //      Expression[in] , AssignmentExpression[in]
            // clear the decorator context when parsing Expression, as it should be unambiguous when parsing a decorator
            var saveDecoratorContext = inDecoratorContext();
            if (saveDecoratorContext) {
                setDecoratorContext(/*val*/ false);
            }
            var pos = getNodePos();
            var expr = parseAssignmentExpressionOrHigher();
            var operatorToken;
            while ((operatorToken = parseOptionalToken(27 /* CommaToken */))) {
                expr = makeBinaryExpression(expr, operatorToken, parseAssignmentExpressionOrHigher(), pos);
            }
            if (saveDecoratorContext) {
                setDecoratorContext(/*val*/ true);
            }
            return expr;
        }
        function parseInitializer() {
            return parseOptional(63 /* EqualsToken */) ? parseAssignmentExpressionOrHigher() : undefined;
        }
        function parseAssignmentExpressionOrHigher() {
            //  AssignmentExpression[in,yield]:
            //      1) ConditionalExpression[?in,?yield]
            //      2) LeftHandSideExpression = AssignmentExpression[?in,?yield]
            //      3) LeftHandSideExpression AssignmentOperator AssignmentExpression[?in,?yield]
            //      4) ArrowFunctionExpression[?in,?yield]
            //      5) AsyncArrowFunctionExpression[in,yield,await]
            //      6) [+Yield] YieldExpression[?In]
            //
            // Note: for ease of implementation we treat productions '2' and '3' as the same thing.
            // (i.e. they're both BinaryExpressions with an assignment operator in it).
            // First, do the simple check if we have a YieldExpression (production '6').
            if (isYieldExpression()) {
                return parseYieldExpression();
            }
            // Then, check if we have an arrow function (production '4' and '5') that starts with a parenthesized
            // parameter list or is an async arrow function.
            // AsyncArrowFunctionExpression:
            //      1) async[no LineTerminator here]AsyncArrowBindingIdentifier[?Yield][no LineTerminator here]=>AsyncConciseBody[?In]
            //      2) CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await][no LineTerminator here]=>AsyncConciseBody[?In]
            // Production (1) of AsyncArrowFunctionExpression is parsed in "tryParseAsyncSimpleArrowFunctionExpression".
            // And production (2) is parsed in "tryParseParenthesizedArrowFunctionExpression".
            //
            // If we do successfully parse arrow-function, we must *not* recurse for productions 1, 2 or 3. An ArrowFunction is
            // not a LeftHandSideExpression, nor does it start a ConditionalExpression.  So we are done
            // with AssignmentExpression if we see one.
            var arrowExpression = tryParseParenthesizedArrowFunctionExpression() || tryParseAsyncSimpleArrowFunctionExpression();
            if (arrowExpression) {
                return arrowExpression;
            }
            // Now try to see if we're in production '1', '2' or '3'.  A conditional expression can
            // start with a LogicalOrExpression, while the assignment productions can only start with
            // LeftHandSideExpressions.
            //
            // So, first, we try to just parse out a BinaryExpression.  If we get something that is a
            // LeftHandSide or higher, then we can try to parse out the assignment expression part.
            // Otherwise, we try to parse out the conditional expression bit.  We want to allow any
            // binary expression here, so we pass in the 'lowest' precedence here so that it matches
            // and consumes anything.
            var pos = getNodePos();
            var expr = parseBinaryExpressionOrHigher(0 /* Lowest */);
            // To avoid a look-ahead, we did not handle the case of an arrow function with a single un-parenthesized
            // parameter ('x => ...') above. We handle it here by checking if the parsed expression was a single
            // identifier and the current token is an arrow.
            if (expr.kind === 79 /* Identifier */ && token() === 38 /* EqualsGreaterThanToken */) {
                return parseSimpleArrowFunctionExpression(pos, expr, /*asyncModifier*/ undefined);
            }
            // Now see if we might be in cases '2' or '3'.
            // If the expression was a LHS expression, and we have an assignment operator, then
            // we're in '2' or '3'. Consume the assignment and return.
            //
            // Note: we call reScanGreaterToken so that we get an appropriately merged token
            // for cases like `> > =` becoming `>>=`
            if (ts.isLeftHandSideExpression(expr) && ts.isAssignmentOperator(reScanGreaterToken())) {
                return makeBinaryExpression(expr, parseTokenNode(), parseAssignmentExpressionOrHigher(), pos);
            }
            // It wasn't an assignment or a lambda.  This is a conditional expression:
            return parseConditionalExpressionRest(expr, pos);
        }
        function isYieldExpression() {
            if (token() === 125 /* YieldKeyword */) {
                // If we have a 'yield' keyword, and this is a context where yield expressions are
                // allowed, then definitely parse out a yield expression.
                if (inYieldContext()) {
                    return true;
                }
                // We're in a context where 'yield expr' is not allowed.  However, if we can
                // definitely tell that the user was trying to parse a 'yield expr' and not
                // just a normal expr that start with a 'yield' identifier, then parse out
                // a 'yield expr'.  We can then report an error later that they are only
                // allowed in generator expressions.
                //
                // for example, if we see 'yield(foo)', then we'll have to treat that as an
                // invocation expression of something called 'yield'.  However, if we have
                // 'yield foo' then that is not legal as a normal expression, so we can
                // definitely recognize this as a yield expression.
                //
                // for now we just check if the next token is an identifier.  More heuristics
                // can be added here later as necessary.  We just need to make sure that we
                // don't accidentally consume something legal.
                return lookAhead(nextTokenIsIdentifierOrKeywordOrLiteralOnSameLine);
            }
            return false;
        }
        function nextTokenIsIdentifierOnSameLine() {
            nextToken();
            return !scanner.hasPrecedingLineBreak() && isIdentifier();
        }
        function parseYieldExpression() {
            var pos = getNodePos();
            // YieldExpression[In] :
            //      yield
            //      yield [no LineTerminator here] [Lexical goal InputElementRegExp]AssignmentExpression[?In, Yield]
            //      yield [no LineTerminator here] * [Lexical goal InputElementRegExp]AssignmentExpression[?In, Yield]
            nextToken();
            if (!scanner.hasPrecedingLineBreak() &&
                (token() === 41 /* AsteriskToken */ || isStartOfExpression())) {
                return finishNode(factory.createYieldExpression(parseOptionalToken(41 /* AsteriskToken */), parseAssignmentExpressionOrHigher()), pos);
            }
            else {
                // if the next token is not on the same line as yield.  or we don't have an '*' or
                // the start of an expression, then this is just a simple "yield" expression.
                return finishNode(factory.createYieldExpression(/*asteriskToken*/ undefined, /*expression*/ undefined), pos);
            }
        }
        function parseSimpleArrowFunctionExpression(pos, identifier, asyncModifier) {
            ts.Debug.assert(token() === 38 /* EqualsGreaterThanToken */, "parseSimpleArrowFunctionExpression should only have been called if we had a =>");
            var parameter = factory.createParameterDeclaration(
            /*decorators*/ undefined, 
            /*modifiers*/ undefined, 
            /*dotDotDotToken*/ undefined, identifier, 
            /*questionToken*/ undefined, 
            /*type*/ undefined, 
            /*initializer*/ undefined);
            finishNode(parameter, identifier.pos);
            var parameters = createNodeArray([parameter], parameter.pos, parameter.end);
            var equalsGreaterThanToken = parseExpectedToken(38 /* EqualsGreaterThanToken */);
            var body = parseArrowFunctionExpressionBody(/*isAsync*/ !!asyncModifier);
            var node = factory.createArrowFunction(asyncModifier, /*typeParameters*/ undefined, parameters, /*type*/ undefined, equalsGreaterThanToken, body);
            return addJSDocComment(finishNode(node, pos));
        }
        function tryParseParenthesizedArrowFunctionExpression() {
            var triState = isParenthesizedArrowFunctionExpression();
            if (triState === 0 /* False */) {
                // It's definitely not a parenthesized arrow function expression.
                return undefined;
            }
            // If we definitely have an arrow function, then we can just parse one, not requiring a
            // following => or { token. Otherwise, we *might* have an arrow function.  Try to parse
            // it out, but don't allow any ambiguity, and return 'undefined' if this could be an
            // expression instead.
            return triState === 1 /* True */ ?
                parseParenthesizedArrowFunctionExpression(/*allowAmbiguity*/ true) :
                tryParse(parsePossibleParenthesizedArrowFunctionExpression);
        }
        //  True        -> We definitely expect a parenthesized arrow function here.
        //  False       -> There *cannot* be a parenthesized arrow function here.
        //  Unknown     -> There *might* be a parenthesized arrow function here.
        //                 Speculatively look ahead to be sure, and rollback if not.
        function isParenthesizedArrowFunctionExpression() {
            if (token() === 20 /* OpenParenToken */ || token() === 29 /* LessThanToken */ || token() === 131 /* AsyncKeyword */) {
                return lookAhead(isParenthesizedArrowFunctionExpressionWorker);
            }
            if (token() === 38 /* EqualsGreaterThanToken */) {
                // ERROR RECOVERY TWEAK:
                // If we see a standalone => try to parse it as an arrow function expression as that's
                // likely what the user intended to write.
                return 1 /* True */;
            }
            // Definitely not a parenthesized arrow function.
            return 0 /* False */;
        }
        function isParenthesizedArrowFunctionExpressionWorker() {
            if (token() === 131 /* AsyncKeyword */) {
                nextToken();
                if (scanner.hasPrecedingLineBreak()) {
                    return 0 /* False */;
                }
                if (token() !== 20 /* OpenParenToken */ && token() !== 29 /* LessThanToken */) {
                    return 0 /* False */;
                }
            }
            var first = token();
            var second = nextToken();
            if (first === 20 /* OpenParenToken */) {
                if (second === 21 /* CloseParenToken */) {
                    // Simple cases: "() =>", "(): ", and "() {".
                    // This is an arrow function with no parameters.
                    // The last one is not actually an arrow function,
                    // but this is probably what the user intended.
                    var third = nextToken();
                    switch (third) {
                        case 38 /* EqualsGreaterThanToken */:
                        case 58 /* ColonToken */:
                        case 18 /* OpenBraceToken */:
                            return 1 /* True */;
                        default:
                            return 0 /* False */;
                    }
                }
                // If encounter "([" or "({", this could be the start of a binding pattern.
                // Examples:
                //      ([ x ]) => { }
                //      ({ x }) => { }
                //      ([ x ])
                //      ({ x })
                if (second === 22 /* OpenBracketToken */ || second === 18 /* OpenBraceToken */) {
                    return 2 /* Unknown */;
                }
                // Simple case: "(..."
                // This is an arrow function with a rest parameter.
                if (second === 25 /* DotDotDotToken */) {
                    return 1 /* True */;
                }
                // Check for "(xxx yyy", where xxx is a modifier and yyy is an identifier. This
                // isn't actually allowed, but we want to treat it as a lambda so we can provide
                // a good error message.
                if (ts.isModifierKind(second) && second !== 131 /* AsyncKeyword */ && lookAhead(nextTokenIsIdentifier)) {
                    return 1 /* True */;
                }
                // If we had "(" followed by something that's not an identifier,
                // then this definitely doesn't look like a lambda.  "this" is not
                // valid, but we want to parse it and then give a semantic error.
                if (!isIdentifier() && second !== 108 /* ThisKeyword */) {
                    return 0 /* False */;
                }
                switch (nextToken()) {
                    case 58 /* ColonToken */:
                        // If we have something like "(a:", then we must have a
                        // type-annotated parameter in an arrow function expression.
                        return 1 /* True */;
                    case 57 /* QuestionToken */:
                        nextToken();
                        // If we have "(a?:" or "(a?," or "(a?=" or "(a?)" then it is definitely a lambda.
                        if (token() === 58 /* ColonToken */ || token() === 27 /* CommaToken */ || token() === 63 /* EqualsToken */ || token() === 21 /* CloseParenToken */) {
                            return 1 /* True */;
                        }
                        // Otherwise it is definitely not a lambda.
                        return 0 /* False */;
                    case 27 /* CommaToken */:
                    case 63 /* EqualsToken */:
                    case 21 /* CloseParenToken */:
                        // If we have "(a," or "(a=" or "(a)" this *could* be an arrow function
                        return 2 /* Unknown */;
                }
                // It is definitely not an arrow function
                return 0 /* False */;
            }
            else {
                ts.Debug.assert(first === 29 /* LessThanToken */);
                // If we have "<" not followed by an identifier,
                // then this definitely is not an arrow function.
                if (!isIdentifier()) {
                    return 0 /* False */;
                }
                // JSX overrides
                if (languageVariant === 1 /* JSX */) {
                    var isArrowFunctionInJsx = lookAhead(function () {
                        var third = nextToken();
                        if (third === 94 /* ExtendsKeyword */) {
                            var fourth = nextToken();
                            switch (fourth) {
                                case 63 /* EqualsToken */:
                                case 31 /* GreaterThanToken */:
                                    return false;
                                default:
                                    return true;
                            }
                        }
                        else if (third === 27 /* CommaToken */ || third === 63 /* EqualsToken */) {
                            return true;
                        }
                        return false;
                    });
                    if (isArrowFunctionInJsx) {
                        return 1 /* True */;
                    }
                    return 0 /* False */;
                }
                // This *could* be a parenthesized arrow function.
                return 2 /* Unknown */;
            }
        }
        function parsePossibleParenthesizedArrowFunctionExpression() {
            var tokenPos = scanner.getTokenPos();
            if (notParenthesizedArrow === null || notParenthesizedArrow === void 0 ? void 0 : notParenthesizedArrow.has(tokenPos)) {
                return undefined;
            }
            var result = parseParenthesizedArrowFunctionExpression(/*allowAmbiguity*/ false);
            if (!result) {
                (notParenthesizedArrow || (notParenthesizedArrow = new ts.Set())).add(tokenPos);
            }
            return result;
        }
        function tryParseAsyncSimpleArrowFunctionExpression() {
            // We do a check here so that we won't be doing unnecessarily call to "lookAhead"
            if (token() === 131 /* AsyncKeyword */) {
                if (lookAhead(isUnParenthesizedAsyncArrowFunctionWorker) === 1 /* True */) {
                    var pos = getNodePos();
                    var asyncModifier = parseModifiersForArrowFunction();
                    var expr = parseBinaryExpressionOrHigher(0 /* Lowest */);
                    return parseSimpleArrowFunctionExpression(pos, expr, asyncModifier);
                }
            }
            return undefined;
        }
        function isUnParenthesizedAsyncArrowFunctionWorker() {
            // AsyncArrowFunctionExpression:
            //      1) async[no LineTerminator here]AsyncArrowBindingIdentifier[?Yield][no LineTerminator here]=>AsyncConciseBody[?In]
            //      2) CoverCallExpressionAndAsyncArrowHead[?Yield, ?Await][no LineTerminator here]=>AsyncConciseBody[?In]
            if (token() === 131 /* AsyncKeyword */) {
                nextToken();
                // If the "async" is followed by "=>" token then it is not a beginning of an async arrow-function
                // but instead a simple arrow-function which will be parsed inside "parseAssignmentExpressionOrHigher"
                if (scanner.hasPrecedingLineBreak() || token() === 38 /* EqualsGreaterThanToken */) {
                    return 0 /* False */;
                }
                // Check for un-parenthesized AsyncArrowFunction
                var expr = parseBinaryExpressionOrHigher(0 /* Lowest */);
                if (!scanner.hasPrecedingLineBreak() && expr.kind === 79 /* Identifier */ && token() === 38 /* EqualsGreaterThanToken */) {
                    return 1 /* True */;
                }
            }
            return 0 /* False */;
        }
        function parseParenthesizedArrowFunctionExpression(allowAmbiguity) {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var modifiers = parseModifiersForArrowFunction();
            var isAsync = ts.some(modifiers, ts.isAsyncModifier) ? 2 /* Await */ : 0 /* None */;
            // Arrow functions are never generators.
            //
            // If we're speculatively parsing a signature for a parenthesized arrow function, then
            // we have to have a complete parameter list.  Otherwise we might see something like
            // a => (b => c)
            // And think that "(b =>" was actually a parenthesized arrow function with a missing
            // close paren.
            var typeParameters = parseTypeParameters();
            var parameters;
            if (!parseExpected(20 /* OpenParenToken */)) {
                if (!allowAmbiguity) {
                    return undefined;
                }
                parameters = createMissingList();
            }
            else {
                parameters = parseParametersWorker(isAsync);
                if (!parseExpected(21 /* CloseParenToken */) && !allowAmbiguity) {
                    return undefined;
                }
            }
            var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
            if (type && !allowAmbiguity && typeHasArrowFunctionBlockingParseError(type)) {
                return undefined;
            }
            // Parsing a signature isn't enough.
            // Parenthesized arrow signatures often look like other valid expressions.
            // For instance:
            //  - "(x = 10)" is an assignment expression parsed as a signature with a default parameter value.
            //  - "(x,y)" is a comma expression parsed as a signature with two parameters.
            //  - "a ? (b): c" will have "(b):" parsed as a signature with a return type annotation.
            //  - "a ? (b): function() {}" will too, since function() is a valid JSDoc function type.
            //  - "a ? (b): (function() {})" as well, but inside of a parenthesized type with an arbitrary amount of nesting.
            //
            // So we need just a bit of lookahead to ensure that it can only be a signature.
            var unwrappedType = type;
            while ((unwrappedType === null || unwrappedType === void 0 ? void 0 : unwrappedType.kind) === 190 /* ParenthesizedType */) {
                unwrappedType = unwrappedType.type; // Skip parens if need be
            }
            var hasJSDocFunctionType = unwrappedType && ts.isJSDocFunctionType(unwrappedType);
            if (!allowAmbiguity && token() !== 38 /* EqualsGreaterThanToken */ && (hasJSDocFunctionType || token() !== 18 /* OpenBraceToken */)) {
                // Returning undefined here will cause our caller to rewind to where we started from.
                return undefined;
            }
            // If we have an arrow, then try to parse the body. Even if not, try to parse if we
            // have an opening brace, just in case we're in an error state.
            var lastToken = token();
            var equalsGreaterThanToken = parseExpectedToken(38 /* EqualsGreaterThanToken */);
            var body = (lastToken === 38 /* EqualsGreaterThanToken */ || lastToken === 18 /* OpenBraceToken */)
                ? parseArrowFunctionExpressionBody(ts.some(modifiers, ts.isAsyncModifier))
                : parseIdentifier();
            var node = factory.createArrowFunction(modifiers, typeParameters, parameters, type, equalsGreaterThanToken, body);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseArrowFunctionExpressionBody(isAsync) {
            if (token() === 18 /* OpenBraceToken */) {
                return parseFunctionBlock(isAsync ? 2 /* Await */ : 0 /* None */);
            }
            if (token() !== 26 /* SemicolonToken */ &&
                token() !== 98 /* FunctionKeyword */ &&
                token() !== 84 /* ClassKeyword */ &&
                isStartOfStatement() &&
                !isStartOfExpressionStatement()) {
                // Check if we got a plain statement (i.e. no expression-statements, no function/class expressions/declarations)
                //
                // Here we try to recover from a potential error situation in the case where the
                // user meant to supply a block. For example, if the user wrote:
                //
                //  a =>
                //      let v = 0;
                //  }
                //
                // they may be missing an open brace.  Check to see if that's the case so we can
                // try to recover better.  If we don't do this, then the next close curly we see may end
                // up preemptively closing the containing construct.
                //
                // Note: even when 'IgnoreMissingOpenBrace' is passed, parseBody will still error.
                return parseFunctionBlock(16 /* IgnoreMissingOpenBrace */ | (isAsync ? 2 /* Await */ : 0 /* None */));
            }
            var savedTopLevel = topLevel;
            topLevel = false;
            var node = isAsync
                ? doInAwaitContext(parseAssignmentExpressionOrHigher)
                : doOutsideOfAwaitContext(parseAssignmentExpressionOrHigher);
            topLevel = savedTopLevel;
            return node;
        }
        function parseConditionalExpressionRest(leftOperand, pos) {
            // Note: we are passed in an expression which was produced from parseBinaryExpressionOrHigher.
            var questionToken = parseOptionalToken(57 /* QuestionToken */);
            if (!questionToken) {
                return leftOperand;
            }
            // Note: we explicitly 'allowIn' in the whenTrue part of the condition expression, and
            // we do not that for the 'whenFalse' part.
            var colonToken;
            return finishNode(factory.createConditionalExpression(leftOperand, questionToken, doOutsideOfContext(disallowInAndDecoratorContext, parseAssignmentExpressionOrHigher), colonToken = parseExpectedToken(58 /* ColonToken */), ts.nodeIsPresent(colonToken)
                ? parseAssignmentExpressionOrHigher()
                : createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ false, ts.Diagnostics._0_expected, ts.tokenToString(58 /* ColonToken */))), pos);
        }
        function parseBinaryExpressionOrHigher(precedence) {
            var pos = getNodePos();
            var leftOperand = parseUnaryExpressionOrHigher();
            return parseBinaryExpressionRest(precedence, leftOperand, pos);
        }
        function isInOrOfKeyword(t) {
            return t === 101 /* InKeyword */ || t === 159 /* OfKeyword */;
        }
        function parseBinaryExpressionRest(precedence, leftOperand, pos) {
            while (true) {
                // We either have a binary operator here, or we're finished.  We call
                // reScanGreaterToken so that we merge token sequences like > and = into >=
                reScanGreaterToken();
                var newPrecedence = ts.getBinaryOperatorPrecedence(token());
                // Check the precedence to see if we should "take" this operator
                // - For left associative operator (all operator but **), consume the operator,
                //   recursively call the function below, and parse binaryExpression as a rightOperand
                //   of the caller if the new precedence of the operator is greater then or equal to the current precedence.
                //   For example:
                //      a - b - c;
                //            ^token; leftOperand = b. Return b to the caller as a rightOperand
                //      a * b - c
                //            ^token; leftOperand = b. Return b to the caller as a rightOperand
                //      a - b * c;
                //            ^token; leftOperand = b. Return b * c to the caller as a rightOperand
                // - For right associative operator (**), consume the operator, recursively call the function
                //   and parse binaryExpression as a rightOperand of the caller if the new precedence of
                //   the operator is strictly grater than the current precedence
                //   For example:
                //      a ** b ** c;
                //             ^^token; leftOperand = b. Return b ** c to the caller as a rightOperand
                //      a - b ** c;
                //            ^^token; leftOperand = b. Return b ** c to the caller as a rightOperand
                //      a ** b - c
                //             ^token; leftOperand = b. Return b to the caller as a rightOperand
                var consumeCurrentOperator = token() === 42 /* AsteriskAsteriskToken */ ?
                    newPrecedence >= precedence :
                    newPrecedence > precedence;
                if (!consumeCurrentOperator) {
                    break;
                }
                if (token() === 101 /* InKeyword */ && inDisallowInContext()) {
                    break;
                }
                if (token() === 127 /* AsKeyword */) {
                    // Make sure we *do* perform ASI for constructs like this:
                    //    var x = foo
                    //    as (Bar)
                    // This should be parsed as an initialized variable, followed
                    // by a function call to 'as' with the argument 'Bar'
                    if (scanner.hasPrecedingLineBreak()) {
                        break;
                    }
                    else {
                        nextToken();
                        leftOperand = makeAsExpression(leftOperand, parseType());
                    }
                }
                else {
                    leftOperand = makeBinaryExpression(leftOperand, parseTokenNode(), parseBinaryExpressionOrHigher(newPrecedence), pos);
                }
            }
            return leftOperand;
        }
        function isBinaryOperator() {
            if (inDisallowInContext() && token() === 101 /* InKeyword */) {
                return false;
            }
            return ts.getBinaryOperatorPrecedence(token()) > 0;
        }
        function makeBinaryExpression(left, operatorToken, right, pos) {
            return finishNode(factory.createBinaryExpression(left, operatorToken, right), pos);
        }
        function makeAsExpression(left, right) {
            return finishNode(factory.createAsExpression(left, right), left.pos);
        }
        function parsePrefixUnaryExpression() {
            var pos = getNodePos();
            return finishNode(factory.createPrefixUnaryExpression(token(), nextTokenAnd(parseSimpleUnaryExpression)), pos);
        }
        function parseDeleteExpression() {
            var pos = getNodePos();
            return finishNode(factory.createDeleteExpression(nextTokenAnd(parseSimpleUnaryExpression)), pos);
        }
        function parseTypeOfExpression() {
            var pos = getNodePos();
            return finishNode(factory.createTypeOfExpression(nextTokenAnd(parseSimpleUnaryExpression)), pos);
        }
        function parseVoidExpression() {
            var pos = getNodePos();
            return finishNode(factory.createVoidExpression(nextTokenAnd(parseSimpleUnaryExpression)), pos);
        }
        function isAwaitExpression() {
            if (token() === 132 /* AwaitKeyword */) {
                if (inAwaitContext()) {
                    return true;
                }
                // here we are using similar heuristics as 'isYieldExpression'
                return lookAhead(nextTokenIsIdentifierOrKeywordOrLiteralOnSameLine);
            }
            return false;
        }
        function parseAwaitExpression() {
            var pos = getNodePos();
            return finishNode(factory.createAwaitExpression(nextTokenAnd(parseSimpleUnaryExpression)), pos);
        }
        /**
         * Parse ES7 exponential expression and await expression
         *
         * ES7 ExponentiationExpression:
         *      1) UnaryExpression[?Yield]
         *      2) UpdateExpression[?Yield] ** ExponentiationExpression[?Yield]
         *
         */
        function parseUnaryExpressionOrHigher() {
            /**
             * ES7 UpdateExpression:
             *      1) LeftHandSideExpression[?Yield]
             *      2) LeftHandSideExpression[?Yield][no LineTerminator here]++
             *      3) LeftHandSideExpression[?Yield][no LineTerminator here]--
             *      4) ++UnaryExpression[?Yield]
             *      5) --UnaryExpression[?Yield]
             */
            if (isUpdateExpression()) {
                var pos = getNodePos();
                var updateExpression = parseUpdateExpression();
                return token() === 42 /* AsteriskAsteriskToken */ ?
                    parseBinaryExpressionRest(ts.getBinaryOperatorPrecedence(token()), updateExpression, pos) :
                    updateExpression;
            }
            /**
             * ES7 UnaryExpression:
             *      1) UpdateExpression[?yield]
             *      2) delete UpdateExpression[?yield]
             *      3) void UpdateExpression[?yield]
             *      4) typeof UpdateExpression[?yield]
             *      5) + UpdateExpression[?yield]
             *      6) - UpdateExpression[?yield]
             *      7) ~ UpdateExpression[?yield]
             *      8) ! UpdateExpression[?yield]
             */
            var unaryOperator = token();
            var simpleUnaryExpression = parseSimpleUnaryExpression();
            if (token() === 42 /* AsteriskAsteriskToken */) {
                var pos = ts.skipTrivia(sourceText, simpleUnaryExpression.pos);
                var end = simpleUnaryExpression.end;
                if (simpleUnaryExpression.kind === 210 /* TypeAssertionExpression */) {
                    parseErrorAt(pos, end, ts.Diagnostics.A_type_assertion_expression_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses);
                }
                else {
                    parseErrorAt(pos, end, ts.Diagnostics.An_unary_expression_with_the_0_operator_is_not_allowed_in_the_left_hand_side_of_an_exponentiation_expression_Consider_enclosing_the_expression_in_parentheses, ts.tokenToString(unaryOperator));
                }
            }
            return simpleUnaryExpression;
        }
        /**
         * Parse ES7 simple-unary expression or higher:
         *
         * ES7 UnaryExpression:
         *      1) UpdateExpression[?yield]
         *      2) delete UnaryExpression[?yield]
         *      3) void UnaryExpression[?yield]
         *      4) typeof UnaryExpression[?yield]
         *      5) + UnaryExpression[?yield]
         *      6) - UnaryExpression[?yield]
         *      7) ~ UnaryExpression[?yield]
         *      8) ! UnaryExpression[?yield]
         *      9) [+Await] await UnaryExpression[?yield]
         */
        function parseSimpleUnaryExpression() {
            switch (token()) {
                case 39 /* PlusToken */:
                case 40 /* MinusToken */:
                case 54 /* TildeToken */:
                case 53 /* ExclamationToken */:
                    return parsePrefixUnaryExpression();
                case 89 /* DeleteKeyword */:
                    return parseDeleteExpression();
                case 112 /* TypeOfKeyword */:
                    return parseTypeOfExpression();
                case 114 /* VoidKeyword */:
                    return parseVoidExpression();
                case 29 /* LessThanToken */:
                    // This is modified UnaryExpression grammar in TypeScript
                    //  UnaryExpression (modified):
                    //      < type > UnaryExpression
                    return parseTypeAssertion();
                case 132 /* AwaitKeyword */:
                    if (isAwaitExpression()) {
                        return parseAwaitExpression();
                    }
                // falls through
                default:
                    return parseUpdateExpression();
            }
        }
        /**
         * Check if the current token can possibly be an ES7 increment expression.
         *
         * ES7 UpdateExpression:
         *      LeftHandSideExpression[?Yield]
         *      LeftHandSideExpression[?Yield][no LineTerminator here]++
         *      LeftHandSideExpression[?Yield][no LineTerminator here]--
         *      ++LeftHandSideExpression[?Yield]
         *      --LeftHandSideExpression[?Yield]
         */
        function isUpdateExpression() {
            // This function is called inside parseUnaryExpression to decide
            // whether to call parseSimpleUnaryExpression or call parseUpdateExpression directly
            switch (token()) {
                case 39 /* PlusToken */:
                case 40 /* MinusToken */:
                case 54 /* TildeToken */:
                case 53 /* ExclamationToken */:
                case 89 /* DeleteKeyword */:
                case 112 /* TypeOfKeyword */:
                case 114 /* VoidKeyword */:
                case 132 /* AwaitKeyword */:
                    return false;
                case 29 /* LessThanToken */:
                    // If we are not in JSX context, we are parsing TypeAssertion which is an UnaryExpression
                    if (languageVariant !== 1 /* JSX */) {
                        return false;
                    }
                // We are in JSX context and the token is part of JSXElement.
                // falls through
                default:
                    return true;
            }
        }
        /**
         * Parse ES7 UpdateExpression. UpdateExpression is used instead of ES6's PostFixExpression.
         *
         * ES7 UpdateExpression[yield]:
         *      1) LeftHandSideExpression[?yield]
         *      2) LeftHandSideExpression[?yield] [[no LineTerminator here]]++
         *      3) LeftHandSideExpression[?yield] [[no LineTerminator here]]--
         *      4) ++LeftHandSideExpression[?yield]
         *      5) --LeftHandSideExpression[?yield]
         * In TypeScript (2), (3) are parsed as PostfixUnaryExpression. (4), (5) are parsed as PrefixUnaryExpression
         */
        function parseUpdateExpression() {
            if (token() === 45 /* PlusPlusToken */ || token() === 46 /* MinusMinusToken */) {
                var pos = getNodePos();
                return finishNode(factory.createPrefixUnaryExpression(token(), nextTokenAnd(parseLeftHandSideExpressionOrHigher)), pos);
            }
            else if (languageVariant === 1 /* JSX */ && token() === 29 /* LessThanToken */ && lookAhead(nextTokenIsIdentifierOrKeywordOrGreaterThan)) {
                // JSXElement is part of primaryExpression
                return parseJsxElementOrSelfClosingElementOrFragment(/*inExpressionContext*/ true);
            }
            var expression = parseLeftHandSideExpressionOrHigher();
            ts.Debug.assert(ts.isLeftHandSideExpression(expression));
            if ((token() === 45 /* PlusPlusToken */ || token() === 46 /* MinusMinusToken */) && !scanner.hasPrecedingLineBreak()) {
                var operator = token();
                nextToken();
                return finishNode(factory.createPostfixUnaryExpression(expression, operator), expression.pos);
            }
            return expression;
        }
        function parseLeftHandSideExpressionOrHigher() {
            // Original Ecma:
            // LeftHandSideExpression: See 11.2
            //      NewExpression
            //      CallExpression
            //
            // Our simplification:
            //
            // LeftHandSideExpression: See 11.2
            //      MemberExpression
            //      CallExpression
            //
            // See comment in parseMemberExpressionOrHigher on how we replaced NewExpression with
            // MemberExpression to make our lives easier.
            //
            // to best understand the below code, it's important to see how CallExpression expands
            // out into its own productions:
            //
            // CallExpression:
            //      MemberExpression Arguments
            //      CallExpression Arguments
            //      CallExpression[Expression]
            //      CallExpression.IdentifierName
            //      import (AssignmentExpression)
            //      super Arguments
            //      super.IdentifierName
            //
            // Because of the recursion in these calls, we need to bottom out first. There are three
            // bottom out states we can run into: 1) We see 'super' which must start either of
            // the last two CallExpression productions. 2) We see 'import' which must start import call.
            // 3)we have a MemberExpression which either completes the LeftHandSideExpression,
            // or starts the beginning of the first four CallExpression productions.
            var pos = getNodePos();
            var expression;
            if (token() === 100 /* ImportKeyword */) {
                if (lookAhead(nextTokenIsOpenParenOrLessThan)) {
                    // We don't want to eagerly consume all import keyword as import call expression so we look ahead to find "("
                    // For example:
                    //      var foo3 = require("subfolder
                    //      import * as foo1 from "module-from-node
                    // We want this import to be a statement rather than import call expression
                    sourceFlags |= 1048576 /* PossiblyContainsDynamicImport */;
                    expression = parseTokenNode();
                }
                else if (lookAhead(nextTokenIsDot)) {
                    // This is an 'import.*' metaproperty (i.e. 'import.meta')
                    nextToken(); // advance past the 'import'
                    nextToken(); // advance past the dot
                    expression = finishNode(factory.createMetaProperty(100 /* ImportKeyword */, parseIdentifierName()), pos);
                    sourceFlags |= 2097152 /* PossiblyContainsImportMeta */;
                }
                else {
                    expression = parseMemberExpressionOrHigher();
                }
            }
            else {
                expression = token() === 106 /* SuperKeyword */ ? parseSuperExpression() : parseMemberExpressionOrHigher();
            }
            // Now, we *may* be complete.  However, we might have consumed the start of a
            // CallExpression or OptionalExpression.  As such, we need to consume the rest
            // of it here to be complete.
            return parseCallExpressionRest(pos, expression);
        }
        function parseMemberExpressionOrHigher() {
            // Note: to make our lives simpler, we decompose the NewExpression productions and
            // place ObjectCreationExpression and FunctionExpression into PrimaryExpression.
            // like so:
            //
            //   PrimaryExpression : See 11.1
            //      this
            //      Identifier
            //      Literal
            //      ArrayLiteral
            //      ObjectLiteral
            //      (Expression)
            //      FunctionExpression
            //      new MemberExpression Arguments?
            //
            //   MemberExpression : See 11.2
            //      PrimaryExpression
            //      MemberExpression[Expression]
            //      MemberExpression.IdentifierName
            //
            //   CallExpression : See 11.2
            //      MemberExpression
            //      CallExpression Arguments
            //      CallExpression[Expression]
            //      CallExpression.IdentifierName
            //
            // Technically this is ambiguous.  i.e. CallExpression defines:
            //
            //   CallExpression:
            //      CallExpression Arguments
            //
            // If you see: "new Foo()"
            //
            // Then that could be treated as a single ObjectCreationExpression, or it could be
            // treated as the invocation of "new Foo".  We disambiguate that in code (to match
            // the original grammar) by making sure that if we see an ObjectCreationExpression
            // we always consume arguments if they are there. So we treat "new Foo()" as an
            // object creation only, and not at all as an invocation.  Another way to think
            // about this is that for every "new" that we see, we will consume an argument list if
            // it is there as part of the *associated* object creation node.  Any additional
            // argument lists we see, will become invocation expressions.
            //
            // Because there are no other places in the grammar now that refer to FunctionExpression
            // or ObjectCreationExpression, it is safe to push down into the PrimaryExpression
            // production.
            //
            // Because CallExpression and MemberExpression are left recursive, we need to bottom out
            // of the recursion immediately.  So we parse out a primary expression to start with.
            var pos = getNodePos();
            var expression = parsePrimaryExpression();
            return parseMemberExpressionRest(pos, expression, /*allowOptionalChain*/ true);
        }
        function parseSuperExpression() {
            var pos = getNodePos();
            var expression = parseTokenNode();
            if (token() === 29 /* LessThanToken */) {
                var startPos = getNodePos();
                var typeArguments = tryParse(parseTypeArgumentsInExpression);
                if (typeArguments !== undefined) {
                    parseErrorAt(startPos, getNodePos(), ts.Diagnostics.super_may_not_use_type_arguments);
                }
            }
            if (token() === 20 /* OpenParenToken */ || token() === 24 /* DotToken */ || token() === 22 /* OpenBracketToken */) {
                return expression;
            }
            // If we have seen "super" it must be followed by '(' or '.'.
            // If it wasn't then just try to parse out a '.' and report an error.
            parseExpectedToken(24 /* DotToken */, ts.Diagnostics.super_must_be_followed_by_an_argument_list_or_member_access);
            // private names will never work with `super` (`super.#foo`), but that's a semantic error, not syntactic
            return finishNode(factory.createPropertyAccessExpression(expression, parseRightSideOfDot(/*allowIdentifierNames*/ true, /*allowPrivateIdentifiers*/ true)), pos);
        }
        function parseJsxElementOrSelfClosingElementOrFragment(inExpressionContext, topInvalidNodePosition, openingTag) {
            var pos = getNodePos();
            var opening = parseJsxOpeningOrSelfClosingElementOrOpeningFragment(inExpressionContext);
            var result;
            if (opening.kind === 279 /* JsxOpeningElement */) {
                var children = parseJsxChildren(opening);
                var closingElement = void 0;
                var lastChild = children[children.length - 1];
                if ((lastChild === null || lastChild === void 0 ? void 0 : lastChild.kind) === 277 /* JsxElement */
                    && !tagNamesAreEquivalent(lastChild.openingElement.tagName, lastChild.closingElement.tagName)
                    && tagNamesAreEquivalent(opening.tagName, lastChild.closingElement.tagName)) {
                    // when an unclosed JsxOpeningElement incorrectly parses its parent's JsxClosingElement,
                    // restructure (<div>(...<span>...</div>)) --> (<div>(...<span>...</>)</div>)
                    // (no need to error; the parent will error)
                    var end = lastChild.children.end;
                    var newLast = finishNode(factory.createJsxElement(lastChild.openingElement, lastChild.children, finishNode(factory.createJsxClosingElement(finishNode(factory.createIdentifier(""), end, end)), end, end)), lastChild.openingElement.pos, end);
                    children = createNodeArray(__spreadArray(__spreadArray([], children.slice(0, children.length - 1), true), [newLast], false), children.pos, end);
                    closingElement = lastChild.closingElement;
                }
                else {
                    closingElement = parseJsxClosingElement(opening, inExpressionContext);
                    if (!tagNamesAreEquivalent(opening.tagName, closingElement.tagName)) {
                        if (openingTag && ts.isJsxOpeningElement(openingTag) && tagNamesAreEquivalent(closingElement.tagName, openingTag.tagName)) {
                            // opening incorrectly matched with its parent's closing -- put error on opening
                            parseErrorAtRange(opening.tagName, ts.Diagnostics.JSX_element_0_has_no_corresponding_closing_tag, ts.getTextOfNodeFromSourceText(sourceText, opening.tagName));
                        }
                        else {
                            // other opening/closing mismatches -- put error on closing
                            parseErrorAtRange(closingElement.tagName, ts.Diagnostics.Expected_corresponding_JSX_closing_tag_for_0, ts.getTextOfNodeFromSourceText(sourceText, opening.tagName));
                        }
                    }
                }
                result = finishNode(factory.createJsxElement(opening, children, closingElement), pos);
            }
            else if (opening.kind === 282 /* JsxOpeningFragment */) {
                result = finishNode(factory.createJsxFragment(opening, parseJsxChildren(opening), parseJsxClosingFragment(inExpressionContext)), pos);
            }
            else {
                ts.Debug.assert(opening.kind === 278 /* JsxSelfClosingElement */);
                // Nothing else to do for self-closing elements
                result = opening;
            }
            // If the user writes the invalid code '<div></div><div></div>' in an expression context (i.e. not wrapped in
            // an enclosing tag), we'll naively try to parse   ^ this as a 'less than' operator and the remainder of the tag
            // as garbage, which will cause the formatter to badly mangle the JSX. Perform a speculative parse of a JSX
            // element if we see a < token so that we can wrap it in a synthetic binary expression so the formatter
            // does less damage and we can report a better error.
            // Since JSX elements are invalid < operands anyway, this lookahead parse will only occur in error scenarios
            // of one sort or another.
            if (inExpressionContext && token() === 29 /* LessThanToken */) {
                var topBadPos_1 = typeof topInvalidNodePosition === "undefined" ? result.pos : topInvalidNodePosition;
                var invalidElement = tryParse(function () { return parseJsxElementOrSelfClosingElementOrFragment(/*inExpressionContext*/ true, topBadPos_1); });
                if (invalidElement) {
                    var operatorToken = createMissingNode(27 /* CommaToken */, /*reportAtCurrentPosition*/ false);
                    ts.setTextRangePosWidth(operatorToken, invalidElement.pos, 0);
                    parseErrorAt(ts.skipTrivia(sourceText, topBadPos_1), invalidElement.end, ts.Diagnostics.JSX_expressions_must_have_one_parent_element);
                    return finishNode(factory.createBinaryExpression(result, operatorToken, invalidElement), pos);
                }
            }
            return result;
        }
        function parseJsxText() {
            var pos = getNodePos();
            var node = factory.createJsxText(scanner.getTokenValue(), currentToken === 12 /* JsxTextAllWhiteSpaces */);
            currentToken = scanner.scanJsxToken();
            return finishNode(node, pos);
        }
        function parseJsxChild(openingTag, token) {
            switch (token) {
                case 1 /* EndOfFileToken */:
                    // If we hit EOF, issue the error at the tag that lacks the closing element
                    // rather than at the end of the file (which is useless)
                    if (ts.isJsxOpeningFragment(openingTag)) {
                        parseErrorAtRange(openingTag, ts.Diagnostics.JSX_fragment_has_no_corresponding_closing_tag);
                    }
                    else {
                        // We want the error span to cover only 'Foo.Bar' in < Foo.Bar >
                        // or to cover only 'Foo' in < Foo >
                        var tag = openingTag.tagName;
                        var start = ts.skipTrivia(sourceText, tag.pos);
                        parseErrorAt(start, tag.end, ts.Diagnostics.JSX_element_0_has_no_corresponding_closing_tag, ts.getTextOfNodeFromSourceText(sourceText, openingTag.tagName));
                    }
                    return undefined;
                case 30 /* LessThanSlashToken */:
                case 7 /* ConflictMarkerTrivia */:
                    return undefined;
                case 11 /* JsxText */:
                case 12 /* JsxTextAllWhiteSpaces */:
                    return parseJsxText();
                case 18 /* OpenBraceToken */:
                    return parseJsxExpression(/*inExpressionContext*/ false);
                case 29 /* LessThanToken */:
                    return parseJsxElementOrSelfClosingElementOrFragment(/*inExpressionContext*/ false, /*topInvalidNodePosition*/ undefined, openingTag);
                default:
                    return ts.Debug.assertNever(token);
            }
        }
        function parseJsxChildren(openingTag) {
            var list = [];
            var listPos = getNodePos();
            var saveParsingContext = parsingContext;
            parsingContext |= 1 << 14 /* JsxChildren */;
            while (true) {
                var child = parseJsxChild(openingTag, currentToken = scanner.reScanJsxToken());
                if (!child)
                    break;
                list.push(child);
                if (ts.isJsxOpeningElement(openingTag)
                    && (child === null || child === void 0 ? void 0 : child.kind) === 277 /* JsxElement */
                    && !tagNamesAreEquivalent(child.openingElement.tagName, child.closingElement.tagName)
                    && tagNamesAreEquivalent(openingTag.tagName, child.closingElement.tagName)) {
                    // stop after parsing a mismatched child like <div>...(<span></div>) in order to reattach the </div> higher
                    break;
                }
            }
            parsingContext = saveParsingContext;
            return createNodeArray(list, listPos);
        }
        function parseJsxAttributes() {
            var pos = getNodePos();
            return finishNode(factory.createJsxAttributes(parseList(13 /* JsxAttributes */, parseJsxAttribute)), pos);
        }
        function parseJsxOpeningOrSelfClosingElementOrOpeningFragment(inExpressionContext) {
            var pos = getNodePos();
            parseExpected(29 /* LessThanToken */);
            if (token() === 31 /* GreaterThanToken */) {
                // See below for explanation of scanJsxText
                scanJsxText();
                return finishNode(factory.createJsxOpeningFragment(), pos);
            }
            var tagName = parseJsxElementName();
            var typeArguments = (contextFlags & 131072 /* JavaScriptFile */) === 0 ? tryParseTypeArguments() : undefined;
            var attributes = parseJsxAttributes();
            var node;
            if (token() === 31 /* GreaterThanToken */) {
                // Closing tag, so scan the immediately-following text with the JSX scanning instead
                // of regular scanning to avoid treating illegal characters (e.g. '#') as immediate
                // scanning errors
                scanJsxText();
                node = factory.createJsxOpeningElement(tagName, typeArguments, attributes);
            }
            else {
                parseExpected(43 /* SlashToken */);
                if (parseExpected(31 /* GreaterThanToken */, /*diagnostic*/ undefined, /*shouldAdvance*/ false)) {
                    // manually advance the scanner in order to look for jsx text inside jsx
                    if (inExpressionContext) {
                        nextToken();
                    }
                    else {
                        scanJsxText();
                    }
                }
                node = factory.createJsxSelfClosingElement(tagName, typeArguments, attributes);
            }
            return finishNode(node, pos);
        }
        function parseJsxElementName() {
            var pos = getNodePos();
            scanJsxIdentifier();
            // JsxElement can have name in the form of
            //      propertyAccessExpression
            //      primaryExpression in the form of an identifier and "this" keyword
            // We can't just simply use parseLeftHandSideExpressionOrHigher because then we will start consider class,function etc as a keyword
            // We only want to consider "this" as a primaryExpression
            var expression = token() === 108 /* ThisKeyword */ ?
                parseTokenNode() : parseIdentifierName();
            while (parseOptional(24 /* DotToken */)) {
                expression = finishNode(factory.createPropertyAccessExpression(expression, parseRightSideOfDot(/*allowIdentifierNames*/ true, /*allowPrivateIdentifiers*/ false)), pos);
            }
            return expression;
        }
        function parseJsxExpression(inExpressionContext) {
            var pos = getNodePos();
            if (!parseExpected(18 /* OpenBraceToken */)) {
                return undefined;
            }
            var dotDotDotToken;
            var expression;
            if (token() !== 19 /* CloseBraceToken */) {
                dotDotDotToken = parseOptionalToken(25 /* DotDotDotToken */);
                // Only an AssignmentExpression is valid here per the JSX spec,
                // but we can unambiguously parse a comma sequence and provide
                // a better error message in grammar checking.
                expression = parseExpression();
            }
            if (inExpressionContext) {
                parseExpected(19 /* CloseBraceToken */);
            }
            else {
                if (parseExpected(19 /* CloseBraceToken */, /*message*/ undefined, /*shouldAdvance*/ false)) {
                    scanJsxText();
                }
            }
            return finishNode(factory.createJsxExpression(dotDotDotToken, expression), pos);
        }
        function parseJsxAttribute() {
            if (token() === 18 /* OpenBraceToken */) {
                return parseJsxSpreadAttribute();
            }
            scanJsxIdentifier();
            var pos = getNodePos();
            return finishNode(factory.createJsxAttribute(parseIdentifierName(), token() !== 63 /* EqualsToken */ ? undefined :
                scanJsxAttributeValue() === 10 /* StringLiteral */ ? parseLiteralNode() :
                    parseJsxExpression(/*inExpressionContext*/ true)), pos);
        }
        function parseJsxSpreadAttribute() {
            var pos = getNodePos();
            parseExpected(18 /* OpenBraceToken */);
            parseExpected(25 /* DotDotDotToken */);
            var expression = parseExpression();
            parseExpected(19 /* CloseBraceToken */);
            return finishNode(factory.createJsxSpreadAttribute(expression), pos);
        }
        function parseJsxClosingElement(open, inExpressionContext) {
            var pos = getNodePos();
            parseExpected(30 /* LessThanSlashToken */);
            var tagName = parseJsxElementName();
            if (parseExpected(31 /* GreaterThanToken */, /*diagnostic*/ undefined, /*shouldAdvance*/ false)) {
                // manually advance the scanner in order to look for jsx text inside jsx
                if (inExpressionContext || !tagNamesAreEquivalent(open.tagName, tagName)) {
                    nextToken();
                }
                else {
                    scanJsxText();
                }
            }
            return finishNode(factory.createJsxClosingElement(tagName), pos);
        }
        function parseJsxClosingFragment(inExpressionContext) {
            var pos = getNodePos();
            parseExpected(30 /* LessThanSlashToken */);
            if (ts.tokenIsIdentifierOrKeyword(token())) {
                parseErrorAtRange(parseJsxElementName(), ts.Diagnostics.Expected_corresponding_closing_tag_for_JSX_fragment);
            }
            if (parseExpected(31 /* GreaterThanToken */, /*diagnostic*/ undefined, /*shouldAdvance*/ false)) {
                // manually advance the scanner in order to look for jsx text inside jsx
                if (inExpressionContext) {
                    nextToken();
                }
                else {
                    scanJsxText();
                }
            }
            return finishNode(factory.createJsxJsxClosingFragment(), pos);
        }
        function parseTypeAssertion() {
            var pos = getNodePos();
            parseExpected(29 /* LessThanToken */);
            var type = parseType();
            parseExpected(31 /* GreaterThanToken */);
            var expression = parseSimpleUnaryExpression();
            return finishNode(factory.createTypeAssertion(type, expression), pos);
        }
        function nextTokenIsIdentifierOrKeywordOrOpenBracketOrTemplate() {
            nextToken();
            return ts.tokenIsIdentifierOrKeyword(token())
                || token() === 22 /* OpenBracketToken */
                || isTemplateStartOfTaggedTemplate();
        }
        function isStartOfOptionalPropertyOrElementAccessChain() {
            return token() === 28 /* QuestionDotToken */
                && lookAhead(nextTokenIsIdentifierOrKeywordOrOpenBracketOrTemplate);
        }
        function tryReparseOptionalChain(node) {
            if (node.flags & 32 /* OptionalChain */) {
                return true;
            }
            // check for an optional chain in a non-null expression
            if (ts.isNonNullExpression(node)) {
                var expr = node.expression;
                while (ts.isNonNullExpression(expr) && !(expr.flags & 32 /* OptionalChain */)) {
                    expr = expr.expression;
                }
                if (expr.flags & 32 /* OptionalChain */) {
                    // this is part of an optional chain. Walk down from `node` to `expression` and set the flag.
                    while (ts.isNonNullExpression(node)) {
                        node.flags |= 32 /* OptionalChain */;
                        node = node.expression;
                    }
                    return true;
                }
            }
            return false;
        }
        function parsePropertyAccessExpressionRest(pos, expression, questionDotToken) {
            var name = parseRightSideOfDot(/*allowIdentifierNames*/ true, /*allowPrivateIdentifiers*/ true);
            var isOptionalChain = questionDotToken || tryReparseOptionalChain(expression);
            var propertyAccess = isOptionalChain ?
                factory.createPropertyAccessChain(expression, questionDotToken, name) :
                factory.createPropertyAccessExpression(expression, name);
            if (isOptionalChain && ts.isPrivateIdentifier(propertyAccess.name)) {
                parseErrorAtRange(propertyAccess.name, ts.Diagnostics.An_optional_chain_cannot_contain_private_identifiers);
            }
            return finishNode(propertyAccess, pos);
        }
        function parseElementAccessExpressionRest(pos, expression, questionDotToken) {
            var argumentExpression;
            if (token() === 23 /* CloseBracketToken */) {
                argumentExpression = createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ true, ts.Diagnostics.An_element_access_expression_should_take_an_argument);
            }
            else {
                var argument = allowInAnd(parseExpression);
                if (ts.isStringOrNumericLiteralLike(argument)) {
                    argument.text = internIdentifier(argument.text);
                }
                argumentExpression = argument;
            }
            parseExpected(23 /* CloseBracketToken */);
            var indexedAccess = questionDotToken || tryReparseOptionalChain(expression) ?
                factory.createElementAccessChain(expression, questionDotToken, argumentExpression) :
                factory.createElementAccessExpression(expression, argumentExpression);
            return finishNode(indexedAccess, pos);
        }
        function parseMemberExpressionRest(pos, expression, allowOptionalChain) {
            while (true) {
                var questionDotToken = void 0;
                var isPropertyAccess = false;
                if (allowOptionalChain && isStartOfOptionalPropertyOrElementAccessChain()) {
                    questionDotToken = parseExpectedToken(28 /* QuestionDotToken */);
                    isPropertyAccess = ts.tokenIsIdentifierOrKeyword(token());
                }
                else {
                    isPropertyAccess = parseOptional(24 /* DotToken */);
                }
                if (isPropertyAccess) {
                    expression = parsePropertyAccessExpressionRest(pos, expression, questionDotToken);
                    continue;
                }
                if (!questionDotToken && token() === 53 /* ExclamationToken */ && !scanner.hasPrecedingLineBreak()) {
                    nextToken();
                    expression = finishNode(factory.createNonNullExpression(expression), pos);
                    continue;
                }
                // when in the [Decorator] context, we do not parse ElementAccess as it could be part of a ComputedPropertyName
                if ((questionDotToken || !inDecoratorContext()) && parseOptional(22 /* OpenBracketToken */)) {
                    expression = parseElementAccessExpressionRest(pos, expression, questionDotToken);
                    continue;
                }
                if (isTemplateStartOfTaggedTemplate()) {
                    expression = parseTaggedTemplateRest(pos, expression, questionDotToken, /*typeArguments*/ undefined);
                    continue;
                }
                return expression;
            }
        }
        function isTemplateStartOfTaggedTemplate() {
            return token() === 14 /* NoSubstitutionTemplateLiteral */ || token() === 15 /* TemplateHead */;
        }
        function parseTaggedTemplateRest(pos, tag, questionDotToken, typeArguments) {
            var tagExpression = factory.createTaggedTemplateExpression(tag, typeArguments, token() === 14 /* NoSubstitutionTemplateLiteral */ ?
                (reScanTemplateHeadOrNoSubstitutionTemplate(), parseLiteralNode()) :
                parseTemplateExpression(/*isTaggedTemplate*/ true));
            if (questionDotToken || tag.flags & 32 /* OptionalChain */) {
                tagExpression.flags |= 32 /* OptionalChain */;
            }
            tagExpression.questionDotToken = questionDotToken;
            return finishNode(tagExpression, pos);
        }
        function parseCallExpressionRest(pos, expression) {
            while (true) {
                expression = parseMemberExpressionRest(pos, expression, /*allowOptionalChain*/ true);
                var questionDotToken = parseOptionalToken(28 /* QuestionDotToken */);
                // handle 'foo<<T>()'
                // parse template arguments only in TypeScript files (not in JavaScript files).
                if ((contextFlags & 131072 /* JavaScriptFile */) === 0 && (token() === 29 /* LessThanToken */ || token() === 47 /* LessThanLessThanToken */)) {
                    // See if this is the start of a generic invocation.  If so, consume it and
                    // keep checking for postfix expressions.  Otherwise, it's just a '<' that's
                    // part of an arithmetic expression.  Break out so we consume it higher in the
                    // stack.
                    var typeArguments = tryParse(parseTypeArgumentsInExpression);
                    if (typeArguments) {
                        if (isTemplateStartOfTaggedTemplate()) {
                            expression = parseTaggedTemplateRest(pos, expression, questionDotToken, typeArguments);
                            continue;
                        }
                        var argumentList = parseArgumentList();
                        var callExpr = questionDotToken || tryReparseOptionalChain(expression) ?
                            factory.createCallChain(expression, questionDotToken, typeArguments, argumentList) :
                            factory.createCallExpression(expression, typeArguments, argumentList);
                        expression = finishNode(callExpr, pos);
                        continue;
                    }
                }
                else if (token() === 20 /* OpenParenToken */) {
                    var argumentList = parseArgumentList();
                    var callExpr = questionDotToken || tryReparseOptionalChain(expression) ?
                        factory.createCallChain(expression, questionDotToken, /*typeArguments*/ undefined, argumentList) :
                        factory.createCallExpression(expression, /*typeArguments*/ undefined, argumentList);
                    expression = finishNode(callExpr, pos);
                    continue;
                }
                if (questionDotToken) {
                    // We failed to parse anything, so report a missing identifier here.
                    var name = createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ false, ts.Diagnostics.Identifier_expected);
                    expression = finishNode(factory.createPropertyAccessChain(expression, questionDotToken, name), pos);
                }
                break;
            }
            return expression;
        }
        function parseArgumentList() {
            parseExpected(20 /* OpenParenToken */);
            var result = parseDelimitedList(11 /* ArgumentExpressions */, parseArgumentExpression);
            parseExpected(21 /* CloseParenToken */);
            return result;
        }
        function parseTypeArgumentsInExpression() {
            if ((contextFlags & 131072 /* JavaScriptFile */) !== 0) {
                // TypeArguments must not be parsed in JavaScript files to avoid ambiguity with binary operators.
                return undefined;
            }
            if (reScanLessThanToken() !== 29 /* LessThanToken */) {
                return undefined;
            }
            nextToken();
            var typeArguments = parseDelimitedList(20 /* TypeArguments */, parseType);
            if (!parseExpected(31 /* GreaterThanToken */)) {
                // If it doesn't have the closing `>` then it's definitely not an type argument list.
                return undefined;
            }
            // If we have a '<', then only parse this as a argument list if the type arguments
            // are complete and we have an open paren.  if we don't, rewind and return nothing.
            return typeArguments && canFollowTypeArgumentsInExpression()
                ? typeArguments
                : undefined;
        }
        function canFollowTypeArgumentsInExpression() {
            switch (token()) {
                case 20 /* OpenParenToken */: // foo<x>(
                case 14 /* NoSubstitutionTemplateLiteral */: // foo<T> `...`
                case 15 /* TemplateHead */: // foo<T> `...${100}...`
                // these are the only tokens can legally follow a type argument
                // list. So we definitely want to treat them as type arg lists.
                // falls through
                case 24 /* DotToken */: // foo<x>.
                case 21 /* CloseParenToken */: // foo<x>)
                case 23 /* CloseBracketToken */: // foo<x>]
                case 58 /* ColonToken */: // foo<x>:
                case 26 /* SemicolonToken */: // foo<x>;
                case 57 /* QuestionToken */: // foo<x>?
                case 34 /* EqualsEqualsToken */: // foo<x> ==
                case 36 /* EqualsEqualsEqualsToken */: // foo<x> ===
                case 35 /* ExclamationEqualsToken */: // foo<x> !=
                case 37 /* ExclamationEqualsEqualsToken */: // foo<x> !==
                case 55 /* AmpersandAmpersandToken */: // foo<x> &&
                case 56 /* BarBarToken */: // foo<x> ||
                case 60 /* QuestionQuestionToken */: // foo<x> ??
                case 52 /* CaretToken */: // foo<x> ^
                case 50 /* AmpersandToken */: // foo<x> &
                case 51 /* BarToken */: // foo<x> |
                case 19 /* CloseBraceToken */: // foo<x> }
                case 1 /* EndOfFileToken */: // foo<x>
                    // these cases can't legally follow a type arg list.  However, they're not legal
                    // expressions either.  The user is probably in the middle of a generic type. So
                    // treat it as such.
                    return true;
                case 27 /* CommaToken */: // foo<x>,
                case 18 /* OpenBraceToken */: // foo<x> {
                // We don't want to treat these as type arguments.  Otherwise we'll parse this
                // as an invocation expression.  Instead, we want to parse out the expression
                // in isolation from the type arguments.
                // falls through
                default:
                    // Anything else treat as an expression.
                    return false;
            }
        }
        function parsePrimaryExpression() {
            switch (token()) {
                case 8 /* NumericLiteral */:
                case 9 /* BigIntLiteral */:
                case 10 /* StringLiteral */:
                case 14 /* NoSubstitutionTemplateLiteral */:
                    return parseLiteralNode();
                case 108 /* ThisKeyword */:
                case 106 /* SuperKeyword */:
                case 104 /* NullKeyword */:
                case 110 /* TrueKeyword */:
                case 95 /* FalseKeyword */:
                    return parseTokenNode();
                case 20 /* OpenParenToken */:
                    return parseParenthesizedExpression();
                case 22 /* OpenBracketToken */:
                    return parseArrayLiteralExpression();
                case 18 /* OpenBraceToken */:
                    return parseObjectLiteralExpression();
                case 131 /* AsyncKeyword */:
                    // Async arrow functions are parsed earlier in parseAssignmentExpressionOrHigher.
                    // If we encounter `async [no LineTerminator here] function` then this is an async
                    // function; otherwise, its an identifier.
                    if (!lookAhead(nextTokenIsFunctionKeywordOnSameLine)) {
                        break;
                    }
                    return parseFunctionExpression();
                case 84 /* ClassKeyword */:
                    return parseClassExpression();
                case 98 /* FunctionKeyword */:
                    return parseFunctionExpression();
                case 103 /* NewKeyword */:
                    return parseNewExpressionOrNewDotTarget();
                case 43 /* SlashToken */:
                case 68 /* SlashEqualsToken */:
                    if (reScanSlashToken() === 13 /* RegularExpressionLiteral */) {
                        return parseLiteralNode();
                    }
                    break;
                case 15 /* TemplateHead */:
                    return parseTemplateExpression(/* isTaggedTemplate */ false);
                case 80 /* PrivateIdentifier */:
                    return parsePrivateIdentifier();
            }
            return parseIdentifier(ts.Diagnostics.Expression_expected);
        }
        function parseParenthesizedExpression() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(20 /* OpenParenToken */);
            var expression = allowInAnd(parseExpression);
            parseExpected(21 /* CloseParenToken */);
            return withJSDoc(finishNode(factory.createParenthesizedExpression(expression), pos), hasJSDoc);
        }
        function parseSpreadElement() {
            var pos = getNodePos();
            parseExpected(25 /* DotDotDotToken */);
            var expression = parseAssignmentExpressionOrHigher();
            return finishNode(factory.createSpreadElement(expression), pos);
        }
        function parseArgumentOrArrayLiteralElement() {
            return token() === 25 /* DotDotDotToken */ ? parseSpreadElement() :
                token() === 27 /* CommaToken */ ? finishNode(factory.createOmittedExpression(), getNodePos()) :
                    parseAssignmentExpressionOrHigher();
        }
        function parseArgumentExpression() {
            return doOutsideOfContext(disallowInAndDecoratorContext, parseArgumentOrArrayLiteralElement);
        }
        function parseArrayLiteralExpression() {
            var pos = getNodePos();
            parseExpected(22 /* OpenBracketToken */);
            var multiLine = scanner.hasPrecedingLineBreak();
            var elements = parseDelimitedList(15 /* ArrayLiteralMembers */, parseArgumentOrArrayLiteralElement);
            parseExpected(23 /* CloseBracketToken */);
            return finishNode(factory.createArrayLiteralExpression(elements, multiLine), pos);
        }
        function parseObjectLiteralElement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            if (parseOptionalToken(25 /* DotDotDotToken */)) {
                var expression = parseAssignmentExpressionOrHigher();
                return withJSDoc(finishNode(factory.createSpreadAssignment(expression), pos), hasJSDoc);
            }
            var decorators = parseDecorators();
            var modifiers = parseModifiers();
            if (parseContextualModifier(136 /* GetKeyword */)) {
                return parseAccessorDeclaration(pos, hasJSDoc, decorators, modifiers, 171 /* GetAccessor */);
            }
            if (parseContextualModifier(148 /* SetKeyword */)) {
                return parseAccessorDeclaration(pos, hasJSDoc, decorators, modifiers, 172 /* SetAccessor */);
            }
            var asteriskToken = parseOptionalToken(41 /* AsteriskToken */);
            var tokenIsIdentifier = isIdentifier();
            var name = parsePropertyName();
            // Disallowing of optional property assignments and definite assignment assertion happens in the grammar checker.
            var questionToken = parseOptionalToken(57 /* QuestionToken */);
            var exclamationToken = parseOptionalToken(53 /* ExclamationToken */);
            if (asteriskToken || token() === 20 /* OpenParenToken */ || token() === 29 /* LessThanToken */) {
                return parseMethodDeclaration(pos, hasJSDoc, decorators, modifiers, asteriskToken, name, questionToken, exclamationToken);
            }
            // check if it is short-hand property assignment or normal property assignment
            // NOTE: if token is EqualsToken it is interpreted as CoverInitializedName production
            // CoverInitializedName[Yield] :
            //     IdentifierReference[?Yield] Initializer[In, ?Yield]
            // this is necessary because ObjectLiteral productions are also used to cover grammar for ObjectAssignmentPattern
            var node;
            var isShorthandPropertyAssignment = tokenIsIdentifier && (token() !== 58 /* ColonToken */);
            if (isShorthandPropertyAssignment) {
                var equalsToken = parseOptionalToken(63 /* EqualsToken */);
                var objectAssignmentInitializer = equalsToken ? allowInAnd(parseAssignmentExpressionOrHigher) : undefined;
                node = factory.createShorthandPropertyAssignment(name, objectAssignmentInitializer);
                // Save equals token for error reporting.
                // TODO(rbuckton): Consider manufacturing this when we need to report an error as it is otherwise not useful.
                node.equalsToken = equalsToken;
            }
            else {
                parseExpected(58 /* ColonToken */);
                var initializer = allowInAnd(parseAssignmentExpressionOrHigher);
                node = factory.createPropertyAssignment(name, initializer);
            }
            // Decorators, Modifiers, questionToken, and exclamationToken are not supported by property assignments and are reported in the grammar checker
            node.decorators = decorators;
            node.modifiers = modifiers;
            node.questionToken = questionToken;
            node.exclamationToken = exclamationToken;
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseObjectLiteralExpression() {
            var pos = getNodePos();
            var openBracePosition = scanner.getTokenPos();
            parseExpected(18 /* OpenBraceToken */);
            var multiLine = scanner.hasPrecedingLineBreak();
            var properties = parseDelimitedList(12 /* ObjectLiteralMembers */, parseObjectLiteralElement, /*considerSemicolonAsDelimiter*/ true);
            if (!parseExpected(19 /* CloseBraceToken */)) {
                var lastError = ts.lastOrUndefined(parseDiagnostics);
                if (lastError && lastError.code === ts.Diagnostics._0_expected.code) {
                    ts.addRelatedInfo(lastError, ts.createDetachedDiagnostic(fileName, openBracePosition, 1, ts.Diagnostics.The_parser_expected_to_find_a_to_match_the_token_here));
                }
            }
            return finishNode(factory.createObjectLiteralExpression(properties, multiLine), pos);
        }
        function parseFunctionExpression() {
            // GeneratorExpression:
            //      function* BindingIdentifier [Yield][opt](FormalParameters[Yield]){ GeneratorBody }
            //
            // FunctionExpression:
            //      function BindingIdentifier[opt](FormalParameters){ FunctionBody }
            var savedDecoratorContext = inDecoratorContext();
            setDecoratorContext(/*val*/ false);
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var modifiers = parseModifiers();
            parseExpected(98 /* FunctionKeyword */);
            var asteriskToken = parseOptionalToken(41 /* AsteriskToken */);
            var isGenerator = asteriskToken ? 1 /* Yield */ : 0 /* None */;
            var isAsync = ts.some(modifiers, ts.isAsyncModifier) ? 2 /* Await */ : 0 /* None */;
            var name = isGenerator && isAsync ? doInYieldAndAwaitContext(parseOptionalBindingIdentifier) :
                isGenerator ? doInYieldContext(parseOptionalBindingIdentifier) :
                    isAsync ? doInAwaitContext(parseOptionalBindingIdentifier) :
                        parseOptionalBindingIdentifier();
            var typeParameters = parseTypeParameters();
            var parameters = parseParameters(isGenerator | isAsync);
            var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
            var body = parseFunctionBlock(isGenerator | isAsync);
            setDecoratorContext(savedDecoratorContext);
            var node = factory.createFunctionExpression(modifiers, asteriskToken, name, typeParameters, parameters, type, body);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseOptionalBindingIdentifier() {
            return isBindingIdentifier() ? parseBindingIdentifier() : undefined;
        }
        function parseNewExpressionOrNewDotTarget() {
            var pos = getNodePos();
            parseExpected(103 /* NewKeyword */);
            if (parseOptional(24 /* DotToken */)) {
                var name = parseIdentifierName();
                return finishNode(factory.createMetaProperty(103 /* NewKeyword */, name), pos);
            }
            var expressionPos = getNodePos();
            var expression = parsePrimaryExpression();
            var typeArguments;
            while (true) {
                expression = parseMemberExpressionRest(expressionPos, expression, /*allowOptionalChain*/ false);
                typeArguments = tryParse(parseTypeArgumentsInExpression);
                if (isTemplateStartOfTaggedTemplate()) {
                    ts.Debug.assert(!!typeArguments, "Expected a type argument list; all plain tagged template starts should be consumed in 'parseMemberExpressionRest'");
                    expression = parseTaggedTemplateRest(expressionPos, expression, /*optionalChain*/ undefined, typeArguments);
                    typeArguments = undefined;
                }
                break;
            }
            var argumentsArray;
            if (token() === 20 /* OpenParenToken */) {
                argumentsArray = parseArgumentList();
            }
            else if (typeArguments) {
                parseErrorAt(pos, scanner.getStartPos(), ts.Diagnostics.A_new_expression_with_type_arguments_must_always_be_followed_by_a_parenthesized_argument_list);
            }
            return finishNode(factory.createNewExpression(expression, typeArguments, argumentsArray), pos);
        }
        // STATEMENTS
        function parseBlock(ignoreMissingOpenBrace, diagnosticMessage) {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var openBracePosition = scanner.getTokenPos();
            if (parseExpected(18 /* OpenBraceToken */, diagnosticMessage) || ignoreMissingOpenBrace) {
                var multiLine = scanner.hasPrecedingLineBreak();
                var statements = parseList(1 /* BlockStatements */, parseStatement);
                if (!parseExpected(19 /* CloseBraceToken */)) {
                    var lastError = ts.lastOrUndefined(parseDiagnostics);
                    if (lastError && lastError.code === ts.Diagnostics._0_expected.code) {
                        ts.addRelatedInfo(lastError, ts.createDetachedDiagnostic(fileName, openBracePosition, 1, ts.Diagnostics.The_parser_expected_to_find_a_to_match_the_token_here));
                    }
                }
                var result = withJSDoc(finishNode(factory.createBlock(statements, multiLine), pos), hasJSDoc);
                if (token() === 63 /* EqualsToken */) {
                    parseErrorAtCurrentToken(ts.Diagnostics.Declaration_or_statement_expected_This_follows_a_block_of_statements_so_if_you_intended_to_write_a_destructuring_assignment_you_might_need_to_wrap_the_the_whole_assignment_in_parentheses);
                    nextToken();
                }
                return result;
            }
            else {
                var statements = createMissingList();
                return withJSDoc(finishNode(factory.createBlock(statements, /*multiLine*/ undefined), pos), hasJSDoc);
            }
        }
        function parseFunctionBlock(flags, diagnosticMessage) {
            var savedYieldContext = inYieldContext();
            setYieldContext(!!(flags & 1 /* Yield */));
            var savedAwaitContext = inAwaitContext();
            setAwaitContext(!!(flags & 2 /* Await */));
            var savedTopLevel = topLevel;
            topLevel = false;
            // We may be in a [Decorator] context when parsing a function expression or
            // arrow function. The body of the function is not in [Decorator] context.
            var saveDecoratorContext = inDecoratorContext();
            if (saveDecoratorContext) {
                setDecoratorContext(/*val*/ false);
            }
            var block = parseBlock(!!(flags & 16 /* IgnoreMissingOpenBrace */), diagnosticMessage);
            if (saveDecoratorContext) {
                setDecoratorContext(/*val*/ true);
            }
            topLevel = savedTopLevel;
            setYieldContext(savedYieldContext);
            setAwaitContext(savedAwaitContext);
            return block;
        }
        function parseEmptyStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(26 /* SemicolonToken */);
            return withJSDoc(finishNode(factory.createEmptyStatement(), pos), hasJSDoc);
        }
        function parseIfStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(99 /* IfKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var expression = allowInAnd(parseExpression);
            parseExpected(21 /* CloseParenToken */);
            var thenStatement = parseStatement();
            var elseStatement = parseOptional(91 /* ElseKeyword */) ? parseStatement() : undefined;
            return withJSDoc(finishNode(factory.createIfStatement(expression, thenStatement, elseStatement), pos), hasJSDoc);
        }
        function parseDoStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(90 /* DoKeyword */);
            var statement = parseStatement();
            parseExpected(115 /* WhileKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var expression = allowInAnd(parseExpression);
            parseExpected(21 /* CloseParenToken */);
            // From: https://mail.mozilla.org/pipermail/es-discuss/2011-August/016188.html
            // 157 min --- All allen at wirfs-brock.com CONF --- "do{;}while(false)false" prohibited in
            // spec but allowed in consensus reality. Approved -- this is the de-facto standard whereby
            //  do;while(0)x will have a semicolon inserted before x.
            parseOptional(26 /* SemicolonToken */);
            return withJSDoc(finishNode(factory.createDoStatement(statement, expression), pos), hasJSDoc);
        }
        function parseWhileStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(115 /* WhileKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var expression = allowInAnd(parseExpression);
            parseExpected(21 /* CloseParenToken */);
            var statement = parseStatement();
            return withJSDoc(finishNode(factory.createWhileStatement(expression, statement), pos), hasJSDoc);
        }
        function parseForOrForInOrForOfStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(97 /* ForKeyword */);
            var awaitToken = parseOptionalToken(132 /* AwaitKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var initializer;
            if (token() !== 26 /* SemicolonToken */) {
                if (token() === 113 /* VarKeyword */ || token() === 119 /* LetKeyword */ || token() === 85 /* ConstKeyword */) {
                    initializer = parseVariableDeclarationList(/*inForStatementInitializer*/ true);
                }
                else {
                    initializer = disallowInAnd(parseExpression);
                }
            }
            var node;
            if (awaitToken ? parseExpected(159 /* OfKeyword */) : parseOptional(159 /* OfKeyword */)) {
                var expression = allowInAnd(parseAssignmentExpressionOrHigher);
                parseExpected(21 /* CloseParenToken */);
                node = factory.createForOfStatement(awaitToken, initializer, expression, parseStatement());
            }
            else if (parseOptional(101 /* InKeyword */)) {
                var expression = allowInAnd(parseExpression);
                parseExpected(21 /* CloseParenToken */);
                node = factory.createForInStatement(initializer, expression, parseStatement());
            }
            else {
                parseExpected(26 /* SemicolonToken */);
                var condition = token() !== 26 /* SemicolonToken */ && token() !== 21 /* CloseParenToken */
                    ? allowInAnd(parseExpression)
                    : undefined;
                parseExpected(26 /* SemicolonToken */);
                var incrementor = token() !== 21 /* CloseParenToken */
                    ? allowInAnd(parseExpression)
                    : undefined;
                parseExpected(21 /* CloseParenToken */);
                node = factory.createForStatement(initializer, condition, incrementor, parseStatement());
            }
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseBreakOrContinueStatement(kind) {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(kind === 245 /* BreakStatement */ ? 81 /* BreakKeyword */ : 86 /* ContinueKeyword */);
            var label = canParseSemicolon() ? undefined : parseIdentifier();
            parseSemicolon();
            var node = kind === 245 /* BreakStatement */
                ? factory.createBreakStatement(label)
                : factory.createContinueStatement(label);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseReturnStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(105 /* ReturnKeyword */);
            var expression = canParseSemicolon() ? undefined : allowInAnd(parseExpression);
            parseSemicolon();
            return withJSDoc(finishNode(factory.createReturnStatement(expression), pos), hasJSDoc);
        }
        function parseWithStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(116 /* WithKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var expression = allowInAnd(parseExpression);
            parseExpected(21 /* CloseParenToken */);
            var statement = doInsideOfContext(16777216 /* InWithStatement */, parseStatement);
            return withJSDoc(finishNode(factory.createWithStatement(expression, statement), pos), hasJSDoc);
        }
        function parseCaseClause() {
            var pos = getNodePos();
            parseExpected(82 /* CaseKeyword */);
            var expression = allowInAnd(parseExpression);
            parseExpected(58 /* ColonToken */);
            var statements = parseList(3 /* SwitchClauseStatements */, parseStatement);
            return finishNode(factory.createCaseClause(expression, statements), pos);
        }
        function parseDefaultClause() {
            var pos = getNodePos();
            parseExpected(88 /* DefaultKeyword */);
            parseExpected(58 /* ColonToken */);
            var statements = parseList(3 /* SwitchClauseStatements */, parseStatement);
            return finishNode(factory.createDefaultClause(statements), pos);
        }
        function parseCaseOrDefaultClause() {
            return token() === 82 /* CaseKeyword */ ? parseCaseClause() : parseDefaultClause();
        }
        function parseCaseBlock() {
            var pos = getNodePos();
            parseExpected(18 /* OpenBraceToken */);
            var clauses = parseList(2 /* SwitchClauses */, parseCaseOrDefaultClause);
            parseExpected(19 /* CloseBraceToken */);
            return finishNode(factory.createCaseBlock(clauses), pos);
        }
        function parseSwitchStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(107 /* SwitchKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var expression = allowInAnd(parseExpression);
            parseExpected(21 /* CloseParenToken */);
            var caseBlock = parseCaseBlock();
            return withJSDoc(finishNode(factory.createSwitchStatement(expression, caseBlock), pos), hasJSDoc);
        }
        function parseThrowStatement() {
            // ThrowStatement[Yield] :
            //      throw [no LineTerminator here]Expression[In, ?Yield];
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(109 /* ThrowKeyword */);
            // Because of automatic semicolon insertion, we need to report error if this
            // throw could be terminated with a semicolon.  Note: we can't call 'parseExpression'
            // directly as that might consume an expression on the following line.
            // Instead, we create a "missing" identifier, but don't report an error. The actual error
            // will be reported in the grammar walker.
            var expression = scanner.hasPrecedingLineBreak() ? undefined : allowInAnd(parseExpression);
            if (expression === undefined) {
                identifierCount++;
                expression = finishNode(factory.createIdentifier(""), getNodePos());
            }
            if (!tryParseSemicolon()) {
                parseErrorForMissingSemicolonAfter(expression);
            }
            return withJSDoc(finishNode(factory.createThrowStatement(expression), pos), hasJSDoc);
        }
        // TODO: Review for error recovery
        function parseTryStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(111 /* TryKeyword */);
            var tryBlock = parseBlock(/*ignoreMissingOpenBrace*/ false);
            var catchClause = token() === 83 /* CatchKeyword */ ? parseCatchClause() : undefined;
            // If we don't have a catch clause, then we must have a finally clause.  Try to parse
            // one out no matter what.
            var finallyBlock;
            if (!catchClause || token() === 96 /* FinallyKeyword */) {
                parseExpected(96 /* FinallyKeyword */, ts.Diagnostics.catch_or_finally_expected);
                finallyBlock = parseBlock(/*ignoreMissingOpenBrace*/ false);
            }
            return withJSDoc(finishNode(factory.createTryStatement(tryBlock, catchClause, finallyBlock), pos), hasJSDoc);
        }
        function parseCatchClause() {
            var pos = getNodePos();
            parseExpected(83 /* CatchKeyword */);
            var variableDeclaration;
            if (parseOptional(20 /* OpenParenToken */)) {
                variableDeclaration = parseVariableDeclaration();
                parseExpected(21 /* CloseParenToken */);
            }
            else {
                // Keep shape of node to avoid degrading performance.
                variableDeclaration = undefined;
            }
            var block = parseBlock(/*ignoreMissingOpenBrace*/ false);
            return finishNode(factory.createCatchClause(variableDeclaration, block), pos);
        }
        function parseDebuggerStatement() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            parseExpected(87 /* DebuggerKeyword */);
            parseSemicolon();
            return withJSDoc(finishNode(factory.createDebuggerStatement(), pos), hasJSDoc);
        }
        function parseExpressionOrLabeledStatement() {
            // Avoiding having to do the lookahead for a labeled statement by just trying to parse
            // out an expression, seeing if it is identifier and then seeing if it is followed by
            // a colon.
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var node;
            var hasParen = token() === 20 /* OpenParenToken */;
            var expression = allowInAnd(parseExpression);
            if (ts.isIdentifier(expression) && parseOptional(58 /* ColonToken */)) {
                node = factory.createLabeledStatement(expression, parseStatement());
            }
            else {
                if (!tryParseSemicolon()) {
                    parseErrorForMissingSemicolonAfter(expression);
                }
                node = factory.createExpressionStatement(expression);
                if (hasParen) {
                    // do not parse the same jsdoc twice
                    hasJSDoc = false;
                }
            }
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function nextTokenIsIdentifierOrKeywordOnSameLine() {
            nextToken();
            return ts.tokenIsIdentifierOrKeyword(token()) && !scanner.hasPrecedingLineBreak();
        }
        function nextTokenIsClassKeywordOnSameLine() {
            nextToken();
            return token() === 84 /* ClassKeyword */ && !scanner.hasPrecedingLineBreak();
        }
        function nextTokenIsFunctionKeywordOnSameLine() {
            nextToken();
            return token() === 98 /* FunctionKeyword */ && !scanner.hasPrecedingLineBreak();
        }
        function nextTokenIsIdentifierOrKeywordOrLiteralOnSameLine() {
            nextToken();
            return (ts.tokenIsIdentifierOrKeyword(token()) || token() === 8 /* NumericLiteral */ || token() === 9 /* BigIntLiteral */ || token() === 10 /* StringLiteral */) && !scanner.hasPrecedingLineBreak();
        }
        function isDeclaration() {
            while (true) {
                switch (token()) {
                    case 113 /* VarKeyword */:
                    case 119 /* LetKeyword */:
                    case 85 /* ConstKeyword */:
                    case 98 /* FunctionKeyword */:
                    case 84 /* ClassKeyword */:
                    case 92 /* EnumKeyword */:
                        return true;
                    // 'declare', 'module', 'namespace', 'interface'* and 'type' are all legal JavaScript identifiers;
                    // however, an identifier cannot be followed by another identifier on the same line. This is what we
                    // count on to parse out the respective declarations. For instance, we exploit this to say that
                    //
                    //    namespace n
                    //
                    // can be none other than the beginning of a namespace declaration, but need to respect that JavaScript sees
                    //
                    //    namespace
                    //    n
                    //
                    // as the identifier 'namespace' on one line followed by the identifier 'n' on another.
                    // We need to look one token ahead to see if it permissible to try parsing a declaration.
                    //
                    // *Note*: 'interface' is actually a strict mode reserved word. So while
                    //
                    //   "use strict"
                    //   interface
                    //   I {}
                    //
                    // could be legal, it would add complexity for very little gain.
                    case 118 /* InterfaceKeyword */:
                    case 151 /* TypeKeyword */:
                        return nextTokenIsIdentifierOnSameLine();
                    case 141 /* ModuleKeyword */:
                    case 142 /* NamespaceKeyword */:
                        return nextTokenIsIdentifierOrStringLiteralOnSameLine();
                    case 126 /* AbstractKeyword */:
                    case 131 /* AsyncKeyword */:
                    case 135 /* DeclareKeyword */:
                    case 121 /* PrivateKeyword */:
                    case 122 /* ProtectedKeyword */:
                    case 123 /* PublicKeyword */:
                    case 144 /* ReadonlyKeyword */:
                        nextToken();
                        // ASI takes effect for this modifier.
                        if (scanner.hasPrecedingLineBreak()) {
                            return false;
                        }
                        continue;
                    case 156 /* GlobalKeyword */:
                        nextToken();
                        return token() === 18 /* OpenBraceToken */ || token() === 79 /* Identifier */ || token() === 93 /* ExportKeyword */;
                    case 100 /* ImportKeyword */:
                        nextToken();
                        return token() === 10 /* StringLiteral */ || token() === 41 /* AsteriskToken */ ||
                            token() === 18 /* OpenBraceToken */ || ts.tokenIsIdentifierOrKeyword(token());
                    case 93 /* ExportKeyword */:
                        var currentToken_1 = nextToken();
                        if (currentToken_1 === 151 /* TypeKeyword */) {
                            currentToken_1 = lookAhead(nextToken);
                        }
                        if (currentToken_1 === 63 /* EqualsToken */ || currentToken_1 === 41 /* AsteriskToken */ ||
                            currentToken_1 === 18 /* OpenBraceToken */ || currentToken_1 === 88 /* DefaultKeyword */ ||
                            currentToken_1 === 127 /* AsKeyword */) {
                            return true;
                        }
                        continue;
                    case 124 /* StaticKeyword */:
                        nextToken();
                        continue;
                    default:
                        return false;
                }
            }
        }
        function isStartOfDeclaration() {
            return lookAhead(isDeclaration);
        }
        function isStartOfStatement() {
            switch (token()) {
                case 59 /* AtToken */:
                case 26 /* SemicolonToken */:
                case 18 /* OpenBraceToken */:
                case 113 /* VarKeyword */:
                case 119 /* LetKeyword */:
                case 98 /* FunctionKeyword */:
                case 84 /* ClassKeyword */:
                case 92 /* EnumKeyword */:
                case 99 /* IfKeyword */:
                case 90 /* DoKeyword */:
                case 115 /* WhileKeyword */:
                case 97 /* ForKeyword */:
                case 86 /* ContinueKeyword */:
                case 81 /* BreakKeyword */:
                case 105 /* ReturnKeyword */:
                case 116 /* WithKeyword */:
                case 107 /* SwitchKeyword */:
                case 109 /* ThrowKeyword */:
                case 111 /* TryKeyword */:
                case 87 /* DebuggerKeyword */:
                // 'catch' and 'finally' do not actually indicate that the code is part of a statement,
                // however, we say they are here so that we may gracefully parse them and error later.
                // falls through
                case 83 /* CatchKeyword */:
                case 96 /* FinallyKeyword */:
                    return true;
                case 100 /* ImportKeyword */:
                    return isStartOfDeclaration() || lookAhead(nextTokenIsOpenParenOrLessThanOrDot);
                case 85 /* ConstKeyword */:
                case 93 /* ExportKeyword */:
                    return isStartOfDeclaration();
                case 131 /* AsyncKeyword */:
                case 135 /* DeclareKeyword */:
                case 118 /* InterfaceKeyword */:
                case 141 /* ModuleKeyword */:
                case 142 /* NamespaceKeyword */:
                case 151 /* TypeKeyword */:
                case 156 /* GlobalKeyword */:
                    // When these don't start a declaration, they're an identifier in an expression statement
                    return true;
                case 123 /* PublicKeyword */:
                case 121 /* PrivateKeyword */:
                case 122 /* ProtectedKeyword */:
                case 124 /* StaticKeyword */:
                case 144 /* ReadonlyKeyword */:
                    // When these don't start a declaration, they may be the start of a class member if an identifier
                    // immediately follows. Otherwise they're an identifier in an expression statement.
                    return isStartOfDeclaration() || !lookAhead(nextTokenIsIdentifierOrKeywordOnSameLine);
                default:
                    return isStartOfExpression();
            }
        }
        function nextTokenIsBindingIdentifierOrStartOfDestructuring() {
            nextToken();
            return isBindingIdentifier() || token() === 18 /* OpenBraceToken */ || token() === 22 /* OpenBracketToken */;
        }
        function isLetDeclaration() {
            // In ES6 'let' always starts a lexical declaration if followed by an identifier or {
            // or [.
            return lookAhead(nextTokenIsBindingIdentifierOrStartOfDestructuring);
        }
        function parseStatement() {
            switch (token()) {
                case 26 /* SemicolonToken */:
                    return parseEmptyStatement();
                case 18 /* OpenBraceToken */:
                    return parseBlock(/*ignoreMissingOpenBrace*/ false);
                case 113 /* VarKeyword */:
                    return parseVariableStatement(getNodePos(), hasPrecedingJSDocComment(), /*decorators*/ undefined, /*modifiers*/ undefined);
                case 119 /* LetKeyword */:
                    if (isLetDeclaration()) {
                        return parseVariableStatement(getNodePos(), hasPrecedingJSDocComment(), /*decorators*/ undefined, /*modifiers*/ undefined);
                    }
                    break;
                case 98 /* FunctionKeyword */:
                    return parseFunctionDeclaration(getNodePos(), hasPrecedingJSDocComment(), /*decorators*/ undefined, /*modifiers*/ undefined);
                case 84 /* ClassKeyword */:
                    return parseClassDeclaration(getNodePos(), hasPrecedingJSDocComment(), /*decorators*/ undefined, /*modifiers*/ undefined);
                case 99 /* IfKeyword */:
                    return parseIfStatement();
                case 90 /* DoKeyword */:
                    return parseDoStatement();
                case 115 /* WhileKeyword */:
                    return parseWhileStatement();
                case 97 /* ForKeyword */:
                    return parseForOrForInOrForOfStatement();
                case 86 /* ContinueKeyword */:
                    return parseBreakOrContinueStatement(244 /* ContinueStatement */);
                case 81 /* BreakKeyword */:
                    return parseBreakOrContinueStatement(245 /* BreakStatement */);
                case 105 /* ReturnKeyword */:
                    return parseReturnStatement();
                case 116 /* WithKeyword */:
                    return parseWithStatement();
                case 107 /* SwitchKeyword */:
                    return parseSwitchStatement();
                case 109 /* ThrowKeyword */:
                    return parseThrowStatement();
                case 111 /* TryKeyword */:
                // Include 'catch' and 'finally' for error recovery.
                // falls through
                case 83 /* CatchKeyword */:
                case 96 /* FinallyKeyword */:
                    return parseTryStatement();
                case 87 /* DebuggerKeyword */:
                    return parseDebuggerStatement();
                case 59 /* AtToken */:
                    return parseDeclaration();
                case 131 /* AsyncKeyword */:
                case 118 /* InterfaceKeyword */:
                case 151 /* TypeKeyword */:
                case 141 /* ModuleKeyword */:
                case 142 /* NamespaceKeyword */:
                case 135 /* DeclareKeyword */:
                case 85 /* ConstKeyword */:
                case 92 /* EnumKeyword */:
                case 93 /* ExportKeyword */:
                case 100 /* ImportKeyword */:
                case 121 /* PrivateKeyword */:
                case 122 /* ProtectedKeyword */:
                case 123 /* PublicKeyword */:
                case 126 /* AbstractKeyword */:
                case 124 /* StaticKeyword */:
                case 144 /* ReadonlyKeyword */:
                case 156 /* GlobalKeyword */:
                    if (isStartOfDeclaration()) {
                        return parseDeclaration();
                    }
                    break;
            }
            return parseExpressionOrLabeledStatement();
        }
        function isDeclareModifier(modifier) {
            return modifier.kind === 135 /* DeclareKeyword */;
        }
        function parseDeclaration() {
            // TODO: Can we hold onto the parsed decorators/modifiers and advance the scanner
            //       if we can't reuse the declaration, so that we don't do this work twice?
            //
            // `parseListElement` attempted to get the reused node at this position,
            // but the ambient context flag was not yet set, so the node appeared
            // not reusable in that context.
            var isAmbient = ts.some(lookAhead(function () { return (parseDecorators(), parseModifiers()); }), isDeclareModifier);
            if (isAmbient) {
                var node = tryReuseAmbientDeclaration();
                if (node) {
                    return node;
                }
            }
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var decorators = parseDecorators();
            var modifiers = parseModifiers();
            if (isAmbient) {
                for (var _i = 0, _a = modifiers; _i < _a.length; _i++) {
                    var m = _a[_i];
                    m.flags |= 8388608 /* Ambient */;
                }
                return doInsideOfContext(8388608 /* Ambient */, function () { return parseDeclarationWorker(pos, hasJSDoc, decorators, modifiers); });
            }
            else {
                return parseDeclarationWorker(pos, hasJSDoc, decorators, modifiers);
            }
        }
        function tryReuseAmbientDeclaration() {
            return doInsideOfContext(8388608 /* Ambient */, function () {
                var node = currentNode(parsingContext);
                if (node) {
                    return consumeNode(node);
                }
            });
        }
        function parseDeclarationWorker(pos, hasJSDoc, decorators, modifiers) {
            switch (token()) {
                case 113 /* VarKeyword */:
                case 119 /* LetKeyword */:
                case 85 /* ConstKeyword */:
                    return parseVariableStatement(pos, hasJSDoc, decorators, modifiers);
                case 98 /* FunctionKeyword */:
                    return parseFunctionDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 84 /* ClassKeyword */:
                    return parseClassDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 118 /* InterfaceKeyword */:
                    return parseInterfaceDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 151 /* TypeKeyword */:
                    return parseTypeAliasDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 92 /* EnumKeyword */:
                    return parseEnumDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 156 /* GlobalKeyword */:
                case 141 /* ModuleKeyword */:
                case 142 /* NamespaceKeyword */:
                    return parseModuleDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 100 /* ImportKeyword */:
                    return parseImportDeclarationOrImportEqualsDeclaration(pos, hasJSDoc, decorators, modifiers);
                case 93 /* ExportKeyword */:
                    nextToken();
                    switch (token()) {
                        case 88 /* DefaultKeyword */:
                        case 63 /* EqualsToken */:
                            return parseExportAssignment(pos, hasJSDoc, decorators, modifiers);
                        case 127 /* AsKeyword */:
                            return parseNamespaceExportDeclaration(pos, hasJSDoc, decorators, modifiers);
                        default:
                            return parseExportDeclaration(pos, hasJSDoc, decorators, modifiers);
                    }
                default:
                    if (decorators || modifiers) {
                        // We reached this point because we encountered decorators and/or modifiers and assumed a declaration
                        // would follow. For recovery and error reporting purposes, return an incomplete declaration.
                        var missing = createMissingNode(275 /* MissingDeclaration */, /*reportAtCurrentPosition*/ true, ts.Diagnostics.Declaration_expected);
                        ts.setTextRangePos(missing, pos);
                        missing.decorators = decorators;
                        missing.modifiers = modifiers;
                        return missing;
                    }
                    return undefined; // TODO: GH#18217
            }
        }
        function nextTokenIsIdentifierOrStringLiteralOnSameLine() {
            nextToken();
            return !scanner.hasPrecedingLineBreak() && (isIdentifier() || token() === 10 /* StringLiteral */);
        }
        function parseFunctionBlockOrSemicolon(flags, diagnosticMessage) {
            if (token() !== 18 /* OpenBraceToken */ && canParseSemicolon()) {
                parseSemicolon();
                return;
            }
            return parseFunctionBlock(flags, diagnosticMessage);
        }
        // DECLARATIONS
        function parseArrayBindingElement() {
            var pos = getNodePos();
            if (token() === 27 /* CommaToken */) {
                return finishNode(factory.createOmittedExpression(), pos);
            }
            var dotDotDotToken = parseOptionalToken(25 /* DotDotDotToken */);
            var name = parseIdentifierOrPattern();
            var initializer = parseInitializer();
            return finishNode(factory.createBindingElement(dotDotDotToken, /*propertyName*/ undefined, name, initializer), pos);
        }
        function parseObjectBindingElement() {
            var pos = getNodePos();
            var dotDotDotToken = parseOptionalToken(25 /* DotDotDotToken */);
            var tokenIsIdentifier = isBindingIdentifier();
            var propertyName = parsePropertyName();
            var name;
            if (tokenIsIdentifier && token() !== 58 /* ColonToken */) {
                name = propertyName;
                propertyName = undefined;
            }
            else {
                parseExpected(58 /* ColonToken */);
                name = parseIdentifierOrPattern();
            }
            var initializer = parseInitializer();
            return finishNode(factory.createBindingElement(dotDotDotToken, propertyName, name, initializer), pos);
        }
        function parseObjectBindingPattern() {
            var pos = getNodePos();
            parseExpected(18 /* OpenBraceToken */);
            var elements = parseDelimitedList(9 /* ObjectBindingElements */, parseObjectBindingElement);
            parseExpected(19 /* CloseBraceToken */);
            return finishNode(factory.createObjectBindingPattern(elements), pos);
        }
        function parseArrayBindingPattern() {
            var pos = getNodePos();
            parseExpected(22 /* OpenBracketToken */);
            var elements = parseDelimitedList(10 /* ArrayBindingElements */, parseArrayBindingElement);
            parseExpected(23 /* CloseBracketToken */);
            return finishNode(factory.createArrayBindingPattern(elements), pos);
        }
        function isBindingIdentifierOrPrivateIdentifierOrPattern() {
            return token() === 18 /* OpenBraceToken */
                || token() === 22 /* OpenBracketToken */
                || token() === 80 /* PrivateIdentifier */
                || isBindingIdentifier();
        }
        function parseIdentifierOrPattern(privateIdentifierDiagnosticMessage) {
            if (token() === 22 /* OpenBracketToken */) {
                return parseArrayBindingPattern();
            }
            if (token() === 18 /* OpenBraceToken */) {
                return parseObjectBindingPattern();
            }
            return parseBindingIdentifier(privateIdentifierDiagnosticMessage);
        }
        function parseVariableDeclarationAllowExclamation() {
            return parseVariableDeclaration(/*allowExclamation*/ true);
        }
        function parseVariableDeclaration(allowExclamation) {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var name = parseIdentifierOrPattern(ts.Diagnostics.Private_identifiers_are_not_allowed_in_variable_declarations);
            var exclamationToken;
            if (allowExclamation && name.kind === 79 /* Identifier */ &&
                token() === 53 /* ExclamationToken */ && !scanner.hasPrecedingLineBreak()) {
                exclamationToken = parseTokenNode();
            }
            var type = parseTypeAnnotation();
            var initializer = isInOrOfKeyword(token()) ? undefined : parseInitializer();
            var node = factory.createVariableDeclaration(name, exclamationToken, type, initializer);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseVariableDeclarationList(inForStatementInitializer) {
            var pos = getNodePos();
            var flags = 0;
            switch (token()) {
                case 113 /* VarKeyword */:
                    break;
                case 119 /* LetKeyword */:
                    flags |= 1 /* Let */;
                    break;
                case 85 /* ConstKeyword */:
                    flags |= 2 /* Const */;
                    break;
                default:
                    ts.Debug.fail();
            }
            nextToken();
            // The user may have written the following:
            //
            //    for (let of X) { }
            //
            // In this case, we want to parse an empty declaration list, and then parse 'of'
            // as a keyword. The reason this is not automatic is that 'of' is a valid identifier.
            // So we need to look ahead to determine if 'of' should be treated as a keyword in
            // this context.
            // The checker will then give an error that there is an empty declaration list.
            var declarations;
            if (token() === 159 /* OfKeyword */ && lookAhead(canFollowContextualOfKeyword)) {
                declarations = createMissingList();
            }
            else {
                var savedDisallowIn = inDisallowInContext();
                setDisallowInContext(inForStatementInitializer);
                declarations = parseDelimitedList(8 /* VariableDeclarations */, inForStatementInitializer ? parseVariableDeclaration : parseVariableDeclarationAllowExclamation);
                setDisallowInContext(savedDisallowIn);
            }
            return finishNode(factory.createVariableDeclarationList(declarations, flags), pos);
        }
        function canFollowContextualOfKeyword() {
            return nextTokenIsIdentifier() && nextToken() === 21 /* CloseParenToken */;
        }
        function parseVariableStatement(pos, hasJSDoc, decorators, modifiers) {
            var declarationList = parseVariableDeclarationList(/*inForStatementInitializer*/ false);
            parseSemicolon();
            var node = factory.createVariableStatement(modifiers, declarationList);
            // Decorators are not allowed on a variable statement, so we keep track of them to report them in the grammar checker.
            node.decorators = decorators;
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseFunctionDeclaration(pos, hasJSDoc, decorators, modifiers) {
            var savedAwaitContext = inAwaitContext();
            var modifierFlags = ts.modifiersToFlags(modifiers);
            parseExpected(98 /* FunctionKeyword */);
            var asteriskToken = parseOptionalToken(41 /* AsteriskToken */);
            // We don't parse the name here in await context, instead we will report a grammar error in the checker.
            var name = modifierFlags & 512 /* Default */ ? parseOptionalBindingIdentifier() : parseBindingIdentifier();
            var isGenerator = asteriskToken ? 1 /* Yield */ : 0 /* None */;
            var isAsync = modifierFlags & 256 /* Async */ ? 2 /* Await */ : 0 /* None */;
            var typeParameters = parseTypeParameters();
            if (modifierFlags & 1 /* Export */)
                setAwaitContext(/*value*/ true);
            var parameters = parseParameters(isGenerator | isAsync);
            var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
            var body = parseFunctionBlockOrSemicolon(isGenerator | isAsync, ts.Diagnostics.or_expected);
            setAwaitContext(savedAwaitContext);
            var node = factory.createFunctionDeclaration(decorators, modifiers, asteriskToken, name, typeParameters, parameters, type, body);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseConstructorName() {
            if (token() === 134 /* ConstructorKeyword */) {
                return parseExpected(134 /* ConstructorKeyword */);
            }
            if (token() === 10 /* StringLiteral */ && lookAhead(nextToken) === 20 /* OpenParenToken */) {
                return tryParse(function () {
                    var literalNode = parseLiteralNode();
                    return literalNode.text === "constructor" ? literalNode : undefined;
                });
            }
        }
        function tryParseConstructorDeclaration(pos, hasJSDoc, decorators, modifiers) {
            return tryParse(function () {
                if (parseConstructorName()) {
                    var typeParameters = parseTypeParameters();
                    var parameters = parseParameters(0 /* None */);
                    var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
                    var body = parseFunctionBlockOrSemicolon(0 /* None */, ts.Diagnostics.or_expected);
                    var node = factory.createConstructorDeclaration(decorators, modifiers, parameters, body);
                    // Attach `typeParameters` and `type` if they exist so that we can report them in the grammar checker.
                    node.typeParameters = typeParameters;
                    node.type = type;
                    return withJSDoc(finishNode(node, pos), hasJSDoc);
                }
            });
        }
        function parseMethodDeclaration(pos, hasJSDoc, decorators, modifiers, asteriskToken, name, questionToken, exclamationToken, diagnosticMessage) {
            var isGenerator = asteriskToken ? 1 /* Yield */ : 0 /* None */;
            var isAsync = ts.some(modifiers, ts.isAsyncModifier) ? 2 /* Await */ : 0 /* None */;
            var typeParameters = parseTypeParameters();
            var parameters = parseParameters(isGenerator | isAsync);
            var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
            var body = parseFunctionBlockOrSemicolon(isGenerator | isAsync, diagnosticMessage);
            var node = factory.createMethodDeclaration(decorators, modifiers, asteriskToken, name, questionToken, typeParameters, parameters, type, body);
            // An exclamation token on a method is invalid syntax and will be handled by the grammar checker
            node.exclamationToken = exclamationToken;
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parsePropertyDeclaration(pos, hasJSDoc, decorators, modifiers, name, questionToken) {
            var exclamationToken = !questionToken && !scanner.hasPrecedingLineBreak() ? parseOptionalToken(53 /* ExclamationToken */) : undefined;
            var type = parseTypeAnnotation();
            var initializer = doOutsideOfContext(8192 /* YieldContext */ | 32768 /* AwaitContext */ | 4096 /* DisallowInContext */, parseInitializer);
            parseSemicolonAfterPropertyName(name, type, initializer);
            var node = factory.createPropertyDeclaration(decorators, modifiers, name, questionToken || exclamationToken, type, initializer);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parsePropertyOrMethodDeclaration(pos, hasJSDoc, decorators, modifiers) {
            var asteriskToken = parseOptionalToken(41 /* AsteriskToken */);
            var name = parsePropertyName();
            // Note: this is not legal as per the grammar.  But we allow it in the parser and
            // report an error in the grammar checker.
            var questionToken = parseOptionalToken(57 /* QuestionToken */);
            if (asteriskToken || token() === 20 /* OpenParenToken */ || token() === 29 /* LessThanToken */) {
                return parseMethodDeclaration(pos, hasJSDoc, decorators, modifiers, asteriskToken, name, questionToken, /*exclamationToken*/ undefined, ts.Diagnostics.or_expected);
            }
            return parsePropertyDeclaration(pos, hasJSDoc, decorators, modifiers, name, questionToken);
        }
        function parseAccessorDeclaration(pos, hasJSDoc, decorators, modifiers, kind) {
            var name = parsePropertyName();
            var typeParameters = parseTypeParameters();
            var parameters = parseParameters(0 /* None */);
            var type = parseReturnType(58 /* ColonToken */, /*isType*/ false);
            var body = parseFunctionBlockOrSemicolon(0 /* None */);
            var node = kind === 171 /* GetAccessor */
                ? factory.createGetAccessorDeclaration(decorators, modifiers, name, parameters, type, body)
                : factory.createSetAccessorDeclaration(decorators, modifiers, name, parameters, body);
            // Keep track of `typeParameters` (for both) and `type` (for setters) if they were parsed those indicate grammar errors
            node.typeParameters = typeParameters;
            if (type && node.kind === 172 /* SetAccessor */)
                node.type = type;
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function isClassMemberStart() {
            var idToken;
            if (token() === 59 /* AtToken */) {
                return true;
            }
            // Eat up all modifiers, but hold on to the last one in case it is actually an identifier.
            while (ts.isModifierKind(token())) {
                idToken = token();
                // If the idToken is a class modifier (protected, private, public, and static), it is
                // certain that we are starting to parse class member. This allows better error recovery
                // Example:
                //      public foo() ...     // true
                //      public @dec blah ... // true; we will then report an error later
                //      export public ...    // true; we will then report an error later
                if (ts.isClassMemberModifier(idToken)) {
                    return true;
                }
                nextToken();
            }
            if (token() === 41 /* AsteriskToken */) {
                return true;
            }
            // Try to get the first property-like token following all modifiers.
            // This can either be an identifier or the 'get' or 'set' keywords.
            if (isLiteralPropertyName()) {
                idToken = token();
                nextToken();
            }
            // Index signatures and computed properties are class members; we can parse.
            if (token() === 22 /* OpenBracketToken */) {
                return true;
            }
            // If we were able to get any potential identifier...
            if (idToken !== undefined) {
                // If we have a non-keyword identifier, or if we have an accessor, then it's safe to parse.
                if (!ts.isKeyword(idToken) || idToken === 148 /* SetKeyword */ || idToken === 136 /* GetKeyword */) {
                    return true;
                }
                // If it *is* a keyword, but not an accessor, check a little farther along
                // to see if it should actually be parsed as a class member.
                switch (token()) {
                    case 20 /* OpenParenToken */: // Method declaration
                    case 29 /* LessThanToken */: // Generic Method declaration
                    case 53 /* ExclamationToken */: // Non-null assertion on property name
                    case 58 /* ColonToken */: // Type Annotation for declaration
                    case 63 /* EqualsToken */: // Initializer for declaration
                    case 57 /* QuestionToken */: // Not valid, but permitted so that it gets caught later on.
                        return true;
                    default:
                        // Covers
                        //  - Semicolons     (declaration termination)
                        //  - Closing braces (end-of-class, must be declaration)
                        //  - End-of-files   (not valid, but permitted so that it gets caught later on)
                        //  - Line-breaks    (enabling *automatic semicolon insertion*)
                        return canParseSemicolon();
                }
            }
            return false;
        }
        function parseClassStaticBlockDeclaration(pos, hasJSDoc, decorators, modifiers) {
            parseExpectedToken(124 /* StaticKeyword */);
            var body = parseClassStaticBlockBody();
            return withJSDoc(finishNode(factory.createClassStaticBlockDeclaration(decorators, modifiers, body), pos), hasJSDoc);
        }
        function parseClassStaticBlockBody() {
            var savedYieldContext = inYieldContext();
            var savedAwaitContext = inAwaitContext();
            setYieldContext(false);
            setAwaitContext(true);
            var body = parseBlock(/*ignoreMissingOpenBrace*/ false);
            setYieldContext(savedYieldContext);
            setAwaitContext(savedAwaitContext);
            return body;
        }
        function parseDecoratorExpression() {
            if (inAwaitContext() && token() === 132 /* AwaitKeyword */) {
                // `@await` is is disallowed in an [Await] context, but can cause parsing to go off the rails
                // This simply parses the missing identifier and moves on.
                var pos = getNodePos();
                var awaitExpression = parseIdentifier(ts.Diagnostics.Expression_expected);
                nextToken();
                var memberExpression = parseMemberExpressionRest(pos, awaitExpression, /*allowOptionalChain*/ true);
                return parseCallExpressionRest(pos, memberExpression);
            }
            return parseLeftHandSideExpressionOrHigher();
        }
        function tryParseDecorator() {
            var pos = getNodePos();
            if (!parseOptional(59 /* AtToken */)) {
                return undefined;
            }
            var expression = doInDecoratorContext(parseDecoratorExpression);
            return finishNode(factory.createDecorator(expression), pos);
        }
        function parseDecorators() {
            var pos = getNodePos();
            var list, decorator;
            while (decorator = tryParseDecorator()) {
                list = ts.append(list, decorator);
            }
            return list && createNodeArray(list, pos);
        }
        function tryParseModifier(permitInvalidConstAsModifier, stopOnStartOfClassStaticBlock, hasSeenStaticModifier) {
            var pos = getNodePos();
            var kind = token();
            if (token() === 85 /* ConstKeyword */ && permitInvalidConstAsModifier) {
                // We need to ensure that any subsequent modifiers appear on the same line
                // so that when 'const' is a standalone declaration, we don't issue an error.
                if (!tryParse(nextTokenIsOnSameLineAndCanFollowModifier)) {
                    return undefined;
                }
            }
            else if (stopOnStartOfClassStaticBlock && token() === 124 /* StaticKeyword */ && lookAhead(nextTokenIsOpenBrace)) {
                return undefined;
            }
            else if (hasSeenStaticModifier && token() === 124 /* StaticKeyword */) {
                return undefined;
            }
            else {
                if (!parseAnyContextualModifier()) {
                    return undefined;
                }
            }
            return finishNode(factory.createToken(kind), pos);
        }
        /*
         * There are situations in which a modifier like 'const' will appear unexpectedly, such as on a class member.
         * In those situations, if we are entirely sure that 'const' is not valid on its own (such as when ASI takes effect
         * and turns it into a standalone declaration), then it is better to parse it and report an error later.
         *
         * In such situations, 'permitInvalidConstAsModifier' should be set to true.
         */
        function parseModifiers(permitInvalidConstAsModifier, stopOnStartOfClassStaticBlock) {
            var pos = getNodePos();
            var list, modifier, hasSeenStatic = false;
            while (modifier = tryParseModifier(permitInvalidConstAsModifier, stopOnStartOfClassStaticBlock, hasSeenStatic)) {
                if (modifier.kind === 124 /* StaticKeyword */)
                    hasSeenStatic = true;
                list = ts.append(list, modifier);
            }
            return list && createNodeArray(list, pos);
        }
        function parseModifiersForArrowFunction() {
            var modifiers;
            if (token() === 131 /* AsyncKeyword */) {
                var pos = getNodePos();
                nextToken();
                var modifier = finishNode(factory.createToken(131 /* AsyncKeyword */), pos);
                modifiers = createNodeArray([modifier], pos);
            }
            return modifiers;
        }
        function parseClassElement() {
            var pos = getNodePos();
            if (token() === 26 /* SemicolonToken */) {
                nextToken();
                return finishNode(factory.createSemicolonClassElement(), pos);
            }
            var hasJSDoc = hasPrecedingJSDocComment();
            var decorators = parseDecorators();
            var modifiers = parseModifiers(/*permitInvalidConstAsModifier*/ true, /*stopOnStartOfClassStaticBlock*/ true);
            if (token() === 124 /* StaticKeyword */ && lookAhead(nextTokenIsOpenBrace)) {
                return parseClassStaticBlockDeclaration(pos, hasJSDoc, decorators, modifiers);
            }
            if (parseContextualModifier(136 /* GetKeyword */)) {
                return parseAccessorDeclaration(pos, hasJSDoc, decorators, modifiers, 171 /* GetAccessor */);
            }
            if (parseContextualModifier(148 /* SetKeyword */)) {
                return parseAccessorDeclaration(pos, hasJSDoc, decorators, modifiers, 172 /* SetAccessor */);
            }
            if (token() === 134 /* ConstructorKeyword */ || token() === 10 /* StringLiteral */) {
                var constructorDeclaration = tryParseConstructorDeclaration(pos, hasJSDoc, decorators, modifiers);
                if (constructorDeclaration) {
                    return constructorDeclaration;
                }
            }
            if (isIndexSignature()) {
                return parseIndexSignatureDeclaration(pos, hasJSDoc, decorators, modifiers);
            }
            // It is very important that we check this *after* checking indexers because
            // the [ token can start an index signature or a computed property name
            if (ts.tokenIsIdentifierOrKeyword(token()) ||
                token() === 10 /* StringLiteral */ ||
                token() === 8 /* NumericLiteral */ ||
                token() === 41 /* AsteriskToken */ ||
                token() === 22 /* OpenBracketToken */) {
                var isAmbient = ts.some(modifiers, isDeclareModifier);
                if (isAmbient) {
                    for (var _i = 0, _a = modifiers; _i < _a.length; _i++) {
                        var m = _a[_i];
                        m.flags |= 8388608 /* Ambient */;
                    }
                    return doInsideOfContext(8388608 /* Ambient */, function () { return parsePropertyOrMethodDeclaration(pos, hasJSDoc, decorators, modifiers); });
                }
                else {
                    return parsePropertyOrMethodDeclaration(pos, hasJSDoc, decorators, modifiers);
                }
            }
            if (decorators || modifiers) {
                // treat this as a property declaration with a missing name.
                var name = createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ true, ts.Diagnostics.Declaration_expected);
                return parsePropertyDeclaration(pos, hasJSDoc, decorators, modifiers, name, /*questionToken*/ undefined);
            }
            // 'isClassMemberStart' should have hinted not to attempt parsing.
            return ts.Debug.fail("Should not have attempted to parse class member declaration.");
        }
        function parseClassExpression() {
            return parseClassDeclarationOrExpression(getNodePos(), hasPrecedingJSDocComment(), /*decorators*/ undefined, /*modifiers*/ undefined, 225 /* ClassExpression */);
        }
        function parseClassDeclaration(pos, hasJSDoc, decorators, modifiers) {
            return parseClassDeclarationOrExpression(pos, hasJSDoc, decorators, modifiers, 256 /* ClassDeclaration */);
        }
        function parseClassDeclarationOrExpression(pos, hasJSDoc, decorators, modifiers, kind) {
            var savedAwaitContext = inAwaitContext();
            parseExpected(84 /* ClassKeyword */);
            // We don't parse the name here in await context, instead we will report a grammar error in the checker.
            var name = parseNameOfClassDeclarationOrExpression();
            var typeParameters = parseTypeParameters();
            if (ts.some(modifiers, ts.isExportModifier))
                setAwaitContext(/*value*/ true);
            var heritageClauses = parseHeritageClauses();
            var members;
            if (parseExpected(18 /* OpenBraceToken */)) {
                // ClassTail[Yield,Await] : (Modified) See 14.5
                //      ClassHeritage[?Yield,?Await]opt { ClassBody[?Yield,?Await]opt }
                members = parseClassMembers();
                parseExpected(19 /* CloseBraceToken */);
            }
            else {
                members = createMissingList();
            }
            setAwaitContext(savedAwaitContext);
            var node = kind === 256 /* ClassDeclaration */
                ? factory.createClassDeclaration(decorators, modifiers, name, typeParameters, heritageClauses, members)
                : factory.createClassExpression(decorators, modifiers, name, typeParameters, heritageClauses, members);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseNameOfClassDeclarationOrExpression() {
            // implements is a future reserved word so
            // 'class implements' might mean either
            // - class expression with omitted name, 'implements' starts heritage clause
            // - class with name 'implements'
            // 'isImplementsClause' helps to disambiguate between these two cases
            return isBindingIdentifier() && !isImplementsClause()
                ? createIdentifier(isBindingIdentifier())
                : undefined;
        }
        function isImplementsClause() {
            return token() === 117 /* ImplementsKeyword */ && lookAhead(nextTokenIsIdentifierOrKeyword);
        }
        function parseHeritageClauses() {
            // ClassTail[Yield,Await] : (Modified) See 14.5
            //      ClassHeritage[?Yield,?Await]opt { ClassBody[?Yield,?Await]opt }
            if (isHeritageClause()) {
                return parseList(22 /* HeritageClauses */, parseHeritageClause);
            }
            return undefined;
        }
        function parseHeritageClause() {
            var pos = getNodePos();
            var tok = token();
            ts.Debug.assert(tok === 94 /* ExtendsKeyword */ || tok === 117 /* ImplementsKeyword */); // isListElement() should ensure this.
            nextToken();
            var types = parseDelimitedList(7 /* HeritageClauseElement */, parseExpressionWithTypeArguments);
            return finishNode(factory.createHeritageClause(tok, types), pos);
        }
        function parseExpressionWithTypeArguments() {
            var pos = getNodePos();
            var expression = parseLeftHandSideExpressionOrHigher();
            var typeArguments = tryParseTypeArguments();
            return finishNode(factory.createExpressionWithTypeArguments(expression, typeArguments), pos);
        }
        function tryParseTypeArguments() {
            return token() === 29 /* LessThanToken */ ?
                parseBracketedList(20 /* TypeArguments */, parseType, 29 /* LessThanToken */, 31 /* GreaterThanToken */) : undefined;
        }
        function isHeritageClause() {
            return token() === 94 /* ExtendsKeyword */ || token() === 117 /* ImplementsKeyword */;
        }
        function parseClassMembers() {
            return parseList(5 /* ClassMembers */, parseClassElement);
        }
        function parseInterfaceDeclaration(pos, hasJSDoc, decorators, modifiers) {
            parseExpected(118 /* InterfaceKeyword */);
            var name = parseIdentifier();
            var typeParameters = parseTypeParameters();
            var heritageClauses = parseHeritageClauses();
            var members = parseObjectTypeMembers();
            var node = factory.createInterfaceDeclaration(decorators, modifiers, name, typeParameters, heritageClauses, members);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseTypeAliasDeclaration(pos, hasJSDoc, decorators, modifiers) {
            parseExpected(151 /* TypeKeyword */);
            var name = parseIdentifier();
            var typeParameters = parseTypeParameters();
            parseExpected(63 /* EqualsToken */);
            var type = token() === 138 /* IntrinsicKeyword */ && tryParse(parseKeywordAndNoDot) || parseType();
            parseSemicolon();
            var node = factory.createTypeAliasDeclaration(decorators, modifiers, name, typeParameters, type);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        // In an ambient declaration, the grammar only allows integer literals as initializers.
        // In a non-ambient declaration, the grammar allows uninitialized members only in a
        // ConstantEnumMemberSection, which starts at the beginning of an enum declaration
        // or any time an integer literal initializer is encountered.
        function parseEnumMember() {
            var pos = getNodePos();
            var hasJSDoc = hasPrecedingJSDocComment();
            var name = parsePropertyName();
            var initializer = allowInAnd(parseInitializer);
            return withJSDoc(finishNode(factory.createEnumMember(name, initializer), pos), hasJSDoc);
        }
        function parseEnumDeclaration(pos, hasJSDoc, decorators, modifiers) {
            parseExpected(92 /* EnumKeyword */);
            var name = parseIdentifier();
            var members;
            if (parseExpected(18 /* OpenBraceToken */)) {
                members = doOutsideOfYieldAndAwaitContext(function () { return parseDelimitedList(6 /* EnumMembers */, parseEnumMember); });
                parseExpected(19 /* CloseBraceToken */);
            }
            else {
                members = createMissingList();
            }
            var node = factory.createEnumDeclaration(decorators, modifiers, name, members);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseModuleBlock() {
            var pos = getNodePos();
            var statements;
            if (parseExpected(18 /* OpenBraceToken */)) {
                statements = parseList(1 /* BlockStatements */, parseStatement);
                parseExpected(19 /* CloseBraceToken */);
            }
            else {
                statements = createMissingList();
            }
            return finishNode(factory.createModuleBlock(statements), pos);
        }
        function parseModuleOrNamespaceDeclaration(pos, hasJSDoc, decorators, modifiers, flags) {
            // If we are parsing a dotted namespace name, we want to
            // propagate the 'Namespace' flag across the names if set.
            var namespaceFlag = flags & 16 /* Namespace */;
            var name = parseIdentifier();
            var body = parseOptional(24 /* DotToken */)
                ? parseModuleOrNamespaceDeclaration(getNodePos(), /*hasJSDoc*/ false, /*decorators*/ undefined, /*modifiers*/ undefined, 4 /* NestedNamespace */ | namespaceFlag)
                : parseModuleBlock();
            var node = factory.createModuleDeclaration(decorators, modifiers, name, body, flags);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseAmbientExternalModuleDeclaration(pos, hasJSDoc, decorators, modifiers) {
            var flags = 0;
            var name;
            if (token() === 156 /* GlobalKeyword */) {
                // parse 'global' as name of global scope augmentation
                name = parseIdentifier();
                flags |= 1024 /* GlobalAugmentation */;
            }
            else {
                name = parseLiteralNode();
                name.text = internIdentifier(name.text);
            }
            var body;
            if (token() === 18 /* OpenBraceToken */) {
                body = parseModuleBlock();
            }
            else {
                parseSemicolon();
            }
            var node = factory.createModuleDeclaration(decorators, modifiers, name, body, flags);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseModuleDeclaration(pos, hasJSDoc, decorators, modifiers) {
            var flags = 0;
            if (token() === 156 /* GlobalKeyword */) {
                // global augmentation
                return parseAmbientExternalModuleDeclaration(pos, hasJSDoc, decorators, modifiers);
            }
            else if (parseOptional(142 /* NamespaceKeyword */)) {
                flags |= 16 /* Namespace */;
            }
            else {
                parseExpected(141 /* ModuleKeyword */);
                if (token() === 10 /* StringLiteral */) {
                    return parseAmbientExternalModuleDeclaration(pos, hasJSDoc, decorators, modifiers);
                }
            }
            return parseModuleOrNamespaceDeclaration(pos, hasJSDoc, decorators, modifiers, flags);
        }
        function isExternalModuleReference() {
            return token() === 145 /* RequireKeyword */ &&
                lookAhead(nextTokenIsOpenParen);
        }
        function nextTokenIsOpenParen() {
            return nextToken() === 20 /* OpenParenToken */;
        }
        function nextTokenIsOpenBrace() {
            return nextToken() === 18 /* OpenBraceToken */;
        }
        function nextTokenIsSlash() {
            return nextToken() === 43 /* SlashToken */;
        }
        function parseNamespaceExportDeclaration(pos, hasJSDoc, decorators, modifiers) {
            parseExpected(127 /* AsKeyword */);
            parseExpected(142 /* NamespaceKeyword */);
            var name = parseIdentifier();
            parseSemicolon();
            var node = factory.createNamespaceExportDeclaration(name);
            // NamespaceExportDeclaration nodes cannot have decorators or modifiers, so we attach them here so we can report them in the grammar checker
            node.decorators = decorators;
            node.modifiers = modifiers;
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseImportDeclarationOrImportEqualsDeclaration(pos, hasJSDoc, decorators, modifiers) {
            parseExpected(100 /* ImportKeyword */);
            var afterImportPos = scanner.getStartPos();
            // We don't parse the identifier here in await context, instead we will report a grammar error in the checker.
            var identifier;
            if (isIdentifier()) {
                identifier = parseIdentifier();
            }
            var isTypeOnly = false;
            if (token() !== 155 /* FromKeyword */ &&
                (identifier === null || identifier === void 0 ? void 0 : identifier.escapedText) === "type" &&
                (isIdentifier() || tokenAfterImportDefinitelyProducesImportDeclaration())) {
                isTypeOnly = true;
                identifier = isIdentifier() ? parseIdentifier() : undefined;
            }
            if (identifier && !tokenAfterImportedIdentifierDefinitelyProducesImportDeclaration()) {
                return parseImportEqualsDeclaration(pos, hasJSDoc, decorators, modifiers, identifier, isTypeOnly);
            }
            // ImportDeclaration:
            //  import ImportClause from ModuleSpecifier ;
            //  import ModuleSpecifier;
            var importClause;
            if (identifier || // import id
                token() === 41 /* AsteriskToken */ || // import *
                token() === 18 /* OpenBraceToken */ // import {
            ) {
                importClause = parseImportClause(identifier, afterImportPos, isTypeOnly);
                parseExpected(155 /* FromKeyword */);
            }
            var moduleSpecifier = parseModuleSpecifier();
            var assertClause;
            if (token() === 129 /* AssertKeyword */ && !scanner.hasPrecedingLineBreak()) {
                assertClause = parseAssertClause();
            }
            parseSemicolon();
            var node = factory.createImportDeclaration(decorators, modifiers, importClause, moduleSpecifier, assertClause);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseAssertEntry() {
            var pos = getNodePos();
            var name = ts.tokenIsIdentifierOrKeyword(token()) ? parseIdentifierName() : parseLiteralLikeNode(10 /* StringLiteral */);
            parseExpected(58 /* ColonToken */);
            var value = parseAssignmentExpressionOrHigher();
            return finishNode(factory.createAssertEntry(name, value), pos);
        }
        function parseAssertClause() {
            var pos = getNodePos();
            parseExpected(129 /* AssertKeyword */);
            var openBracePosition = scanner.getTokenPos();
            if (parseExpected(18 /* OpenBraceToken */)) {
                var multiLine = scanner.hasPrecedingLineBreak();
                var elements = parseDelimitedList(24 /* AssertEntries */, parseAssertEntry, /*considerSemicolonAsDelimiter*/ true);
                if (!parseExpected(19 /* CloseBraceToken */)) {
                    var lastError = ts.lastOrUndefined(parseDiagnostics);
                    if (lastError && lastError.code === ts.Diagnostics._0_expected.code) {
                        ts.addRelatedInfo(lastError, ts.createDetachedDiagnostic(fileName, openBracePosition, 1, ts.Diagnostics.The_parser_expected_to_find_a_to_match_the_token_here));
                    }
                }
                return finishNode(factory.createAssertClause(elements, multiLine), pos);
            }
            else {
                var elements = createNodeArray([], getNodePos(), /*end*/ undefined, /*hasTrailingComma*/ false);
                return finishNode(factory.createAssertClause(elements, /*multiLine*/ false), pos);
            }
        }
        function tokenAfterImportDefinitelyProducesImportDeclaration() {
            return token() === 41 /* AsteriskToken */ || token() === 18 /* OpenBraceToken */;
        }
        function tokenAfterImportedIdentifierDefinitelyProducesImportDeclaration() {
            // In `import id ___`, the current token decides whether to produce
            // an ImportDeclaration or ImportEqualsDeclaration.
            return token() === 27 /* CommaToken */ || token() === 155 /* FromKeyword */;
        }
        function parseImportEqualsDeclaration(pos, hasJSDoc, decorators, modifiers, identifier, isTypeOnly) {
            parseExpected(63 /* EqualsToken */);
            var moduleReference = parseModuleReference();
            parseSemicolon();
            var node = factory.createImportEqualsDeclaration(decorators, modifiers, isTypeOnly, identifier, moduleReference);
            var finished = withJSDoc(finishNode(node, pos), hasJSDoc);
            return finished;
        }
        function parseImportClause(identifier, pos, isTypeOnly) {
            // ImportClause:
            //  ImportedDefaultBinding
            //  NameSpaceImport
            //  NamedImports
            //  ImportedDefaultBinding, NameSpaceImport
            //  ImportedDefaultBinding, NamedImports
            // If there was no default import or if there is comma token after default import
            // parse namespace or named imports
            var namedBindings;
            if (!identifier ||
                parseOptional(27 /* CommaToken */)) {
                namedBindings = token() === 41 /* AsteriskToken */ ? parseNamespaceImport() : parseNamedImportsOrExports(268 /* NamedImports */);
            }
            return finishNode(factory.createImportClause(isTypeOnly, identifier, namedBindings), pos);
        }
        function parseModuleReference() {
            return isExternalModuleReference()
                ? parseExternalModuleReference()
                : parseEntityName(/*allowReservedWords*/ false);
        }
        function parseExternalModuleReference() {
            var pos = getNodePos();
            parseExpected(145 /* RequireKeyword */);
            parseExpected(20 /* OpenParenToken */);
            var expression = parseModuleSpecifier();
            parseExpected(21 /* CloseParenToken */);
            return finishNode(factory.createExternalModuleReference(expression), pos);
        }
        function parseModuleSpecifier() {
            if (token() === 10 /* StringLiteral */) {
                var result = parseLiteralNode();
                result.text = internIdentifier(result.text);
                return result;
            }
            else {
                // We allow arbitrary expressions here, even though the grammar only allows string
                // literals.  We check to ensure that it is only a string literal later in the grammar
                // check pass.
                return parseExpression();
            }
        }
        function parseNamespaceImport() {
            // NameSpaceImport:
            //  * as ImportedBinding
            var pos = getNodePos();
            parseExpected(41 /* AsteriskToken */);
            parseExpected(127 /* AsKeyword */);
            var name = parseIdentifier();
            return finishNode(factory.createNamespaceImport(name), pos);
        }
        function parseNamedImportsOrExports(kind) {
            var pos = getNodePos();
            // NamedImports:
            //  { }
            //  { ImportsList }
            //  { ImportsList, }
            // ImportsList:
            //  ImportSpecifier
            //  ImportsList, ImportSpecifier
            var node = kind === 268 /* NamedImports */
                ? factory.createNamedImports(parseBracketedList(23 /* ImportOrExportSpecifiers */, parseImportSpecifier, 18 /* OpenBraceToken */, 19 /* CloseBraceToken */))
                : factory.createNamedExports(parseBracketedList(23 /* ImportOrExportSpecifiers */, parseExportSpecifier, 18 /* OpenBraceToken */, 19 /* CloseBraceToken */));
            return finishNode(node, pos);
        }
        function parseExportSpecifier() {
            var hasJSDoc = hasPrecedingJSDocComment();
            return withJSDoc(parseImportOrExportSpecifier(274 /* ExportSpecifier */), hasJSDoc);
        }
        function parseImportSpecifier() {
            return parseImportOrExportSpecifier(269 /* ImportSpecifier */);
        }
        function parseImportOrExportSpecifier(kind) {
            var pos = getNodePos();
            // ImportSpecifier:
            //   BindingIdentifier
            //   IdentifierName as BindingIdentifier
            // ExportSpecifier:
            //   IdentifierName
            //   IdentifierName as IdentifierName
            var checkIdentifierIsKeyword = ts.isKeyword(token()) && !isIdentifier();
            var checkIdentifierStart = scanner.getTokenPos();
            var checkIdentifierEnd = scanner.getTextPos();
            var isTypeOnly = false;
            var propertyName;
            var canParseAsKeyword = true;
            var name = parseIdentifierName();
            if (name.escapedText === "type") {
                // If the first token of an import specifier is 'type', there are a lot of possibilities,
                // especially if we see 'as' afterwards:
                //
                // import { type } from "mod";          - isTypeOnly: false,   name: type
                // import { type as } from "mod";       - isTypeOnly: true,    name: as
                // import { type as as } from "mod";    - isTypeOnly: false,   name: as,    propertyName: type
                // import { type as as as } from "mod"; - isTypeOnly: true,    name: as,    propertyName: as
                if (token() === 127 /* AsKeyword */) {
                    // { type as ...? }
                    var firstAs = parseIdentifierName();
                    if (token() === 127 /* AsKeyword */) {
                        // { type as as ...? }
                        var secondAs = parseIdentifierName();
                        if (ts.tokenIsIdentifierOrKeyword(token())) {
                            // { type as as something }
                            isTypeOnly = true;
                            propertyName = firstAs;
                            name = parseNameWithKeywordCheck();
                            canParseAsKeyword = false;
                        }
                        else {
                            // { type as as }
                            propertyName = name;
                            name = secondAs;
                            canParseAsKeyword = false;
                        }
                    }
                    else if (ts.tokenIsIdentifierOrKeyword(token())) {
                        // { type as something }
                        propertyName = name;
                        canParseAsKeyword = false;
                        name = parseNameWithKeywordCheck();
                    }
                    else {
                        // { type as }
                        isTypeOnly = true;
                        name = firstAs;
                    }
                }
                else if (ts.tokenIsIdentifierOrKeyword(token())) {
                    // { type something ...? }
                    isTypeOnly = true;
                    name = parseNameWithKeywordCheck();
                }
            }
            if (canParseAsKeyword && token() === 127 /* AsKeyword */) {
                propertyName = name;
                parseExpected(127 /* AsKeyword */);
                name = parseNameWithKeywordCheck();
            }
            if (kind === 269 /* ImportSpecifier */ && checkIdentifierIsKeyword) {
                parseErrorAt(checkIdentifierStart, checkIdentifierEnd, ts.Diagnostics.Identifier_expected);
            }
            var node = kind === 269 /* ImportSpecifier */
                ? factory.createImportSpecifier(isTypeOnly, propertyName, name)
                : factory.createExportSpecifier(isTypeOnly, propertyName, name);
            return finishNode(node, pos);
            function parseNameWithKeywordCheck() {
                checkIdentifierIsKeyword = ts.isKeyword(token()) && !isIdentifier();
                checkIdentifierStart = scanner.getTokenPos();
                checkIdentifierEnd = scanner.getTextPos();
                return parseIdentifierName();
            }
        }
        function parseNamespaceExport(pos) {
            return finishNode(factory.createNamespaceExport(parseIdentifierName()), pos);
        }
        function parseExportDeclaration(pos, hasJSDoc, decorators, modifiers) {
            var savedAwaitContext = inAwaitContext();
            setAwaitContext(/*value*/ true);
            var exportClause;
            var moduleSpecifier;
            var assertClause;
            var isTypeOnly = parseOptional(151 /* TypeKeyword */);
            var namespaceExportPos = getNodePos();
            if (parseOptional(41 /* AsteriskToken */)) {
                if (parseOptional(127 /* AsKeyword */)) {
                    exportClause = parseNamespaceExport(namespaceExportPos);
                }
                parseExpected(155 /* FromKeyword */);
                moduleSpecifier = parseModuleSpecifier();
            }
            else {
                exportClause = parseNamedImportsOrExports(272 /* NamedExports */);
                // It is not uncommon to accidentally omit the 'from' keyword. Additionally, in editing scenarios,
                // the 'from' keyword can be parsed as a named export when the export clause is unterminated (i.e. `export { from "moduleName";`)
                // If we don't have a 'from' keyword, see if we have a string literal such that ASI won't take effect.
                if (token() === 155 /* FromKeyword */ || (token() === 10 /* StringLiteral */ && !scanner.hasPrecedingLineBreak())) {
                    parseExpected(155 /* FromKeyword */);
                    moduleSpecifier = parseModuleSpecifier();
                }
            }
            if (moduleSpecifier && token() === 129 /* AssertKeyword */ && !scanner.hasPrecedingLineBreak()) {
                assertClause = parseAssertClause();
            }
            parseSemicolon();
            setAwaitContext(savedAwaitContext);
            var node = factory.createExportDeclaration(decorators, modifiers, isTypeOnly, exportClause, moduleSpecifier, assertClause);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function parseExportAssignment(pos, hasJSDoc, decorators, modifiers) {
            var savedAwaitContext = inAwaitContext();
            setAwaitContext(/*value*/ true);
            var isExportEquals;
            if (parseOptional(63 /* EqualsToken */)) {
                isExportEquals = true;
            }
            else {
                parseExpected(88 /* DefaultKeyword */);
            }
            var expression = parseAssignmentExpressionOrHigher();
            parseSemicolon();
            setAwaitContext(savedAwaitContext);
            var node = factory.createExportAssignment(decorators, modifiers, isExportEquals, expression);
            return withJSDoc(finishNode(node, pos), hasJSDoc);
        }
        function setExternalModuleIndicator(sourceFile) {
            // Try to use the first top-level import/export when available, then
            // fall back to looking for an 'import.meta' somewhere in the tree if necessary.
            sourceFile.externalModuleIndicator =
                ts.forEach(sourceFile.statements, isAnExternalModuleIndicatorNode) ||
                    getImportMetaIfNecessary(sourceFile);
        }
        function isAnExternalModuleIndicatorNode(node) {
            return hasModifierOfKind(node, 93 /* ExportKeyword */)
                || ts.isImportEqualsDeclaration(node) && ts.isExternalModuleReference(node.moduleReference)
                || ts.isImportDeclaration(node)
                || ts.isExportAssignment(node)
                || ts.isExportDeclaration(node) ? node : undefined;
        }
        function getImportMetaIfNecessary(sourceFile) {
            return sourceFile.flags & 2097152 /* PossiblyContainsImportMeta */ ?
                walkTreeForExternalModuleIndicators(sourceFile) :
                undefined;
        }
        function walkTreeForExternalModuleIndicators(node) {
            return isImportMeta(node) ? node : forEachChild(node, walkTreeForExternalModuleIndicators);
        }
        /** Do not use hasModifier inside the parser; it relies on parent pointers. Use this instead. */
        function hasModifierOfKind(node, kind) {
            return ts.some(node.modifiers, function (m) { return m.kind === kind; });
        }
        function isImportMeta(node) {
            return ts.isMetaProperty(node) && node.keywordToken === 100 /* ImportKeyword */ && node.name.escapedText === "meta";
        }
        var ParsingContext;
        (function (ParsingContext) {
            ParsingContext[ParsingContext["SourceElements"] = 0] = "SourceElements";
            ParsingContext[ParsingContext["BlockStatements"] = 1] = "BlockStatements";
            ParsingContext[ParsingContext["SwitchClauses"] = 2] = "SwitchClauses";
            ParsingContext[ParsingContext["SwitchClauseStatements"] = 3] = "SwitchClauseStatements";
            ParsingContext[ParsingContext["TypeMembers"] = 4] = "TypeMembers";
            ParsingContext[ParsingContext["ClassMembers"] = 5] = "ClassMembers";
            ParsingContext[ParsingContext["EnumMembers"] = 6] = "EnumMembers";
            ParsingContext[ParsingContext["HeritageClauseElement"] = 7] = "HeritageClauseElement";
            ParsingContext[ParsingContext["VariableDeclarations"] = 8] = "VariableDeclarations";
            ParsingContext[ParsingContext["ObjectBindingElements"] = 9] = "ObjectBindingElements";
            ParsingContext[ParsingContext["ArrayBindingElements"] = 10] = "ArrayBindingElements";
            ParsingContext[ParsingContext["ArgumentExpressions"] = 11] = "ArgumentExpressions";
            ParsingContext[ParsingContext["ObjectLiteralMembers"] = 12] = "ObjectLiteralMembers";
            ParsingContext[ParsingContext["JsxAttributes"] = 13] = "JsxAttributes";
            ParsingContext[ParsingContext["JsxChildren"] = 14] = "JsxChildren";
            ParsingContext[ParsingContext["ArrayLiteralMembers"] = 15] = "ArrayLiteralMembers";
            ParsingContext[ParsingContext["Parameters"] = 16] = "Parameters";
            ParsingContext[ParsingContext["JSDocParameters"] = 17] = "JSDocParameters";
            ParsingContext[ParsingContext["RestProperties"] = 18] = "RestProperties";
            ParsingContext[ParsingContext["TypeParameters"] = 19] = "TypeParameters";
            ParsingContext[ParsingContext["TypeArguments"] = 20] = "TypeArguments";
            ParsingContext[ParsingContext["TupleElementTypes"] = 21] = "TupleElementTypes";
            ParsingContext[ParsingContext["HeritageClauses"] = 22] = "HeritageClauses";
            ParsingContext[ParsingContext["ImportOrExportSpecifiers"] = 23] = "ImportOrExportSpecifiers";
            ParsingContext[ParsingContext["AssertEntries"] = 24] = "AssertEntries";
            ParsingContext[ParsingContext["Count"] = 25] = "Count"; // Number of parsing contexts
        })(ParsingContext || (ParsingContext = {}));
        var Tristate;
        (function (Tristate) {
            Tristate[Tristate["False"] = 0] = "False";
            Tristate[Tristate["True"] = 1] = "True";
            Tristate[Tristate["Unknown"] = 2] = "Unknown";
        })(Tristate || (Tristate = {}));
        var JSDocParser;
        (function (JSDocParser) {
            function parseJSDocTypeExpressionForTests(content, start, length) {
                initializeState("file.js", content, 99 /* Latest */, /*_syntaxCursor:*/ undefined, 1 /* JS */);
                scanner.setText(content, start, length);
                currentToken = scanner.scan();
                var jsDocTypeExpression = parseJSDocTypeExpression();
                var sourceFile = createSourceFile("file.js", 99 /* Latest */, 1 /* JS */, /*isDeclarationFile*/ false, [], factory.createToken(1 /* EndOfFileToken */), 0 /* None */);
                var diagnostics = ts.attachFileToDiagnostics(parseDiagnostics, sourceFile);
                if (jsDocDiagnostics) {
                    sourceFile.jsDocDiagnostics = ts.attachFileToDiagnostics(jsDocDiagnostics, sourceFile);
                }
                clearState();
                return jsDocTypeExpression ? { jsDocTypeExpression: jsDocTypeExpression, diagnostics: diagnostics } : undefined;
            }
            JSDocParser.parseJSDocTypeExpressionForTests = parseJSDocTypeExpressionForTests;
            // Parses out a JSDoc type expression.
            function parseJSDocTypeExpression(mayOmitBraces) {
                var pos = getNodePos();
                var hasBrace = (mayOmitBraces ? parseOptional : parseExpected)(18 /* OpenBraceToken */);
                var type = doInsideOfContext(4194304 /* JSDoc */, parseJSDocType);
                if (!mayOmitBraces || hasBrace) {
                    parseExpectedJSDoc(19 /* CloseBraceToken */);
                }
                var result = factory.createJSDocTypeExpression(type);
                fixupParentReferences(result);
                return finishNode(result, pos);
            }
            JSDocParser.parseJSDocTypeExpression = parseJSDocTypeExpression;
            function parseJSDocNameReference() {
                var pos = getNodePos();
                var hasBrace = parseOptional(18 /* OpenBraceToken */);
                var p2 = getNodePos();
                var entityName = parseEntityName(/* allowReservedWords*/ false);
                while (token() === 80 /* PrivateIdentifier */) {
                    reScanHashToken(); // rescan #id as # id
                    nextTokenJSDoc(); // then skip the #
                    entityName = finishNode(factory.createJSDocMemberName(entityName, parseIdentifier()), p2);
                }
                if (hasBrace) {
                    parseExpectedJSDoc(19 /* CloseBraceToken */);
                }
                var result = factory.createJSDocNameReference(entityName);
                fixupParentReferences(result);
                return finishNode(result, pos);
            }
            JSDocParser.parseJSDocNameReference = parseJSDocNameReference;
            function parseIsolatedJSDocComment(content, start, length) {
                initializeState("", content, 99 /* Latest */, /*_syntaxCursor:*/ undefined, 1 /* JS */);
                var jsDoc = doInsideOfContext(4194304 /* JSDoc */, function () { return parseJSDocCommentWorker(start, length); });
                var sourceFile = { languageVariant: 0 /* Standard */, text: content };
                var diagnostics = ts.attachFileToDiagnostics(parseDiagnostics, sourceFile);
                clearState();
                return jsDoc ? { jsDoc: jsDoc, diagnostics: diagnostics } : undefined;
            }
            JSDocParser.parseIsolatedJSDocComment = parseIsolatedJSDocComment;
            function parseJSDocComment(parent, start, length) {
                var saveToken = currentToken;
                var saveParseDiagnosticsLength = parseDiagnostics.length;
                var saveParseErrorBeforeNextFinishedNode = parseErrorBeforeNextFinishedNode;
                var comment = doInsideOfContext(4194304 /* JSDoc */, function () { return parseJSDocCommentWorker(start, length); });
                ts.setParent(comment, parent);
                if (contextFlags & 131072 /* JavaScriptFile */) {
                    if (!jsDocDiagnostics) {
                        jsDocDiagnostics = [];
                    }
                    jsDocDiagnostics.push.apply(jsDocDiagnostics, parseDiagnostics);
                }
                currentToken = saveToken;
                parseDiagnostics.length = saveParseDiagnosticsLength;
                parseErrorBeforeNextFinishedNode = saveParseErrorBeforeNextFinishedNode;
                return comment;
            }
            JSDocParser.parseJSDocComment = parseJSDocComment;
            var JSDocState;
            (function (JSDocState) {
                JSDocState[JSDocState["BeginningOfLine"] = 0] = "BeginningOfLine";
                JSDocState[JSDocState["SawAsterisk"] = 1] = "SawAsterisk";
                JSDocState[JSDocState["SavingComments"] = 2] = "SavingComments";
                JSDocState[JSDocState["SavingBackticks"] = 3] = "SavingBackticks";
            })(JSDocState || (JSDocState = {}));
            var PropertyLikeParse;
            (function (PropertyLikeParse) {
                PropertyLikeParse[PropertyLikeParse["Property"] = 1] = "Property";
                PropertyLikeParse[PropertyLikeParse["Parameter"] = 2] = "Parameter";
                PropertyLikeParse[PropertyLikeParse["CallbackParameter"] = 4] = "CallbackParameter";
            })(PropertyLikeParse || (PropertyLikeParse = {}));
            function parseJSDocCommentWorker(start, length) {
                if (start === void 0) { start = 0; }
                var content = sourceText;
                var end = length === undefined ? content.length : start + length;
                length = end - start;
                ts.Debug.assert(start >= 0);
                ts.Debug.assert(start <= end);
                ts.Debug.assert(end <= content.length);
                // Check for /** (JSDoc opening part)
                if (!isJSDocLikeText(content, start)) {
                    return undefined;
                }
                var tags;
                var tagsPos;
                var tagsEnd;
                var linkEnd;
                var commentsPos;
                var comments = [];
                var parts = [];
                // + 3 for leading /**, - 5 in total for /** */
                return scanner.scanRange(start + 3, length - 5, function () {
                    // Initially we can parse out a tag.  We also have seen a starting asterisk.
                    // This is so that /** * @type */ doesn't parse.
                    var state = 1 /* SawAsterisk */;
                    var margin;
                    // + 4 for leading '/** '
                    // + 1 because the last index of \n is always one index before the first character in the line and coincidentally, if there is no \n before start, it is -1, which is also one index before the first character
                    var indent = start - (content.lastIndexOf("\n", start) + 1) + 4;
                    function pushComment(text) {
                        if (!margin) {
                            margin = indent;
                        }
                        comments.push(text);
                        indent += text.length;
                    }
                    nextTokenJSDoc();
                    while (parseOptionalJsdoc(5 /* WhitespaceTrivia */))
                        ;
                    if (parseOptionalJsdoc(4 /* NewLineTrivia */)) {
                        state = 0 /* BeginningOfLine */;
                        indent = 0;
                    }
                    loop: while (true) {
                        switch (token()) {
                            case 59 /* AtToken */:
                                if (state === 0 /* BeginningOfLine */ || state === 1 /* SawAsterisk */) {
                                    removeTrailingWhitespace(comments);
                                    if (!commentsPos)
                                        commentsPos = getNodePos();
                                    addTag(parseTag(indent));
                                    // NOTE: According to usejsdoc.org, a tag goes to end of line, except the last tag.
                                    // Real-world comments may break this rule, so "BeginningOfLine" will not be a real line beginning
                                    // for malformed examples like `/** @param {string} x @returns {number} the length */`
                                    state = 0 /* BeginningOfLine */;
                                    margin = undefined;
                                }
                                else {
                                    pushComment(scanner.getTokenText());
                                }
                                break;
                            case 4 /* NewLineTrivia */:
                                comments.push(scanner.getTokenText());
                                state = 0 /* BeginningOfLine */;
                                indent = 0;
                                break;
                            case 41 /* AsteriskToken */:
                                var asterisk = scanner.getTokenText();
                                if (state === 1 /* SawAsterisk */ || state === 2 /* SavingComments */) {
                                    // If we've already seen an asterisk, then we can no longer parse a tag on this line
                                    state = 2 /* SavingComments */;
                                    pushComment(asterisk);
                                }
                                else {
                                    // Ignore the first asterisk on a line
                                    state = 1 /* SawAsterisk */;
                                    indent += asterisk.length;
                                }
                                break;
                            case 5 /* WhitespaceTrivia */:
                                // only collect whitespace if we're already saving comments or have just crossed the comment indent margin
                                var whitespace = scanner.getTokenText();
                                if (state === 2 /* SavingComments */) {
                                    comments.push(whitespace);
                                }
                                else if (margin !== undefined && indent + whitespace.length > margin) {
                                    comments.push(whitespace.slice(margin - indent));
                                }
                                indent += whitespace.length;
                                break;
                            case 1 /* EndOfFileToken */:
                                break loop;
                            case 18 /* OpenBraceToken */:
                                state = 2 /* SavingComments */;
                                var commentEnd = scanner.getStartPos();
                                var linkStart = scanner.getTextPos() - 1;
                                var link = parseJSDocLink(linkStart);
                                if (link) {
                                    if (!linkEnd) {
                                        removeLeadingNewlines(comments);
                                    }
                                    parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd !== null && linkEnd !== void 0 ? linkEnd : start, commentEnd));
                                    parts.push(link);
                                    comments = [];
                                    linkEnd = scanner.getTextPos();
                                    break;
                                }
                            // fallthrough if it's not a {@link sequence
                            default:
                                // Anything else is doc comment text. We just save it. Because it
                                // wasn't a tag, we can no longer parse a tag on this line until we hit the next
                                // line break.
                                state = 2 /* SavingComments */;
                                pushComment(scanner.getTokenText());
                                break;
                        }
                        nextTokenJSDoc();
                    }
                    removeTrailingWhitespace(comments);
                    if (parts.length && comments.length) {
                        parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd !== null && linkEnd !== void 0 ? linkEnd : start, commentsPos));
                    }
                    if (parts.length && tags)
                        ts.Debug.assertIsDefined(commentsPos, "having parsed tags implies that the end of the comment span should be set");
                    var tagsArray = tags && createNodeArray(tags, tagsPos, tagsEnd);
                    return finishNode(factory.createJSDocComment(parts.length ? createNodeArray(parts, start, commentsPos) : comments.length ? comments.join("") : undefined, tagsArray), start, end);
                });
                function removeLeadingNewlines(comments) {
                    while (comments.length && (comments[0] === "\n" || comments[0] === "\r")) {
                        comments.shift();
                    }
                }
                function removeTrailingWhitespace(comments) {
                    while (comments.length && comments[comments.length - 1].trim() === "") {
                        comments.pop();
                    }
                }
                function isNextNonwhitespaceTokenEndOfFile() {
                    // We must use infinite lookahead, as there could be any number of newlines :(
                    while (true) {
                        nextTokenJSDoc();
                        if (token() === 1 /* EndOfFileToken */) {
                            return true;
                        }
                        if (!(token() === 5 /* WhitespaceTrivia */ || token() === 4 /* NewLineTrivia */)) {
                            return false;
                        }
                    }
                }
                function skipWhitespace() {
                    if (token() === 5 /* WhitespaceTrivia */ || token() === 4 /* NewLineTrivia */) {
                        if (lookAhead(isNextNonwhitespaceTokenEndOfFile)) {
                            return; // Don't skip whitespace prior to EoF (or end of comment) - that shouldn't be included in any node's range
                        }
                    }
                    while (token() === 5 /* WhitespaceTrivia */ || token() === 4 /* NewLineTrivia */) {
                        nextTokenJSDoc();
                    }
                }
                function skipWhitespaceOrAsterisk() {
                    if (token() === 5 /* WhitespaceTrivia */ || token() === 4 /* NewLineTrivia */) {
                        if (lookAhead(isNextNonwhitespaceTokenEndOfFile)) {
                            return ""; // Don't skip whitespace prior to EoF (or end of comment) - that shouldn't be included in any node's range
                        }
                    }
                    var precedingLineBreak = scanner.hasPrecedingLineBreak();
                    var seenLineBreak = false;
                    var indentText = "";
                    while ((precedingLineBreak && token() === 41 /* AsteriskToken */) || token() === 5 /* WhitespaceTrivia */ || token() === 4 /* NewLineTrivia */) {
                        indentText += scanner.getTokenText();
                        if (token() === 4 /* NewLineTrivia */) {
                            precedingLineBreak = true;
                            seenLineBreak = true;
                            indentText = "";
                        }
                        else if (token() === 41 /* AsteriskToken */) {
                            precedingLineBreak = false;
                        }
                        nextTokenJSDoc();
                    }
                    return seenLineBreak ? indentText : "";
                }
                function parseTag(margin) {
                    ts.Debug.assert(token() === 59 /* AtToken */);
                    var start = scanner.getTokenPos();
                    nextTokenJSDoc();
                    var tagName = parseJSDocIdentifierName(/*message*/ undefined);
                    var indentText = skipWhitespaceOrAsterisk();
                    var tag;
                    switch (tagName.escapedText) {
                        case "author":
                            tag = parseAuthorTag(start, tagName, margin, indentText);
                            break;
                        case "implements":
                            tag = parseImplementsTag(start, tagName, margin, indentText);
                            break;
                        case "augments":
                        case "extends":
                            tag = parseAugmentsTag(start, tagName, margin, indentText);
                            break;
                        case "class":
                        case "constructor":
                            tag = parseSimpleTag(start, factory.createJSDocClassTag, tagName, margin, indentText);
                            break;
                        case "public":
                            tag = parseSimpleTag(start, factory.createJSDocPublicTag, tagName, margin, indentText);
                            break;
                        case "private":
                            tag = parseSimpleTag(start, factory.createJSDocPrivateTag, tagName, margin, indentText);
                            break;
                        case "protected":
                            tag = parseSimpleTag(start, factory.createJSDocProtectedTag, tagName, margin, indentText);
                            break;
                        case "readonly":
                            tag = parseSimpleTag(start, factory.createJSDocReadonlyTag, tagName, margin, indentText);
                            break;
                        case "override":
                            tag = parseSimpleTag(start, factory.createJSDocOverrideTag, tagName, margin, indentText);
                            break;
                        case "deprecated":
                            hasDeprecatedTag = true;
                            tag = parseSimpleTag(start, factory.createJSDocDeprecatedTag, tagName, margin, indentText);
                            break;
                        case "this":
                            tag = parseThisTag(start, tagName, margin, indentText);
                            break;
                        case "enum":
                            tag = parseEnumTag(start, tagName, margin, indentText);
                            break;
                        case "arg":
                        case "argument":
                        case "param":
                            return parseParameterOrPropertyTag(start, tagName, 2 /* Parameter */, margin);
                        case "return":
                        case "returns":
                            tag = parseReturnTag(start, tagName, margin, indentText);
                            break;
                        case "template":
                            tag = parseTemplateTag(start, tagName, margin, indentText);
                            break;
                        case "type":
                            tag = parseTypeTag(start, tagName, margin, indentText);
                            break;
                        case "typedef":
                            tag = parseTypedefTag(start, tagName, margin, indentText);
                            break;
                        case "callback":
                            tag = parseCallbackTag(start, tagName, margin, indentText);
                            break;
                        case "see":
                            tag = parseSeeTag(start, tagName, margin, indentText);
                            break;
                        default:
                            tag = parseUnknownTag(start, tagName, margin, indentText);
                            break;
                    }
                    return tag;
                }
                function parseTrailingTagComments(pos, end, margin, indentText) {
                    // some tags, like typedef and callback, have already parsed their comments earlier
                    if (!indentText) {
                        margin += end - pos;
                    }
                    return parseTagComments(margin, indentText.slice(margin));
                }
                function parseTagComments(indent, initialMargin) {
                    var commentsPos = getNodePos();
                    var comments = [];
                    var parts = [];
                    var linkEnd;
                    var state = 0 /* BeginningOfLine */;
                    var previousWhitespace = true;
                    var margin;
                    function pushComment(text) {
                        if (!margin) {
                            margin = indent;
                        }
                        comments.push(text);
                        indent += text.length;
                    }
                    if (initialMargin !== undefined) {
                        // jump straight to saving comments if there is some initial indentation
                        if (initialMargin !== "") {
                            pushComment(initialMargin);
                        }
                        state = 1 /* SawAsterisk */;
                    }
                    var tok = token();
                    loop: while (true) {
                        switch (tok) {
                            case 4 /* NewLineTrivia */:
                                state = 0 /* BeginningOfLine */;
                                // don't use pushComment here because we want to keep the margin unchanged
                                comments.push(scanner.getTokenText());
                                indent = 0;
                                break;
                            case 59 /* AtToken */:
                                if (state === 3 /* SavingBackticks */
                                    || state === 2 /* SavingComments */ && (!previousWhitespace || lookAhead(isNextJSDocTokenWhitespace))) {
                                    // @ doesn't start a new tag inside ``, and inside a comment, only after whitespace or not before whitespace
                                    comments.push(scanner.getTokenText());
                                    break;
                                }
                                scanner.setTextPos(scanner.getTextPos() - 1);
                            // falls through
                            case 1 /* EndOfFileToken */:
                                // Done
                                break loop;
                            case 5 /* WhitespaceTrivia */:
                                if (state === 2 /* SavingComments */ || state === 3 /* SavingBackticks */) {
                                    pushComment(scanner.getTokenText());
                                }
                                else {
                                    var whitespace = scanner.getTokenText();
                                    // if the whitespace crosses the margin, take only the whitespace that passes the margin
                                    if (margin !== undefined && indent + whitespace.length > margin) {
                                        comments.push(whitespace.slice(margin - indent));
                                    }
                                    indent += whitespace.length;
                                }
                                break;
                            case 18 /* OpenBraceToken */:
                                state = 2 /* SavingComments */;
                                var commentEnd = scanner.getStartPos();
                                var linkStart = scanner.getTextPos() - 1;
                                var link = parseJSDocLink(linkStart);
                                if (link) {
                                    parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd !== null && linkEnd !== void 0 ? linkEnd : commentsPos, commentEnd));
                                    parts.push(link);
                                    comments = [];
                                    linkEnd = scanner.getTextPos();
                                }
                                else {
                                    pushComment(scanner.getTokenText());
                                }
                                break;
                            case 61 /* BacktickToken */:
                                if (state === 3 /* SavingBackticks */) {
                                    state = 2 /* SavingComments */;
                                }
                                else {
                                    state = 3 /* SavingBackticks */;
                                }
                                pushComment(scanner.getTokenText());
                                break;
                            case 41 /* AsteriskToken */:
                                if (state === 0 /* BeginningOfLine */) {
                                    // leading asterisks start recording on the *next* (non-whitespace) token
                                    state = 1 /* SawAsterisk */;
                                    indent += 1;
                                    break;
                                }
                            // record the * as a comment
                            // falls through
                            default:
                                if (state !== 3 /* SavingBackticks */) {
                                    state = 2 /* SavingComments */; // leading identifiers start recording as well
                                }
                                pushComment(scanner.getTokenText());
                                break;
                        }
                        previousWhitespace = token() === 5 /* WhitespaceTrivia */;
                        tok = nextTokenJSDoc();
                    }
                    removeLeadingNewlines(comments);
                    removeTrailingWhitespace(comments);
                    if (parts.length) {
                        if (comments.length) {
                            parts.push(finishNode(factory.createJSDocText(comments.join("")), linkEnd !== null && linkEnd !== void 0 ? linkEnd : commentsPos));
                        }
                        return createNodeArray(parts, commentsPos, scanner.getTextPos());
                    }
                    else if (comments.length) {
                        return comments.join("");
                    }
                }
                function isNextJSDocTokenWhitespace() {
                    var next = nextTokenJSDoc();
                    return next === 5 /* WhitespaceTrivia */ || next === 4 /* NewLineTrivia */;
                }
                function parseJSDocLink(start) {
                    var linkType = tryParse(parseJSDocLinkPrefix);
                    if (!linkType) {
                        return undefined;
                    }
                    nextTokenJSDoc(); // start at token after link, then skip any whitespace
                    skipWhitespace();
                    // parseEntityName logs an error for non-identifier, so create a MissingNode ourselves to avoid the error
                    var p2 = getNodePos();
                    var name = ts.tokenIsIdentifierOrKeyword(token())
                        ? parseEntityName(/*allowReservedWords*/ true)
                        : undefined;
                    if (name) {
                        while (token() === 80 /* PrivateIdentifier */) {
                            reScanHashToken(); // rescan #id as # id
                            nextTokenJSDoc(); // then skip the #
                            name = finishNode(factory.createJSDocMemberName(name, parseIdentifier()), p2);
                        }
                    }
                    var text = [];
                    while (token() !== 19 /* CloseBraceToken */ && token() !== 4 /* NewLineTrivia */ && token() !== 1 /* EndOfFileToken */) {
                        text.push(scanner.getTokenText());
                        nextTokenJSDoc();
                    }
                    var create = linkType === "link" ? factory.createJSDocLink
                        : linkType === "linkcode" ? factory.createJSDocLinkCode
                            : factory.createJSDocLinkPlain;
                    return finishNode(create(name, text.join("")), start, scanner.getTextPos());
                }
                function parseJSDocLinkPrefix() {
                    skipWhitespaceOrAsterisk();
                    if (token() === 18 /* OpenBraceToken */
                        && nextTokenJSDoc() === 59 /* AtToken */
                        && ts.tokenIsIdentifierOrKeyword(nextTokenJSDoc())) {
                        var kind = scanner.getTokenValue();
                        if (kind === "link" || kind === "linkcode" || kind === "linkplain") {
                            return kind;
                        }
                    }
                }
                function parseUnknownTag(start, tagName, indent, indentText) {
                    return finishNode(factory.createJSDocUnknownTag(tagName, parseTrailingTagComments(start, getNodePos(), indent, indentText)), start);
                }
                function addTag(tag) {
                    if (!tag) {
                        return;
                    }
                    if (!tags) {
                        tags = [tag];
                        tagsPos = tag.pos;
                    }
                    else {
                        tags.push(tag);
                    }
                    tagsEnd = tag.end;
                }
                function tryParseTypeExpression() {
                    skipWhitespaceOrAsterisk();
                    return token() === 18 /* OpenBraceToken */ ? parseJSDocTypeExpression() : undefined;
                }
                function parseBracketNameInPropertyAndParamTag() {
                    // Looking for something like '[foo]', 'foo', '[foo.bar]' or 'foo.bar'
                    var isBracketed = parseOptionalJsdoc(22 /* OpenBracketToken */);
                    if (isBracketed) {
                        skipWhitespace();
                    }
                    // a markdown-quoted name: `arg` is not legal jsdoc, but occurs in the wild
                    var isBackquoted = parseOptionalJsdoc(61 /* BacktickToken */);
                    var name = parseJSDocEntityName();
                    if (isBackquoted) {
                        parseExpectedTokenJSDoc(61 /* BacktickToken */);
                    }
                    if (isBracketed) {
                        skipWhitespace();
                        // May have an optional default, e.g. '[foo = 42]'
                        if (parseOptionalToken(63 /* EqualsToken */)) {
                            parseExpression();
                        }
                        parseExpected(23 /* CloseBracketToken */);
                    }
                    return { name: name, isBracketed: isBracketed };
                }
                function isObjectOrObjectArrayTypeReference(node) {
                    switch (node.kind) {
                        case 147 /* ObjectKeyword */:
                            return true;
                        case 182 /* ArrayType */:
                            return isObjectOrObjectArrayTypeReference(node.elementType);
                        default:
                            return ts.isTypeReferenceNode(node) && ts.isIdentifier(node.typeName) && node.typeName.escapedText === "Object" && !node.typeArguments;
                    }
                }
                function parseParameterOrPropertyTag(start, tagName, target, indent) {
                    var typeExpression = tryParseTypeExpression();
                    var isNameFirst = !typeExpression;
                    skipWhitespaceOrAsterisk();
                    var _a = parseBracketNameInPropertyAndParamTag(), name = _a.name, isBracketed = _a.isBracketed;
                    var indentText = skipWhitespaceOrAsterisk();
                    if (isNameFirst && !lookAhead(parseJSDocLinkPrefix)) {
                        typeExpression = tryParseTypeExpression();
                    }
                    var comment = parseTrailingTagComments(start, getNodePos(), indent, indentText);
                    var nestedTypeLiteral = target !== 4 /* CallbackParameter */ && parseNestedTypeLiteral(typeExpression, name, target, indent);
                    if (nestedTypeLiteral) {
                        typeExpression = nestedTypeLiteral;
                        isNameFirst = true;
                    }
                    var result = target === 1 /* Property */
                        ? factory.createJSDocPropertyTag(tagName, name, isBracketed, typeExpression, isNameFirst, comment)
                        : factory.createJSDocParameterTag(tagName, name, isBracketed, typeExpression, isNameFirst, comment);
                    return finishNode(result, start);
                }
                function parseNestedTypeLiteral(typeExpression, name, target, indent) {
                    if (typeExpression && isObjectOrObjectArrayTypeReference(typeExpression.type)) {
                        var pos = getNodePos();
                        var child = void 0;
                        var children = void 0;
                        while (child = tryParse(function () { return parseChildParameterOrPropertyTag(target, indent, name); })) {
                            if (child.kind === 338 /* JSDocParameterTag */ || child.kind === 345 /* JSDocPropertyTag */) {
                                children = ts.append(children, child);
                            }
                        }
                        if (children) {
                            var literal = finishNode(factory.createJSDocTypeLiteral(children, typeExpression.type.kind === 182 /* ArrayType */), pos);
                            return finishNode(factory.createJSDocTypeExpression(literal), pos);
                        }
                    }
                }
                function parseReturnTag(start, tagName, indent, indentText) {
                    if (ts.some(tags, ts.isJSDocReturnTag)) {
                        parseErrorAt(tagName.pos, scanner.getTokenPos(), ts.Diagnostics._0_tag_already_specified, tagName.escapedText);
                    }
                    var typeExpression = tryParseTypeExpression();
                    return finishNode(factory.createJSDocReturnTag(tagName, typeExpression, parseTrailingTagComments(start, getNodePos(), indent, indentText)), start);
                }
                function parseTypeTag(start, tagName, indent, indentText) {
                    if (ts.some(tags, ts.isJSDocTypeTag)) {
                        parseErrorAt(tagName.pos, scanner.getTokenPos(), ts.Diagnostics._0_tag_already_specified, tagName.escapedText);
                    }
                    var typeExpression = parseJSDocTypeExpression(/*mayOmitBraces*/ true);
                    var comments = indent !== undefined && indentText !== undefined ? parseTrailingTagComments(start, getNodePos(), indent, indentText) : undefined;
                    return finishNode(factory.createJSDocTypeTag(tagName, typeExpression, comments), start);
                }
                function parseSeeTag(start, tagName, indent, indentText) {
                    var isMarkdownOrJSDocLink = token() === 22 /* OpenBracketToken */
                        || lookAhead(function () { return nextTokenJSDoc() === 59 /* AtToken */ && ts.tokenIsIdentifierOrKeyword(nextTokenJSDoc()) && scanner.getTokenValue() === "link"; });
                    var nameExpression = isMarkdownOrJSDocLink ? undefined : parseJSDocNameReference();
                    var comments = indent !== undefined && indentText !== undefined ? parseTrailingTagComments(start, getNodePos(), indent, indentText) : undefined;
                    return finishNode(factory.createJSDocSeeTag(tagName, nameExpression, comments), start);
                }
                function parseAuthorTag(start, tagName, indent, indentText) {
                    var commentStart = getNodePos();
                    var textOnly = parseAuthorNameAndEmail();
                    var commentEnd = scanner.getStartPos();
                    var comments = parseTrailingTagComments(start, commentEnd, indent, indentText);
                    if (!comments) {
                        commentEnd = scanner.getStartPos();
                    }
                    var allParts = typeof comments !== "string"
                        ? createNodeArray(ts.concatenate([finishNode(textOnly, commentStart, commentEnd)], comments), commentStart) // cast away readonly
                        : textOnly.text + comments;
                    return finishNode(factory.createJSDocAuthorTag(tagName, allParts), start);
                }
                function parseAuthorNameAndEmail() {
                    var comments = [];
                    var inEmail = false;
                    var token = scanner.getToken();
                    while (token !== 1 /* EndOfFileToken */ && token !== 4 /* NewLineTrivia */) {
                        if (token === 29 /* LessThanToken */) {
                            inEmail = true;
                        }
                        else if (token === 59 /* AtToken */ && !inEmail) {
                            break;
                        }
                        else if (token === 31 /* GreaterThanToken */ && inEmail) {
                            comments.push(scanner.getTokenText());
                            scanner.setTextPos(scanner.getTokenPos() + 1);
                            break;
                        }
                        comments.push(scanner.getTokenText());
                        token = nextTokenJSDoc();
                    }
                    return factory.createJSDocText(comments.join(""));
                }
                function parseImplementsTag(start, tagName, margin, indentText) {
                    var className = parseExpressionWithTypeArgumentsForAugments();
                    return finishNode(factory.createJSDocImplementsTag(tagName, className, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
                }
                function parseAugmentsTag(start, tagName, margin, indentText) {
                    var className = parseExpressionWithTypeArgumentsForAugments();
                    return finishNode(factory.createJSDocAugmentsTag(tagName, className, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
                }
                function parseExpressionWithTypeArgumentsForAugments() {
                    var usedBrace = parseOptional(18 /* OpenBraceToken */);
                    var pos = getNodePos();
                    var expression = parsePropertyAccessEntityNameExpression();
                    var typeArguments = tryParseTypeArguments();
                    var node = factory.createExpressionWithTypeArguments(expression, typeArguments);
                    var res = finishNode(node, pos);
                    if (usedBrace) {
                        parseExpected(19 /* CloseBraceToken */);
                    }
                    return res;
                }
                function parsePropertyAccessEntityNameExpression() {
                    var pos = getNodePos();
                    var node = parseJSDocIdentifierName();
                    while (parseOptional(24 /* DotToken */)) {
                        var name = parseJSDocIdentifierName();
                        node = finishNode(factory.createPropertyAccessExpression(node, name), pos);
                    }
                    return node;
                }
                function parseSimpleTag(start, createTag, tagName, margin, indentText) {
                    return finishNode(createTag(tagName, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
                }
                function parseThisTag(start, tagName, margin, indentText) {
                    var typeExpression = parseJSDocTypeExpression(/*mayOmitBraces*/ true);
                    skipWhitespace();
                    return finishNode(factory.createJSDocThisTag(tagName, typeExpression, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
                }
                function parseEnumTag(start, tagName, margin, indentText) {
                    var typeExpression = parseJSDocTypeExpression(/*mayOmitBraces*/ true);
                    skipWhitespace();
                    return finishNode(factory.createJSDocEnumTag(tagName, typeExpression, parseTrailingTagComments(start, getNodePos(), margin, indentText)), start);
                }
                function parseTypedefTag(start, tagName, indent, indentText) {
                    var _a;
                    var typeExpression = tryParseTypeExpression();
                    skipWhitespaceOrAsterisk();
                    var fullName = parseJSDocTypeNameWithNamespace();
                    skipWhitespace();
                    var comment = parseTagComments(indent);
                    var end;
                    if (!typeExpression || isObjectOrObjectArrayTypeReference(typeExpression.type)) {
                        var child = void 0;
                        var childTypeTag = void 0;
                        var jsDocPropertyTags = void 0;
                        var hasChildren = false;
                        while (child = tryParse(function () { return parseChildPropertyTag(indent); })) {
                            hasChildren = true;
                            if (child.kind === 341 /* JSDocTypeTag */) {
                                if (childTypeTag) {
                                    parseErrorAtCurrentToken(ts.Diagnostics.A_JSDoc_typedef_comment_may_not_contain_multiple_type_tags);
                                    var lastError = ts.lastOrUndefined(parseDiagnostics);
                                    if (lastError) {
                                        ts.addRelatedInfo(lastError, ts.createDetachedDiagnostic(fileName, 0, 0, ts.Diagnostics.The_tag_was_first_specified_here));
                                    }
                                    break;
                                }
                                else {
                                    childTypeTag = child;
                                }
                            }
                            else {
                                jsDocPropertyTags = ts.append(jsDocPropertyTags, child);
                            }
                        }
                        if (hasChildren) {
                            var isArrayType = typeExpression && typeExpression.type.kind === 182 /* ArrayType */;
                            var jsdocTypeLiteral = factory.createJSDocTypeLiteral(jsDocPropertyTags, isArrayType);
                            typeExpression = childTypeTag && childTypeTag.typeExpression && !isObjectOrObjectArrayTypeReference(childTypeTag.typeExpression.type) ?
                                childTypeTag.typeExpression :
                                finishNode(jsdocTypeLiteral, start);
                            end = typeExpression.end;
                        }
                    }
                    // Only include the characters between the name end and the next token if a comment was actually parsed out - otherwise it's just whitespace
                    end = end || comment !== undefined ?
                        getNodePos() :
                        ((_a = fullName !== null && fullName !== void 0 ? fullName : typeExpression) !== null && _a !== void 0 ? _a : tagName).end;
                    if (!comment) {
                        comment = parseTrailingTagComments(start, end, indent, indentText);
                    }
                    var typedefTag = factory.createJSDocTypedefTag(tagName, typeExpression, fullName, comment);
                    return finishNode(typedefTag, start, end);
                }
                function parseJSDocTypeNameWithNamespace(nested) {
                    var pos = scanner.getTokenPos();
                    if (!ts.tokenIsIdentifierOrKeyword(token())) {
                        return undefined;
                    }
                    var typeNameOrNamespaceName = parseJSDocIdentifierName();
                    if (parseOptional(24 /* DotToken */)) {
                        var body = parseJSDocTypeNameWithNamespace(/*nested*/ true);
                        var jsDocNamespaceNode = factory.createModuleDeclaration(
                        /*decorators*/ undefined, 
                        /*modifiers*/ undefined, typeNameOrNamespaceName, body, nested ? 4 /* NestedNamespace */ : undefined);
                        return finishNode(jsDocNamespaceNode, pos);
                    }
                    if (nested) {
                        typeNameOrNamespaceName.isInJSDocNamespace = true;
                    }
                    return typeNameOrNamespaceName;
                }
                function parseCallbackTagParameters(indent) {
                    var pos = getNodePos();
                    var child;
                    var parameters;
                    while (child = tryParse(function () { return parseChildParameterOrPropertyTag(4 /* CallbackParameter */, indent); })) {
                        parameters = ts.append(parameters, child);
                    }
                    return createNodeArray(parameters || [], pos);
                }
                function parseCallbackTag(start, tagName, indent, indentText) {
                    var fullName = parseJSDocTypeNameWithNamespace();
                    skipWhitespace();
                    var comment = parseTagComments(indent);
                    var parameters = parseCallbackTagParameters(indent);
                    var returnTag = tryParse(function () {
                        if (parseOptionalJsdoc(59 /* AtToken */)) {
                            var tag = parseTag(indent);
                            if (tag && tag.kind === 339 /* JSDocReturnTag */) {
                                return tag;
                            }
                        }
                    });
                    var typeExpression = finishNode(factory.createJSDocSignature(/*typeParameters*/ undefined, parameters, returnTag), start);
                    if (!comment) {
                        comment = parseTrailingTagComments(start, getNodePos(), indent, indentText);
                    }
                    return finishNode(factory.createJSDocCallbackTag(tagName, typeExpression, fullName, comment), start);
                }
                function escapedTextsEqual(a, b) {
                    while (!ts.isIdentifier(a) || !ts.isIdentifier(b)) {
                        if (!ts.isIdentifier(a) && !ts.isIdentifier(b) && a.right.escapedText === b.right.escapedText) {
                            a = a.left;
                            b = b.left;
                        }
                        else {
                            return false;
                        }
                    }
                    return a.escapedText === b.escapedText;
                }
                function parseChildPropertyTag(indent) {
                    return parseChildParameterOrPropertyTag(1 /* Property */, indent);
                }
                function parseChildParameterOrPropertyTag(target, indent, name) {
                    var canParseTag = true;
                    var seenAsterisk = false;
                    while (true) {
                        switch (nextTokenJSDoc()) {
                            case 59 /* AtToken */:
                                if (canParseTag) {
                                    var child = tryParseChildTag(target, indent);
                                    if (child && (child.kind === 338 /* JSDocParameterTag */ || child.kind === 345 /* JSDocPropertyTag */) &&
                                        target !== 4 /* CallbackParameter */ &&
                                        name && (ts.isIdentifier(child.name) || !escapedTextsEqual(name, child.name.left))) {
                                        return false;
                                    }
                                    return child;
                                }
                                seenAsterisk = false;
                                break;
                            case 4 /* NewLineTrivia */:
                                canParseTag = true;
                                seenAsterisk = false;
                                break;
                            case 41 /* AsteriskToken */:
                                if (seenAsterisk) {
                                    canParseTag = false;
                                }
                                seenAsterisk = true;
                                break;
                            case 79 /* Identifier */:
                                canParseTag = false;
                                break;
                            case 1 /* EndOfFileToken */:
                                return false;
                        }
                    }
                }
                function tryParseChildTag(target, indent) {
                    ts.Debug.assert(token() === 59 /* AtToken */);
                    var start = scanner.getStartPos();
                    nextTokenJSDoc();
                    var tagName = parseJSDocIdentifierName();
                    skipWhitespace();
                    var t;
                    switch (tagName.escapedText) {
                        case "type":
                            return target === 1 /* Property */ && parseTypeTag(start, tagName);
                        case "prop":
                        case "property":
                            t = 1 /* Property */;
                            break;
                        case "arg":
                        case "argument":
                        case "param":
                            t = 2 /* Parameter */ | 4 /* CallbackParameter */;
                            break;
                        default:
                            return false;
                    }
                    if (!(target & t)) {
                        return false;
                    }
                    return parseParameterOrPropertyTag(start, tagName, target, indent);
                }
                function parseTemplateTagTypeParameter() {
                    var typeParameterPos = getNodePos();
                    var isBracketed = parseOptionalJsdoc(22 /* OpenBracketToken */);
                    if (isBracketed) {
                        skipWhitespace();
                    }
                    var name = parseJSDocIdentifierName(ts.Diagnostics.Unexpected_token_A_type_parameter_name_was_expected_without_curly_braces);
                    var defaultType;
                    if (isBracketed) {
                        skipWhitespace();
                        parseExpected(63 /* EqualsToken */);
                        defaultType = doInsideOfContext(4194304 /* JSDoc */, parseJSDocType);
                        parseExpected(23 /* CloseBracketToken */);
                    }
                    if (ts.nodeIsMissing(name)) {
                        return undefined;
                    }
                    return finishNode(factory.createTypeParameterDeclaration(name, /*constraint*/ undefined, defaultType), typeParameterPos);
                }
                function parseTemplateTagTypeParameters() {
                    var pos = getNodePos();
                    var typeParameters = [];
                    do {
                        skipWhitespace();
                        var node = parseTemplateTagTypeParameter();
                        if (node !== undefined) {
                            typeParameters.push(node);
                        }
                        skipWhitespaceOrAsterisk();
                    } while (parseOptionalJsdoc(27 /* CommaToken */));
                    return createNodeArray(typeParameters, pos);
                }
                function parseTemplateTag(start, tagName, indent, indentText) {
                    // The template tag looks like one of the following:
                    //   @template T,U,V
                    //   @template {Constraint} T
                    //
                    // According to the [closure docs](https://github.com/google/closure-compiler/wiki/Generic-Types#multiple-bounded-template-types):
                    //   > Multiple bounded generics cannot be declared on the same line. For the sake of clarity, if multiple templates share the same
                    //   > type bound they must be declared on separate lines.
                    //
                    // TODO: Determine whether we should enforce this in the checker.
                    // TODO: Consider moving the `constraint` to the first type parameter as we could then remove `getEffectiveConstraintOfTypeParameter`.
                    // TODO: Consider only parsing a single type parameter if there is a constraint.
                    var constraint = token() === 18 /* OpenBraceToken */ ? parseJSDocTypeExpression() : undefined;
                    var typeParameters = parseTemplateTagTypeParameters();
                    return finishNode(factory.createJSDocTemplateTag(tagName, constraint, typeParameters, parseTrailingTagComments(start, getNodePos(), indent, indentText)), start);
                }
                function parseOptionalJsdoc(t) {
                    if (token() === t) {
                        nextTokenJSDoc();
                        return true;
                    }
                    return false;
                }
                function parseJSDocEntityName() {
                    var entity = parseJSDocIdentifierName();
                    if (parseOptional(22 /* OpenBracketToken */)) {
                        parseExpected(23 /* CloseBracketToken */);
                        // Note that y[] is accepted as an entity name, but the postfix brackets are not saved for checking.
                        // Technically usejsdoc.org requires them for specifying a property of a type equivalent to Array<{ x: ...}>
                        // but it's not worth it to enforce that restriction.
                    }
                    while (parseOptional(24 /* DotToken */)) {
                        var name = parseJSDocIdentifierName();
                        if (parseOptional(22 /* OpenBracketToken */)) {
                            parseExpected(23 /* CloseBracketToken */);
                        }
                        entity = createQualifiedName(entity, name);
                    }
                    return entity;
                }
                function parseJSDocIdentifierName(message) {
                    if (!ts.tokenIsIdentifierOrKeyword(token())) {
                        return createMissingNode(79 /* Identifier */, /*reportAtCurrentPosition*/ !message, message || ts.Diagnostics.Identifier_expected);
                    }
                    identifierCount++;
                    var pos = scanner.getTokenPos();
                    var end = scanner.getTextPos();
                    var originalKeywordKind = token();
                    var text = internIdentifier(scanner.getTokenValue());
                    var result = finishNode(factory.createIdentifier(text, /*typeArguments*/ undefined, originalKeywordKind), pos, end);
                    nextTokenJSDoc();
                    return result;
                }
            }
        })(JSDocParser = Parser.JSDocParser || (Parser.JSDocParser = {}));
    })(Parser || (Parser = {}));
    var IncrementalParser;
    (function (IncrementalParser) {
        function updateSourceFile(sourceFile, newText, textChangeRange, aggressiveChecks) {
            aggressiveChecks = aggressiveChecks || ts.Debug.shouldAssert(2 /* Aggressive */);
            checkChangeRange(sourceFile, newText, textChangeRange, aggressiveChecks);
            if (ts.textChangeRangeIsUnchanged(textChangeRange)) {
                // if the text didn't change, then we can just return our current source file as-is.
                return sourceFile;
            }
            if (sourceFile.statements.length === 0) {
                // If we don't have any statements in the current source file, then there's no real
                // way to incrementally parse.  So just do a full parse instead.
                return Parser.parseSourceFile(sourceFile.fileName, newText, sourceFile.languageVersion, /*syntaxCursor*/ undefined, /*setParentNodes*/ true, sourceFile.scriptKind);
            }
            // Make sure we're not trying to incrementally update a source file more than once.  Once
            // we do an update the original source file is considered unusable from that point onwards.
            //
            // This is because we do incremental parsing in-place.  i.e. we take nodes from the old
            // tree and give them new positions and parents.  From that point on, trusting the old
            // tree at all is not possible as far too much of it may violate invariants.
            var incrementalSourceFile = sourceFile;
            ts.Debug.assert(!incrementalSourceFile.hasBeenIncrementallyParsed);
            incrementalSourceFile.hasBeenIncrementallyParsed = true;
            Parser.fixupParentReferences(incrementalSourceFile);
            var oldText = sourceFile.text;
            var syntaxCursor = createSyntaxCursor(sourceFile);
            // Make the actual change larger so that we know to reparse anything whose lookahead
            // might have intersected the change.
            var changeRange = extendToAffectedRange(sourceFile, textChangeRange);
            checkChangeRange(sourceFile, newText, changeRange, aggressiveChecks);
            // Ensure that extending the affected range only moved the start of the change range
            // earlier in the file.
            ts.Debug.assert(changeRange.span.start <= textChangeRange.span.start);
            ts.Debug.assert(ts.textSpanEnd(changeRange.span) === ts.textSpanEnd(textChangeRange.span));
            ts.Debug.assert(ts.textSpanEnd(ts.textChangeRangeNewSpan(changeRange)) === ts.textSpanEnd(ts.textChangeRangeNewSpan(textChangeRange)));
            // The is the amount the nodes after the edit range need to be adjusted.  It can be
            // positive (if the edit added characters), negative (if the edit deleted characters)
            // or zero (if this was a pure overwrite with nothing added/removed).
            var delta = ts.textChangeRangeNewSpan(changeRange).length - changeRange.span.length;
            // If we added or removed characters during the edit, then we need to go and adjust all
            // the nodes after the edit.  Those nodes may move forward (if we inserted chars) or they
            // may move backward (if we deleted chars).
            //
            // Doing this helps us out in two ways.  First, it means that any nodes/tokens we want
            // to reuse are already at the appropriate position in the new text.  That way when we
            // reuse them, we don't have to figure out if they need to be adjusted.  Second, it makes
            // it very easy to determine if we can reuse a node.  If the node's position is at where
            // we are in the text, then we can reuse it.  Otherwise we can't.  If the node's position
            // is ahead of us, then we'll need to rescan tokens.  If the node's position is behind
            // us, then we'll need to skip it or crumble it as appropriate
            //
            // We will also adjust the positions of nodes that intersect the change range as well.
            // By doing this, we ensure that all the positions in the old tree are consistent, not
            // just the positions of nodes entirely before/after the change range.  By being
            // consistent, we can then easily map from positions to nodes in the old tree easily.
            //
            // Also, mark any syntax elements that intersect the changed span.  We know, up front,
            // that we cannot reuse these elements.
            updateTokenPositionsAndMarkElements(incrementalSourceFile, changeRange.span.start, ts.textSpanEnd(changeRange.span), ts.textSpanEnd(ts.textChangeRangeNewSpan(changeRange)), delta, oldText, newText, aggressiveChecks);
            // Now that we've set up our internal incremental state just proceed and parse the
            // source file in the normal fashion.  When possible the parser will retrieve and
            // reuse nodes from the old tree.
            //
            // Note: passing in 'true' for setNodeParents is very important.  When incrementally
            // parsing, we will be reusing nodes from the old tree, and placing it into new
            // parents.  If we don't set the parents now, we'll end up with an observably
            // inconsistent tree.  Setting the parents on the new tree should be very fast.  We
            // will immediately bail out of walking any subtrees when we can see that their parents
            // are already correct.
            var result = Parser.parseSourceFile(sourceFile.fileName, newText, sourceFile.languageVersion, syntaxCursor, /*setParentNodes*/ true, sourceFile.scriptKind);
            result.commentDirectives = getNewCommentDirectives(sourceFile.commentDirectives, result.commentDirectives, changeRange.span.start, ts.textSpanEnd(changeRange.span), delta, oldText, newText, aggressiveChecks);
            result.impliedNodeFormat = sourceFile.impliedNodeFormat;
            return result;
        }
        IncrementalParser.updateSourceFile = updateSourceFile;
        function getNewCommentDirectives(oldDirectives, newDirectives, changeStart, changeRangeOldEnd, delta, oldText, newText, aggressiveChecks) {
            if (!oldDirectives)
                return newDirectives;
            var commentDirectives;
            var addedNewlyScannedDirectives = false;
            for (var _i = 0, oldDirectives_1 = oldDirectives; _i < oldDirectives_1.length; _i++) {
                var directive = oldDirectives_1[_i];
                var range = directive.range, type = directive.type;
                // Range before the change
                if (range.end < changeStart) {
                    commentDirectives = ts.append(commentDirectives, directive);
                }
                else if (range.pos > changeRangeOldEnd) {
                    addNewlyScannedDirectives();
                    // Node is entirely past the change range.  We need to move both its pos and
                    // end, forward or backward appropriately.
                    var updatedDirective = {
                        range: { pos: range.pos + delta, end: range.end + delta },
                        type: type
                    };
                    commentDirectives = ts.append(commentDirectives, updatedDirective);
                    if (aggressiveChecks) {
                        ts.Debug.assert(oldText.substring(range.pos, range.end) === newText.substring(updatedDirective.range.pos, updatedDirective.range.end));
                    }
                }
                // Ignore ranges that fall in change range
            }
            addNewlyScannedDirectives();
            return commentDirectives;
            function addNewlyScannedDirectives() {
                if (addedNewlyScannedDirectives)
                    return;
                addedNewlyScannedDirectives = true;
                if (!commentDirectives) {
                    commentDirectives = newDirectives;
                }
                else if (newDirectives) {
                    commentDirectives.push.apply(commentDirectives, newDirectives);
                }
            }
        }
        function moveElementEntirelyPastChangeRange(element, isArray, delta, oldText, newText, aggressiveChecks) {
            if (isArray) {
                visitArray(element);
            }
            else {
                visitNode(element);
            }
            return;
            function visitNode(node) {
                var text = "";
                if (aggressiveChecks && shouldCheckNode(node)) {
                    text = oldText.substring(node.pos, node.end);
                }
                // Ditch any existing LS children we may have created.  This way we can avoid
                // moving them forward.
                if (node._children) {
                    node._children = undefined;
                }
                ts.setTextRangePosEnd(node, node.pos + delta, node.end + delta);
                if (aggressiveChecks && shouldCheckNode(node)) {
                    ts.Debug.assert(text === newText.substring(node.pos, node.end));
                }
                forEachChild(node, visitNode, visitArray);
                if (ts.hasJSDocNodes(node)) {
                    for (var _i = 0, _a = node.jsDoc; _i < _a.length; _i++) {
                        var jsDocComment = _a[_i];
                        visitNode(jsDocComment);
                    }
                }
                checkNodePositions(node, aggressiveChecks);
            }
            function visitArray(array) {
                array._children = undefined;
                ts.setTextRangePosEnd(array, array.pos + delta, array.end + delta);
                for (var _i = 0, array_9 = array; _i < array_9.length; _i++) {
                    var node = array_9[_i];
                    visitNode(node);
                }
            }
        }
        function shouldCheckNode(node) {
            switch (node.kind) {
                case 10 /* StringLiteral */:
                case 8 /* NumericLiteral */:
                case 79 /* Identifier */:
                    return true;
            }
            return false;
        }
        function adjustIntersectingElement(element, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta) {
            ts.Debug.assert(element.end >= changeStart, "Adjusting an element that was entirely before the change range");
            ts.Debug.assert(element.pos <= changeRangeOldEnd, "Adjusting an element that was entirely after the change range");
            ts.Debug.assert(element.pos <= element.end);
            // We have an element that intersects the change range in some way.  It may have its
            // start, or its end (or both) in the changed range.  We want to adjust any part
            // that intersects such that the final tree is in a consistent state.  i.e. all
            // children have spans within the span of their parent, and all siblings are ordered
            // properly.
            // We may need to update both the 'pos' and the 'end' of the element.
            // If the 'pos' is before the start of the change, then we don't need to touch it.
            // If it isn't, then the 'pos' must be inside the change.  How we update it will
            // depend if delta is positive or negative. If delta is positive then we have
            // something like:
            //
            //  -------------------AAA-----------------
            //  -------------------BBBCCCCCCC-----------------
            //
            // In this case, we consider any node that started in the change range to still be
            // starting at the same position.
            //
            // however, if the delta is negative, then we instead have something like this:
            //
            //  -------------------XXXYYYYYYY-----------------
            //  -------------------ZZZ-----------------
            //
            // In this case, any element that started in the 'X' range will keep its position.
            // However any element that started after that will have their pos adjusted to be
            // at the end of the new range.  i.e. any node that started in the 'Y' range will
            // be adjusted to have their start at the end of the 'Z' range.
            //
            // The element will keep its position if possible.  Or Move backward to the new-end
            // if it's in the 'Y' range.
            var pos = Math.min(element.pos, changeRangeNewEnd);
            // If the 'end' is after the change range, then we always adjust it by the delta
            // amount.  However, if the end is in the change range, then how we adjust it
            // will depend on if delta is positive or negative.  If delta is positive then we
            // have something like:
            //
            //  -------------------AAA-----------------
            //  -------------------BBBCCCCCCC-----------------
            //
            // In this case, we consider any node that ended inside the change range to keep its
            // end position.
            //
            // however, if the delta is negative, then we instead have something like this:
            //
            //  -------------------XXXYYYYYYY-----------------
            //  -------------------ZZZ-----------------
            //
            // In this case, any element that ended in the 'X' range will keep its position.
            // However any element that ended after that will have their pos adjusted to be
            // at the end of the new range.  i.e. any node that ended in the 'Y' range will
            // be adjusted to have their end at the end of the 'Z' range.
            var end = element.end >= changeRangeOldEnd ?
                // Element ends after the change range.  Always adjust the end pos.
                element.end + delta :
                // Element ends in the change range.  The element will keep its position if
                // possible. Or Move backward to the new-end if it's in the 'Y' range.
                Math.min(element.end, changeRangeNewEnd);
            ts.Debug.assert(pos <= end);
            if (element.parent) {
                ts.Debug.assertGreaterThanOrEqual(pos, element.parent.pos);
                ts.Debug.assertLessThanOrEqual(end, element.parent.end);
            }
            ts.setTextRangePosEnd(element, pos, end);
        }
        function checkNodePositions(node, aggressiveChecks) {
            if (aggressiveChecks) {
                var pos_2 = node.pos;
                var visitNode_1 = function (child) {
                    ts.Debug.assert(child.pos >= pos_2);
                    pos_2 = child.end;
                };
                if (ts.hasJSDocNodes(node)) {
                    for (var _i = 0, _a = node.jsDoc; _i < _a.length; _i++) {
                        var jsDocComment = _a[_i];
                        visitNode_1(jsDocComment);
                    }
                }
                forEachChild(node, visitNode_1);
                ts.Debug.assert(pos_2 <= node.end);
            }
        }
        function updateTokenPositionsAndMarkElements(sourceFile, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta, oldText, newText, aggressiveChecks) {
            visitNode(sourceFile);
            return;
            function visitNode(child) {
                ts.Debug.assert(child.pos <= child.end);
                if (child.pos > changeRangeOldEnd) {
                    // Node is entirely past the change range.  We need to move both its pos and
                    // end, forward or backward appropriately.
                    moveElementEntirelyPastChangeRange(child, /*isArray*/ false, delta, oldText, newText, aggressiveChecks);
                    return;
                }
                // Check if the element intersects the change range.  If it does, then it is not
                // reusable.  Also, we'll need to recurse to see what constituent portions we may
                // be able to use.
                var fullEnd = child.end;
                if (fullEnd >= changeStart) {
                    child.intersectsChange = true;
                    child._children = undefined;
                    // Adjust the pos or end (or both) of the intersecting element accordingly.
                    adjustIntersectingElement(child, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta);
                    forEachChild(child, visitNode, visitArray);
                    if (ts.hasJSDocNodes(child)) {
                        for (var _i = 0, _a = child.jsDoc; _i < _a.length; _i++) {
                            var jsDocComment = _a[_i];
                            visitNode(jsDocComment);
                        }
                    }
                    checkNodePositions(child, aggressiveChecks);
                    return;
                }
                // Otherwise, the node is entirely before the change range.  No need to do anything with it.
                ts.Debug.assert(fullEnd < changeStart);
            }
            function visitArray(array) {
                ts.Debug.assert(array.pos <= array.end);
                if (array.pos > changeRangeOldEnd) {
                    // Array is entirely after the change range.  We need to move it, and move any of
                    // its children.
                    moveElementEntirelyPastChangeRange(array, /*isArray*/ true, delta, oldText, newText, aggressiveChecks);
                    return;
                }
                // Check if the element intersects the change range.  If it does, then it is not
                // reusable.  Also, we'll need to recurse to see what constituent portions we may
                // be able to use.
                var fullEnd = array.end;
                if (fullEnd >= changeStart) {
                    array.intersectsChange = true;
                    array._children = undefined;
                    // Adjust the pos or end (or both) of the intersecting array accordingly.
                    adjustIntersectingElement(array, changeStart, changeRangeOldEnd, changeRangeNewEnd, delta);
                    for (var _i = 0, array_10 = array; _i < array_10.length; _i++) {
                        var node = array_10[_i];
                        visitNode(node);
                    }
                    return;
                }
                // Otherwise, the array is entirely before the change range.  No need to do anything with it.
                ts.Debug.assert(fullEnd < changeStart);
            }
        }
        function extendToAffectedRange(sourceFile, changeRange) {
            // Consider the following code:
            //      void foo() { /; }
            //
            // If the text changes with an insertion of / just before the semicolon then we end up with:
            //      void foo() { //; }
            //
            // If we were to just use the changeRange a is, then we would not rescan the { token
            // (as it does not intersect the actual original change range).  Because an edit may
            // change the token touching it, we actually need to look back *at least* one token so
            // that the prior token sees that change.
            var maxLookahead = 1;
            var start = changeRange.span.start;
            // the first iteration aligns us with the change start. subsequent iteration move us to
            // the left by maxLookahead tokens.  We only need to do this as long as we're not at the
            // start of the tree.
            for (var i = 0; start > 0 && i <= maxLookahead; i++) {
                var nearestNode = findNearestNodeStartingBeforeOrAtPosition(sourceFile, start);
                ts.Debug.assert(nearestNode.pos <= start);
                var position = nearestNode.pos;
                start = Math.max(0, position - 1);
            }
            var finalSpan = ts.createTextSpanFromBounds(start, ts.textSpanEnd(changeRange.span));
            var finalLength = changeRange.newLength + (changeRange.span.start - start);
            return ts.createTextChangeRange(finalSpan, finalLength);
        }
        function findNearestNodeStartingBeforeOrAtPosition(sourceFile, position) {
            var bestResult = sourceFile;
            var lastNodeEntirelyBeforePosition;
            forEachChild(sourceFile, visit);
            if (lastNodeEntirelyBeforePosition) {
                var lastChildOfLastEntireNodeBeforePosition = getLastDescendant(lastNodeEntirelyBeforePosition);
                if (lastChildOfLastEntireNodeBeforePosition.pos > bestResult.pos) {
                    bestResult = lastChildOfLastEntireNodeBeforePosition;
                }
            }
            return bestResult;
            function getLastDescendant(node) {
                while (true) {
                    var lastChild = ts.getLastChild(node);
                    if (lastChild) {
                        node = lastChild;
                    }
                    else {
                        return node;
                    }
                }
            }
            function visit(child) {
                if (ts.nodeIsMissing(child)) {
                    // Missing nodes are effectively invisible to us.  We never even consider them
                    // When trying to find the nearest node before us.
                    return;
                }
                // If the child intersects this position, then this node is currently the nearest
                // node that starts before the position.
                if (child.pos <= position) {
                    if (child.pos >= bestResult.pos) {
                        // This node starts before the position, and is closer to the position than
                        // the previous best node we found.  It is now the new best node.
                        bestResult = child;
                    }
                    // Now, the node may overlap the position, or it may end entirely before the
                    // position.  If it overlaps with the position, then either it, or one of its
                    // children must be the nearest node before the position.  So we can just
                    // recurse into this child to see if we can find something better.
                    if (position < child.end) {
                        // The nearest node is either this child, or one of the children inside
                        // of it.  We've already marked this child as the best so far.  Recurse
                        // in case one of the children is better.
                        forEachChild(child, visit);
                        // Once we look at the children of this node, then there's no need to
                        // continue any further.
                        return true;
                    }
                    else {
                        ts.Debug.assert(child.end <= position);
                        // The child ends entirely before this position.  Say you have the following
                        // (where $ is the position)
                        //
                        //      <complex expr 1> ? <complex expr 2> $ : <...> <...>
                        //
                        // We would want to find the nearest preceding node in "complex expr 2".
                        // To support that, we keep track of this node, and once we're done searching
                        // for a best node, we recurse down this node to see if we can find a good
                        // result in it.
                        //
                        // This approach allows us to quickly skip over nodes that are entirely
                        // before the position, while still allowing us to find any nodes in the
                        // last one that might be what we want.
                        lastNodeEntirelyBeforePosition = child;
                    }
                }
                else {
                    ts.Debug.assert(child.pos > position);
                    // We're now at a node that is entirely past the position we're searching for.
                    // This node (and all following nodes) could never contribute to the result,
                    // so just skip them by returning 'true' here.
                    return true;
                }
            }
        }
        function checkChangeRange(sourceFile, newText, textChangeRange, aggressiveChecks) {
            var oldText = sourceFile.text;
            if (textChangeRange) {
                ts.Debug.assert((oldText.length - textChangeRange.span.length + textChangeRange.newLength) === newText.length);
                if (aggressiveChecks || ts.Debug.shouldAssert(3 /* VeryAggressive */)) {
                    var oldTextPrefix = oldText.substr(0, textChangeRange.span.start);
                    var newTextPrefix = newText.substr(0, textChangeRange.span.start);
                    ts.Debug.assert(oldTextPrefix === newTextPrefix);
                    var oldTextSuffix = oldText.substring(ts.textSpanEnd(textChangeRange.span), oldText.length);
                    var newTextSuffix = newText.substring(ts.textSpanEnd(ts.textChangeRangeNewSpan(textChangeRange)), newText.length);
                    ts.Debug.assert(oldTextSuffix === newTextSuffix);
                }
            }
        }
        function createSyntaxCursor(sourceFile) {
            var currentArray = sourceFile.statements;
            var currentArrayIndex = 0;
            ts.Debug.assert(currentArrayIndex < currentArray.length);
            var current = currentArray[currentArrayIndex];
            var lastQueriedPosition = -1 /* Value */;
            return {
                currentNode: function (position) {
                    // Only compute the current node if the position is different than the last time
                    // we were asked.  The parser commonly asks for the node at the same position
                    // twice.  Once to know if can read an appropriate list element at a certain point,
                    // and then to actually read and consume the node.
                    if (position !== lastQueriedPosition) {
                        // Much of the time the parser will need the very next node in the array that
                        // we just returned a node from.So just simply check for that case and move
                        // forward in the array instead of searching for the node again.
                        if (current && current.end === position && currentArrayIndex < (currentArray.length - 1)) {
                            currentArrayIndex++;
                            current = currentArray[currentArrayIndex];
                        }
                        // If we don't have a node, or the node we have isn't in the right position,
                        // then try to find a viable node at the position requested.
                        if (!current || current.pos !== position) {
                            findHighestListElementThatStartsAtPosition(position);
                        }
                    }
                    // Cache this query so that we don't do any extra work if the parser calls back
                    // into us.  Note: this is very common as the parser will make pairs of calls like
                    // 'isListElement -> parseListElement'.  If we were unable to find a node when
                    // called with 'isListElement', we don't want to redo the work when parseListElement
                    // is called immediately after.
                    lastQueriedPosition = position;
                    // Either we don'd have a node, or we have a node at the position being asked for.
                    ts.Debug.assert(!current || current.pos === position);
                    return current;
                }
            };
            // Finds the highest element in the tree we can find that starts at the provided position.
            // The element must be a direct child of some node list in the tree.  This way after we
            // return it, we can easily return its next sibling in the list.
            function findHighestListElementThatStartsAtPosition(position) {
                // Clear out any cached state about the last node we found.
                currentArray = undefined;
                currentArrayIndex = -1 /* Value */;
                current = undefined;
                // Recurse into the source file to find the highest node at this position.
                forEachChild(sourceFile, visitNode, visitArray);
                return;
                function visitNode(node) {
                    if (position >= node.pos && position < node.end) {
                        // Position was within this node.  Keep searching deeper to find the node.
                        forEachChild(node, visitNode, visitArray);
                        // don't proceed any further in the search.
                        return true;
                    }
                    // position wasn't in this node, have to keep searching.
                    return false;
                }
                function visitArray(array) {
                    if (position >= array.pos && position < array.end) {
                        // position was in this array.  Search through this array to see if we find a
                        // viable element.
                        for (var i = 0; i < array.length; i++) {
                            var child = array[i];
                            if (child) {
                                if (child.pos === position) {
                                    // Found the right node.  We're done.
                                    currentArray = array;
                                    currentArrayIndex = i;
                                    current = child;
                                    return true;
                                }
                                else {
                                    if (child.pos < position && position < child.end) {
                                        // Position in somewhere within this child.  Search in it and
                                        // stop searching in this array.
                                        forEachChild(child, visitNode, visitArray);
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    // position wasn't in this array, have to keep searching.
                    return false;
                }
            }
        }
        IncrementalParser.createSyntaxCursor = createSyntaxCursor;
        var InvalidPosition;
        (function (InvalidPosition) {
            InvalidPosition[InvalidPosition["Value"] = -1] = "Value";
        })(InvalidPosition || (InvalidPosition = {}));
    })(IncrementalParser || (IncrementalParser = {}));
    /** @internal */
    function isDeclarationFileName(fileName) {
        return ts.fileExtensionIsOneOf(fileName, [".d.ts" /* Dts */, ".d.mts" /* Dmts */, ".d.cts" /* Dcts */]);
    }
    ts.isDeclarationFileName = isDeclarationFileName;
    /*@internal*/
    function processCommentPragmas(context, sourceText) {
        var pragmas = [];
        for (var _i = 0, _a = ts.getLeadingCommentRanges(sourceText, 0) || ts.emptyArray; _i < _a.length; _i++) {
            var range = _a[_i];
            var comment = sourceText.substring(range.pos, range.end);
            extractPragmas(pragmas, range, comment);
        }
        context.pragmas = new ts.Map();
        for (var _b = 0, pragmas_1 = pragmas; _b < pragmas_1.length; _b++) {
            var pragma = pragmas_1[_b];
            if (context.pragmas.has(pragma.name)) {
                var currentValue = context.pragmas.get(pragma.name);
                if (currentValue instanceof Array) {
                    currentValue.push(pragma.args);
                }
                else {
                    context.pragmas.set(pragma.name, [currentValue, pragma.args]);
                }
                continue;
            }
            context.pragmas.set(pragma.name, pragma.args);
        }
    }
    ts.processCommentPragmas = processCommentPragmas;
    /*@internal*/
    function processPragmasIntoFields(context, reportDiagnostic) {
        context.checkJsDirective = undefined;
        context.referencedFiles = [];
        context.typeReferenceDirectives = [];
        context.libReferenceDirectives = [];
        context.amdDependencies = [];
        context.hasNoDefaultLib = false;
        context.pragmas.forEach(function (entryOrList, key) {
            // TODO: The below should be strongly type-guarded and not need casts/explicit annotations, since entryOrList is related to
            // key and key is constrained to a union; but it's not (see GH#21483 for at least partial fix) :(
            switch (key) {
                case "reference": {
                    var referencedFiles_1 = context.referencedFiles;
                    var typeReferenceDirectives_1 = context.typeReferenceDirectives;
                    var libReferenceDirectives_1 = context.libReferenceDirectives;
                    ts.forEach(ts.toArray(entryOrList), function (arg) {
                        var _a = arg.arguments, types = _a.types, lib = _a.lib, path = _a.path;
                        if (arg.arguments["no-default-lib"]) {
                            context.hasNoDefaultLib = true;
                        }
                        else if (types) {
                            typeReferenceDirectives_1.push({ pos: types.pos, end: types.end, fileName: types.value });
                        }
                        else if (lib) {
                            libReferenceDirectives_1.push({ pos: lib.pos, end: lib.end, fileName: lib.value });
                        }
                        else if (path) {
                            referencedFiles_1.push({ pos: path.pos, end: path.end, fileName: path.value });
                        }
                        else {
                            reportDiagnostic(arg.range.pos, arg.range.end - arg.range.pos, ts.Diagnostics.Invalid_reference_directive_syntax);
                        }
                    });
                    break;
                }
                case "amd-dependency": {
                    context.amdDependencies = ts.map(ts.toArray(entryOrList), function (x) { return ({ name: x.arguments.name, path: x.arguments.path }); });
                    break;
                }
                case "amd-module": {
                    if (entryOrList instanceof Array) {
                        for (var _i = 0, entryOrList_1 = entryOrList; _i < entryOrList_1.length; _i++) {
                            var entry = entryOrList_1[_i];
                            if (context.moduleName) {
                                // TODO: It's probably fine to issue this diagnostic on all instances of the pragma
                                reportDiagnostic(entry.range.pos, entry.range.end - entry.range.pos, ts.Diagnostics.An_AMD_module_cannot_have_multiple_name_assignments);
                            }
                            context.moduleName = entry.arguments.name;
                        }
                    }
                    else {
                        context.moduleName = entryOrList.arguments.name;
                    }
                    break;
                }
                case "ts-nocheck":
                case "ts-check": {
                    // _last_ of either nocheck or check in a file is the "winner"
                    ts.forEach(ts.toArray(entryOrList), function (entry) {
                        if (!context.checkJsDirective || entry.range.pos > context.checkJsDirective.pos) {
                            context.checkJsDirective = {
                                enabled: key === "ts-check",
                                end: entry.range.end,
                                pos: entry.range.pos
                            };
                        }
                    });
                    break;
                }
                case "jsx":
                case "jsxfrag":
                case "jsximportsource":
                case "jsxruntime":
                    return; // Accessed directly
                default: ts.Debug.fail("Unhandled pragma kind"); // Can this be made into an assertNever in the future?
            }
        });
    }
    ts.processPragmasIntoFields = processPragmasIntoFields;
    var namedArgRegExCache = new ts.Map();
    function getNamedArgRegEx(name) {
        if (namedArgRegExCache.has(name)) {
            return namedArgRegExCache.get(name);
        }
        var result = new RegExp("(\\s".concat(name, "\\s*=\\s*)(?:(?:'([^']*)')|(?:\"([^\"]*)\"))"), "im");
        namedArgRegExCache.set(name, result);
        return result;
    }
    var tripleSlashXMLCommentStartRegEx = /^\/\/\/\s*<(\S+)\s.*?\/>/im;
    var singleLinePragmaRegEx = /^\/\/\/?\s*@(\S+)\s*(.*)\s*$/im;
    function extractPragmas(pragmas, range, text) {
        var tripleSlash = range.kind === 2 /* SingleLineCommentTrivia */ && tripleSlashXMLCommentStartRegEx.exec(text);
        if (tripleSlash) {
            var name = tripleSlash[1].toLowerCase(); // Technically unsafe cast, but we do it so the below check to make it safe typechecks
            var pragma = ts.commentPragmas[name];
            if (!pragma || !(pragma.kind & 1 /* TripleSlashXML */)) {
                return;
            }
            if (pragma.args) {
                var argument = {};
                for (var _i = 0, _a = pragma.args; _i < _a.length; _i++) {
                    var arg = _a[_i];
                    var matcher = getNamedArgRegEx(arg.name);
                    var matchResult = matcher.exec(text);
                    if (!matchResult && !arg.optional) {
                        return; // Missing required argument, don't parse
                    }
                    else if (matchResult) {
                        var value = matchResult[2] || matchResult[3];
                        if (arg.captureSpan) {
                            var startPos = range.pos + matchResult.index + matchResult[1].length + 1;
                            argument[arg.name] = {
                                value: value,
                                pos: startPos,
                                end: startPos + value.length
                            };
                        }
                        else {
                            argument[arg.name] = value;
                        }
                    }
                }
                pragmas.push({ name: name, args: { arguments: argument, range: range } });
            }
            else {
                pragmas.push({ name: name, args: { arguments: {}, range: range } });
            }
            return;
        }
        var singleLine = range.kind === 2 /* SingleLineCommentTrivia */ && singleLinePragmaRegEx.exec(text);
        if (singleLine) {
            return addPragmaForMatch(pragmas, range, 2 /* SingleLine */, singleLine);
        }
        if (range.kind === 3 /* MultiLineCommentTrivia */) {
            var multiLinePragmaRegEx = /@(\S+)(\s+.*)?$/gim; // Defined inline since it uses the "g" flag, which keeps a persistent index (for iterating)
            var multiLineMatch = void 0;
            while (multiLineMatch = multiLinePragmaRegEx.exec(text)) {
                addPragmaForMatch(pragmas, range, 4 /* MultiLine */, multiLineMatch);
            }
        }
    }
    function addPragmaForMatch(pragmas, range, kind, match) {
        if (!match)
            return;
        var name = match[1].toLowerCase(); // Technically unsafe cast, but we do it so they below check to make it safe typechecks
        var pragma = ts.commentPragmas[name];
        if (!pragma || !(pragma.kind & kind)) {
            return;
        }
        var args = match[2]; // Split on spaces and match up positionally with definition
        var argument = getNamedPragmaArguments(pragma, args);
        if (argument === "fail")
            return; // Missing required argument, fail to parse it
        pragmas.push({ name: name, args: { arguments: argument, range: range } });
        return;
    }
    function getNamedPragmaArguments(pragma, text) {
        if (!text)
            return {};
        if (!pragma.args)
            return {};
        var args = ts.trimString(text).split(/\s+/);
        var argMap = {};
        for (var i = 0; i < pragma.args.length; i++) {
            var argument = pragma.args[i];
            if (!args[i] && !argument.optional) {
                return "fail";
            }
            if (argument.captureSpan) {
                return ts.Debug.fail("Capture spans not yet implemented for non-xml pragmas");
            }
            argMap[argument.name] = args[i];
        }
        return argMap;
    }
    /** @internal */
    function tagNamesAreEquivalent(lhs, rhs) {
        if (lhs.kind !== rhs.kind) {
            return false;
        }
        if (lhs.kind === 79 /* Identifier */) {
            return lhs.escapedText === rhs.escapedText;
        }
        if (lhs.kind === 108 /* ThisKeyword */) {
            return true;
        }
        // If we are at this statement then we must have PropertyAccessExpression and because tag name in Jsx element can only
        // take forms of JsxTagNameExpression which includes an identifier, "this" expression, or another propertyAccessExpression
        // it is safe to case the expression property as such. See parseJsxElementName for how we parse tag name in Jsx element
        return lhs.name.escapedText === rhs.name.escapedText &&
            tagNamesAreEquivalent(lhs.expression, rhs.expression);
    }
    ts.tagNamesAreEquivalent = tagNamesAreEquivalent;
})(ts || (ts = {}));
