namespace ULD.Fs

module DTOs =

  type public CommentDefinition = {
    startMarker: string
    endMarker: string
    treatAs: string
  }

  type public CommentsDefinition = {
    documentationComments: CommentDefinition list
    normalComments: CommentDefinition list
  }

  type public SymbolDefinition =
    | Action of command: string
    | NonTerminal of referencedRule: string
    | OneOf of allowNone: bool * options: string list
    | Whitespace
    | LineEnd
    | String of text: string
    | Digit
    | Letter
    | LetterOrDigit
    | LowercaseLetter
    | UppercaseLetter
    | Character
    | CharacterOf of chars: char list
    | CharacterExcept of chars: char list

  let (|Terminal|) (ruleDef: SymbolDefinition) =
    match ruleDef with
    | Action _ -> false
    | NonTerminal _ -> false
    | OneOf _ -> false
    | _ -> true

  type public LanguageDefinition = {
    name: string option
    filePattern: string option
    version: string
    comments: CommentsDefinition
    startRules: string list
    rules: Map<string, SymbolDefinition list>
  }

