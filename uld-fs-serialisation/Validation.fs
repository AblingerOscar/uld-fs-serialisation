namespace ULD.Fs.Serialisation

open ULD.Fs.DTOs

module public ULDValidation =
  module private Validators =
    type WarningOrError =
    | Warning of message: string
    | Error of message: string

    let private getAllRuleNames (langDef: LanguageDefinition) =
      langDef.rules
      |> Map.toSeq
      |> Seq.map fst
      |> Set.ofSeq

    let private nameIsNotEmpty (langDef: LanguageDefinition) = [ // warning only
        match langDef.name with
        | Some name ->
          if name.Length = 0 then
            yield Warning "Name is empty – you can omit it"
        | _ -> ()
      ]
    let private filePatternIsNotEmpty (langDef: LanguageDefinition) = [ // warning only
        // based upon https://microsoft.github.io/language-server-protocol/specifications/specification-current/#documentFilter
        match langDef.filePattern with
        | Some pattern ->
           if pattern.Length = 0 then
            yield Warning "FilePattern is empty – you can omit it"
        | _ -> ()
      ]
    let private versionIsV1_0_0 (langDef: LanguageDefinition) = [ // warning only
        if langDef.version <> "v1.0.0" then
          yield Warning "Version is not 'v1.0.0 – this validation might be out of date"
      ]
    
    let private commentElementsAreNotEmpty (getElement: CommentDefinition -> string) (commentElementName: string) (explanation: string) (langDef: LanguageDefinition) = [
        let nrOfEmptyStarts =
          langDef.comments.documentationComments
          |> List.append langDef.comments.normalComments
          |> List.map getElement
          |> List.filter(fun start -> start.Length = 0)
          |> List.length

        if nrOfEmptyStarts <> 0 then
          if nrOfEmptyStarts = 1 then
            yield Error $"A comment definition {commentElementName} is empty – empty strings would match everything, {explanation}"
          else
            yield Error $"{nrOfEmptyStarts} comment definition {commentElementName}s are empty – empty strings would match everything, {explanation}"
      ]
    let private commentStartsAreNotEmpty = commentElementsAreNotEmpty (fun comment -> comment.start) "start" "so everything would be a comment"
    let private commentEndsAreNotEmpty =  commentElementsAreNotEmpty (fun comment -> comment.end') "end" "so comments would immediatedly end"
    let private noRuleHasAnEmptyName (langDef: LanguageDefinition) = [
        let nrOfEmptyRuleNames =
          langDef.rules
          |> Map.toSeq
          |> Seq.map fst
          |> Seq.filter (String.length >> (=) 0)
          |> Seq.length

        if nrOfEmptyRuleNames <> 0 then
          yield Error "A rule name is empty"
      ]
    let private noRuleIsUnreferenced (langDef: LanguageDefinition) = [ // warning only
        let ruleNames =
          getAllRuleNames langDef
          |> Set.ofSeq
        let ruleReferences =
          langDef.rules
          |> Map.toList
          |> List.collect snd
          |> List.choose (fun symbol ->
            match symbol with
            | NonTerminal referencedRule -> Some [ referencedRule ]
            | OneOf (_, referencedRules) -> Some referencedRules
            | _ -> None)
          |> List.collect id
          |> List.append langDef.startRules
          |> Set.ofSeq

        for unreferencedRule in Set.difference ruleNames ruleReferences do
          yield Warning $"Rule '{unreferencedRule}' is never referenced – you can omit it"
      ]
    let private noRuleHasNoSymbols (langDef: LanguageDefinition) = [ // warning only
      yield!
        langDef.rules
        |> Map.toList
        |> List.filter (snd >> List.isEmpty)
        |> List.map (fun rule -> Warning $"Rule '{rule}' has no symbols – you can omit it")
    ]
    let private atLeastOneStartRule (langDef: LanguageDefinition) = [
        if langDef.startRules.Length = 0 then
          yield Error "No start rule defined"
      ]
    let private noStartRuleReferencesAnUnknownRule (langDef: LanguageDefinition) = [
        let ruleNames = getAllRuleNames langDef

        let undefinedRules =
          langDef.startRules
          |> List.filter (ruleNames.Contains >> not)

        for undefinedRule in undefinedRules do
          yield Warning $"Starting rule '{undefinedRule}' does not exist"
      ]
    let private noNonTerminalsReferencesAnUnknownRule (langDef: LanguageDefinition) = [
        let ruleNames = getAllRuleNames langDef

        for rule in langDef.rules |> Map.toSeq do
          let undefinedReferencedRules =
            rule
            |> snd
            |> List.choose (fun symbol ->
              match symbol with
              | NonTerminal referencedRule -> Some referencedRule
              | _-> None
            )
            |> List.distinct
            |> List.filter (ruleNames.Contains >> not)

          for undefinedRule in undefinedReferencedRules do
            yield Error $"Rule '{undefinedRule}' does not have a defined production (referenced in a NonTerminal symbol in rule '{fst rule}')"
      ]
    let private ACTION_REGEX = System.Text.RegularExpressions.Regex "^(?:identifier|identifierType (inner|set [^ ]+)|identifierKind set [^ ]+|declaration|definition|implementation|folding (start|end)|environment (pop|(push|import) (fixed|inner) [^ ]*))$"
    let private noActionHasAnInvalidCommands (langDef: LanguageDefinition) = [
        for rule in langDef.rules |> Map.toSeq do
          let invalidActionCommands =
            rule
            |> snd
            |> List.choose (fun symbol ->
              match symbol with
              | Action command -> Some command
              | _ -> None
            )
            |> List.distinct
            |> List.filter (ACTION_REGEX.IsMatch >> not)

          for invalidCommand in invalidActionCommands do
            yield Error $"Command '{invalidCommand}' is not recognised (appears in rule '{fst rule}')"
      ]
    let private noOneOfOptionReferencesAnUnknownRule (langDef: LanguageDefinition) = [
        let ruleNames = getAllRuleNames langDef

        for rule in langDef.rules |> Map.toSeq do
          let undefinedReferencedRules =
            rule
            |> snd
            |> List.choose (fun symbol ->
              match symbol with
              | OneOf (_, referencedRule) -> Some referencedRule
              | _-> None
            )
            |> List.collect id
            |> List.distinct
            |> List.filter (ruleNames.Contains >> not)

          for undefinedRule in undefinedReferencedRules do
            yield Error $"Rule '{undefinedRule}' does not have a defined production (referenced in an OneOf symbol in rule '{fst rule}')"
      ]
    let private noOneOfIsEmptyAndDoesNotAllowNone (langDef: LanguageDefinition) = [
        let ruleContainsEmptyOneOf =
          snd
          >> List.choose (fun symbol ->
            match symbol with
            | OneOf (false, referencedRule) -> Some referencedRule
            | _-> None
          )
          >> List.filter List.isEmpty
          >> List.isEmpty
          >> not

        for rule in langDef.rules |> Map.toSeq do
          if ruleContainsEmptyOneOf rule then
            yield Error $"A OneOf has no options and does not allow none – this symbol matches nothing (found in rule '{fst rule}')"
      ]
    let private noOneOfIsEmptyAndDoesAllowNone (langDef: LanguageDefinition) = [ // warning only
        let ruleContainsEmptyOneOf =
          snd
          >> List.choose (fun symbol ->
            match symbol with
            | OneOf (true, referencedRule) -> Some referencedRule
            | _-> None
          )
          >> List.filter List.isEmpty
          >> List.isEmpty
          >> not

        for rule in langDef.rules |> Map.toSeq do
          if ruleContainsEmptyOneOf rule then
            yield Warning $"A OneOf has no options, but allows none – you can omit this symbol (found in rule '{fst rule}')"
      ]
    let private noCharacterOfIsEmpty (langDef: LanguageDefinition) = [
        let ruleContainsEmptyCharacterOf =
          snd
          >> List.choose (fun symbol ->
            match symbol with
            | CharacterOf characters -> Some characters
            | _-> None
          )
          >> List.filter List.isEmpty
          >> List.isEmpty
          >> not

        for rule in langDef.rules |> Map.toSeq do
          if ruleContainsEmptyCharacterOf rule then
            yield Error $"A CharacterOf has no characters defined (found in rule '{fst rule}')"
      ]
    let private hasDuplicates (l: 'a list) =
      let rec recHasDuplicates (knownElements: Set<'a>) nextElements =
        match nextElements with
        | head::tail ->
          if knownElements.Contains head then
            true
          else
            recHasDuplicates (knownElements.Add head) tail
        | [] -> false

      recHasDuplicates Set.empty l
    let private noCharacterOfHasDuplicateCharacters (langDef: LanguageDefinition) = [ // warning only
        for rule in langDef.rules |> Map.toSeq do
          yield!
            rule
            |> snd
            |> List.choose (fun symbol ->
              match symbol with
              | CharacterOf characters -> Some characters
              | _ -> None
            )
            |> List.filter hasDuplicates
            |> List.map (Array.ofList >> string)
            |> List.map (fun characters ->
              Warning $"A CharacterOf with the characters '{characters}' has duplicate characters (found in rule '{fst rule}') – you can simplify it"
            )
      ]
    let private noCharacterExceptIsEmpty (langDef: LanguageDefinition) = [
        let ruleContainsEmptyCharacterExcept =
          snd
          >> List.choose (fun symbol ->
            match symbol with
            | CharacterExcept characters -> Some characters
            | _-> None
          )
          >> List.filter List.isEmpty
          >> List.isEmpty
          >> not

        for rule in langDef.rules |> Map.toSeq do
          if ruleContainsEmptyCharacterExcept rule then
            yield Error $"A CharacterExcept has no characters defined (found in rule '{fst rule}')"
      ]
    let private noCharacterExceptHasDuplicateCharacters (langDef: LanguageDefinition) = [ // warning only
        for rule in langDef.rules |> Map.toSeq do
          yield!
            rule
            |> snd
            |> List.choose (fun symbol ->
              match symbol with
              | CharacterExcept characters -> Some characters
              | _ -> None
            )
            |> List.filter hasDuplicates
            |> List.map (Array.ofList >> System.String)
            |> List.map (fun characters ->
              Warning $"A CharacterExcept with the characters '{characters}' has duplicate characters (found in rule '{fst rule}') – you can simplify it"
            )
      ]

    let ALL_VALIDATORS: (LanguageDefinition -> WarningOrError list) list = [
      nameIsNotEmpty;
      versionIsV1_0_0;
      commentStartsAreNotEmpty;
      commentEndsAreNotEmpty;
      noRuleHasAnEmptyName;
      noRuleIsUnreferenced;
      noRuleHasNoSymbols;
      atLeastOneStartRule;
      noStartRuleReferencesAnUnknownRule;
      noNonTerminalsReferencesAnUnknownRule;
      noActionHasAnInvalidCommands;
      noOneOfOptionReferencesAnUnknownRule;
      noOneOfIsEmptyAndDoesNotAllowNone;
      noOneOfIsEmptyAndDoesAllowNone;
      noCharacterOfIsEmpty;
      noCharacterOfHasDuplicateCharacters;
      noCharacterExceptIsEmpty;
      noCharacterExceptHasDuplicateCharacters;
    ]
    
  type ValidationResult =
    {
      Errors: string list
      Warnings: string list
    }
    static member Empty = { Errors = []; Warnings = [] }

  let validate (langDef: LanguageDefinition): ValidationResult =
    Validators.ALL_VALIDATORS
    |> List.collect (fun validator -> validator langDef)
    |> List.fold (fun state warningOrError ->
      match warningOrError with
      | Validators.WarningOrError.Warning message -> { state with Warnings = message::state.Warnings }
      | Validators.WarningOrError.Error message -> { state with Errors = message::state.Errors }
      ) ValidationResult.Empty
