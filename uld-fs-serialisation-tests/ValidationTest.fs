namespace ULD.Fs.Tests

module ValidationTest =

  open FsUnit
  open NUnit.Framework
  open ULD.Fs.Serialisation.ULDValidation
  open ULD.Fs.DTOs

  type InitMsgUtils () =
    inherit FSharpCustomMessageFormatter ()

  [<SetUp>]
  let Setup () =
    ()

  let VALID_LANGUAGE_DEFINITIONS: LanguageDefinition = {
    name = Some "TestLanguage"
    filePattern = Some "**/*.lang"
    version = "v1.0.0"
    comments = {
      documentationComments = [{
        start = "/*"
        end' = "*/"
        treatAs = " "
      }]
      normalComments = [{
        start = "(*"
        end' = "*)"
        treatAs = ""
      }]
    }
    startRules = [ "startRule1"; "startRule2" ]
    rules = Map.empty
      .Add("startRule1", [
        NonTerminal "$$ws"
        String "public"
        NonTerminal "startRule2"
      ])
      .Add("startRule2", [
        NonTerminal "$$ws"
        String "void"
        NonTerminal "$$ws"
        NonTerminal "identifier"
        String "("
        NonTerminal "$$ws"
        String ")"
        NonTerminal "$$ws"
      ])
      .Add("identifier", [
        Action "identifier"
        Letter
        OneOf (true, [ "identifier|onlyLetters" ])
        Action "identifier"
      ])
      .Add("identifier|onlyLetters", [
        LetterOrDigit
        OneOf (true, [ "identifier|onlyLetters" ])
      ])
      .Add("$$ws", [
        Whitespace
        OneOf (true, [ "$$ws" ])
      ])
  }

  [<Test>]
  let ``Valid language definition`` () =
    validate VALID_LANGUAGE_DEFINITIONS
    |> should equal ValidationResult.Empty

  [<Test>]
  let ``Name is empty`` () =
    // given: a language definition with an empty name
    let langDef = { VALID_LANGUAGE_DEFINITIONS with name = Some "" }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a missing name warning
    result.Warnings |> should equal [ "Name is empty – you can omit it" ]
    result.Errors |> should be Empty

  [<Test>]
  let ``Version is not v1.0.0`` () =
    // given: a language definition with a newer version
    let langDef = { VALID_LANGUAGE_DEFINITIONS with version = "v1.0.1" }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a false version warning
    result.Warnings |> should equal [ "Version is not 'v1.0.0 – this validation might be out of date" ]
    result.Errors |> should be Empty

  [<Test>]
  let ``Comment starts are empty`` () =
    // given: a language definition with a comment definition with an empty start
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with comments = {
          documentationComments = []
          normalComments = [{
            start = ""
            end' = "*)"
            treatAs = " "
          }]
        }
      }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty comment start error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "A comment definition start is empty – empty strings would match everything, so everything would be a comment" ]

  [<Test>]
  let ``Multiple Comment starts are empty`` () =
    // given: a language definition with a normal and a documentationcomment definition with an empty start
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with comments = {
          documentationComments = [{
            start = ""
            end' = "*)"
            treatAs = " "
          }]
          normalComments = [{
            start = ""
            end' = "*)"
            treatAs = " "
          }]
        }
      }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty comment start error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "2 comment definition starts are empty – empty strings would match everything, so everything would be a comment" ]

  [<Test>]
  let ``Comment ends are empty`` () =
    // given: a language definition with a comment definition with an empty start
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with comments = {
          documentationComments = []
          normalComments = [{
            start = "(*"
            end' = ""
            treatAs = " "
          }]
        }
      }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty comment start error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "A comment definition end is empty – empty strings would match everything, so comments would immediatedly end" ]

  [<Test>]
  let ``Multiple Comment ends are empty`` () =
    // given: a language definition with a normal and a documentationcomment definition with an empty start
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with comments = {
          documentationComments = [{
            start = "(*"
            end' = ""
            treatAs = " "
          }]
          normalComments = [{
            start = "(*"
            end' = ""
            treatAs = " "
          }]
        }
      }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty comment start error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "2 comment definition ends are empty – empty strings would match everything, so comments would immediatedly end" ]

  [<Test>]
  let ``A rule has an empty name`` () =
    // given: a language definition with a rule with an empty name
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules.Add("", [ NonTerminal "" ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty rule name Error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "A rule name is empty" ]

  [<Test>]
  let ``A rule is not referenced by anything`` () =
    // given: a language definition with a rule that is never referenced
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules.Add("a letter", [ Letter ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a rule-not-referenced warning
    result.Warnings |> should equal [ "Rule 'a letter' is never referenced – you can omit it" ]
    result.Errors |> should be Empty

  [<Test>]
  let ``A rule has no symbols`` () =
    // given: a language definition with a rule that has no productions
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("a letter", [ NonTerminal "a letter"; NonTerminal "emptyRule" ])
          .Add("emptyRule", [])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty rule production warning
    result.Warnings |> should equal [ "Rule 'emptyRule' has no symbols – you can omit it" ]
    result.Errors |> should be Empty

  [<Test>]
  let ``No start rule is defined`` () =
    // given: a language definition with no starting rule
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with startRules = []
             rules = Map.empty
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a missing starting rule error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "No start rule defined" ]

  [<Test>]
  let ``A start rule references a non-existing rule`` () =
    // given: a language definition with a non-existing starting rule
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with startRules = [ "unknown rule"; "startRule1" ]
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a missing starting rule warning
    result.Warnings |> should equal [ "Starting rule 'unknown rule' does not exist" ]
    result.Errors |> should be Empty

  [<Test>]
  let ``A NonTerminal references a non-existing rule`` () =
    // given: a language definition with a non-terminal referencing a non-existing rule
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("new rule", [ NonTerminal "unknown rule"; NonTerminal "new rule" ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a missing referenced rule error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "Rule 'unknown rule' does not have a defined production (referenced in a NonTerminal symbol in rule 'new rule')" ]
  
  [<Test>]
  let ``An action has an invalid command`` () =
    // given: a language definition with an invalid action
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("new rule", [ NonTerminal "new rule"; Action "invalid action" ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an invalid action error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "Command 'invalid action' is not recognised (appears in rule 'new rule')" ]
  
  [<Test>]
  let ``A OneOf option references an unknown rule`` () =
    // given: a language definition with an OneOf referencing a non-existing rule
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("new rule", [ OneOf (false, [ "new rule"; "unknown rule" ]) ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a missing referenced rule error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "Rule 'unknown rule' does not have a defined production (referenced in an OneOf symbol in rule 'new rule')" ]
  
  [<Test>]
  let ``A OneOf is empty and doesn't allow none`` () =
    // given: a language definition with an empty OneOf
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("referencing rule", [ OneOf (false, [ "referencing rule"; "new rule" ]) ])
          .Add("new rule", [ OneOf (false, []) ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty OneOf error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "A OneOf has no options and does not allow none – this symbol matches nothing (found in rule 'new rule')" ]
  
  [<Test>]
  let ``A OneOf is empty and does allow none`` () =
    // given: a language definition with an empty OneOf
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("referencing rule", [ OneOf (false, [ "referencing rule"; "new rule" ]) ])
          .Add("new rule", [ OneOf (true, []) ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty OneOf warning
    result.Warnings |> should equal [ "A OneOf has no options, but allows none – you can omit this symbol (found in rule 'new rule')" ]
    result.Errors |> should be Empty
  
  [<Test>]
  let ``A CharacterOf is empty`` () =
    // given: a language definition with an empty CharacterOf
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("referencing rule", [ OneOf (false, [ "referencing rule"; "new rule" ]) ])
          .Add("new rule", [ CharacterOf [] ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty CharacterOf error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "A CharacterOf has no characters defined (found in rule 'new rule')" ]
  
  [<Test>]
  let ``No CharacterOf has duplicate characters`` () =
    // given: a language definition with a CharacterOf with duplicate characters
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("referencing rule", [ OneOf (false, [ "referencing rule"; "new rule" ]) ])
          .Add("new rule", [ CharacterOf [ 'a'; 'b'; 'a' ] ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a duplicate characters warning
    result.Warnings |> should equal [ "A CharacterOf with the characters 'aba' has duplicate characters (found in rule 'new rule') – you can simplify it" ]
    result.Errors |> should be Empty
  
  [<Test>]
  let ``No CharacterExcept is empty`` () =
    // given: a language definition with an empty CharacterExcept
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("referencing rule", [ OneOf (false, [ "referencing rule"; "new rule" ]) ])
          .Add("new rule", [ CharacterExcept [] ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns an empty CharacterExcept error
    result.Warnings |> should be Empty
    result.Errors |> should equal [ "A CharacterExcept has no characters defined (found in rule 'new rule')" ]
  
  [<Test>]
  let ``No CharacterExcept has duplicate characters`` () =
    // given: a language definition with a CharacterExcept with duplicate characters
    let langDef = {
      VALID_LANGUAGE_DEFINITIONS
        with rules = VALID_LANGUAGE_DEFINITIONS.rules
          .Add("referencing rule", [ OneOf (false, [ "referencing rule"; "new rule" ]) ])
          .Add("new rule", [ CharacterExcept [ 'a'; 'b'; 'a' ] ])
    }

    // when: validating this language definition
    let result = validate langDef

    // then: it returns a duplicate characters warning
    result.Warnings |> should equal [ "A CharacterExcept with the characters 'aba' has duplicate characters (found in rule 'new rule') – you can simplify it" ]
    result.Errors |> should be Empty
