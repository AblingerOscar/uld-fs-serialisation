namespace ULD.Fs.Tests

module Serialisation =

  open FsUnit
  open NUnit.Framework
  open ULD.Fs.Serialisation.ULDSerialser
  open ULD.Fs.DTOs

  type InitMsgUtils () =
    inherit FSharpCustomMessageFormatter ()

  [<SetUp>]
  let Setup () =
      ()

  let isOk (value: Result<'a, 'b>): 'a option =
    match value with
    | Ok res ->
      Some res
    | Error msg ->
      Assert.Fail ("Expected Ok, but found Error" + msg.ToString())
      None

  let isErrorWith (expectedResult: 'b) (value: Result<'a, 'b>): 'b option =
    match value with
    | Ok msg ->
      Assert.Fail ("Expected Error, but found Ok" + msg.ToString())
      None
    | Error res ->
      res |> should equal expectedResult
      Some res


  let languageDefinitionIsEqualTo (expected: LanguageDefinition) (actual: LanguageDefinition) =
    should equal (expected.name) (actual.name)
    should equal (expected.filePattern) (actual.filePattern)
    should equal (expected.version) (actual.version)
    should equal (expected.comments) (actual.comments)
    should equal (expected.startRules) (actual.startRules)
    should equal (expected.rules) (actual.rules)

  [<Test>]
  let ``Valid language definition with all features returns the correct language definition`` () =
    // given: a valid language definition text with all possible symbols
    let validLanguageDefiniton = """<?xml version="1.0" encoding="utf-8" standalone="yes"?>
  <languageDefinition p1:name="SimpleGrammar" p1:filePattern="**/*.simple" p1:version="v1.0.0" xmlns:p1="https://github.com/AblingerOscar/autosupport-definition" xmlns="https://github.com/AblingerOscar/autosupport-definition">
    <p1:comments>
      <p1:normalComments />
      <p1:documentationComments>
        <p1:comment>
          <p1:start>/*</p1:start>
          <p1:end>*/</p1:end>
          <p1:treatAs> </p1:treatAs>
        </p1:comment>
      </p1:documentationComments>
    </p1:comments>
    <p1:startRules>
      <p1:startRule>SimpleGrammar</p1:startRule>
    </p1:startRules>
    <p1:rules>
      <p1:rule p1:name="$$ws">
        <p1:whitespace />
        <p1:oneOf p1:allowNone="true">
          <p1:option>$$ws</p1:option>
        </p1:oneOf>
      </p1:rule>
      <p1:rule p1:name="letter">
        <p1:oneOf p1:allowNone="false">
          <p1:option>$$any_uppercase</p1:option>
          <p1:option>$$any_lowercase</p1:option>
        </p1:oneOf>
      </p1:rule>
      <p1:rule p1:name="digit">
        <p1:characterOf>0123456789</p1:characterOf>
      </p1:rule>
      <p1:rule p1:name="identifier|0_1$zeroOrMore">
        <p1:oneOf p1:allowNone="false">
          <p1:option>letter</p1:option>
          <p1:option>digit</p1:option>
        </p1:oneOf>
        <p1:oneOf p1:allowNone="true">
          <p1:option>identifier|0_1$zeroOrMore</p1:option>
        </p1:oneOf>
      </p1:rule>
      <p1:rule p1:name="identifier">
        <p1:string>let</p1:string>
        <p1:nonTerminal p1:referencedRule="$$ws" />
        <p1:action>identifier</p1:action>
        <p1:nonTerminal p1:referencedRule="letter" />
        <p1:oneOf p1:allowNone="true">
          <p1:option>identifier|0_1$zeroOrMore</p1:option>
        </p1:oneOf>
        <p1:action>identifier</p1:action>
        <p1:nonTerminal p1:referencedRule="$$ws" />
        <p1:lineEnd />
      </p1:rule>
      <p1:rule p1:name="otherSymbols">
        <p1:digit />
        <p1:letter />
        <p1:letterOrDigit />
        <p1:lowercaseLetter />
        <p1:uppercaseLetter />
        <p1:character />
        <p1:characterExcept >aeiou</p1:characterExcept>
      </p1:rule>
    </p1:rules>
  </languageDefinition>
  """

    let expected = {
      name = Some "SimpleGrammar"
      filePattern = Some "**/*.simple"
      version = "v1.0.0"
      comments = {
        documentationComments = [
          {
            start = "/*"
            end' = "*/"
            treatAs = " "
          }
        ]
        normalComments = [ ]
      }
      startRules = [ "SimpleGrammar" ]
      rules = Map.empty<string, SymbolDefinition list>
        .Add("$$ws", [ Whitespace; OneOf (true, [ "$$ws" ]) ])
        .Add("letter", [ OneOf (false, [ "$$any_uppercase"; "$$any_lowercase" ] ) ])
        .Add("digit", [ CharacterOf ("0123456789" |> Seq.toList) ])
        .Add("identifier|0_1$zeroOrMore", [ OneOf (false, [ "letter"; "digit" ]); OneOf (true, [ "identifier|0_1$zeroOrMore" ]) ])
        .Add("identifier", [
          String "let";
          NonTerminal "$$ws";
          Action "identifier";
          NonTerminal "letter";
          OneOf (true, [ "identifier|0_1$zeroOrMore" ]);
          Action "identifier";
          NonTerminal "$$ws";
          LineEnd;
        ])
        .Add("otherSymbols", [
          Digit;
          Letter;
          LetterOrDigit;
          LowercaseLetter;
          UppercaseLetter;
          Character;
          CharacterExcept ("aeiou" |> Seq.toList);
        ])
    }

    // when: this XML is deserialised
    let res = deserialiseFromString validLanguageDefiniton

    // then: a language definition file with the expected values is returned
    res
    |> isOk
    |> Option.iter (languageDefinitionIsEqualTo expected)


  [<Test>]
  let ``Text that isn't a xml file results in an error`` () =
    // when: a string that isn't XML is deserialised
    let ret = deserialiseFromString "not XML"

    // then: an error is returned
    ret
    |> isErrorWith (seq { "Data at the root level is invalid. Line 1, position 1." })
    |> ignore

  [<Test>]
  let ``XML file without a languageDefiniton`` () =
    // given: a XML that is valid according to the .xsd, but does not have the correct root element
    let commentXmlPart = """<?xml version="1.0" encoding="utf-8" standalone="yes"?>
  <p1:comments xmlns:p1="https://github.com/AblingerOscar/autosupport-definition" xmlns="https://github.com/AblingerOscar/autosupport-definition">
    <p1:normalComments />
    <p1:documentationComments>
      <p1:comment>
        <p1:start>/*</p1:start>
        <p1:end>*/</p1:end>
        <p1:treatAs> </p1:treatAs>
      </p1:comment>
    </p1:documentationComments>
  </p1:comments>
  """

    // when: this XML is deserialised
    let res = deserialiseFromString commentXmlPart

    // then: an error is returned that mentiones the missing root element
    res
    |> isErrorWith (seq { "Could not find root element <languageDefinition>" })
    |> ignore

  [<Test>]
  let ``Invalid elements in rules`` () =
    // given: a XML that has invalid symbols in a rule
    let commentXmlPart = """<?xml version="1.0" encoding="utf-8" standalone="yes"?>
  <languageDefinition p1:name="SimpleGrammar" p1:filePattern="**/*.simple" p1:version="v1.0.0" xmlns:p1="https://github.com/AblingerOscar/autosupport-definition" xmlns="https://github.com/AblingerOscar/autosupport-definition">
    <p1:comments>
      <p1:normalComments />
      <p1:documentationComments />
    </p1:comments>
    <p1:startRules>
      <p1:startRule>TestGrammar</p1:startRule>
    </p1:startRules>
    <p1:rules>
      <p1:rule p1:name="TestGrammar">
        <action>some action</action>
        <foo></foo>
        <bar></bar>
        <oof></oof>
        <action>some action</action>
      </p1:rule>
    </p1:rules>
  </languageDefinition>
  """

    // when: this XML is deserialised
    let res = deserialiseFromString commentXmlPart

    // then: an error is returned listing the invalid symbols
    res
    |> isErrorWith (seq {
        "Rule TestGrammar contains the following elements that cannot be parsed: foo, bar, oof."
      })
    |> ignore

  [<Test>]
  let ``Invalid elements outside of <rule> are ignored`` () =
    // given: a XML that has invalid symbols in a rule
    let commentXmlPart = """<?xml version="1.0" encoding="utf-8" standalone="yes"?>
  <languageDefinition p1:name="TestGrammar" p1:filePattern="**/*.simple" p1:version="v1.0.0" xmlns:p1="https://github.com/AblingerOscar/autosupport-definition" xmlns="https://github.com/AblingerOscar/autosupport-definition">
    <foo></foo>
    <p1:comments>
      <p1:normalComments />
      <p1:documentationComments />
      <foo></foo>
    </p1:comments>
    <p1:startRules>
      <foo></foo>
      <p1:startRule>TestGrammar</p1:startRule>
    </p1:startRules>
    <p1:rules>
      <p1:rule p1:name="TestGrammar">
        <action>some action</action>
      </p1:rule>
      <foo></foo>
    </p1:rules>
  </languageDefinition>
  """

    let expected = {
      name = Some "TestGrammar"
      filePattern = Some "**/*.simple"
      version = "v1.0.0"
      comments = {
        normalComments = []
        documentationComments = []
      }
      startRules = [ "TestGrammar" ]
      rules = Map.empty
        .Add("TestGrammar", [ Action "some action" ])
    }

    // when: this XML is deserialised
    let res = deserialiseFromString commentXmlPart

    // then: success is returned
    res
    |> isOk
    |> Option.iter (languageDefinitionIsEqualTo expected)
