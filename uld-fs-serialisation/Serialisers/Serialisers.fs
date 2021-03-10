module ULD.Fs.Serialisation.Serialisers

open ULD.Fs.DTOs

type SerialisationError = string seq
type LanguageDefinitionXML = FSharp.Data.XmlProvider<Schema = "https://raw.githubusercontent.com/AblingerOscar/uld-definition/main/v1.0.0-schema.xsd">

let parseLanguageDefinition (text: string) =
  let parsedDef = LanguageDefinitionXML.Parse text
  parsedDef.LanguageDefinition

let symbolParsers =
  Map.empty<string, System.Xml.Linq.XElement -> SymbolDefinition>
    .Add("action", (fun el -> Action el.Value))
    .Add("nonTerminal", (fun el -> NonTerminal (el.Attribute (System.Xml.Linq.XName.op_Implicit "referencedRule")).Value))
    .Add("oneOf", (fun el ->
      let options =
        el.Elements ()
        |> Seq.map (fun optionEl -> optionEl.Value)
        |> Seq.toList
      let allowNone =
        el.Attributes ()
        |> Seq.tryFind (fun attr -> attr.Name.LocalName = "allowNone")
        |> Option.map (fun attr -> attr.Value)
        |> Option.map bool.Parse
        |> Option.defaultValue false

      OneOf (allowNone, options)))
    .Add("whitespace", (fun _ -> Whitespace))
    .Add("lineEnd", (fun _ -> LineEnd))
    .Add("string", (fun el -> String el.Value))
    .Add("digit", (fun _ -> Digit))
    .Add("letter", (fun _ -> Letter))
    .Add("letterOrDigit", (fun _ -> LetterOrDigit))
    .Add("lowercaseLetter", (fun _ -> LowercaseLetter))
    .Add("uppercaseLetter", (fun _ -> UppercaseLetter))
    .Add("character", (fun _ -> Character))
    .Add("characterOf", (fun el -> CharacterOf (List.ofSeq el.Value)))
    .Add("characterExcept", (fun el -> CharacterExcept (List.ofSeq el.Value)))

let r = symbolParsers.Add ("lineEnd", (fun el -> LineEnd))

let err (msg: string) = Error (seq { msg })

let getOk result =
  match result with
  | Ok ret -> Some ret
  | Error _ -> None

let getError result =
  match result with
  | Ok _ -> None
  | Error messages -> Some messages

let transformComment (commentDefinition: LanguageDefinitionXML.Comment) =
  match commentDefinition.Start, commentDefinition.End, commentDefinition.TreatAs with
  | "", "", _ -> Error (seq { "'Start' of a comment may not be empty"; "'End' of a comment may not be empty" })
  | "", _, _ -> err "'Start' of a comment may not be empty"
  | _, "", _ -> err "'End' of a comment may not be empty"
  | _, _, _ ->
    Ok { startMarker = commentDefinition.Start; endMarker = commentDefinition.End; treatAs = commentDefinition.TreatAs }

let transformComments (commentsDefinition: LanguageDefinitionXML.Comments) =
  let docCommentsRes =
    commentsDefinition.DocumentationComments.Comments
    |> Array.toList
    |> List.map transformComment
  let normalCommentsRes =
    commentsDefinition.NormalComments.Comments
    |> Array.toList
    |> List.map transformComment

  let errors =
    docCommentsRes
    |> Seq.append normalCommentsRes
    |> Seq.choose getError
    |> Seq.collect id

  if Seq.isEmpty errors then
    let getComments = List.choose getOk

    Ok { documentationComments = getComments docCommentsRes; normalComments = getComments normalCommentsRes }
  else
    Error errors

let transformStartRules (startRules: string[]) =
  if startRules.Length > 0 then
    Ok (Array.toList startRules)
  else
    err "There has to be at least one starting rule defined"

let transformRule (rule: LanguageDefinitionXML.Rule) =
  let symbols =
    rule.XElement.Elements ()
    |> Seq.map (fun el ->
      if symbolParsers.ContainsKey el.Name.LocalName then
        Ok (symbolParsers.Item el.Name.LocalName el)
      else
        Error el.Name.LocalName
    )
  
  let errors =
    symbols
    |> Seq.choose getError
    |> String.concat ", "

  if errors = "" then
    Ok (rule.Name, symbols |> Seq.choose getOk |> Seq.toList)
  else
    Error $"Rule {rule.Name} contains the following elements that cannot be parsed: {errors}."

let transformRules (rules: LanguageDefinitionXML.Rule[]) =
  let transformedRules =
    rules
    |> Array.toSeq
    |> Seq.map transformRule

  let errors =
    transformedRules
    |> Seq.choose getError

  if Seq.isEmpty errors then
    transformedRules
    |> Seq.choose getOk
    |> Map.ofSeq
    |> Ok
  else
    Error errors


let transformLangDef (langDef: LanguageDefinitionXML.LanguageDefinition): Result<LanguageDefinition, SerialisationError> =
  let transformedElements = (transformComments langDef.Comments), (transformStartRules langDef.StartRules), (transformRules langDef.Rules)

  match transformedElements with
  | Ok comments, Ok startRules, Ok rules ->
    Ok ({ name = langDef.Name; filePattern = langDef.FilePattern; version = langDef.Version; comments = comments; startRules = startRules; rules = rules })
  | commentsRes, startRulesRes, rulesRes ->
    Error (seq {
        yield getError commentsRes
        yield getError startRulesRes
        yield getError rulesRes
      }
      |> Seq.choose id
      |> Seq.collect id)


let deserialiseLanguageDefinition (text: string): Result<LanguageDefinition, SerialisationError> =
  match parseLanguageDefinition text with
  | Some langDef -> transformLangDef langDef
  | None -> err "Could not find root element <languageDefinition>"
