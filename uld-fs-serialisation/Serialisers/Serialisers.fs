namespace ULD.Fs.Serialisation

module private Serialisers =

  open ULD.Fs.DTOs

  type SerialisationError = string seq
  type LanguageDefinitionXML = FSharp.Data.XmlProvider<Schema = "https://raw.githubusercontent.com/AblingerOscar/uld-definition/main/v1.0.0-schema.xsd">

  let getAttribute (localName: string) (el: System.Xml.Linq.XElement) =
    el.Attributes()
    |> Seq.tryFind (fun attr -> attr.Name.LocalName = localName)
    |> Option.map (fun attr -> attr.Value)

  let symbolParsers =
    Map.empty<string, System.Xml.Linq.XElement -> SymbolDefinition>
      .Add("action", (fun el -> Action el.Value))
      .Add("nonTerminal", (fun el -> NonTerminal (getAttribute "referencedRule" el |> Option.get)))
      .Add("oneOf", (fun el ->
        let options =
          el.Elements ()
          |> Seq.map (fun optionEl -> optionEl.Value)
          |> Seq.toList
        let allowNone =
          getAttribute "allowNone" el
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

  let err (msg: string) = Error (seq { msg })

  let getOk result =
    match result with
    | Ok ret -> Some ret
    | Error _ -> None

  let getError result =
    match result with
    | Ok _ -> None
    | Error messages -> Some messages

  let transformComment (commentDefinition: LanguageDefinitionXML.Comment) = {
      start = commentDefinition.Start
      end' = commentDefinition.End
      treatAs = commentDefinition.TreatAs
    }

  let transformComments (commentsDefinition: LanguageDefinitionXML.Comments) =
    let docCommentsRes =
      commentsDefinition.DocumentationComments.Comments
      |> Array.toList
      |> List.map transformComment
    let normalCommentsRes =
      commentsDefinition.NormalComments.Comments
      |> Array.toList
      |> List.map transformComment

    {
      documentationComments = docCommentsRes
      normalComments = normalCommentsRes
    }

  let transformStartRules = Array.toList

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
    transformRules langDef.Rules
    |> Result.map (fun rules -> {
        name = langDef.Name
        filePattern = langDef.FilePattern
        version = langDef.Version
        comments = transformComments langDef.Comments
        startRules = transformStartRules langDef.StartRules
        rules = rules
      })

  let parseLanguageDefinition (text: string) =
    try
      let parsedDef = LanguageDefinitionXML.Parse text
      match parsedDef.LanguageDefinition with
      | Some langDef -> Ok langDef
      | None -> err "Could not find root element <languageDefinition>"
    with
      | :? System.Xml.XmlException as ex -> err ex.Message

  let deserialiseLanguageDefinition (text: string): Result<LanguageDefinition, SerialisationError> =
    parseLanguageDefinition text
    |> Result.bind transformLangDef
