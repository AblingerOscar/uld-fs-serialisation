namespace ULD.Fs.Serialisation

open ULD.Fs.DTOs
open System.IO
open ULD.Fs.Serialisation.Serialisers

module public ULDSerialser =
  
  type DeserialisationResult = Result<LanguageDefinition, SerialisationError>

  let deserialiseFromString(text: string): DeserialisationResult =
    deserialiseLanguageDefinition text

  let deserialiseFromFile(file: string): DeserialisationResult =
    try
      File.ReadLines file
      |> Seq.cast<string>
      |> Seq.fold (+) ""
      |> deserialiseFromString
    with
    | ex -> Error (seq { ex.Message })
