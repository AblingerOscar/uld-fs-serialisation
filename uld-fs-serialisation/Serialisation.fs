module ULD.Fs.Serialisation

open DTOs
open System.IO
open ULD.Fs.Serialisation.Serialisers

let deserialiseFromString(text: string): Result<LanguageDefinition, SerialisationError> =
  deserialiseLanguageDefinition text

let deserialiseFromFile(file: string): Result<LanguageDefinition, SerialisationError> =
  try
    File.ReadLines file
    |> Seq.cast<string>
    |> Seq.fold (+) ""
    |> deserialiseFromString
  with
  | ex -> Error (seq { ex.Message })
