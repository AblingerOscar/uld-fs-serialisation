namespace ULD.Fs.Serialisation

open ULD.Fs.DTOs
open ULD.Fs.Serialisation.Serialisers

module public ULDSerialser =
  
  type DeserialisationResult = Result<LanguageDefinition, SerialisationError>

  let deserialiseFromString(text: string): DeserialisationResult =
    deserialiseLanguageDefinitionFromString text

  let deserialiseFromFile(file: string): DeserialisationResult =
    deserialiseLanguageDefinitionFromFile file
