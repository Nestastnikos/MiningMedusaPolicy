module Types
  module CommonTypes =
    type Nametype =
      | Parent
      | Other

    type Uid = Uid of string
    type FullPath = FullPath of string
    type Proctitle = Proctitle of string

    type ProcessIdentifier = Uid * Proctitle

    type Path = { FullPath: string; Segments: string list; Depth: int }

    type NametypeRaw = string
    type IndexRaw = string
