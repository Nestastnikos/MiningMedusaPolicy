module Types
  module CommonTypes =
    type Nametype =
      | Parent
      | Other

    type Uid = string
    type FullPath = string
    type Proctitle = string

    type ProcessIdentifier = Uid * Proctitle

    type Path = { FullPath: string; Segments: string list; Depth: int }

    type NametypeRaw = string
    type IndexRaw = string
