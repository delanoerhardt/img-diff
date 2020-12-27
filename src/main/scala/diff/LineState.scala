package diff

sealed trait LineState
case object LineUnchanged extends LineState
case object LineAdded extends LineState
case object LineRemoved extends LineState