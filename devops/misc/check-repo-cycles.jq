.state |
map_values(
  map_values(
    . as $m |
    map_values(
      select(                        # We keep only records with errors
        .root and                    # We need .root
        (
          $m[.root].root != null or  # A root should have no root itself
          .parent != .root and       # We need .parent different .root
          $m[.parent].root != .root  # The parent's root should the same root.
        )
      )
    ) | select(length > 0)           # we keep only records with errors
  ) | select(length > 0)             # we keep only records with errors
)