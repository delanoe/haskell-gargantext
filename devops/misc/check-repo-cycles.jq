.state |
to_entries[] | .key as $ty     | .value |
to_entries[] | .key as $list   | .value as $m | .value |
("\($list): \($m|length) size" | debug) as $_ |
to_entries[] | .key as $ngram  | .value |
select(.root) | debug |
select(                        # We keep only records with errors
  .root and                    # We need .root
  (
    $m[.root].root != null or  # A root should have no root itself
    .parent != .root and       # We need .parent different .root
    $m[.parent].root != .root  # The parent's root should the same root.
  )
) |
{$ty, $list, $ngram, data: .}