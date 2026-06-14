# Flix `Util.Json` standard library

Verified against the Flix 0.73.0 stdlib source inside `flix.jar`
(`src/library/Util/Json*.flix`). The library does BOTH parsing and
serialization — it is not read-only.

- **Value type:** `pub enum Json` with cases `JObject(Map[String, Json])`,
  `JString(String)`, `JNumber(BigDecimal)`, `JBool(Bool)`,
  `JArray(Vector[Json])`, `JNull`. Import cases with
  `use Util.Json.Json.{JObject, JString, JNumber, JBool}`.
- **Serialize:** `Util.Json.toCompactString(j): String` (single line),
  `toPrettyString(indent, j)`, and the `ToString[Json]` instance
  (`toString` = compact). Numbers render via `BigDecimal.toPlainString`,
  so `JNumber(Int64.toBigDecimal(42i64))` → `"42"` (clean integer, not
  `"42.0"`). `JObject` keys are output in canonical/sorted order.
- **Parse:** `parse(s): Result[JsonError, Json]`;
  `decode(s): Result[JsonError, a] with FromJson[a]`.
- **Field access when decoding:** `decodeAtKey(key, json)` (required) and
  `decodeAtKeyOpt(key, json)` returning `Option`.
- **Traits:** `Util.Json.ToJson` (instances for String, Int8/16/32/64,
  BigInt, Bool, Option, Vector, List, Set, Map, tuples) and
  `Util.Json.FromJson`. `ToJson[Map[k,v]]` needs `ToString[k]` +
  `ToJson[v]` and is homogeneous — for an object mixing string and number
  values, build `JObject(Map#{...})` with explicit `JString`/`JNumber`
  constructors instead of routing through `ToJson[Map]`.

**Tip:** to find the real API of any Flix stdlib module, extract its source
from the compiler jar (e.g. nix-store path
`.../flix-0.73.0/share/java/flix/flix.jar`, member
`src/library/Util/Json.flix`) — far more reliable than guessing.
