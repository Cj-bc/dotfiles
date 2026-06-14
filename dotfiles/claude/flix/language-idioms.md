# Flix language idioms

Verified in practice (Flix 0.73.0).

- **Map literal:** `Map#{ "k" => v, ... }`; empty map `Map#{}` or
  `Map.empty()`. Maps are homogeneous in value type.
- **Importing enum cases:** `use Mod.EnumName.{CaseA, CaseB}` brings cases
  into scope; `use Mod.fnName` brings a function into scope.
- **Numeric literals:** suffixes pick the type — `42i64` (Int64), `42i32`,
  `123ii` (BigInt). Convert with `Int64.toBigDecimal(x)`, etc.
- **Testing:** functions annotated `@Test` returning `Unit \ Assert`.
  Assertions via the `Assert` module: `Assert.assertEq(expected = a, b)`,
  `Assert.assertTrue(b)`, `Assert.fail("msg")`. Run the whole suite with
  `flix test` (`flix build` to just compile) from the package root (the dir
  containing `flix.toml`).
- **Effect handlers (used heavily to mock in tests):**
  `run { ...effectful... } with handler SomeEffect { def op(args, k) = k(result) }`
  — `k` is the continuation; pass it the operation's result (often
  `k(Ok(...))` / `k(Err(...))`). Example: mocking an HTTP effect with
  `with handler Net.Http.Http { def request(_, k) = k(Ok(resp)) }`.
- **Mutable state:** `region rc { let r = Ref.fresh(rc, init); Ref.put(v, r);
  Ref.get(r) }` — refs live inside a `region` scope.
- **Result idioms:** `forM (x <- res1; y <- res2) yield ...` for monadic
  chaining over `Result`; pattern-match `case Ok(_)` / `case Err(_)`.
- **Package manifest:** `flix.toml` (TOML: `[package]`, `flix = "0.73.0"`).

See also `util-json.md` in this directory for the JSON stdlib.
