namespace FuzzDecode

open System

/// Decoder<'T> consumes a byte array and may produce a value and remainder.
type Decoder<'T> = byte[] -> int -> int * Option<'T>

module Structure =

  /// Consume a single byte.
  let u8 : Decoder<byte> =
    fun data offset ->
      if offset < data.Length then
        let value = data[offset]
        (offset + 1, Some value)
      else
        (offset, None)

  /// Decode a bool (false for even byte, true for odd).
  let bool : Decoder<bool> =
    fun data offset ->
      let (next, v) = u8 data offset
      match v with
      | Some b -> (next, Some (b % 2uy = 1uy))
      | None -> (offset, None)

  /// Decode an int32 using 4 bytes, little endian.
  let int32 : Decoder<int32> =
    fun data offset ->
      if offset + 4 <= data.Length then
        let v =
          BitConverter.ToInt32(data, offset)
        (offset + 4, Some v)
      else
        (offset, None)

  /// Decode a printable ASCII character.
  let asciiChar : Decoder<char> =
    fun data offset ->
      let (next, v) = u8 data offset
      match v with
      | Some b when b >= 32uy && b <= 126uy ->
          (next, Some (char b))
      | _ -> (offset, None)

  /// Map over the output of a decoder.
  let map f (d: Decoder<'T>) : Decoder<'U> =
    fun data offset ->
      let (next, v) = d data offset
      match v with
      | Some x -> (next, Some (f x))
      | None -> (offset, None)

  /// Sequentially combine decoders.
  let bind (d: Decoder<'T>) (f: 'T -> Decoder<'U>) : Decoder<'U> =
    fun data offset ->
      let (next, v) = d data offset
      match v with
      | Some x -> f x data next
      | None -> (offset, None)

  /// Decoder computation expression builder.
  type StructureBuilder() =
    member _.Bind(d, f) = bind d f
    member _.Return(x) = fun _ offset -> (offset, Some x)
    member _.Zero() = fun _ offset -> (offset, None)
    member _.ReturnFrom(d) = d

  let structure = StructureBuilder()

  /// Run a decoder from the start of a byte array.
  let run (decoder: Decoder<'T>) (data: byte[]) : Option<'T> =
    let (_, result) = decoder data 0
    result
