namespace FuzzDecode

open System

/// Decoder<'T> consumes bytes from the input and *always* produces a value.
/// It returns the produced value and the new offset into the byte array.
type Decoder<'T> = byte[] -> int -> 'T * int

module Decoder =

  /// Consume a single byte. If input is exhausted, return a default (0).
  let u8 : Decoder<byte> =
    fun data offset ->
      if offset < data.Length then
        (data[offset], offset + 1)
      else
        (0uy, offset) // Always succeed, return default value.

  /// Decode a bool (false for even byte, true for odd).
  let bool : Decoder<bool> =
    fun data offset ->
      let (b, next) = u8 data offset
      (b % 2uy = 1uy, next)

  /// Decode an int32 using 4 bytes. Uses defaults if input is short.
  let int32 : Decoder<int32> =
    fun data offset ->
      // Create a buffer that's definitely 4 bytes long
      let buffer = Array.zeroCreate 4
      let bytesToCopy = min 4 (data.Length - offset)
      if bytesToCopy > 0 then
        Array.blit data offset buffer 0 bytesToCopy
      
      let v = BitConverter.ToInt32(buffer, 0)
      (v, offset + bytesToCopy)

  /// Decode a printable ASCII character. If not printable, wraps around.
  let asciiChar : Decoder<char> =
    fun data offset ->
      let (b, next) = u8 data offset
      // Map any byte to the printable range
      let printable = 32uy + (b % (127uy - 32uy))
      (char printable, next)

  /// Map over the output of a decoder.
  let map (f: 'T -> 'U) (d: Decoder<'T>) : Decoder<'U> =
    fun data offset ->
      let (v, next) = d data offset
      (f v, next)

  /// Sequentially combine decoders.
  let bind (d: Decoder<'T>) (f: 'T -> Decoder<'U>) : Decoder<'U> =
    fun data offset ->
      let (v, next) = d data offset
      f v data next

  /// Decoder computation expression builder.
  type DecoderBuilder() =
    member _.Bind(d, f) = bind d f
    member _.Return(x) = fun _ offset -> (x, offset)
    member _.ReturnFrom(d) = d

  let decode = DecoderBuilder()

  /// Run a decoder from the start of a byte array.
  let run (decoder: Decoder<'T>) (data: byte[]) : 'T =
    let (res, _) = decoder data 0
    res
