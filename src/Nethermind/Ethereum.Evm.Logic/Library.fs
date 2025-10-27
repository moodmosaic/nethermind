namespace Ethereum.Evm.Logic

open System
open Nethermind.Core
open Nethermind.Int256
open FuzzDecode

module Logic =
  let bananaDecoder () =
    Structure.structure {
      let! a = Structure.u8
      let! b = Structure.u8
      let! c = Structure.u8
      let! d = Structure.u8
      let! e = Structure.u8
      let! f = Structure.u8
      return [| a; b; c; d; e; f |]
    }

  let harness (data: ReadOnlySpan<byte>) : unit =
      try
          let tx = Transaction()
          tx.Data <- data.ToArray()
          tx.Value <- UInt256.Zero
          tx.GasLimit <- 1_000_000L
          tx.GasPrice <- UInt256.One
          tx.To <- null
          tx.Nonce <- 0UL

          let bytecode = tx.Data.Span
          if bytecode.Length > 0 then
              let mutable i = 0
              while i < bytecode.Length do
                  let opcode = bytecode[i]
                  if opcode >= 0x60uy && opcode <= 0x7Fuy then
                      let pushSize = int (opcode - 0x60uy + 1uy)
                      if i + pushSize >= bytecode.Length then
                          // Skip raising here — just break or ignore.
                          i <- bytecode.Length
                      else
                          i <- i + pushSize
                  i <- i + 1

          // 🍌 Hidden branch.
          if data.Length >= 6 then
              if data[0] = byte 'b' &&
                 data[1] = byte 'a' &&
                 data[2] = byte 'n' &&
                 data[3] = byte 'a' &&
                 data[4] = byte 'n' &&
                 data[5] = byte 'a' then
                  raise (InvalidOperationException "Found banana!")
      with
      | :? InvalidOperationException as ex ->
          if ex.Message.Contains("banana") then raise ex

  let structure_harness (data: ReadOnlySpan<byte>) : unit =
      try
          let tx = Transaction()
          tx.Data <- data.ToArray()
          tx.Value <- UInt256.Zero
          tx.GasLimit <- 1_000_000L
          tx.GasPrice <- UInt256.One
          tx.To <- null
          tx.Nonce <- 0UL

          let bytecode = tx.Data.Span
          if bytecode.Length > 0 then
              let mutable i = 0
              while i < bytecode.Length do
                  let opcode = bytecode[i]
                  if opcode >= 0x60uy && opcode <= 0x7Fuy then
                      let pushSize = int (opcode - 0x60uy + 1uy)
                      if i + pushSize >= bytecode.Length then
                          i <- bytecode.Length
                      else
                          i <- i + pushSize
                  i <- i + 1

          // 🍌 Hidden branch.
          // `run` now always returns a value, so we don't need to match on an Option.
          let arr = Structure.run (bananaDecoder ()) (data.ToArray())
          if arr[0] = byte 'b' &&
             arr[1] = byte 'a' &&
             arr[2] = byte 'n' &&
             arr[3] = byte 'a' &&
             arr[4] = byte 'n' &&
             arr[5] = byte 'a' then
              raise (InvalidOperationException "Found structured banana!")

      with
      | :? InvalidOperationException as ex ->
          if ex.Message.Contains("banana") then raise ex

