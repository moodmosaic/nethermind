module Program

open System
open SharpFuzz
open Nethermind.Core
open Nethermind.Int256

let harness (data: ReadOnlySpan<byte>) : unit =
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
            raise (InvalidOperationException
                     "PUSH extends beyond bytecode.")
          i <- i + pushSize
        i <- i + 1

[<EntryPoint>]
let main _ =
    // SharpFuzz expects a real Main entrypoint here.
    Fuzzer.LibFuzzer.Run(fun data -> harness data)
    0
