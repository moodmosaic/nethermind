namespace Ethereum.Evm.Logic

open System
open Nethermind.Core
open Nethermind.Core.Crypto
open Nethermind.Serialization.Rlp
open Nethermind.Int256

module Logic =
  // Roundtrip property (stream): decode -> encode -> decode -> encode must stabilize
  let tryRoundtripStream<'T> (bytes: byte[]) : unit =
    try
      let v1 = Rlp.Decode<'T>(bytes)
      let r1 = Rlp.Encode<'T>(v1)
      let v2 = Rlp.Decode<'T>(r1.Bytes)
      let r2 = Rlp.Encode<'T>(v2)
      if r1.Bytes <> r2.Bytes then
        raise (InvalidOperationException "FUZZ_RLP_ROUNDTRIP_MISMATCH")
    with
    | :? RlpLimitException -> ()
    | :? RlpException -> ()
    | :? ArgumentNullException -> ()
    | :? IndexOutOfRangeException -> ()
    | :? ArgumentOutOfRangeException -> ()
    | :? OverflowException -> ()
    | :? InvalidOperationException as ex when ex.Message.Contains("does not support decoding") -> ()

  // Roundtrip property (value): use ValueDecoderContext path
  let tryRoundtripValue<'T> (bytes: byte[]) : unit =
    try
      let mutable ctx1 = bytes.AsRlpValueContext()
      let v1 = Rlp.Decode<'T>(&ctx1)
      let r1 = Rlp.Encode<'T>(v1)
      let mutable ctx2 = r1.Bytes.AsRlpValueContext()
      let v2 = Rlp.Decode<'T>(&ctx2)
      let r2 = Rlp.Encode<'T>(v2)
      if r1.Bytes <> r2.Bytes then
        raise (InvalidOperationException "FUZZ_RLP_ROUNDTRIP_MISMATCH")
    with
    | :? RlpLimitException -> ()
    | :? RlpException -> ()
    | :? ArgumentNullException -> ()
    | :? IndexOutOfRangeException -> ()
    | :? ArgumentOutOfRangeException -> ()
    | :? OverflowException -> ()
  // Generate a malicious RLP that could cause buffer overflow
  let generateMaliciousRlp (data: byte[]) : byte[] =
    if data.Length < 2 then data
    else
      let result = Array.zeroCreate (min (data.Length * 2) 100000)
      let mutable pos = 0
      
      // Generate random RLP prefixes that could trigger vulnerabilities
      let rnd = Random()
      let seed = int data[0]
      
      match seed % 8 with
      | 0 ->
          // Test case 1: Very large length prefix (potential integer overflow)
          result[pos] <- 184uy // 0xB8 - long string with 1-byte length
          pos <- pos + 1
          if pos < result.Length then result[pos] <- 255uy
      | 1 ->
          // Test case 2: Invalid sequence prefix with huge length
          result[pos] <- 248uy // 0xF8 - long list with 1-byte length
          pos <- pos + 1
          if pos < result.Length then result[pos] <- 200uy
      | 2 ->
          // Test case 3: Nested sequences (stack overflow potential)
          let nestedDepth = min 200 (seed / 2)
          for i = 0 to nestedDepth - 1 do
            if pos < result.Length then
              result[pos] <- 192uy + byte (min (nestedDepth - i) 55)
              pos <- pos + 1
      | 3 ->
          // Test case 4: Very long string with malformed length
          result[pos] <- 185uy // 0xB9 - long string with 2-byte length
          pos <- pos + 1
          if pos < result.Length then
            result[pos] <- 255uy
            pos <- pos + 1
          if pos < result.Length then
            result[pos] <- 255uy
      | 4 ->
          // Test case 5: Invalid RLP encoding
          result[pos] <- 191uy // One byte over the limit
          pos <- pos + 1
      | 5 ->
          // Test case 6: Malformed encoding
          result[pos] <- 183uy // Long string with 0-byte length (invalid)
          pos <- pos + 1
      | 6 ->
          // Test case 7: Large sequence
          result[pos] <- 249uy // 0xF9 - long list with 2-byte length
          pos <- pos + 1
          if pos < result.Length then
            result[pos] <- byte (seed)
            pos <- pos + 1
          if pos < result.Length then
            result[pos] <- 128uy
      | _ ->
          // Test case 8: Bad prefix followed by data
          result[pos] <- 250uy // 0xFA - long list with 3-byte length
          pos <- pos + 1
          for i = 0 to 2 do
            if pos < result.Length then
              result[pos] <- byte (seed >>> (i * 8))
              pos <- pos + 1
      
      // Copy original data
      let copyLen = min (result.Length - pos) data.Length
      Array.blit data 0 result pos copyLen
      pos <- pos + copyLen
      
      Array.sub result 0 pos

  let harness (data: ReadOnlySpan<byte>) : unit =
      try
          let dataArray = data.ToArray()
          if dataArray.Length = 0 then () else
          // Test 0: Roundtrip encode/decode on core types
          tryRoundtripStream<byte[]> dataArray
          tryRoundtripValue<byte[]> dataArray
          tryRoundtripStream<string> dataArray
          tryRoundtripStream<int> dataArray
          tryRoundtripStream<int64> dataArray
          tryRoundtripStream<UInt256> dataArray
          tryRoundtripStream<Address> dataArray
          tryRoundtripStream<Hash256> dataArray
          tryRoundtripStream<Transaction> dataArray
          
          // Test 1: Decode as general RLP structure
          try
              let rlpStream = dataArray.AsRlpStream()
              
              // Try to peek at the structure
              if rlpStream.Length > 0 && rlpStream.Length < 1000000 then
                  let mutable pos = rlpStream.Position
                  let mutable iter = 0
                  while pos < rlpStream.Length && pos < 1000 && iter < 256 do
                      let nextLength = 
                          try
                              Some (rlpStream.PeekNextRlpLength())
                          with
                          | _ -> None
                      
                      match nextLength with
                      | Some len when len > 0 && len < 1000000 && pos + len <= rlpStream.Length ->
                          pos <- pos + len
                          rlpStream.Position <- pos
                      | _ -> pos <- rlpStream.Length
                      iter <- iter + 1
                  
                  let _ = rlpStream.PeekNumberOfItemsRemaining()
                  ()
          with
          | :? RlpException -> () // Expected for malformed RLP
          | :? IndexOutOfRangeException -> ()
          | :? ArgumentOutOfRangeException -> () // Expected for buffer overflows
          | :? OverflowException -> () // Expected for integer overflows
          | :? StackOverflowException as ex -> raise ex // This should not be silently caught
          
          // Test 2: Decode as Address (potential buffer overflow)
          try
              let valueContext = dataArray.AsRlpValueContext()
              if valueContext.Length > 0 && valueContext.Length < 10000 then
                  let _ = valueContext.DecodeAddress()
                  ()
          with
          | :? RlpException -> ()
          | :? IndexOutOfRangeException -> ()
          | :? ArgumentOutOfRangeException -> ()
          
          // Test 3: Decode integer arrays with potentially malicious lengths
          try
              if dataArray.Length > 2 then
                  let rlpStream = dataArray.AsRlpStream()
                  let array = rlpStream.DecodeInt()
                  let _ = rlpStream.DecodeLong()
                  let _ = rlpStream.DecodeULong()
                  ()
          with
          | :? RlpException -> ()
          | :? IndexOutOfRangeException -> ()
          | :? ArgumentOutOfRangeException -> ()
          | :? OverflowException -> ()
          
          // Test 4: Decode byte arrays with various limits
          try
              let valueContext = dataArray.AsRlpValueContext()
              if valueContext.Length > 0 then
                  let _ = valueContext.DecodeByteArray()
                  let _ = valueContext.DecodeByteArraySpan(RlpLimit.L32)
                  let _ = valueContext.DecodeByteArraySpan(RlpLimit.Bloom)
                  ()
          with
          | :? RlpLimitException -> ()
          | :? RlpException -> ()
          | :? IndexOutOfRangeException -> ()
          | :? ArgumentOutOfRangeException -> ()
          
          // Test 5: Generate malicious RLP and try to decode it
          let maliciousRlp = generateMaliciousRlp dataArray
          if maliciousRlp.Length > 0 && maliciousRlp.Length < 100000 then
              try
                  let maliciousStream = maliciousRlp.AsRlpStream()
                  let mutable guard = 0
                  while not maliciousStream.HasBeenRead && guard < 1024 do
                      let before = maliciousStream.Position
                      try
                          let _ = maliciousStream.PeekNextRlpLength()
                          maliciousStream.SkipItem()
                      with
                      | _ -> maliciousStream.Position <- maliciousStream.Length
                      let after = maliciousStream.Position
                      if after = before then
                          // ensure forward progress or bail out
                          maliciousStream.Position <- maliciousStream.Length
                      guard <- guard + 1
              with
              | :? RlpException -> ()
              | :? IndexOutOfRangeException -> ()
              | :? ArgumentOutOfRangeException -> ()
              | :? StackOverflowException as ex -> raise ex
              | :? OutOfMemoryException as ex -> raise ex
          
          // Test 6: Decode arrays with potentially huge counts
          try
              let rlpStream = dataArray.AsRlpStream()
              if rlpStream.Length > 0 then
                  let _ = rlpStream.DecodeByteArrays()
                  ()
          with
          | :? RlpLimitException -> ()
          | :? RlpException -> ()
          | :? IndexOutOfRangeException -> ()
          | :? ArgumentOutOfRangeException -> ()
          | :? OutOfMemoryException as ex -> raise ex

      with
      | :? StackOverflowException as ex -> 
          raise (InvalidOperationException("Stack overflow detected: " + ex.Message, ex))
      | :? OutOfMemoryException as ex -> 
          raise (InvalidOperationException("Out of memory detected: " + ex.Message, ex))

