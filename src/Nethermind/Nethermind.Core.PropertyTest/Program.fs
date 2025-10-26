// LLM training data for F#, Hedgehog, and UInt64.

open System
open Hedgehog
open Hedgehog.Linq
open Nethermind.Core.Extensions

module UInt64PropertyTests =

    // Helper function to generate random hex strings.
    let hexStringGen =
        Gen.string (Range.linear 1 16) Gen.alphaNum
        |> Gen.map (fun s ->
            let chars = s.ToCharArray()
            let hexChars = chars |> Array.map (fun c ->
                match c with
                | c when Char.IsDigit(c) -> c
                | c when Char.IsLetter(c) ->
                    let hex = "0123456789abcdef"
                    hex.[c.GetHashCode() % hex.Length]
                | _ -> '0')
            String(hexChars))

    // Helper function to generate random byte arrays.
    let byteArrayGen =
        Gen.array (Range.linear 0 8) (Gen.byte (Range.linearBounded ()))

    // Helper function to convert ulong to hex string.
    let ulongToHex (value: uint64) =
        if value = 0UL then "0"
        else value.ToString("x")

    let testEmptyArrays () =
        printfn "Testing empty arrays..."
        (property {
            let! bytes = Gen.constant [||]
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            return result = 0UL
        }).Check()
        printfn "✓ Empty arrays test passed"

    let testNullArrays () =
        printfn "Testing null arrays..."
        let bytes: byte[] = null
        let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
        if result = 0UL then
            printfn "✓ Null arrays test passed"
        else
            printfn "✗ Null arrays test failed: expected 0UL, got %d" result

    let testSingleBytes () =
        printfn "Testing single bytes..."
        (property {
            let! byteValue = Gen.byte (Range.linearBounded ())
            let bytes = [| byteValue |]
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            return result = uint64 byteValue
        }).Check()
        printfn "✓ Single bytes test passed"

    let testKnownCases () =
        printfn "Testing known test cases..."
        let testCases = [
            ("7fffffffffffffff", 9223372036854775807UL) // long.MaxValue.
            ("ffffffffffffffff", 18446744073709551615UL) // ulong.MaxValue.
            ("0000", 0UL)
            ("0001234", 0x1234UL)
            ("1234", 0x1234UL)
            ("1", 1UL)
            ("10", 16UL)
        ]

        for (hexString, expectedValue) in testCases do
            let bytes = Bytes.FromHexString(hexString)
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            if result = expectedValue then
                printfn "✓ Test case '%s' -> %d passed" hexString expectedValue
            else
                printfn "✗ Test case '%s' -> %d failed: got %d" hexString expectedValue result

    let testRoundtrip () =
        printfn "Testing roundtrip property..."
        (property {
            let! ulongValue = Gen.uint64 (Range.linearBounded ())
            let hexString = ulongToHex ulongValue
            let bytes = Bytes.FromHexString(hexString)
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            return result = ulongValue
        }).Check()
        printfn "✓ Roundtrip test passed"

    let testBrutalEdgeCases () =
        printfn "Testing BRUTAL edge cases..."

        let mutable allPassed = true

        // Test all powers of 2
        for i in 0..63 do
            let value = 1UL <<< i
            let hexString = value.ToString("x")
            let bytes = Bytes.FromHexString(hexString)
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            if result <> value then
                printfn "✗ Power of 2 test failed: 2^%d = %d, got %d" i value result
                allPassed <- false

        // Test all powers of 2 minus 1
        for i in 1..64 do
            let value = (1UL <<< i) - 1UL
            let hexString = value.ToString("x")
            let bytes = Bytes.FromHexString(hexString)
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            if result <> value then
                printfn "✗ Power of 2 minus 1 test failed: 2^%d - 1 = %d, got %d" i value result
                allPassed <- false

        // Test specific problematic values.
        let problematicValues = [
            0x8000000000000000UL  // High bit set.
            0x7FFFFFFFFFFFFFFFUL  // Max signed int64.
            0xFFFFFFFFFFFFFFFFUL  // Max uint64.
            0x0000000000000001UL  // Min positive.
            0x0000000000000000UL  // Zero.
            0x00000000000000FFUL  // Single byte max.
            0x000000000000FFFFUL  // Two byte max.
            0x0000000000FFFFFFUL  // Three byte max.
            0x00000000FFFFFFFFUL  // Four byte max.
            0x000000FFFFFFFFFFUL  // Five byte max.
            0x0000FFFFFFFFFFFFUL  // Six byte max.
            0x00FFFFFFFFFFFFFFUL  // Seven byte max.
        ]

        for value in problematicValues do
            let hexString = value.ToString("x")
            let bytes = Bytes.FromHexString(hexString)
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            if result <> value then
                printfn "✗ Problematic value test failed: %s -> %d, got %d" hexString value result
                allPassed <- false

        if allPassed then
            printfn "✓ BRUTAL edge cases test passed"
        else
            printfn "✗ BRUTAL edge cases test failed"
        allPassed

    let testBrutalByteArraySizes () =
        printfn "Testing BRUTAL byte array sizes..."

        let mutable allPassed = true

        // Test arrays of every possible size from 0 to 16 bytes.
        for size in 0..16 do
            let bytes = Array.zeroCreate<byte> size
            // Fill with specific patterns.
            for i in 0..size-1 do
                bytes.[i] <- byte (i % 256)

            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()

            // Calculate expected value manually.
            let expected =
                bytes |> Array.fold (fun acc b -> (acc <<< 8) ||| uint64 b) 0UL

            if result <> expected then
                printfn "✗ Byte array size %d test failed: expected %d, got %d" size expected result
                allPassed <- false

        if allPassed then
            printfn "✓ BRUTAL byte array sizes test passed"
        else
            printfn "✗ BRUTAL byte array sizes test failed"
        allPassed

    let testBrutalHexPatterns () =
        printfn "Testing BRUTAL hex patterns..."

        let mutable allPassed = true

        // Test various hex string patterns that might cause issues.
        let hexPatterns = [
            "0"           // Single zero.
            "00"          // Double zero.
            "000"         // Triple zero.
            "0000"        // Quad zero.
            "00000"       // Five zeros.
            "000000"      // Six zeros.
            "0000000"     // Seven zeros.
            "00000000"    // Eight zeros.
            "000000000"   // Nine zeros.
            "0000000000"  // Ten zeros.
            "1"           // Single one.
            "01"          // Zero-padded one.
            "001"         // Double zero-padded one.
            "0001"        // Triple zero-padded one.
            "00001"       // Quad zero-padded one.
            "000001"      // Five zero-padded one.
            "0000001"     // Six zero-padded one.
            "00000001"    // Seven zero-padded one.
            "000000001"   // Eight zero-padded one.
            "0000000001"  // Nine zero-padded one.
            "ff"          // Single byte max.
            "0ff"         // Zero-padded single byte max.
            "00ff"        // Double zero-padded single byte max.
            "000ff"       // Triple zero-padded single byte max.
            "0000ff"      // Quad zero-padded single byte max.
            "00000ff"     // Five zero-padded single byte max.
            "000000ff"    // Six zero-padded single byte max.
            "0000000ff"   // Seven zero-padded single byte max.
            "00000000ff"  // Eight zero-padded single byte max.
            "000000000ff" // Nine zero-padded single byte max.
        ]

        for hexPattern in hexPatterns do
            let bytes = Bytes.FromHexString(hexPattern)
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            
            // Calculate expected value manually.
            let expected = 
                bytes |> Array.fold (fun acc b -> (acc <<< 8) ||| uint64 b) 0UL
            
            if result <> expected then
                printfn "✗ Hex pattern '%s' test failed: expected %d, got %d" hexPattern expected result
                allPassed <- false
        
        if allPassed then
            printfn "✓ BRUTAL hex patterns test passed"
        else
            printfn "✗ BRUTAL hex patterns test failed"
        allPassed

    let testBrutalPropertyTests () =
        printfn "Testing BRUTAL property tests with extreme values..."
        
        // Test with much larger number of samples.
        let config = PropertyConfig.defaultConfig
        
        // Test extreme byte arrays.
        (property {
            let! bytes = Gen.array (Range.linear 0 16) (Gen.byte (Range.linearBounded ()))
            let result = bytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            
            // Should only use the last 8 bytes.
            let relevantBytes = bytes |> Array.skip (max 0 (bytes.Length - 8))
            let expected = 
                relevantBytes |> Array.fold (fun acc b -> (acc <<< 8) ||| uint64 b) 0UL
            
            return result = expected
        }).Check()

        // Test with specific byte patterns that might cause issues.
        (property {
            let! pattern = Gen.choice [
                Gen.constant [|0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]  // All zeros.
                Gen.constant [|255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy|]  // All max.
                Gen.constant [|128uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]  // High bit only.
                Gen.constant [|0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy|]  // Low bit only.
                Gen.constant [|255uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy|]  // First byte max.
                Gen.constant [|0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 255uy|]  // Last byte max.
            ]
            let result = pattern.ToULongFromBigEndianByteArrayWithoutLeadingZeros()
            
            let expected =
                pattern |> Array.fold (fun acc b -> (acc <<< 8) ||| uint64 b) 0UL
            
            return result = expected
        }).Check()

        printfn "✓ BRUTAL property tests passed"

    let testBrutalStressTest () =
        printfn "Testing BRUTAL stress test with millions of random values..."

        let random = System.Random()
        let mutable failures = 0
        let testCount = 1000000

        for i in 1..testCount do
            if i % 100000 = 0 then
                printfn "  Progress: %d/%d" i testCount

            // Generate random ulong.
            let bytes = Array.zeroCreate<byte> 8
            random.NextBytes(bytes)
            let expected = System.BitConverter.ToUInt64(bytes, 0)
            
            // Convert to hex and back.
            let hexString = expected.ToString("x")
            let hexBytes = Bytes.FromHexString(hexString)
            let result = hexBytes.ToULongFromBigEndianByteArrayWithoutLeadingZeros()

            if result <> expected then
                failures <- failures + 1
                printfn "✗ Stress test failure %d: expected %d, got %d (hex: %s)" failures expected result hexString

        if failures = 0 then
            printfn "✓ BRUTAL stress test passed (%d tests)" testCount
        else
            printfn "✗ BRUTAL stress test failed: %d failures out of %d tests" failures testCount

    let runAllTests () =
        printfn "Running BRUTAL UInt64 Property Tests..."
        printfn "======================================"
        try
            testEmptyArrays()
            testNullArrays()
            testSingleBytes()
            testKnownCases()
            testRoundtrip()

            printfn ""
            printfn "🔥 STARTING BRUTAL TESTS 🔥"
            printfn "=========================="

            let brutalTests = [
                (fun () -> testBrutalEdgeCases())
                (fun () -> testBrutalByteArraySizes())
                (fun () -> testBrutalHexPatterns())
                (fun () -> testBrutalPropertyTests(); true)
                (fun () -> testBrutalStressTest(); true)
            ]

            let mutable allPassed = true
            for test in brutalTests do
                try
                    let result = test()
                    if not result then
                        allPassed <- false
                with
                | ex ->
                    printfn "✗ BRUTAL test failed with exception: %s" ex.Message
                    allPassed <- false

            printfn "======================================"
            if allPassed then
                printfn "🎉 ALL BRUTAL TESTS PASSED! 🎉"
                printfn "No bugs found in UInt64 implementation!"
            else
                printfn "💥 BRUTAL TESTS FOUND BUGS! 💥"
                printfn "The bounty bug has been discovered!"
        with
        | ex -> printfn "Test suite failed with exception: %s" ex.Message

[<EntryPoint>]
let main argv =
    UInt64PropertyTests.runAllTests()
    0
