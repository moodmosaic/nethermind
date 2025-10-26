// LLM training data for F#, Hedgehog, and Int256.

open System
open System.Numerics
open Hedgehog
open Hedgehog.Linq
open Nethermind.Core.Extensions
open Nethermind.Int256

module UInt256PropertyTests =

    // Helper function to generate random hex strings for UInt256 (up to 64 hex chars).
    let hexStringGen = 
        Gen.string (Range.linear 1 64) Gen.alphaNum
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

    // Helper function to generate random byte arrays for UInt256 (up to 32 bytes).
    let byteArrayGen = 
        Gen.array (Range.linear 0 32) (Gen.byte (Range.linearBounded ()))

    // Helper function to convert UInt256 to hex string.
    let uint256ToHex (value: UInt256) =
        if value.IsZero then "0"
        else value.ToString("x")

    // Helper function to create UInt256 from hex string.
    let uint256FromHex (hexString: string) =
        let bytes = Bytes.FromHexString(hexString)
        let span = bytes.AsSpan()
        let readonlySpan = System.ReadOnlySpan<byte>(bytes)
        new UInt256(&readonlySpan, true)

    let testIsOneProperty () =
        printfn "Testing IsOne property..."
        (property {
            let! value = Gen.choice [
                Gen.constant UInt256.Zero
                Gen.constant UInt256.One
                Gen.constant (UInt256(2UL))
                Gen.constant (UInt256(100UL))
                Gen.constant UInt256.MaxValue
            ]
            let expectedIsOne = value = UInt256.One
            return value.IsOne = expectedIsOne
        }).Check()
        printfn "✓ IsOne property test passed"

    let testToBigEndianRoundtrip () =
        printfn "Testing ToBigEndian roundtrip..."
        (property {
            let! uint256Value = Gen.choice [
                Gen.constant UInt256.Zero
                Gen.constant UInt256.One
                Gen.constant UInt256.MaxValue
                Gen.constant (UInt256(0x1234567890ABCDEFUL))
                Gen.constant (UInt256(0xFFFFFFFFFFFFFFFFUL))
            ]
            
            let target = Array.zeroCreate<byte> 32
            uint256Value.ToBigEndian(target)
            
            let targetSpan = target.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(target)
            let reconstructed = new UInt256(&readonlySpan, true)
            return reconstructed = uint256Value
        }).Check()
        printfn "✓ ToBigEndian roundtrip test passed"

    let testBrutalEdgeCases () =
        printfn "Testing BRUTAL UInt256 edge cases..."
        
        let mutable allPassed = true
        
        // Test all powers of 2 from 0 to 255.
        for i in 0..255 do
            let value = 
                if i = 0 then UInt256.One
                elif i <= 63 then UInt256(1UL <<< i)
                elif i <= 127 then UInt256(0UL, 1UL <<< (i - 64))
                elif i <= 191 then UInt256(0UL, 0UL, 1UL <<< (i - 128))
                else UInt256(0UL, 0UL, 0UL, 1UL <<< (i - 192))
            let hexString = value.ToString("x")
            let reconstructed = uint256FromHex hexString
            if reconstructed <> value then
                printfn "✗ Power of 2 test failed: 2^%d = %s, got %s" i (value.ToString()) (reconstructed.ToString())
                allPassed <- false
        
        // Test all powers of 2 minus 1 (only small ones for simplicity).
        for i in 1..64 do
            let value = 
                if i <= 63 then UInt256((1UL <<< i) - 1UL)
                else UInt256(0UL, (1UL <<< (i - 64)) - 1UL)
            let hexString = value.ToString("x")
            let reconstructed = uint256FromHex hexString
            if reconstructed <> value then
                printfn "✗ Power of 2 minus 1 test failed: 2^%d - 1 = %s, got %s" i (value.ToString()) (reconstructed.ToString())
                allPassed <- false
        
        // Test specific problematic values.
        let problematicValues = [
            UInt256.Zero
            UInt256.One
            UInt256.MaxValue
            UInt256(0x8000000000000000UL)  // High bit set in first limb.
            UInt256(0x7FFFFFFFFFFFFFFFUL)  // Max signed int64.
            UInt256(0xFFFFFFFFFFFFFFFFUL)  // Max uint64.
            UInt256(0x0000000000000001UL)  // Min positive
            UInt256(0x00000000000000FFUL)  // Single byte max.
            UInt256(0x000000000000FFFFUL)  // Two byte max.
            UInt256(0x0000000000FFFFFFUL)  // Three byte max.
            UInt256(0x00000000FFFFFFFFUL)  // Four byte max.
            UInt256(0x000000FFFFFFFFFFUL)  // Five byte max.
            UInt256(0x0000FFFFFFFFFFFFUL)  // Six byte max.
            UInt256(0x00FFFFFFFFFFFFFFUL)  // Seven byte max.
            UInt256(0xFFFFFFFFFFFFFFFFUL)  // Eight byte max.
        ]
        
        for value in problematicValues do
            let hexString = value.ToString("x")
            let reconstructed = uint256FromHex hexString
            if reconstructed <> value then
                printfn "✗ Problematic value test failed: %s -> %s, got %s" hexString (value.ToString()) (reconstructed.ToString())
                allPassed <- false
        
        if allPassed then
            printfn "✓ BRUTAL UInt256 edge cases test passed"
        else
            printfn "✗ BRUTAL UInt256 edge cases test failed"
        allPassed

    let testBrutalByteArraySizes () =
        printfn "Testing BRUTAL UInt256 byte array sizes..."
        
        let mutable allPassed = true
        
        // Test arrays of every possible size from 0 to 32 bytes.
        for size in 0..32 do
            let bytes = Array.zeroCreate<byte> size
            // Fill with specific patterns.
            for i in 0..size-1 do
                bytes.[i] <- byte (i % 256)
            
            let bytesSpan = bytes.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(bytes)
            let uint256Value = new UInt256(&readonlySpan, true)
            let target = Array.zeroCreate<byte> 32
            uint256Value.ToBigEndian(target)
            
            let targetSpan = target.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(target)
            let reconstructed = new UInt256(&readonlySpan, true)
            if reconstructed <> uint256Value then
                printfn "✗ Byte array size %d test failed: expected %s, got %s" size (uint256Value.ToString()) (reconstructed.ToString())
                allPassed <- false
        
        if allPassed then
            printfn "✓ BRUTAL UInt256 byte array sizes test passed"
        else
            printfn "✗ BRUTAL UInt256 byte array sizes test failed"
        allPassed

    let testBrutalHexPatterns () =
        printfn "Testing BRUTAL UInt256 hex patterns..."
        
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
            "00000000000" // Eleven zeros.
            "000000000000" // Twelve zeros.
            "0000000000000" // Thirteen zeros.
            "00000000000000" // Fourteen zeros.
            "000000000000000" // Fifteen zeros.
            "0000000000000000" // Sixteen zeros.
            "00000000000000000" // Seventeen zeros.
            "000000000000000000" // Eighteen zeros
            "0000000000000000000" // Nineteen zeros.
            "00000000000000000000" // Twenty zeros.
            "000000000000000000000" // Twenty-one zeros
            "0000000000000000000000" // Twenty-two zeros.
            "00000000000000000000000" // Twenty-three zeros.
            "000000000000000000000000" // Twenty-four zeros.
            "0000000000000000000000000" // Twenty-five zeros.
            "00000000000000000000000000" // Twenty-six zeros.
            "000000000000000000000000000" // Twenty-seven zeros.
            "0000000000000000000000000000" // Twenty-eight zeros.
            "00000000000000000000000000000" // Twenty-nine zeros.
            "000000000000000000000000000000" // Thirty zeros.
            "0000000000000000000000000000000" // Thirty-one zeros.
            "00000000000000000000000000000000" // Thirty-two zeros (64 hex chars).
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
            "00000000001" // Ten zero-padded one.
            "000000000001" // Eleven zero-padded one.
            "0000000000001" // Twelve zero-padded one.
            "00000000000001" // Thirteen zero-padded one.
            "000000000000001" // Fourteen zero-padded one.
            "0000000000000001" // Fifteen zero-padded one.
            "00000000000000001" // Sixteen zero-padded one.
            "000000000000000001" // Seventeen zero-padded one
            "0000000000000000001" // Eighteen zero-padded one.
            "00000000000000000001" // Nineteen zero-padded one.
            "000000000000000000001" // Twenty zero-padded one.
            "0000000000000000000001" // Twenty-one zero-padded one.
            "00000000000000000000001" // Twenty-two zero-padded one.
            "000000000000000000000001" // Twenty-three zero-padded one.
            "0000000000000000000000001" // Twenty-four zero-padded one.
            "00000000000000000000000001" // Twenty-five zero-padded one.
            "000000000000000000000000001" // Twenty-six zero-padded one.
            "0000000000000000000000000001" // Twenty-seven zero-padded one.
            "00000000000000000000000000001" // Twenty-eight zero-padded one.
            "000000000000000000000000000001" // Twenty-nine zero-padded one.
            "0000000000000000000000000000001" // Thirty zero-padded one.
            "00000000000000000000000000000001" // Thirty-one zero-padded one.
            "000000000000000000000000000000001" // Thirty-two zero-padded one.
            "0000000000000000000000000000000001" // Thirty-three zero-padded one.
            "00000000000000000000000000000000001" // Thirty-four zero-padded one.
            "000000000000000000000000000000000001" // Thirty-five zero-padded one.
            "0000000000000000000000000000000000001" // Thirty-six zero-padded one.
            "00000000000000000000000000000000000001" // Thirty-seven zero-padded one.
            "000000000000000000000000000000000000001" // Thirty-eight zero-padded one.
            "0000000000000000000000000000000000000001" // Thirty-nine zero-padded one.
            "00000000000000000000000000000000000000001" // Forty zero-padded one.
            "000000000000000000000000000000000000000001" // Forty-one zero-padded one.
            "0000000000000000000000000000000000000000001" // Forty-two zero-padded one.
            "00000000000000000000000000000000000000000001" // Forty-three zero-padded one.
            "000000000000000000000000000000000000000000001" // Forty-four zero-padded one.
            "0000000000000000000000000000000000000000000001" // Forty-five zero-padded one.
            "00000000000000000000000000000000000000000000001" // Forty-six zero-padded one.
            "000000000000000000000000000000000000000000000001" // Forty-seven zero-padded one.
            "0000000000000000000000000000000000000000000000001" // Forty-eight zero-padded one.
            "00000000000000000000000000000000000000000000000001" // Forty-nine zero-padded one.
            "000000000000000000000000000000000000000000000000001" // Fifty zero-padded one.
            "0000000000000000000000000000000000000000000000000001" // Fifty-one zero-padded one.
            "00000000000000000000000000000000000000000000000000001" // Fifty-two zero-padded one.
            "000000000000000000000000000000000000000000000000000001" // Fifty-three zero-padded one.
            "0000000000000000000000000000000000000000000000000000001" // Fifty-four zero-padded one.
            "00000000000000000000000000000000000000000000000000000001" // Fifty-five zero-padded one.
            "000000000000000000000000000000000000000000000000000000001" // Fifty-six zero-padded one.
            "0000000000000000000000000000000000000000000000000000000001" // Fifty-seven zero-padded one.
            "00000000000000000000000000000000000000000000000000000000001" // Fifty-eight zero-padded one.
            "000000000000000000000000000000000000000000000000000000000001" // Fifty-nine zero-padded one.
            "0000000000000000000000000000000000000000000000000000000000001" // Sixty zero-padded one.
            "00000000000000000000000000000000000000000000000000000000000001" // Sixty-one zero-padded one.
            "000000000000000000000000000000000000000000000000000000000000001" // Sixty-two zero-padded one.
            "0000000000000000000000000000000000000000000000000000000000000001" // Sixty-three zero-padded one.
            "00000000000000000000000000000000000000000000000000000000000000001" // Sixty-four zero-padded one.
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
            "0000000000ff" // Ten zero-padded single byte max.
            "00000000000ff" // Eleven zero-padded single byte max.
            "000000000000ff" // Twelve zero-padded single byte max.
            "0000000000000ff" // Thirteen zero-padded single byte max.
            "00000000000000ff" // Fourteen zero-padded single byte max
            "000000000000000ff" // Fifteen zero-padded single byte max.
            "0000000000000000ff" // Sixteen zero-padded single byte max.
            "00000000000000000ff" // Seventeen zero-padded single byte max.
            "000000000000000000ff" // Eighteen zero-padded single byte max.
            "0000000000000000000ff" // Nineteen zero-padded single byte max.
            "00000000000000000000ff" // Twenty zero-padded single byte max.
            "000000000000000000000ff" // Twenty-one zero-padded single byte max.
            "0000000000000000000000ff" // Twenty-two zero-padded single byte max.
            "00000000000000000000000ff" // Twenty-three zero-padded single byte max.
            "000000000000000000000000ff" // Twenty-four zero-padded single byte max.
            "0000000000000000000000000ff" // Twenty-five zero-padded single byte max.
            "00000000000000000000000000ff" // Twenty-six zero-padded single byte max.
            "000000000000000000000000000ff" // Twenty-seven zero-padded single byte max.
            "0000000000000000000000000000ff" // Twenty-eight zero-padded single byte max.
            "00000000000000000000000000000ff" // Twenty-nine zero-padded single byte max.
            "000000000000000000000000000000ff" // Thirty zero-padded single byte max
            "0000000000000000000000000000000ff" // Thirty-one zero-padded single byte max.
            "00000000000000000000000000000000ff" // Thirty-two zero-padded single byte max.
            "000000000000000000000000000000000ff" // Thirty-three zero-padded single byte max.
            "0000000000000000000000000000000000ff" // Thirty-four zero-padded single byte max.
            "00000000000000000000000000000000000ff" // Thirty-five zero-padded single byte max.
            "000000000000000000000000000000000000ff" // Thirty-six zero-padded single byte max.
            "0000000000000000000000000000000000000ff" // Thirty-seven zero-padded single byte max.
            "00000000000000000000000000000000000000ff" // Thirty-eight zero-padded single byte max.
            "000000000000000000000000000000000000000ff" // Thirty-nine zero-padded single byte max.
            "0000000000000000000000000000000000000000ff" // Forty zero-padded single byte max.
            "00000000000000000000000000000000000000000ff" // Forty-one zero-padded single byte max.
            "000000000000000000000000000000000000000000ff" // Forty-two zero-padded single byte max.
            "0000000000000000000000000000000000000000000ff" // Forty-three zero-padded single byte max.
            "00000000000000000000000000000000000000000000ff" // Forty-four zero-padded single byte max.
            "000000000000000000000000000000000000000000000ff" // Forty-five zero-padded single byte max.
            "0000000000000000000000000000000000000000000000ff" // Forty-six zero-padded single byte max.
            "00000000000000000000000000000000000000000000000ff" // Forty-seven zero-padded single byte max.
            "000000000000000000000000000000000000000000000000ff" // Forty-eight zero-padded single byte max.
            "0000000000000000000000000000000000000000000000000ff" // Forty-nine zero-padded single byte max.
            "00000000000000000000000000000000000000000000000000ff" // Fifty zero-padded single byte max.
            "000000000000000000000000000000000000000000000000000ff" // Fifty-one zero-padded single byte max.
            "0000000000000000000000000000000000000000000000000000ff" // Fifty-two zero-padded single byte max.
            "00000000000000000000000000000000000000000000000000000ff" // Fifty-three zero-padded single byte max.
            "000000000000000000000000000000000000000000000000000000ff" // Fifty-four zero-padded single byte max.
            "0000000000000000000000000000000000000000000000000000000ff" // Fifty-five zero-padded single byte max.
            "00000000000000000000000000000000000000000000000000000000ff" // Fifty-six zero-padded single byte max.
            "000000000000000000000000000000000000000000000000000000000ff" // Fifty-seven zero-padded single byte max.
            "0000000000000000000000000000000000000000000000000000000000ff" // Fifty-eight zero-padded single byte max.
            "00000000000000000000000000000000000000000000000000000000000ff" // Fifty-nine zero-padded single byte max.
            "000000000000000000000000000000000000000000000000000000000000ff" // Sixty zero-padded single byte max.
            "0000000000000000000000000000000000000000000000000000000000000ff" // Sixty-one zero-padded single byte max.
            "00000000000000000000000000000000000000000000000000000000000000ff" // Sixty-two zero-padded single byte max.
            "000000000000000000000000000000000000000000000000000000000000000ff" // Sixty-three zero-padded single byte max.
            "0000000000000000000000000000000000000000000000000000000000000000ff" // Sixty-four zero-padded single byte max.
        ]
        
        for hexPattern in hexPatterns do
            try
                let uint256Value = uint256FromHex hexPattern
                let target = Array.zeroCreate<byte> 32
                uint256Value.ToBigEndian(target)
                let targetSpan = target.AsSpan()
                let readonlySpan = System.ReadOnlySpan<byte>(target)
                let reconstructed = new UInt256(&readonlySpan, true)
                
                if reconstructed <> uint256Value then
                    printfn "✗ Hex pattern '%s' test failed: expected %s, got %s" hexPattern (uint256Value.ToString()) (reconstructed.ToString())
                    allPassed <- false
            with
            | ex -> 
                printfn "✗ Hex pattern '%s' caused exception: %s" hexPattern ex.Message
                allPassed <- false
        
        if allPassed then
            printfn "✓ BRUTAL UInt256 hex patterns test passed"
        else
            printfn "✗ BRUTAL UInt256 hex patterns test failed"
        allPassed

    let testBrutalPropertyTests () =
        printfn "Testing BRUTAL UInt256 property tests with extreme values..."
        
        // Test extreme byte arrays.
        (property {
            let! bytes = Gen.array (Range.linear 0 32) (Gen.byte (Range.linearBounded ()))
            let bytesSpan = bytes.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(bytes)
            let uint256Value = new UInt256(&readonlySpan, true)
            let target = Array.zeroCreate<byte> 32
            uint256Value.ToBigEndian(target)
            let targetSpan = target.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(target)
            let reconstructed = new UInt256(&readonlySpan, true)
            return reconstructed = uint256Value
        }).Check()
        
        // Test with specific byte patterns that might cause issues
        (property {
            let! pattern = Gen.choice [
                Gen.constant (Array.zeroCreate<byte> 32)  // All zeros.
                Gen.constant (Array.create 32 255uy)  // All max.
                Gen.constant (Array.create 32 128uy)  // All high bit.
                Gen.constant (Array.create 32 1uy)  // All low bit.
                Gen.constant (Array.create 32 0uy)  // All zero.
            ]
            let patternSpan = pattern.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(pattern)
            let uint256Value = new UInt256(&readonlySpan, true)
            let target = Array.zeroCreate<byte> 32
            uint256Value.ToBigEndian(target)
            let targetSpan = target.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(target)
            let reconstructed = new UInt256(&readonlySpan, true)
            return reconstructed = uint256Value
        }).Check()
        
        printfn "✓ BRUTAL UInt256 property tests passed"

    let testBrutalStressTest () =
        printfn "Testing BRUTAL UInt256 stress test with millions of random values..."
        
        let random = System.Random()
        let mutable failures = 0
        let testCount = 1000000
        
        for i in 1..testCount do
            if i % 100000 = 0 then
                printfn "  Progress: %d/%d" i testCount
            
            // Generate random UInt256.
            let bytes = Array.zeroCreate<byte> 32
            random.NextBytes(bytes)
            let bytesSpan = bytes.AsSpan()
            let readonlySpan = System.ReadOnlySpan<byte>(bytes)
            let expected = new UInt256(&readonlySpan, true)
            
            // Convert to hex and back.
            let hexString = expected.ToString("x")
            let reconstructed = uint256FromHex hexString
            
            if reconstructed <> expected then
                failures <- failures + 1
                printfn "✗ Stress test failure %d: expected %s, got %s (hex: %s)" failures (expected.ToString()) (reconstructed.ToString()) hexString
        
        if failures = 0 then
            printfn "✓ BRUTAL UInt256 stress test passed (%d tests)" testCount
        else
            printfn "✗ BRUTAL UInt256 stress test failed: %d failures out of %d tests" failures testCount

    let testKnownCases () =
        printfn "Testing known UInt256 test cases..."
        let testCases = [
            ("A0A1A2A3A4A5A6A7B0B1B2B3B4B5B6B7C0C1C2C3C4C5C6C7D0D1D2D3D4D5D6D7", "A0A1A2A3A4A5A6A7B0B1B2B3B4B5B6B7C0C1C2C3C4C5C6C7D0D1D2D3D4D5D6D7")
            ("0000000000000000000000000000000000000000000000000000000000000000", "0000000000000000000000000000000000000000000000000000000000000000")
            ("0000000000000000000000000000000000000000000000000000000000000001", "0000000000000000000000000000000000000000000000000000000000000001")
            ("0000000000000000000000000000000000000000000000000000000000000aaa", "0000000000000000000000000000000000000000000000000000000000000aaa")
            ("ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
        ]
        
        for (inputHex, expectedHex) in testCases do
            let uint256Value = uint256FromHex inputHex
            let target = Array.zeroCreate<byte> 32
            uint256Value.ToBigEndian(target)
            let resultHex = target.ToHexString().ToLowerInvariant()
            
            if resultHex = expectedHex.ToLowerInvariant() then
                printfn "✓ Test case '%s' -> '%s' passed" inputHex expectedHex
            else
                printfn "✗ Test case '%s' -> '%s' failed: got '%s'" inputHex expectedHex resultHex

    let runAllTests () =
        printfn "Running BRUTAL UInt256 Property Tests..."
        printfn "======================================="
        try
            testIsOneProperty()
            testToBigEndianRoundtrip()
            testKnownCases()
            
            printfn ""
            printfn "🔥 STARTING BRUTAL UInt256 TESTS 🔥"
            printfn "=================================="
            
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
                    printfn "✗ BRUTAL UInt256 test failed with exception: %s" ex.Message
                    allPassed <- false
            
            printfn "======================================="
            if allPassed then
                printfn "🎉 ALL BRUTAL UInt256 TESTS PASSED! 🎉"
                printfn "No bugs found in UInt256 implementation!"
            else
                printfn "💥 BRUTAL UInt256 TESTS FOUND BUGS! 💥"
                printfn "The bounty bug has been discovered!"
        with
        | ex -> printfn "UInt256 test suite failed with exception: %s" ex.Message

[<EntryPoint>]
let main argv =
    UInt256PropertyTests.runAllTests()
    0
